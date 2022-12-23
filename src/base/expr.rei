#*
    Reusable Expressions
*#

use super::[Operator]

export Lhs: Expr
export Rhs: Expr

// in list-like scopes you dont need commas
// otherwise you do
// X: A, B
// X: (A B)

# Extendable by core and std. Or by another compiler suite/"builtin library"
export Expr: enum {
    # utf8 namespaced string
    IdentExpr: String
    # no namespacing, e.g. for general and sequential defs
    BareIdentExpr: String
    LiteralExpr: Numeric | BaseString

    # base defines a few operators it cares about
    OperatorExpr: enum {
        BinaryOp: (Lhs, Operator, Rhs)
        UnaryOp: (UnaryType, Operator, Expr)
        BareOp: (Operator)
    }

    # expr.expr
    FieldAccessExpr

    # {expr*}
    ScopeExpr
    # ident {expr}
    MacroScopeExpr
    # ident ident[generic_params?] scope_expr?
    MacroIdentExpr
    # (expr) to prevent ListExpr which takes (expr*)?
    ParenExpr

    // keyword expr?
    ReturnExpr
    YieldExpr
    // where expr
    WhereExpr

    # expr ? expr : expr
    TernaryExpr
    # expr ?: expr
    ElvisExpr

    # expr...
    VariadicExpr
    
    # (), basically paren expr but empty
    EmptyExpr

    # var.field
    InstanceFieldExpr
    # expr (args?). NOTE: can also be a macro. Since fns should be eagerly expanded, that is a thing
    CallExpr

    # [generic_item_expr*]
    GenericExpr
    # [expr*]
    ListExpr
    # [expr]
    IndexExpr
    // Array constructor
    // [expr; expr]
    // ArrayConstructorExpr

    # (let|const|mut)
    SequentialDef: (VarModifier, Box[Rhs])

    # static lhs = rhs. NOTE: lazy is a core:: definition
    StaticExpr: Box[Expr]

    GeneralDef: enum {
        # A: B
        Alias
        Callable
        Object: Complex | Enum
        # X: extend Y... including X: extend... which means X: extend Self
        Extend
        # X: trait...
        Trait
        # X: impl... or X: impl Add...
        Impl
        # Either scope or callable scope
        Macro
    }
}

Variable: {
    // either an object or something
    type_ident: Ident
}

// only self contained expressions
reorder_operations: (expr: Expr) -> Expr {
    mut res = Expr()

    // maybe you can just parse it...?
}

// RUST API
use Rust::reic::lex::LEXER

# reexport
export LEXER: ReiLexer

#*
    PARSING LOGIC to create the AST
*#

use core::expr::*
use std::expr::*

export ParserError: Token
export ParseRes: Expr | ParserError

/*
    ! the unwrap() is not as useful as providing a stronger error message and possible routes to recovery?
*/

Range: Range[Size]

// for each method, reset the peek counter on Error
implicit_peek_reset[T: Iter::Peek]: annotation (block: GeneralDef[T]) {
    block.exprs.for_each(expr => {
        match expr {
            Fn {
                // if method, then set peek = curr_index
                if Err => block.reset_peek()
            }
        }
    })
}

Parser: {
    // associated state / components
    tokens: Vec[(Token, Range)]
    curr_index: Size
    input_string: String
    peek_index: Size

    // priority slots for expressions
    low_prio: []
    med_prio: []
    high_prio: []

    // NOTE: the IDE should add : _ or : Vec<_>, String
    new: (tokens: _, input_string: _) -> Self => Self {tokens, curr_index: 0, input_string }

    // Parser API
    accept: (&mut self, token: Token) -> LexData? => _accept(self.curr_index) == token? LexData(token) : Err()

    expect: (&mut self, token: token) -> LexData => self.accept(token).unwrap()

    Iter::Peek: impl {
        reset_peek: (&mut self) => self.peek_index = self.curr_index

        peek: (&mut self, n: (Size > 0) = 1) -> Token => _accept(self.peek_index, n)
    }

    // more generic accept
    _accept: (index: &mut Index, increment: Int) -> Token {
        index += increment
        self.tokens[index].Token.unwrap_or(Token::EOF)
    }
}

// NOTE: default args and contracts work like arg_name: ArgType <modifiers> <contracts> <default>

// Base expressions
@implicit_peek_reset
Parser: extend {
    // doing it this way gives you maximum control and freedom
    expr: (&mut self) -> ParseRes {
        // must be in the order defined in the grammar
        self.object_decl()!
        self.call()!
    }

    // should be least priority
    variadic_expr: (&mut self) -> ParseRes {
        let expr = self.expr()?
        self.expect(Token::Variadic)

        Ok(Variadic(expr))
    }

    // low priority
    group_expr: (&mut self) -> ParseRes {
        self.accept(Token::LParen)?

        let expr = self.expr()

        self.expect(Token::RParen)

        Ok(Group(expr))
    }

    // fn or callable maybe
    callable_decl: (&mut self) -> ParseRes {
        tuple_decl()!

        let fn_type = self.accept(Token::Annotation)
        let fn_type = if !fn_type => self.accept(Token::Macro)
        let fn_type = if !fn_type => self.accept(Token::LParen)?

        let params = self.params()

        let ret_type_expr = self.ret_type_expr()

        Ok(GeneralDef::Callable(fn_type, params, ret_type_expr))
    }

    namespaced_type_expr: (&mut self) -> ParseRes {
        self.accept(Token::Identifier)?

        // while accept ::
        let ident_map = [while self.accept(Token::DoubleColon) => self.expect(Token::Identifier)]

        // NOTE: the constructor doesnt do anything fancy I think, maybe allow relative vs absolute addressing?
        Ok(NamespacedType(ident_map))
    }

    alias_obj: (&mut self) -> ParseRes => self.namespaced_type_expr()?

    primitive_expr: (&mut self) -> ParseRes => self.accept(Token::String)?: self.accept(Token::Numeric)?

    tuple_decl: (&mut self) -> ParseRes {
        self.accept(LParen)?
        let expr = self.expr().unwrap()
        self.expect(RParen)?

        Ok(Tuple(expr))
    }

    // note anon fns and objects should be declared with local (binop declares using the same obj types)
    object_decl: (&mut self) -> ParseRes {
        // get ident
        let ident = self.ident_expr()?

        // get generics
        let generics = self.generic_params()?

        self.expect(Token::Colon)

        // get object type
        let obj_type = self.obj_type_expr().unwrap()
        mut res = Object()

        //* the ?: operator auto unwraps the value
        res.body = self.scope_expr() ?: self.arrow_expr().unwrap()

        // objects can also have a default value
        let defaults = self.default_arg()

        res.obj_type = obj_type
        res.generics = generics
        res.ident = ident
        res.defaults = defaults
        
        Ok(res)
    }

    obj_type_expr: () -> ParseRes {
        alias_decl()!
        fn_decl()!
        enum_decl()!
        data_decl()!
        mod_decl()!
        trait_decl()!
        impl_decl()!
    }

    scope_expr: (&mut self) -> ParseRes {
        self.accept(Token::LBracket)

        // maybe list comprehension?
        // Scope([while let Ok expr = self.expr() => body.push(expr)])
        body = [while let Ok expr = self.expr() => body.push(expr)]

        self.expect(Token::RBracket)

        Ok(Scope(body))
    }

    binary_expr: (&mut self) -> ParserRes {
        // try to match a self contained expr (including literal or ident)
        let lhs = self.expr()?

        // maybe have a hash table Token: Fn and do a loop?
        // NOTE: a return in for_each returns from the current outer scope
        // BINARY_OPERATORS.first(b => self.accept(b))?
        // let bin_operator = BINARY_OPERATORS.first(b => self.accept(b)).unwrap()
        let bin_op = Operators[self.next()].unwrap()

        let rhs = self.expr().unwrap()

        bin_op(lhs, bin_op, rhs)
    }

    # find a unary expression with the operator before the payload
    unary_op_prefix: (&mut self) -> ParseRes {
        // first knows of Result<T, E>
        let unary_op = UnaryOps.first(u => self.accept(u))?
        Ok(Unary(Prefix, unary_op, self.expr()))
    }

    return_expr: (&mut self) -> ParseRes {
        self.accept(Token::Return)?
        Ok Return(self.expr().unwrap())
    }

    yield_expr: (&mut self) -> ParseRes {
        self.accept(Token::Yield)?
        Ok Yield(self.expr().unwrap())
    }

    break_expr: (&mut self) -> ParseRes => self.accept(Token::Break)?

    continue_expr: (&mut self) -> ParseRes => self.accept(Token::Continue)?

    where_expr: (&mut self) -> ParseRes {
        self.accept(Token::Where)?

        self.expect(LParen)

        let items = [while let res = self.where_item() => res]

        self.expect(RParen)

        Ok(Where(items))
    }

    where_item: (&mut self) -> ParseRes => self.condition_expr() ?: self.expr()?

    macro_invocation: (&mut self) -> ParseRes {
        // maybe have a procedural parser?
        let x = self.ident_expr()?
    }

    ident_expr: (&mut self) -> ParseRes {
        // note: a _ anywhere directly in a fn body means its unimplemented at that point, so if it gets there, it should panic
        // in a signature, it panics at compile time
        // _
        // find baseident :: baseident :: baseident, maybe thats possible directly?
        self.accept(Token::NSIdentifier)?
    }

    // is it possible to implicitly reset the peek counter per function on a return Error?
    // maybe just remember it on the base expr? no cause you need to call the next one right afterwards on the same expr
    // so reset it on Error or use effects or something. Or have an implicit way to track the peek counter per call with a marking or something
    // maybe a marker field like peek: Int
    // macro_expr: (&mut self) {
    //     // self.peek + 1
    //     let ident = self.ident_expr()?
    //     // check either scope or ident
    //     let ident2 = self.ident_expr()?
    // }
}

// BinaryOp: ()
// BinOpFn: (Expr, BinaryOperator, Expr) -> ()

// core::Index[T]
/*
BinaryOp: impl core::Index(token: Token) -> BinOpFn? {
    // either this or a fn annotation above binop_* but might not be as easy to understand?
    static const MAP = {
        // there may be specific binary operations based on the context?
        // hmm maybe do this during the descent?
        Plus = binop_plus
        Minus = binop_minus
    }
}
*/
