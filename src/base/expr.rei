#*
    Reusable Expressions
*#

use super::[Operator]

Lhs: Expr
Rhs: Expr

// in list-like scopes you dont need commas
// otherwise you do
// X: A, B
// X: (A B)

# Extendable by core and std. Or by another compiler suite/"builtin library"
Expr: enum {
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

Variable: Ident

#*
    PARSING
*#

// use core::expr::*
// use std::expr::*

ParserError: Token | String
ParseRes: Expr | ParserError

// great idea, just make a new function like split_first: (self, String) -> (String, String)
// and Char is castable to String, and the other way around if len is 0, otherwise UB?
// maybe overload the function to match String; 1

char: (input: String, to_match: Char) -> Parser[CharUtf8, String] => input.split_first(to_match)
string: (input: String, to_match: String) -> Parser[String, String] => input.split_first(to_match)

one_of: (input: String, to_match: [String]) -> Parser[_] => _

// 1+
many: (input: String) => _
// does this work?
many: (self) => many

// 0*
zero_or_many: (input: String) => _

// consumes 1 of by on either side
padded_by: (input: String, to_match: String, by: String) => _

// consumes all of by on either side
padded_by_many: (input: String, to_match: String, by: String) => _

ws: " \t\n"
hs: " \t"

// here, then is just a function rather than a trait
then: (f: Parser) -> Parser[_] => _
// left associative
(~): Unary(then, Postfix, precedence=Precedence::Low)

// idk actually
then_hs: _
then_ws: _
// always consume all
then_maybe_hs: _
then_maybe_ws: _

// (>>): Unary(then, Postfix, precedence=Precedence::Low)

// e.g. many(ws)

// EXPRS

// many_ws: many(ws)
// many_hs: many(hs)
// padded_by_hs: padded_by_many(hs)
// padded_by_ws: padded_by_many(ws)

// or has a high priority in expr and is left associative with early cutoff
expr: (input: String) -> Expr => def() or context()

def: (input: String) -> Parser[Def] {
    raw_ident(input)
    .then_maybe_hs(generic_param_expr)
    .then_maybe_ws(colon)
    .then_ws(def_body)
}

def_body: (input: String) -> Parser[DefBody] => parameterised or algebraic_expr or replace_expr

parameterised: (String) => paren_param_list($1).many().
