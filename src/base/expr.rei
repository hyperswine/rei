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

Variable: Ident

#*
    PARSING
*#

// use core::expr::*
// use std::expr::*

export ParserError: Token | String
export ParseRes: Expr | ParserError

// great idea, just make a new function like split_first: (self, String) -> (String, String)
// and Char is castable to String, and the other way around if len is 0, otherwise UB?
// maybe overload the function to match String; 1

char: (input: String, to_match: Char) -> (CharUtf8, String) => input.split_first(to_match)
string: (input: String, to_match: String) -> (String, String) => input.split_first(to_match)

one_of: _

many: _
