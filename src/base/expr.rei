#*
    Reusable Expressions
*#

use super::types::[Operator BinaryOperator UnaryOperator]

export Lhs: Expr
export Rhs: Expr

# Extendable by core and std. Or by another compiler suite/"builtin library"
export Expr: enum {
    # utf8 namespaced string
    IdentExpr: String
    # no namespacing, e.g. for general and sequential defs
    BareIdentExpr: String
    LiteralExpr: Numeric | BaseString

    OperatorExpr: enum {
        BinaryOp: (Lhs, BinaryOperator, Rhs)
        UnaryOp: (UnaryType, UnaryOperator, Rhs)
    }

    # {expr*}
    ScopeExpr
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
    # expr (args?)
    CallExpr

    # [generic_item_expr*]
    GenericExpr
    # [expr*]
    ListExpr
    # [expr]
    IndexExpr

    # (let|const|mut)
    SequentialDef: (VarModifier, Box[Rhs])

    # static lhs = rhs. NOTE: lazy is a core:: definition
    StaticExpr: Box[Expr]

    GeneralDef: enum {
        # A: B
        Alias
        Callable
        Object: Complex | Enum
        Extend
        Trait
        Impl
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
