#*
    Reusable Expressions
*#

use super::types::[Operator]

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
        BinaryOp: (Lhs, Operator, Rhs)
        UnaryOp: (UnaryType, Operator, Rhs)
    }

    # expr.expr
    FieldAccessExpr

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
