#*
    Reusable Expressions
*#

use super::types::[Operator]

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
