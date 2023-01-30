#*
    Contains useful primitives for writing low level software on supported platforms
*#

// you can also make CoreExpr: extend Expr

use base::expr::Expr

lower: (expr: Expr) {
    match expr {}
}

// how does a macro driver work? it should kind of just work I think, you should just call it on an fn
// macro_expand(driver, &expr)
// then it will try to find the exprs in the expr that match the order specified in driver

// complete homoiconicity
// always resolved and shadowed in this order: custom -> std -> core -> base
// rei reflection => the main expr() fn is able to run all registered functions (at link or compile time)
// through string to symbol lookup during late AST descent or later on

// some ops may be only unary some maybe only binary

/*
UnaryOperator: enum {
    Prefix: enum {
        Star: Deref
        Ampersand: Ref
        Tilde: BitwiseNot
    }
    Postfix: enum {
        QuestionMark: PropagateOk
        ExclamationMark: PropagateErr
        Ellipsis2: Range
        Ellipsis3: Variadic
    }
}
*/

// oh maybe the other way around? how to match a trait then? maybe use annotations?
// if impl for Bits, can use for Numeric and String

/*
BinaryOperator: enum {
    Star: Mul
    Plus: Add
    Minus: Sub
    ForwardSlash: Div

    Ampersand: BitwiseAnd
    VBar: BitwiseOr
    Superscript: BitwiseXor

    Gt: Gt
    Lt: Lt
    Gte: Gte
    Lte: Lte
    
    Equiv: Equiv

    Ellipsis2: Range
}
*/
