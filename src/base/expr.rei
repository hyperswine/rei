#*
    Reusable Expressions
*#

export Lhs: Expr
export Rhs: Expr

# Extendable by core and std. Or by another compiler suite/"builtin library"
export Expr: enum {
    AnnotationExpr
    VisExpr

    CallExpr
    MethodCallExpr
    OperatorExpr: enum {
        BinaryOp: (BinaryOperator, Lhs, Rhs)
        UnaryOp: (UnaryOperator, Rhs)
    }
    BlockExpr
    GroupExpr
    
    ReturnExpr
    YieldExpr

    TernaryExpr
    WhereExpr
    VariadicExpr
    AnonFnExpr
    EmptyExpr

    Mod
    Fn
    Object
    Trait
    Impl
    Type
}

// LOWERING EXPR's to make them more verbose at first or just to make them closer to phantasm

ComputeBlock: {
    instructions: Vec<IRInstruction>
}

// shouldnt you traverse the tree and build a new tree based on the expr
// and type inference?

// numerics or string
lower: (expr: PrimitiveExpr) -> IRInstruction {
    // lower and return
    match expr {
        Bits => ()
        Numeric => ()
        String => ()
    }
}

lower: (expr: IdentExpr) {}

Variable: {
    // either an object or something
    type_ident: Ident
}

// only self contained expressions
reorder_operations: (expr: Expr) -> Expr {
    mut res = Expr()

    // maybe you can just parse it...?
}

// wait wait... some operators can be unary some can be binary

# Not ordered, simply the mechanism, no policy
export Operator: enum {
    Tilde: "~"
    ExclamationMark
    QuestionMark
    Ampersand: "&"
    Superscript: "^"
    Backtick
    At: "@"
    
    VBar: "|"
    DoubleVBar: "||"
    DollarSign
    PercentSign

    Plus
    Minus
    Star
    ForwardSlash: "/"
    BackSlash: "\\"

    Gt: ">"
    Gte: ">="
    Lt
    Lte
    Equals
    Equivalent: "=="

    Colon: ":"
    Comma: ","
    DoubleColon: "::"
    SemiColon: ";"

    Ellipsis2: ".."
    Ellipsis3: "..."
}

use Rust::reic::lex::LEXER

# reexport
export LEXER: ReiLexer

# incremental compile expr
export compile_expr: (string: String) -> Vec[Instruction] {
    let expr = parse(LEXER.lex(file_contents)).expr()
    lower(expr)
}
