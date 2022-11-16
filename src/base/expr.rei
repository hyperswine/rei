// EXPRESSIONS

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
        UnaryOp: (BinaryOperator, Rhs)
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

UNARY_OP_PRECENDENCE: [QuestionMark ExclamationMark Star Ampersand]
BITWISE_OP_PRECEDENCE: [Not And Or XOr]
OP_PRECEDENCE: [Paren BITWISE_OP_PRECEDENCE Star LeftSlash Modulo Plus Minus]

// only self contained expressions
reorder_operations: (expr: Expr) -> Expr {
    mut res = Expr()

    // maybe you can just parse it...?
}
