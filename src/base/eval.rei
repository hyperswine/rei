#*
    Interpret AST for consteval and AST/tree sitter interpretation
*#

// any incremental interpretation?

export default interpret_expr: (expr: Expr) -> ReiType<T*> {
    match expr {
        AnnotationExpr => {
            eprintln("Annotations are not supported in Rei")
        }
        VisExpr => {
            eprintln("Annotations are not supported in Rei")
        }
        CallExpr => {
            // setup stack and pass control to the code
            
        }
        OperatorExpr(operation) => {
            interpet_operator_expr(operation)
        }
    }
}

// All expressions return expressions. Until they get to bare functions that call OS services (including stack allocation, heap alloc, fs services, etc) or arithmetic
interpet_operator_expr: (operation: OperatorExpr) -> Expr? {
    let handle_arithmetic = (x, op, y) => {
        match op {
            Plus => {
                // x must impl trait Add<Y>
                x.add(y)
            }
        }
    }

    match operation {
        BinaryOp(op, lhs, rhs) => {
            // arithmetic
            interpret_arithmetic(lhs, op, rhs)?

            // assignment
            if op: interpret_assignment(lhs, rhs)?

            // misc. e.g. error propagation
            interpret_sugar(lhs, op, rhs)?
        }
        // UNARY OPS CAN BE PREFIX OR POSTFIX
        UnaryOp(op, expr, position) => {
            position is Prefix? interpret_unary_prefix_op(op, expr) : interpret_unary_postfix_op(op, expr)
        }
    }
}

interpret_unary_prefix_op: (op: UnaryOp, rhs: Expr, position: Position) {
    match op {
        Star => _
        Ampersand => _
        Dollar => _
        ExclamationMark => _
        QuestionMark => _
        PlusPlus => _
        MinusMinus => _
        Tilde => _
    }
}

interpret_unary_postfix_op: (op: UnaryOp, lhs: Expr, position: Position) {
    match op {
        Star => _
        Ampersand => _
        Dollar => _
        ExclamationMark => _
        QuestionMark => {
            // try to evaluate the lhs expression into a value (ref, pointer, value)

        }
        PlusPlus => _
        MinusMinus => _
        Tilde => _
    }
}

// core::types::Arithmetic impls most of the arithmetic
// maybe <X: Arithmetic<Y>, Y>
// the problem is that something like X.add(Y) may return Z
// maybe search for its return type before hand and return a Box<T> or fat pointer
interpret_arithmetic: [X, Y](lhs: X, op: ArithmeticOp, rhs: Y) ->  ArithmeticExpr? {
    // use reflection to see if lhs.add<y> exists
    let executor = lhs.reflect(ArithmeticOp<Y>)?
    executor(lhs, rhs)

    // I think you basically recursively call this and/or the main interpret expr?
    // uhh maybe if X and Y are both ReiType or data or complex then it could work
}

interpret_assignment: (lhs: Expr, rhs: Expr) {
    // expect an assignable value
    let val = get_value(Expr)
}

// Expressions always evaluate to a value, e.g. () or T()
// Wait... no, interpret_expr should return a type T (ReiType enum?)
get_value: (expr: Expr) {}

mut stack = Stack()

fn_call: (program_specification: Expr, args: Args) {
    // a is just an expr

    // whenever a function body defines a local var, it just gets pushed onto the stack as its working memory
    // stack.push()
    // maybe instead of having a fn_call, just push stuff onto memory when a var define is called!!
}
