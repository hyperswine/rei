#*
    Lower an Expression
*#

use Rust::cranelift::prelude::*
use super::expr::[Ident ReiType Expr]
use super::optimizer::*

export Symbol: BaseSymbol | ScopeSymbol

// reitype is a base feature
// nvm it seems, I seem to care about modular and ELEGANT programming

export BaseSymbol: {
    ident: Ident
    type: ReiType
    node: &Expr
}

export ScopeSymbol: {
    inner_scope: Box[Symbol]
}

find_symbol: (parent: _, ident: _, item_type: _) -> Expr | CompileError {}
    
# get type (ident?) of expr
eval_type: (expr: Expr) -> Ident? {}

/*
    Initial Lowering. Basically involves expanding elements to their complete form
    Only care about the "non overloadable operators in a non macro context"
*/
lower: (expr: Expr) -> Expr {
    match expr {
        Macro {

        }
        BinaryOp (lhs, op, rhs) {
            match op {
                Elvis {
                    // convert to an if statement? or jump to label?
                    // maybe just a generic condition?
                    Condition(cond=lhs, lhs, rhs)
                }
                // let someone else handle it
                _ => BinaryOp (lhs, op, rhs)
            }
        }
        UnaryOp (unary_type, op, expr) {
            match op {
                UnaryPrefixOp (op, expr) {
                    Exclamation {
                        // search for the neq method impl or derive
                        self.find_symbol(expr)
                    }
                }
                UnaryPostfixOp (expr, op) {}
            }
            
        }
    }
}

/*
    Optimising Pass
*/
optimize: (expr: Expr) -> Expr {
    match expr {
        ConditionGroup (expr) {
            // NOTE: expr actually gets matched to Vec<Condition> 
            let jump_fn = DirectJumpFn(conditions=expr)
        }
    }
}

/*
    Desugar Pass
*/
desugar: (expr: Expr) {}

/*
    Mostly to deal with operator expressions, binary and unary
    You should provide a list? No
    I think you should just use effects. Passing around a large set of state is ehh
*/
clarify: (expr: Expr) {
    match expr {
        UnaryOp (order, op, expr) {
            // maybe get the trait or fn for it
            // then get the impl

            match order {
                Prefix {
                    // naive, let base spec handle or pass in an array of UnaryOps
                    // let handler_symbol = UnaryOps[op]

                }
                Postfix {

                }
            }
        }
        BinaryOp (lhs, op, rhs) {

        }
    }
}

/*
    Lower further to IR
*/
// lower to phantasm sm
lower_sm: (expr: Expr) -> PhantasmSchematic | CompileError {
    match expr {
        // dont deal with Unary and Binary Ops here, just ignore them for later
        // UnaryOp (type, op, expr)
        // BinaryOp (lhs, op, rhs)
    }
}

// lower to phantasm risc
lower_risc: (, expr: Expr) {}

// lower to cranelift
lower_cranelift: (, expr: Expr) -> CraneliftSchematic | CompileError {}

/*
    Facts:
    - base provides mechanisms, not policies. Adding, subtracting, etc dont really make sense per se
    - maybe binary and unary operations using a specific operator doesnt make sense in base
    - effects seem to compose and maybe work pretty well with functional programming and incremental compile & propagation upwards
    - base functions seem to be better if you just used direct functions without any object based state like self, which can cause many modularity problems and elegance
    - maybe closures, oh maybe ask for the fn that impls it? ehh idk. clarify is the driver I think but it doesnt have to know everything, just helps with clarifying unknowns from exactly what you want, like a councillor
    - something about an enum mapping between ops and traits somewhere. But then also need to find that impl right there and then? seems to make sense to preprovide the mapping and clarify the impls on the fly?
*/

export CraneliftSchematic: CraneliftIR

# incremental compile expr
export compile_expr: (string: String) -> Vec[Instruction] {
    let expr = parse(LEXER.lex(file_contents)).expr()
    lower(expr)
}
