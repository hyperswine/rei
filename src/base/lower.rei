#*
    Lower an Expression
*#

use Rust::cranelift::prelude::*
use super::expr::[Ident ReiType Expr]
use super::optimizer::*

export Symbol: BaseSymbol | ScopeSymbol

// reitype is a base feature

export BaseSymbol: {
    ident: Ident
    type: ReiType
    node: &Expr
}

export ScopeSymbol: {
    inner_scope: Box[Symbol]
}

# A live lowerer of expressions and its upper cached symbol
export Lowerer: {
    root_symbol: Symbol

    find_symbol: (&self, parent: _, ident: _, item_type: _) -> Expr | CompileError {}
    
    # get type (ident?) of expr
    eval_type: (&mut self, expr: Expr) -> Ident? {}

    /*
        Initial Lowering. Basically involves expanding elements to their complete form
    */
    lower: (&mut self, expr: Expr) -> Expr {
        match expr {
            BinaryOp (lhs, op, rhs) {
                match op {
                    Elvis {
                        // convert to an if statement? or jump to label?
                        // maybe just a generic condition?
                        Condition(cond=lhs, lhs, rhs)
                    }
                }
            }
            // op expr
            UnaryPrefixOp (op, expr) {
                match op {
                    Exclamation {
                        // search for the neq method impl or derive
                        self.find_symbol(expr)
                    }
                }
            }
            UnaryPostfixOp (expr, op) {

            }
        }
    }

    /*
        Optimising Pass
    */
    optimize: (&mut self, expr: Expr) -> Expr {
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
    desugar: (&mut self) {
        // match specific expressions
    }

    /*
        Lower further to IR
    */

    // lower to phantasm sm
    lower_sm: (&mut self, expr: Expr) -> PhantasmSchematic | CompileError {
        match expr {
            BinaryOp (op, lhs, rhs) {
                // maybe yield op here?
                // match op {
                //     Add {
                //         // naive impl, hardcoupled. Instead, use an effect yield to get the required symbol from caller
                //         // let add_fn = self.find_symbol(parent=lower_sm(lhs), ident=Add, item_type=Fn) ?:
                //             // return CompileError("Couldn't find {ident}: impl Add...")
                //     }
                // }

                // find the fn expr to handle the lhs and rhs
                let operator_handler: &Expr = yield op
            }
        }
    }

    // lower to phantasm risc
    lower_risc: (&self, expr: Expr) {}

    // lower to cranelift
    lower_cranelift: (&self, expr: Expr) -> CraneliftSchematic | CompileError {}
}

export CraneliftSchematic: CraneliftIR

# incremental compile expr
export compile_expr: (string: String) -> Vec[Instruction] {
    let expr = parse(LEXER.lex(file_contents)).expr()
    lower(expr)
}
