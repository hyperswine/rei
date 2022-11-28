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

// ! seems like this version of lowering involves some form of marking as unknowns, basically replacing it with a fn call
// and something to do with knowing about macros and operators
// maybe core::lower should implement these instead?

/*
    Initial Lowering. Basically involves expanding elements to their complete form
    Only care about the "non overloadable operators in a non macro context"
*/
lower: (expr: Expr) -> Expr {
    match expr {
        GeneralDef {
            match expr {
                Macro {
                    // ...take each case and keep that as a driver somewhere like macro_driver or macro_expand
                    // then call that for ident expressions that match it
                    // does base know how to handle that? maybe it doesnt at first? or it doesnt try to lower everything too hard just incase
                    // some exprs are macro calls. Maybe build macro into core lib instead? uhh
                    // wait so if you do something like ident ...stuff that might mean a macro
                    // maybe all macros need to be invoked like fns?
                    // yea like rust
                    // so

                    // ident (expr...) or ident {expr...} might mean a macro or something else, maybe we can change it to arcen(...)?
                    // so you need to find the fn then to parse the invocation properly...?

                    // so when you have something like expr {...} that could mean a whole bunch of things
                    // depending on the first expr
                    // uhh I can see a whole bunch of problems with that. So maybe in another pass like a macro pass in base?
                    // should macro pass go first? uhh no

                    // so a macro expr should have () {} or {} {} conditions
                    // macro invocations through macro_name (e1, e2 e3 - e4) or macro_name {+expr, -expr e3 e5}
                    // are generally parseable in base if their identifiers dont conflict with base' modifiers

                    // exprs in a (paren body) or {scope body} allow literals, idents, unary and binary operators
                    // maybe we also allow single_op expressions? in parenthesis and scopes
                    // because thats better
                }
            }
        }
        BinaryOp (lhs, op, rhs) {
            match op {
                // maybe just parse these directly?
                Elvis {
                    // convert to an if statement? or jump to label?
                    // maybe just a generic condition?
                    let cond = lower(lhs)
                    // kind of have to make sure it doesnt double compute
                    Condition(cond, cond, rhs)
                }
                Ellipsis2 {
                    Range(lower(expr), lower(expr))
                }
                // let someone else handle it
                _ => BinaryOp (lhs, op, rhs)
            }
        }
        UnaryOp (unary_type, op, expr) {
            match op {
                Prefix {
                    // means PartialEq::neq
                    // Exclamation {}
                    Ellipsis2 {
                        // 0..this numeric val
                        // maybe reinterpret it as that?
                        Range(BASE_NUMERIC_VALUE, lower(expr))
                    }
                }
                Postfix {
                    // propagate expr as primary type of a binary type
                    QuestionMark {
                        // the inference should be able to detect it if its the same type, or wrap an Ok/Err on core
                        Return(expr)
                    }
                    // propagate expr as secondary type of a binary type
                    Exclamation {
                        Return(expr)
                    }
                }
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
