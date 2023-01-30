#*
    Lower an Expression
*#

use Rust::cranelift::prelude::*
use super::expr::[Ident ReiType Expr]
use super::optimizer::*

Symbol: BaseSymbol | ScopeSymbol

// reitype is a base feature
// nvm it seems, I seem to care about modular and ELEGANT programming

BaseSymbol: {
    ident: Ident
    type: ReiType
    node: &Expr
}

ScopeSymbol: {
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
        // some are defined as A: Plus as well. Idk what to do
        BinaryOp (lhs, op, rhs) {
            match op {
                Plus {
                    // find the impl of X: { Plus: impl }
                    // basically if the symtab is good enough, it should be quite good?
                    // or require that def somewhere. If assumed to exist, can kinda continue. But we need the return type dont we? for type inference
                }
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

CraneliftSchematic: CraneliftIR

# incremental compile expr
compile_expr: (string: String) -> Vec[Instruction] {
    let expr = parse(LEXER.lex(file_contents)).expr()
    lower(expr)
}

/*
    Constant Evaluation of Expressions, including type inference and full value replacement
*/

// Any incremental interpretation? Interpret AST for consteval and AST/tree sitter interpretation

default interpret_expr: (expr: Expr) -> ReiType<T*> {
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

#*
    Complete Lifetime Elision (CLE)
*#

// ensure all borrows in a scope are valid, mostly to do with mutability
/*
let a = 3

let b = &a

{
    b //valid!

    let c = &a //valid!
}

let d = c //invalid, c does not exist in this scope

f: (a: &Int, b: &String) => ()

main: () {}
*/

elide_lt: () => ()

/*
    Possible Optimisations
*/

# always generate this so if you add more conds you can incrementally remake the hash
# maybe place this in expr or codegen or optimizer
DirectJumpFn: {
    a: u64
    b: u64

    // would prob be propagated to the lower IR or be constructed there? or just conditions here
    (conditions: Vec[Condition]) -> Self {
        let conds = conditions.map(next_cond => {
            // cond, lhs, rhs
            next_cond.cond
        })

        // basically generate a universal hash fn that maps to either lhs or rhs (just random labels) based on the hashed result
        let n_labels = conditions.len()

        // conds are boolean expressions? so either you do a full pattern match
        // or consider each result and multiplex them to a single bitwise function
        let m = n_labels
        let M = std::log2(m)
        
        // The scheme assumes the number of bins is a power of two, m = 2^M. Let w be the number of bits in a machine word
        let a, b = loop {
            // can also generate like 2-10 at once and check them all
            let a = std::random(Odd, l=1, r=2^64)
            let b = std::random(r=2^(64-M))
            // test for conflicts
            conds.map(x => (a*x+b)>>(w-M)).has_duplicates() ? (a, b) : break
        }

        // store a
        Self {a, b}
    }

    hash: (a: u64, x: u64, w: u64, M: u64) => (a*x) >> (w-M)
}
