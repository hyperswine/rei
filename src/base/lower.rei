#*
    Lower Expr
*#

use Rust::cranelift::prelude::*

export SymbolTable: {
    // item_type: Module | Item
    // Item: Variable | Fn | Object | Extension
    // list of sorted key: val entries of ident: (item_type, child_items: SymbolTable?)
}

# always generate this so if you add more conds you can incrementally remake the hash
DirectJumpFn: {
    a: u64

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
        let a = loop {
            // can also generate like 2-10 at once and check them all
            let a = std::random(Odd, l=1, r=2^64)
            let b = std::random(r=2^(64-M))
            // test for conflicts
            conds.map(x => (a*x+b)>>(w-M)).has_duplicates() ? a : break
        }

        // store a
        Self {a}
    }

    hash: (a: u64, x: u64, w: u64, M: u64) => (a*x) >> (w-M)
}

export Lowerer: {
    symtab: SymbolTable

    find_symbol: (&self, parent: _, ident: _, item_type: _) -> Expr | CompileError {}

    /*
        Initial Lowering. Basically involves expanding elements to their complete form
    */
    lower: (&mut self, expr: Expr) -> Expr {
        match expr {
            BinaryOp (op, lhs, rhs) {
                match op {
                    Elvis {
                        // convert to an if statement? or jump to label?
                        // maybe just a generic condition?
                        Condition(cond=lhs, lhs, rhs)
                    }
                }
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
                match op {
                    Add {
                        // request the Add trait from symtab? use effects in the form of propagation?
                        // maybe the ident and type inference needs separate? instead of the same fn?
                        // NOTE: the grammar and parser kind of just works in that if there is an error with one of the fns, it will propagate an Err
                        let add_fn = self.find_symbol(parent=lower_sm(lhs), ident=Add, item_type=Fn) ?:
                            return CompileError("Couldn't find {ident}: impl Add...")

                        // return the lhs.add(rhs)?
                        // or call Add?
                        // wait what about the thing
                        // should we build another thing while we're at it?
                        // or should that be built prior?
                        // hmm maybe that should be built prior

                        // so x::X::Add actually becomes x_X_Add
                        // and the op actually becomes x_X_Add x y
                        // push x
                        // push y
                        // call x_X_Add
                    }
                }
            }
        }
    }

    // lower to phantasm risc
    lower_risc: (&self, expr: Expr) {}

    // lower to cranelift
    lower_cranelift: (&self, expr: Expr) -> CraneliftSchematic | CompileError {}
}

export CraneliftSchematic: CraneliftIR

// call expr or parenthesis expr?
// a parenthesis with comma , should be a call right?
// an empty parenthesis by itself should be empty
// a parenthesis right after an ident may be a call, for example
// x() is a call
// x () is not a call! but rather ident empty

/*
classes of operators and operations

binary op (=, x, 5)
=, ident, literal
x = 5

binary op (/, binop_child, 5)
x + 5 / 5
binop_child (+, x, 5)

unary op (postfix, ?, x)
x?

unary op (postfix, !, x::y)
!x::y

so how is used and unused?

var_def (immutable, x, rhs)
let x = f()?
rhs unary op (postfix, call_expr(f, empty), ?)
f()?

var_def (immutable, x, rhs)
let x = f() ?: g
rhs elvis (lhs, rhs)
lhs call_expr(f, empty)
rhs ident

so in order:
let mut const

"DEFINITION"
:
=

[expr]
{expr}
(expr)
!expr ?expr
expr! expr?
~
$

"BINARY"
& | ^ and &= |= ^=
/ * % and /= *= %=
- + and -= +=

OVERLOADED?

[] can mean array context or generic context
array: [expr1], [expr1..expr2] where expr generally resolve to numeric
generic: [expr1], [expr1, expr2, expr3] where expr can be many things

# should mean array
let x = []

# should mean generic
let x = T[x]()

# should be generic
X[T]: Y[T]

# should mean array
x[3]
let g = x[len()]
let g = x[0..s].for_each(val => ())

so generic can only be used in contexts like
universal definition or local definition
ident brackets expr call
ident brackets expr

an array can only be used like
ident brackets expr
ident brackets

in a local context, generics are usually bound by calls like T[x]()
in a global context, generics are usually bound to definitions

arrays can also be used in global or local contexts

maybe allow operator precedence to be defined?
ehh nah
*/
