#*
    Lower Expr
*#

lower_expr: (expr: Expr) {
    match expr {
        BinaryOp(op, lhs, rhs) => {
            match op {
                
            }
        }
    }
}

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
