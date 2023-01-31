---
layout: default
title: Grammar
---

## Base Language Specification

Expressions, expressions, expressions.

```rust
// ' ' means any whitespace. Means follow
// ~ means horizontal whitespace. Means directly follow

// expressions followed by any whitespace
rei: expr+

expr: def_expr | context_expr | misc_expr

// in most cases, def expressions should just work, except within idk..?
def_expr: general_def
// operations, calls
context_expr: operator_expr | var_def_expr | call_expr
misc_expr: paren_expr | scope_expr | expr_list | literal_expr

// in a bunch of cases, might not be sensible
expr_list: expr ~ (expr ~ ",")+

all_expr: 
    | paren_expr
    | empty_expr
    // | bracket_list_expr
    | unary_op
    | binary_op
    | ternary_op
    | general_def
    | scope_expr
    | modified_scope_expr
    | var_def_expr
    | literal_expr
    | ident_expr
    | ident_list_expr
    // | comma_expr

// GENERAL DEF
general_def: raw_ident generic_param_expr? ":" general_def_type
general_def_type: parameterised_expr | ("extend"? algebraic_expr) | mod_scope | replace_expr
generic_param_expr: "[" generic_param* "]"
generic_param: (raw_ident ~ ":")? ident impl_expr?

// ALGEBRAIC
algebraic_expr: alias_expr | enum_expr | product_expr
alias_expr: ident
enum_expr: ("enum" scope_expr) | (ident ("|" ident)+)

// PARAMETERISED
parameterised_expr: paren_param_list+ ret_type? (eval_expr | scope_expr)
paren_param_list: "(" param ("," param)* ")"
param: raw_ident type_expr? refinements?
type_expr: ":" ident arg_expr?
refinements: refinement ("," refinement)*
refinement: unary_refinement_op ident_or_literal

// PARAM can be either an anonymous param without a label
// (Int)
// ((Int) -> ())
// ((String, String) -> Int < 5)
// or it could have a label

impl_expr: "impl" ident ("+" ident)*

// dont try to flesh it out too hard
replace_expr: "replace" parameterised_expr | scope_expr

// generally not a definition expression, but a context expr?
// expr = def_expr | context_expr
call_expr: expr ~ "(" comma_expr ")"

// maybe a self contained expression?
// cant encode 1 equals to, mutual exclusion with default call

raw_ident: pub_ident | priv_ident
priv_ident: "_"pub_ident
pub_ident: "[a-zA-Z][\w\d_]"

ident_or_literal: ident | literal

ident: namespaced_ident
namespaced_ident: raw_ident ("::"raw_ident)*

literal_expr: numeric | string

eval_expr: "=>" expr

comma_expr[expr]: expr ~ ("," expr)* ","?
empty_expr: "(" ")"
bracket_expr: "[" expr "]"
bracket_list_expr: bracket_expr
var_def_expr: "let" | "mut" | "const" ident "=" expr
ternary_op: (expr ~ "?" ~ expr ":" expr) | (expr ~ "?:" expr)

keywords: "self" | "Self" | "return" | "async" | "await" | "yield" | "export" | "mod" | "trait" | "impl" | "deref" | "ref"

// OPERATORS
// increasing order of precedence
binary_op: expr ~ binary_operator ~ expr
binary_operator: "&" | "|" | "^" | "*" | "/" | "+" | "-" | "==" | "="
postfix_unary_operator: "?" | "!"
prefix_unary_operator: "~" | "*" | "&"
unary_refinement_op: binary_operator
self_op: "."

refinement_expr: expr

// MACROS
scope_expr: "{" expr* "}"
modified_scope_expr: m1_expr | m2_expr
m1_expr: raw_ident ~ scope_expr
m2_expr: raw_ident ~ macro_params? scope_expr
```

## Examples

```rust
1 + 1 * 3

1 * *3 + 1
// 1 parsed as numeric
// 1 * rhs parsed as binary op

*3 + 1
// *3 parsed as unary prefix
// + 1

* 3 + 1

// could either mean deref <expr>
// or mult <expr>

f: (x: Int)
// the following two are equivalent. If you have Int() and = at once, that is an error
// or maybe just x: Int = Int(5)
// depends on style guide and styler
f: (x: Int(5))
f: (x: Int = 5)
f: (x: Int < 5)
f: (x: Int < 5, < 10, = 10)

f: (x: Int < 5, < 10, y: String = "f")
f: (x: Int lessthan(5), y: String)

// means op 3
f: (Int) => + 3
// means 3, input is irrelevant, will show unused input
f: (Int) => +3

g: ((Int, Int, Int)) => + (3, 6, 9)

// normal generic
T[T1, T2]: {
    a: T1
}

// depends on the value of T1, rather than just T1
// functions that take in T with v now are able to differentiate between different variants of T
T[v T1]: {
    a: T1
}

T: impl Plus (v self, v Self) -> Self => ... 

f: (t1: v T, t2: v T) {
    t1 + t2
}

// what replace actually is

x: replace () => ()

// virtual impl I guess
replace: impl Index(i: Int) => .. invoke macro

// associativity...
// everything is left associative
// BODMAS

Box {
    bg_color=Blue

    Box
    HBox
    Flex
    Text
}

// using dependent type parameter as the default
V[v: x::V]: {
    v1: Int(v)
}

// where is it useful? well when you can write functions specifically taking in the dependent type parameters into account
k: (arg[v: V == 0]) {}

index: trait (i: _) -> ()

// static refinement
array: impl index(i >= 0) -> () {}

Array[T, N]: [T; N]

Array[T, N]: impl Index(i < N)

// if it sees it can apply a binary op, it will do so before the unary
// if Mul doesnt exist, cannot compile? well Mul basically cant exist
(*): BinaryOp(precedence=10, bind=Mul)
(*): UnaryOp(5, Prefix, Deref)

(++): UnaryOp(8, Postfix, Increment)

// op is like trait but for operators

Numeric: impl Increment(self) => self + 1

5++

// scopes. NOTE: mod is irrelevant

plus_ten: {
    // if you call g(), get 10
    () => 10

    // g(10) = 20
    // private function
    _(Int) => + 10

    // error! cannot add two args a b to scalar 10
    // (Int, Int) => + 10

    // a is unused
    (a: Int) (b: Int) => $1 + b
    (Int) (Int) => $1 + $2
}

// you cant import unlabelled names as you must qualify them directly
// use plus_ten::()
use plus_ten

plus_ten()

let x = plus_ten(1)
// 12
x(1)

// will not run on as a program on neutron
let x = Address(0x0)

let x = 10
// reference the data at x. What this means depends on the context and usage
let y = &x

// (1, 2, 3): (Numeric, Numeric, Numeric)
1, 2, 3

// let x = y evaluates to (), let y = z evaluates to ()
// ((), ()): (Empty, Empty)
let x = y, let y = z
```
