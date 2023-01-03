---
layout: default
title: Grammar
---

## Base Language Specification

Expressions, expressions, expressions.

```rust
expr: 
    | paren_expr
    | comma_expr
    | empty_expr
    | bracket_list_expr
    | ternary_expr
    | unary_op
    | binary_op
    | general_def
    | scope_expr
    | modified_scope_expr
    | var_def_expr

binary_op: expr binary_operator expr
scope_expr: "{" expr* "}"
modified_scope_expr: m1_expr | m2_expr
m1_expr: ident scope_expr
m2_expr: ident macro_params? scope_expr
general_def: ident generic_params? ":" expr
parameterised_expr: paren_params | bracket_params | curly_brace_params

comma_expr: expr "," expr
empty_expr: "(" ")"
bracket_expr: "[" expr "]"
bracket_list_expr: bracket_expr
var_def_expr: "let" | "mut" | "const" ident "=" expr
ternary_expr: (expr "?" expr ":" expr) | (expr "?:" expr)

keywords: "return" | "async" | "await" | "yield" | "export" | "mod" | "trait" | "impl"

// increasing order of precedence
binary_operator: |"&" | "|" | "^" | "*" | "/" | "+" | "-" | "==" | "="
postfix_unary_operator: "?" | "!"
prefix_unary_operator: "~" | "*" | "&"

refinement_expr: expr
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
```
