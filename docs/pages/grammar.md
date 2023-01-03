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
    | unary_op
    | binary_op
    | general_def
    | scope_expr
    | modified_scope_expr

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

keywords: "return" | "async" | "await" | "yield" | "export" | "mod" | "trait" | "impl"

// increasing order of precedence
binary_operator: "*" | "/" | "+" | "-" | "==" | "="
postfix_unary_operator: "?" | "!"
prefix_unary_operator: "*" | "&"
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
```