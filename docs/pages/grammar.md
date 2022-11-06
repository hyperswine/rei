---
layout: default
title: Grammar
---

## Base Language Specification (Simplified)

The only thing that really matters are universal and local definitions. The other things include aggregate types like arrays, records, objects. What everything becomes at the end of the day is some specific width bits like 1, 8, 32, 64, 128. And aggregates of those bits in some order. The interpretation of the bits are library defined. The alignment and padding with 1-bit types and aggregate types are library defined.

```rust
// universal def
ident generics? ":" universal_modifier? expr
// local def
var_modifier? ident ":" expr

expr: namespaced_ident | callable_expr | unary_expr | binary_expr
// namespaced_ident: ident ("::" ident)*

// literals
literal: numeric | raw_string

// array [T] array_expr: "[" expr* "]", interpreted via certain semantics

universal_modifier: "mod" | "macro"
var_modifier: "let" | "mut" | "const"

// callable
callable_expr: "(" params? ")" return_type_expr? eval_expr

eval_expr: scope_expr | arrow_expr

scope_expr: expr*
arrow_expr: "=>" expr
```
