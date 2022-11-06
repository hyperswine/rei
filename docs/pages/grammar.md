---
layout: default
title: Grammar
---

## Base Language Specification

The base language specification is meant to be a very generic way of representing the concept of a "program" running on some "system". It provides the fundamental intuitive rules of program flow and data.

```rust
// PREI ONLY... 
// input: mod_expr
// otherwise, incrementally compile each "region" or file that has changed. Prob just each file like vscode

// LOCAL EXPRESSIONS (mostly, though some can be placed in a universal scope)

expr: object | call_expr | method_call_expr | operator_expr | export_expr  | scope_expr | return_expr | yield_expr | break_expr | underscore_match_expr | index_expr | group_expr | tertiary_expr | where_expr | variadic_expr | anon_fn_expr | namespaced_expr | empty_expr

// BASIC

call_expr: expr "(" args? ")"
method_call_expr: expr "." call_expr

// Yield in rei "saves" the state of execution and "returns"
yield_expr: "yield" expr?
return_expr: "return" expr?

empty_expr: "(" ")"

fn_declaration: ident ":" "(" args? ")" return_type_decl?
return_type_decl: "->" namespaced_expr

// In match arms
underscore_match_expr: "_" "=>" expr

// Sugar for array indexing
index_expr: "[" expr "]"

// params can have a > x though cant they?
args: arg ("," arg)*
arg: (expr | default_arg) condition?
default_arg: ident ("=" expr)?
condition: operator expr

anon_fn_expr: anon_fn_params "=>" expr
anon_fn_params: "(" ident? ")" | ident

annotation_expr: "@" annotation_item+
annotation_item = ident paren_expr? ("=" expr)?

range_expr: expr range range
range: ".."

variadic_expr: ident variadic expr
variadic: "..."

scope_expr: "{" expr* "}"
unsafe_expr: "unsafe" expr

var_def: var_kw expr "=" expr
var_kw: "let" | "mut" | "const"
var_mod: "mut" | "const"
var_redef: expr "=" expr

lifetime_modifier: "static" | "lazy"

// Combinations
array_expr: "[" expr "]"

// GROUP / PAREN EXPR. Basically, should be more "left" on the AST
group_expr: "(" expr ")"

// GENERICS
generic_params: "[" generic_item ("," generic_item)* "]"
// Ident ":"? namespaced_type_expr
generic_item: expr

// WHERE, for generic bounds and contracts
where_expr: "where" "(" (where_item ",")* ")"
where_item: (expr | condition)

fn_mod: "const" | "unsafe" | "system"

// FUNCTIONS
fn_type_decl: ident ":" fn_mod* "(" params? ")" return_type?

// All overloadable operators
operator_expr: unary_op | binary_op
overloadable_operator = "{OVERLOADABLE_OPERATOR}"
unary_op: ref_expr | deref_expr | (ident unary_operator)
binary_op: expr binary_operator expr
assignment_op: expr "=" expr // HIGH priority

// Tertiary
tertiary_expr: expr "?" expr ":" expr

paren_expr: "(" operator_expr ")"
param: ident (":" ident)?

ident_namespace: ident ("::" ident)*
// Basic types
ident: camel_case_ident | snake_case_ident
camel_case_ident: r"&*[_a-zA-Z]\w*"
snake_case_ident: r"&*[_a-zA-Z]\w*"
literal: numeric | string
numeric: integer | float
// + is implicit. If you write an explicit +, that is taken as a binary op
integer: "-"? \d+

string: double_quoted | single_quoted | tick_quoted
double_quoted: "\"{multiline_string}\""
multiline_string: "(?:[^\"]|\\.)*"

// keywords that can be interpreted as an identifier
self_sugar: "self"
self_kw: "Self"
outer_kw: "outer"
super_kw: "super"

// namespaced expr
namespaced_type_expr: ident ("::" ident)?

// visibility and packages
vis: "import" | export
export: "export" "default"? expr

// can be used for reexport as well
import_list: import_ident ("::" | ("," import_ident))*
import_ident: ident | "*"

// DEFAULT VAL
default_val: "=" expr

// OBJECT
object: ident generic_params? ":" (alias_obj | mod_obj | primitive_obj | data_obj | fn_obj | trait_obj | impl_obj) where_expr? (arrow_expr | scope_expr) default_val?
// either a c like struct, enum or complex
data_obj: ("enum"|"complex")? scope_expr
// fn or tuple. Right now annotations and macros are defined as special fns but I think they need their own () => {} match arms. Basically, in your scope expr, instead of expecting Expr*, expect Macro match arm expr*
fn_obj: tuple_obj | ( ("annotation"|"macro")? "(" params? ")" return_type_expr? )
// "expr" usually means a self contained expression like return 1 + 5 * (g.dec() - oop(5, 3))
// whereas a scope_expr allows multiple independent expressions to be listed in order
tuple_obj: "(" expr ")"
primitive_obj: "String" | "Numeric"
arrow_expr: "=>" expr

mod_obj: "mod"
alias_obj: namespaced_type_expr

// traits
trait_obj: "trait"
impl_obj: "impl"

// required for trait fields
fn_decl: ident generic_params? ":" fn_obj

trait_item: fn_decl
impl_item: fn_obj
```

## Core Library Specification

The core library adds new features which involve grammatical rules and functions. These features are based on an understanding of classical computing with rust-like semantics and ease of expressing your intent within a certain "context".

```rust
float: "-"? "([0-9]+\.[0-9]*|\.[0-9]+)"

// override base
literal: numeric | string

// useful expr for bare metal stack and heap usage
ref_expr: "&"+ expr
deref_expr: "*"+ expr

// sugar for Result<T ,E>
res_expr: "|" expr "," expr "|"
res_expr: expr "|" expr
// sugar for Option<T>
optional_expr: expr "?"
// sugar for quick ternary
quick_ternary_ok: expr "?" ":" expr

// MAYBE make expr ? a question_mark expression like ternary?
// basically a unary expression that should be interpreted in the context of the scope
// in AST traversal

// sugar for propagate Error or else assign T
propagate_err: expr "?"
propagate_res: expr "!"

// sugar for let Ok/Err = something
if_let_ok: "?" expr
if_let_err: "!" expr

// bitwise binary ops
bitwise_and: expr "&" expr
bitwise_or: expr "|" expr
bitwise_xor: expr "^" expr

// NOTE:
// expr operator "=" expr
// e.g. *=, +=, &=, ^= are evaluated to expr1 = expr1 op expr2
binary_op_sugar: expr binary_operator "=" expr

// unary
bitwise_not: "~" expr
```

NOTES:

`?:` is a token itself. Its called the Elvis.

We always postfix unary ops before prefix unary ops. We always try binary ops before any unary op.

## Simplified

The only thing that really matters are universal and local definitions. The other things include aggregate types like arrays, records, objects. What everything becomes at the end of the day is some specific width bits like 1, 8, 32, 64, 128. And aggregates of those bits in some order. The interpretation of the bits are library defined. The alignment and padding with 1-bit types and aggregate types are library defined.

```rust
// universal def
ident generics? ":" universal_modifier? expr
// local def
var_modifier? ident ":" expr

expr: namespaced_ident | callable_expr
// namespaced_ident: ident ("::" ident)*

// literals
literal: numeric | raw_string

// array [T] array_expr: "[" expr* "]", interpreted via certain semantics

universal_modifier: "mod" | "macro"
var_modifier: "let" | "mut" | "const"

// callable
callable_expr: "(" params? ")" return_type_expr? eval_expr

eval_expr: scope_expr | arrow_expr
```
