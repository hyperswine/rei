---
layout: default
title: Rei
nav_order: 1
---

## What is Rei?

Rei is a language that is built to just work. A lot of focus on compile time semantics like borrowing. Full polymorphism and inheritance is also supported, with unsafe bodies for extra control.

Unlike rust, we have automatic lifetime deductions though you are free to add lifetimes `'a` as you wish for clarity. As if it was a comment. Sometimes they may be needed to solve certain cases?

Being a highly compile time based language like Rust, we do a little static analysis. With LSP integration to highlight common tokenisation & parsing errors as well as `reic --check` errors. Runtime errors can be minimised as long as the programmer knows his intent.

## Types

In rei, the only compiler defined types are primitives: Numerics, Bytes, Arrays, Bools. Complexes include objects and enums. Callables contain functions and tuples. Out of these types, bytes are usually not parsed directly but rather strings (pretty much all strings are just an arbitrary concatentation of bytes). They are a nice way to do conversion in the base compiler.

Traits, impls, etc. build ontop of these primitives. Macros and annotations build ontop of functions.

Rei core also builds on these types to result in core::types::primitive::i32,u32,u64,u128... etc. Byte strings with b"". Regex with core::regex r"". Raw strings with core::raw raw"". F (formatted) strings with the default "". With f strings, you can also use $ instead of {}. Character strings are one-length strings delimited by single quotes ''.

### Aliases

Aliases are like rust aliases where `A: B` sets `A` to `B`.

### Constants

Constants are everywhere, such as pi, euler's number, 4K, etc. Constants can be defined in most scopes like modules, objects, function scopes. Constants defined in the module scope may be exported and visible to all other scopes that way. But constants defined in local scopes may not be exported.

### Exporting

Only module level definitions (universal and constant) may be exported. Although its prob better to use Object-default val to define a global constant. When you `export <item>` you allow that item to be imported from its fully qualified path. When you do `export(...) item` you allow that item to be imported from the listed namespaces such as `pkg, prelude, super`.

By default the lang server autofills using fully qualified paths.

### Objects: Data, Enum and Complexes

Objects are the main structure for composing and modelling data. Object scopes are special scopes where you have key: val fields and associated functions. Functions starting with `self` are simply sugar for methods. Note methods are simply any other associated function and belong to the current module namespace.

### Tuples

Tuples are a special type. Unlike objects, they do not have a key: value mapping but rather rely on array like indexing. Tuples are a special function type that basically returns itself when called.

## Everything is an Expression

Rei is composed of a list of organised expressions. Self contained expressions and evaluation expressions.

Everything is an expression. An expression evaluates to a value. Expressions that arent assigned to a variable or returned by the end of a scope are simply abandoned.

Rei can be most thought of as a multipurpose, data-driven, functional, systems programming language, with an emphasis on static analysis and ergonomics in a human centered environment. Rei tries to maximise agency and with rapid prototyping. Write now, optimise later. Though the compiler is very "strict" in another sense like rustc, dealing with nulls and borrowing rules, and the language itself forces one to at least write sensible or good-ish code or else go out of their way to write potentially bad code.

An example:

```rust
get_data: () -> UInt {
    // expression evaluates to (), since mut var definition expr
    mut x = 1

    // expression evaluates to (), since scoped if expr
    for i in 0..10 {
        // expression evaluates to (), since ident ~ operator
        x++
    }

    // expression evaluates to x (identifier)
    // IDE parse warning: ditched non-trivial expr return
    x

    // expression evaluates to x (identifier)
    x
}
```

## Reidoc

You use hash `#` comments at certain locations, e.g. the beginning of a file, right before a function expr, and other non trivial or exported blocks. Variable definitions can have a hash comment if exported.

## Features

Features of rei:

- pointers
  - references, basically safe auto pointers
    - parameterisation or pass by reference
    - immutable and explicit mutable references. Only one immutable reference at one time with no other references during mutable borrow
- operators
  - strict, defined order for simplicity and intuitiveness
  - operator overloading
  - bitwise ops
  - arithmetic math ops, BODMAS
- function calls
  - method calls
- pattern matching
  - enums
  - if else expr
  - ternary
    - elvis
- loop expr
  - for range or val in array/tuple
  - while bool expr
- arrow expressions
- strong typing
  - all types are first class citizens and treated similarly
  - overloading in the same scope, requires different signatures (mostly params)
  - universal definitions of types
    - generics
    - unsafe
      - by default, all types are safe in that they have no side effects
      - unsafe allows side effects and use of outer scoped mutable variables defined at module level
    - objects
      - key-val data
      - tagged union enums
      - inline subtyping with enum fields
      - object promotion and automatic base typing/sub enumming
      - methods, basically sugar so you dont have to type the whole thing
    - callables
      - functions
      - macros, hygenic and "builtin" with reic::macro_api auto linked. No quotes
        - annotations and global once type expr
      - tuples
    - shared functionality on types
      - traits and impls
  - primitive types
    - numeric of arbitrary precision based on specification
    - strings of character like types
      - interpolation with "{}"
    - arrays of a single type
- contracts
  - propagation, "comparison memory"
  - where clauses for types and generics
    - dependent types from contracts
      - arrays of static size
- algebraic effects
  - async/await, a pattern for concurrency (esp IO) and parallelism
  - yield and resume
- anonymous functions, type inference
- const, immutable and mutable variables, type inference based on literals and rhs
  - shadowing
  - mutation of value directly in memory location only possible with mut
- static and lazy variables
  - lazy and once initialised variables in a local scope, similar to consteval but at first use, after which the code is modified to use that directly. Usually dynamically allocated
- namespaced identifiers
  - use `<namespace ident>` semantics for importing
  - export `<universal def>` for exporting
    - default exports
    - exports to specific modules like pkg, prelude, super
- iterators
  - map, reduce, anonymous methods
  - parallel iterators with `.iter_parallel()`
- affine types
  - used only once
  - ownership, only one function may own a variable at once
  - release build basically uses linear types, all unused vars are eliminated
- result types
  - declared with `Result[T, E]`
  - optional types declared with T?
    - optional error types without an Ok value declared with T!
  - result propagation with `?` and `!`
