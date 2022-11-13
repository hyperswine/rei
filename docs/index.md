---
layout: default
title: Rei
nav_order: 1
---

## Overview: What is Rei?

Rei is a language that is built to flow. Maximal focus on static analysis. What should result is a mixture of performance, elegance and productivity (also through ergonomics, see `rein`). A feeling that the language just works once you've set everything up.

Rei can be seen as a minimal language and api which allows you to build complex abstractions on top of. Rei is meant to accustom super low level programming and systems programming in a direct manner. It takes inspiration from rust, lisp, typescript and the ascended theory of computation.

## Types

Bitfields. In the realm of the ascended theory of computation, we take the lowest, most atomic form of data to be a single bit. When combined and permutated, form more complex data. We then interpret those permutations of discrete bits to form the reasoning and logic of our program and complex data types. Furthermore if we consider how the data is transferred, e.g. streamed or bulk transferred, the logic we use could be significantly different. There are also additional topics such as padding, alignment, etc. which may be required for the underlying system's implementatin, e.g. SSD's are partitioned into 512B-4K pages.

In the rei base language spec, the root type is `Bits`. That is, 0-INF permutations of bits. This can is represented through an array of booleans with index `0` as the first bit and index `size - 1` as the last bit.

## Features

An semi complete feature list of rei (including core and std libraries):

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
