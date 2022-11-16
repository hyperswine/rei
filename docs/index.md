---
layout: default
title: Rei
nav_order: 1
---

## Overview

Rei is a general purpose programming language that flows across spacetime, with a maximal focus on static analysis. What should result is a mixture of performance, elegance and productivity (also through ergonomics, see `rein`). A feeling that the language just works once you've set everything up.

Rei can be seen as a minimal language and api which allows you to build complex abstractions on top of. Rei is meant to accustom super low level programming and systems programming in a direct manner. It takes inspiration from rust, lisp, typescript and the ascended theory of computation.

## Types

Bitfields. In the realm of the ascended theory of computation, we take the lowest, most atomic form of data to be a single bit. When combined and permutated, form more complex data. We then interpret those permutations of discrete bits to form the reasoning and logic of our program and complex data types. Furthermore if we consider how the data is transferred, e.g. streamed or bulk transferred, the logic we use could be significantly different. There are also additional topics such as padding, alignment, etc. which may be required for the underlying system's implementatin, e.g. SSD's are partitioned into 512B-4K pages.

In the rei base language spec, the root type is `Bits`. That is, 0-INF permutations of bits. This can is represented through an array of booleans with index `0` as the first bit and index `size - 1` as the last bit.

## Functional

Rei can be seen as a functional-first language. It is meant to be general purpose, and does have object-oriented capabilities, but many of its concepts borrow from functional programming literature.

Coming from an OOP background, one may be familiar with classes, methods, inheritance, vtables, and all the design patterns associated with it such as factories, builders, visitors, etc.

With rei-style functional programming, one should instead think in terms of enumerated objects, pattern matching, effect purity, closures, generics and monomorphism, function composition and functional macros. Also, rei exposes an ergonomic interface to declare objects in an OO-style while maintaining the underlying, driving functional paradigm. E.g. the `extend` keyword helps to modularise and simulate the feel of inheritance, while all your doing is creating another variant of the base enum.

Functional programming is generally much more concise since everything is treated as a function. Rei encourages one to write callable contexts (a more generalised version of a function or control block with its own context). The `core` library encourages use of function composition and chaining, higher order functions, generic functions, wrappers and aggregations with map-based operations like `map`, `filter`, `reduce`, `match` to map over them or pattern match.

## Blocks

The most abstract concept of computation is an instruction. A more helpful concept is that of a block of instructions. A sequence of instructions carried from start to finish. Blocks come in many different types: normal blocks, enumerated blocks, extension blocks, parameterised blocks, macro blocks, module blocks.

A block may be an anonymous (unlabelled) block. Anonymous blocks are basically blocks with their own local namespace that inherits the scope rules from its parent block. E.g. an anonymous block placed somewhere in a function would just be its own local namespace + the function's namespace and executed in the function's context. An anonymous block somewhere in an object's body would be similar. It would be basically an extension of the object within the object itself, like a local namespace for functions you dont want other functions in the object to know of. An anonymous block somewhere in a module block is again similar, though they cannot be exported. Anon blocks inside objects and modules are not recommended and are usually just going to be optimised out since you cant target them directly, unless you are assigning a variable to them (but that cant be done inside module blocks and object blocks anyway).

## Prei

`Project tool for Rei` is a universal utility and framework for writing somewhat standard rei applications. It enforces a certain project structure, with `src, examples, docs, tests, etc`. Your main source files are structured similar to cargo, with implicit file-modules and directory-submodules for managing complex themes.

What prei also does is allow stronger integration with `rein` and its language analysis features. Most features found in LSPs nowadays are supported by using prei as an intermediary. What prei does is cache queries and store a single, global data structure to allow fast incremental compilation. The underlying engine it uses is rei's base language library which implements the lexing, parsing and codegen functions among other features defined in the base rei spec. You can think of `prei` like cargo, but with a more powerful interface and extra responsibilites for creating and building entire projects in an efficient manner.

## Demand Driven Compilation

Rei's base implementation is structured as a demand-driven model. It exposes a set of API functions that usually do independent tasks and can potentially allow better parallelisation through a manager like prei.

The base implementation is meant to be a minimal, self standing set of APIs to allow the programmer to build pretty much whatever they want. Most compilers would implement features like modules, traits, etc directly in the compiler itself. But rei takes a different approach. There is no global symbol table per se, but rather a symbol table API which you can use to manage modules (which is also partially know by prei). There is no cache, no builtin pipelines, etc. Rei is not supposed to be a complex system that maximises throughput but rather a general purpose processor that uses queues and atomic operations to process instructions. There is hardly any state (if there even is any).

A good way to look at the base spec is to think of it as a bunch of independent pure functions. Many of the things they do involve parsing code into nodes and subtrees which you can then either use straight away in e.g. prei or call another function on it like lowering it further. In this regard, it is possible to form a fully fledged environment around rei and its utilities and libraries.

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
