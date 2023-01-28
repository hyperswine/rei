---
layout: default
title: Quick Thoughts
---

## Defining Types

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

## Static Guarentees

Unlike rust, we have automatic lifetime deductions though you are free to add lifetimes `'a` as you wish for clarity. As if it was a comment. Sometimes they may be needed to solve certain cases?

Being a highly compile time based language like Rust, we do a little static analysis. With LSP integration to highlight common tokenisation & parsing errors as well as `reic --check` errors. Runtime errors can be minimised as long as the programmer knows his intent.

Full polymorphism and inheritance is also supported, with unsafe bodies for extra control.

## Reidoc

You use hash `#` comments at certain locations, e.g. the beginning of a file, right before a function expr, and other non trivial or exported blocks. Variable definitions can have a hash comment if exported.

## Compiler Process

Rei follows a similar pattern to rust's compilation process. First, prei converts all source files to modules and all modules to a hierarchy of namespaces ending with the root `pkg` namespace. It does this in parallel for each source file, and parses each file in parallel. The resulting AST is combined together and sent to the analyser.

The analyser does a few things. First, note that a few artifacts were generated: the AST and Symbol Table. The symtab provides a unique identifier enforcement for each namespace and a cache for nodes of the AST given a namespace, identifier and rei type. The analyser first descends the AST, converting certain expressions to values (constexpr) and overall trying to reduce expressions to constants. Its main job however, is to "lower" the AST into a more generic form of instruction flow that can be more readily converted into assembly.

### Lowering

The lowering process involves:

- converting all "sugared" expression nodes into the same form in MIR. E.g. all ternary, if-else, match, elvis statements are converted to the same universal hashed branch block for PIR or jump table or branch statements (depends on size) in cranelift
- converting all local definitions aka variable definitions into stack slots in cranelift or recursive definitions in PIR
- converting all boxed values or external memory definitions into syscalls on unix or just API fns. Note boxed values are just pointers, basically u64 values. In cranelift, I think there are ways to point to external addresses. In PIR its even simpler

### Macro Expansion

Macros can be seen as compiler plugins that are treated as meta programs. Macros are parsed in the usual style just like any other function. But unlike regular functions, they are always "constexpr". So during descent pass 1, each macro invocation is evaluated lazily. A macro fn itself may also be reduced like any other expr. Its like reflection. When you first bump into the macro invocation, you go to that macro in the symtab and use that fn to evaluate the inputs. But unlike usual params, the params directly target the compiler's types like Expr.

E.g.

```rust
x: replace {
    (x: Ident y: Expr) {}

    (x: Ident "," y: Expr) {}
}
```
