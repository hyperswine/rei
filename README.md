# Rei

A programming language that just works.

## Design

- Simple
- Fast
- Abstraction friendly, `std` and official libs highly recommended. Allows low and high level code from bare metal asm to ml all in one proj

[Here is the coolest thing](https://docs.python.org/3/library/ast.html).

## Macros in Std

Not built into the language at least. Available as a library function `core::macro`.

```rust
// core is always linked to all packages
// use core::prelude::*
// if using std instead, core isnt linked
// use std::prelude::*

// macros should be upper case unlike functions or classes
macro Empty(
    ($x:expr) => {},
    () = {}
)

// In neutron, processes exit 0 by default
// or exit with a code. The 'return' is implicit
fn main() {
    Empty("There is nothing here, move on")
}
```

## AST

reic's job is to tokenise a file into tokens, parse them into an phantasm-readable AST (Strict YAML), and turn it into a phantasm IR file. Then assemble that phantasm IR into an object file. When used wiht many files, `reic` implicitly invokes `ld` which links the assembled objects together into either a library (static/dynamic) or executable ELF.

The platform is implicit as `reic` is usually symlinked to the actual target triple executable, e.g. `aarch64-neutron-reic`. And the default is exported to PATH.

The AST generated has no "order" per se. Since every field should be unique. Statements and expressions do have order. Some unordered items are still placed in a list as well.

The format of the AST:

```yml
filename: "filename"
content:
    - function_def:
        ident: "func"
        params:
            - ident: "x"
              type: "function_ref"
            - ident: "y"
              type: "i32"
        return:
        body:
            - function_call_expr:
                ident: "x"
                args:
    - class_def:
        ident: "A"
        inherits:
        implements:
        body:
            - function_def:
                ident: "A"
                params:
                return:
    - let_stmt:
        ident: "a"
        expr:
            function_call_expr:
                ident: "A"
                args:
```

for

```rust
// filename.rei

fn func(x: &fn, y: i32) {
    x()
}

// note the same AST as class A() {}
class A {
    A() {}
}

let a = A()
```

Since pest is a PEG generator, it is LL (top down). That means we dont start at the main production and build it recursively. We start at the root and branch out. Without going back. I think its a recursive descent parser.

## Phantasm IR

Phantasm can then be generated from the StrictYAML AST.

```asm
// note symbols will be mangled if possible. Good to prevent any possible mix ups
// something like a sha 256 randomisation scheme
@section program_load
// any global vars are always initialised (constexpr if possible)
call A_A_constructor
load_mem ret0 -> x

@section program_defines

func:
    move_reg arg0 -> temp0
    move_reg arg1 -> temp1
    // note if more than 7 args, use stack
    call arg0
    ret

// classes are basically data + functions local to it + any inner code
// a constructor is like any other function. It creates a copy of A: data, initialises it to the args, and returns it
A_A_constructor:
    alloc_stack A.data
    // initialise a field: load val -> A.data.field 
    load_addr A.data -> ret0
    ret

@section data
// x
x: A

A: data {

}
```
