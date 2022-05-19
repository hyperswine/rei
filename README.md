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
