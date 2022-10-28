---
layout: default
title: Phantasm Backend
---

## Phantasm Usage

Example:

```rust
// note symbols will be mangled if possible. Good to prevent any possible mix ups
// something like a sha 256 randomisation scheme
@code

_init:
    // any global vars are always initialised (constexpr if possible)
    call A_A_constructor
    x = ret0

func:
    t0 = a0
    t1 = a1
    call arg0
    return

A_A_constructor:
    sp = sp - 8
    x0 = A.data
    return

@data
x: A

A: ...
```
