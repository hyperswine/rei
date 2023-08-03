---
layout: default
title: Linearization
---

Everything in rei can be seen as an n-order abstraction of code and data.
For example, a parameterized context can be seen as its max nested element + 1.

In order for these abstractions to be useful in terms of actually executing and evaluating to more data and code, they have to be linearized first.

```rust
// responsible for labeling
Query: {
    "mod1_mod2_name" : &Node
}

// responsible for data and relationships between instructions & data
Node: [
    Instruction [call neighbor1, add 2 + d1u8]
    Y [u8, Z [ 100 ]]
]
```

Example queries, e.g. from a main() program or a REPL.

```rust
Instruction [ call "mod1_mod2_name" ]

// on a REPL or evaluation/scripting environment, all expressions are "returned" and "displayed"
// many call ops affect the SP. On a scripting env, there is a pseudo stack that is kinda like a full executor stack
Instruction [ call "mod1_mod2_name" ]
Instruction [ display sp ]
```

What happens when you call 1 + 1.

```rust
Instruction [ StackOverwrite binary_add 1 1 ] => val
Display val
```
