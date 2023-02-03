---
layout: default
title: Examples
---

## Function-Block

Rei works with the executor based architecture in that functions directly correspond to a `task` made of a sequence of `instruction`s. At face value, it would seem kinda similar to imperative execution, but it has some differences.

Functions are directly broken into separate parts based on their `return` blocks.

```rust
f: () -> _ {
    let x = 1

    // a function is called
    let y = g()

    // value is returned
    return y
}

g: () => 5
```

Naively, one could use a "stack" to represent function execution. But notice that it isn't actually needed. That is, if you can change it to:

```rust
f: () -> _ {
    let x = 1

    return g()
}

g: () => 5
```

But that is also those specific cases. What if you had something more complex? Then, what you could do, is partition the function into separate parts and call them.

```rust
k_main0: () -> Result {
    main(_)
}

// in main.rei, all main functions are autofilled with -> Result
main[G]: (i: Int, s: String, g: G...) -> Result {
    // Pair/2 may be inlined
    let x = Pair("Hi", "Bye")
    let y = Pair("Hi2", "Bye2")

    // depending on profile to be built, x + y may be inlined
    x + y

    ()
}

// can you also specify infix, precedence here or maybe somewhere else?
+[Rhs, Res]: trait (self, Rhs) -> Res

// alias types have Self(...) constructor auto implemented
Pair[T]: (T, T)

// represents all T, recursive implementation
Pair[T impl +]: impl +(self, other: Self) -> Self =>
    (self[0] + other[0], self[1] + other[1])

// shadow more general definition
Pair[Int]: impl +(self, other: Self) -> Self =>
    (self[0] + other[0], self[1] + other[1])

// assume no inlining, that means all function calls are executed with a call aka a jump therefore main becomes

main:
    jump Pair
    jump Pair

Pair:
    dependent_jump main // actually more like a hash depending on the input jump

// recompute entire table or add subtable if get rid or add new blocks
dependent_jump:
    res = hash input
    jump res
```

## GUI Example

To write a gui framework in std rei:

```rust
Component: {
    // draw your stuff as vectors
    render: trait () -> RenderSpec
}

Box: extend Component {
    render: impl () -> RenderSpec {
        // call child renderspecs and expand your box to fit them via a simple box algorithm
        let children_spec = self.children.map(c => c.render())
        let height, width = box(children_spec)

        RenderSpec(height, width, bg_color)
    }
}

draw: (root: Component) {
    let render_spec = root.render()
    // rasterise and stuff...
}
```

Powerful thing about functional programming => ability to store state and action mappings via closures and pass that into another context. Then use that closure to change that context, e.g. in a render loop. With affine types/ownership we have zero cost mapping. No copying, we literally just pass the entire thing, e.g. via an offset.
