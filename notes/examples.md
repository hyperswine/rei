---
layout: default
title: Examples
---

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

## Example Code Snippets

```rust
1 + 1 * 3

1 * *3 + 1
// 1 parsed as numeric
// 1 * rhs parsed as binary op

*3 + 1
// *3 parsed as unary prefix
// + 1

* 3 + 1

// could either mean deref <expr>
// or mult <expr>

f: (x: Int)
// the following two are equivalent. If you have Int() and = at once, that is an error
// or maybe just x: Int = Int(5)
// depends on style guide and styler
f: (x: Int(5))
f: (x: Int = 5)
f: (x: Int < 5)
f: (x: Int < 5, < 10, = 10)

f: (x: Int < 5, < 10, y: String = "f")
f: (x: Int lessthan(5), y: String)

// normal generic
T[T1, T2]: {
    a: T1
}

// depends on the value of T1, rather than just T1
// functions that take in T with v now are able to differentiate between different variants of T
T[v T1]: {
    a: T1
}

T: impl Plus (v self, v Self) -> Self => ...

f: (t1: v T, t2: v T) {
    t1 + t2
}

// what replace actually is

x: replace () => ()

// virtual impl I guess
replace: impl Index(i: Int) => .. invoke macro

// associativity...
// everything is left associative
// BODMAS

Box {
    bg_color=Blue

    Box
    HBox
    Flex
    Text
}

// using dependent type parameter as the default
V[v: x::V]: {
    v1: Int(v)
}

// where is it useful? well when you can write functions specifically taking in the dependent type parameters into account
k: (arg[v: V == 0]) {}

index: trait (i: _) -> ()

// static refinement
array: impl index(i >= 0) -> () {}

Array[T, N]: [T; N]

Array[T, N]: impl Index(i < N)
```
