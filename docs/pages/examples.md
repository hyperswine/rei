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
