/*
    Rei Graphics API
*/

// primitives for quads, textures, models, etc...
// kind of like vulkan, rust-gpu and wgpu
// but good
// also stuff like assimp builtin...

Quad: {}

Texture: enum {
    Tex2D
    Tex3D
}

Model: {}

Vertex[Size]: [f16; Size]

Glb: {
    spec: GlbScene

    // parse bytes glb into scene graph or glb object?
    new: (bytes: Bytes) {
        Self { parse_glb(bytes) }
    }
}

Scene: {
    model: Model
    children: Vec[Model]
}

export load_glb: (path: Path) -> _ {
    let bytes = read_bytes(path)?

    Glb(bytes)
}
