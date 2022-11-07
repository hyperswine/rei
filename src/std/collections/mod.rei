#*
    For useful collections like vec, box, etc
*#

use pkg::common::Size
use core::prelude::alloc

# A standard vector
export Vec[T]: {
    // private field
    _internal: {
        arr: *mut T
        size: Size
    }

    const _items_to_allocate_default = 8
    
    () -> Self => Self { _internal: { arr: alloc(_items_to_allocate_default) } }
}

export Tree[T]: {
    _tree: {
        root: *mut T
        n_nodes: Size
        depth: Size
        width: Size
    }

    # Call default constructors only works for non anonymous objects
    // () -> Self => Self {}
}
