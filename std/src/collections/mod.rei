#*
    For useful collections like vec, box, etc
*#

use pkg::common::Size
use core::prelude::Allocator

# A standard vector
export Vec: complex {
    __internal: data {
        arr: *mut _
        size: Size
    }
    __constants: data {
        items_to_allocate_default: Size = 8
    }

    impl Default() -> Self {
        Self {
            __internal: {arr: alloc(__constants.items_to_allocate_default)}
        }
    }
}

export Tree: complex {
    data tree {
        root: *mut _
        n_nodes: Size
        depth: Size
        width: Size
    }

    # Call default constructors
    impl Default() -> Self {
        Self {}
    }
}
