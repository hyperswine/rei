#*
    Allocator to make use of dynamically sized objects
*#

// ? basically, allocate from the cache which is fast
// if not in cache, combine
// this provides the mechanism to do that

export Allocator: trait {
    alloc: (&self, size_bytes: Size) -> ()
    dealloc: (&self) -> ()
    grow: (&self, grow_by: Size) -> ()
    shrink: (&self, shrink_by: Size) -> ()
}

// on neutron, some things may be in neutronapi instead?
// maybe that wont even be needed
// just external drivers
