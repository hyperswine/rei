#*
    Allocator to make use of dynamically sized objects
*#

export Allocator: trait {
    alloc: (&self, size_bytes: Size) -> ()
    dealloc: (&self) -> ()
    grow: (&self, grow_by: Size) -> ()
    shrink: (&self, shrink_by: Size) -> ()
}

// maybe no need...
// on neutron at least

// on neutron, use vDSO neutronapi
