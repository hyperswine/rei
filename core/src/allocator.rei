#*
    Allocator to make use of dynamically sized objects
*#

pub Allocator: trait {
    alloc: (&self, size_bytes: Size) -> ()
    dealloc: (&self) -> ()
    grow: (&self, grow_by: Size) -> ()
    shrink: (&self, shrink_by: Size) -> ()
}
