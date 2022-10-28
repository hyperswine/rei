// Prei auto imports these items into your entire pkg namespace
// its like a global namespace that every submodule can reference

use core::types::*

type Index = UInt

export Range: trait {
    fn index(start: Index, end: Index=Index())
}

// reexported from core
// type Size = usize
// type Byte = u8
