#*
    Memory Management Primitives
*#

// cfg riscv, DRAM may start at 0x40000000
// chunk based management

StartAddr: u64

#*
    Chunk-based Allocation
*#

Chunk: (StartAddr, Size)

// more chunks can be added by the use of a connecting chunk
// so it isnt circular
ChunkCache[Size]: (Index, [Chunk; Size])

ChunkCache[Size]: extend {
    // if full, create another chunkcache and point to it
    pop: (&mut self) -> Chunk {
        let index = self.Index
        self.Index = clamp(0, self.Index - 1)

        self[1][index]
    }

    push: (&mut self, chunk: Chunk) {}
}

// (): impl From[Result[T, E]] () => Ok(T())
// RING BUFFER (MPMC)

# MPMC, Lock Free
export RingBuffer[T, N]: {
    data: [T; N]

    () -> Self => Self()
}

export Clone: trait {}

export Copy: trait {}
