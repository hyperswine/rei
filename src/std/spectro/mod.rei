@!cfg(feature="spectro")

Wire: Wire[0]
Wire[N]: [bool; N]

Bits: Vec[bool]
Bits[N]: [bool; N]

// auto generate quick Bits1-16,32,64,128?? hmm nah

// maybe the root element itself is self contained
// doesnt have an output. And must be linked
// maybe allow simulate to also take in different inputs

// for simulate overload 0, we dont care about the external interface

// simulate an element, usually the root element...!
simulate: (clock: Clock, element: Element, input: Input) {
    clock.run(() => element(input))
}

# mostly to interface with the mapper (FPGA)
RootElement: trait (input: Input) -> Output

// a clock has a freq of ticks
Clock: u64

// prob use core::time? or core::sync for clock...?
// basically need to interact with quartz component
// simulate quartz with core::time

Clock: extend {
    // should be inlined usually
    run: (&self, f: () -> Output) {
        // wait for freq. This is the implicit pattern
        stall(self)

        // call element with its bound inputs
        f()
    }
}

Schematic: {
    gates: Tree[Gate]
}

// maybe just make gates 2->1 or 1->1
// dont bother trying to do an n->1...?

Gate[In]: {
    inputs: Bits[In]
    output: Bit
}

Gate: enum {
    And
    Or
    Xor
    Nand
    Not
}

# Generate an RTL or map from the elements connected?
generate: (f: Fn) -> Schematic? {
    // for each & and | expression, convert to a Gate
    match f {
        BinOp(lhs, op, rhs) => {
            match op {
                Ampersand => Gate::And
            }
        }
        _ => ()
    }
}

lower: (root_elem: Fn) -> Schematic? {
    // it recursively calls generate?
    generate(root_elem)
}
