#*
    Codegen API
*#

use phantasm::prelude::*

// allow phantasm and cranelift code to include and implement these
// probably a good idea to use phantasm api directly here 

// Codegen: trait {}

// flat map of symbols to nodes
ReiDataflowIR: Map[Symbol, Node]

ReiDataflowIR: impl Codegen {
    generate_phantasm: (self) {
        self.iter(
            (sym, node) => match node {
                Compute {
                    // for each instruction, convert to corresponding phantasm instruction
                }
                Data {
                    // for each flattened complex data field, convert to corresponding point/signed numeric n-bit based on required precision
                }
            }
        )
    }
}
