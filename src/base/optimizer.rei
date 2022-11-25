# always generate this so if you add more conds you can incrementally remake the hash
# maybe place this in expr or codegen or optimizer
export DirectJumpFn: {
    a: u64
    b: u64

    // would prob be propagated to the lower IR or be constructed there? or just conditions here
    (conditions: Vec[Condition]) -> Self {
        let conds = conditions.map(next_cond => {
            // cond, lhs, rhs
            next_cond.cond
        })

        // basically generate a universal hash fn that maps to either lhs or rhs (just random labels) based on the hashed result
        let n_labels = conditions.len()

        // conds are boolean expressions? so either you do a full pattern match
        // or consider each result and multiplex them to a single bitwise function
        let m = n_labels
        let M = std::log2(m)
        
        // The scheme assumes the number of bins is a power of two, m = 2^M. Let w be the number of bits in a machine word
        let a, b = loop {
            // can also generate like 2-10 at once and check them all
            let a = std::random(Odd, l=1, r=2^64)
            let b = std::random(r=2^(64-M))
            // test for conflicts
            conds.map(x => (a*x+b)>>(w-M)).has_duplicates() ? (a, b) : break
        }

        // store a
        Self {a, b}
    }

    hash: (a: u64, x: u64, w: u64, M: u64) => (a*x) >> (w-M)
}
