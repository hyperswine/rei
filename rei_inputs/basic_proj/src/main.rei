use std::prelude::*

fn main() {
    println("Hello from sample!")

    // operator statements & operator expressions
    // expressions that exist by themselves are statements
    // i.e. if not assigned a let or const
    7 + 4 // evaluates to 11, and ditched

    // variable definitions
    let x = 2 + 5
    
}

class C {}

data D {
    a: Int
    // illegal
    // b: D
    b: ref D
}

# In rei, all references must have `ref` in front of it

/* Hi */
/* A multiline comment is anything between / and * not and not * and / 
otherwise it will end */
