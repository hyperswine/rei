use std::prelude::*

fn main() {
    println("Hello from sample!")
}

class C {}

data D {
    a: Int
    // illegal
    // b: D
    b: ref D
}

# In rei, all references must have `ref` in front of it
