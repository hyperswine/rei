# Examples

Here are a bunch of examples of rei code:

```rust
// Variables
let v = 10
const k = 8

let g: std::Int = 12
const m: std::String = "Hi".to_string()

// Functions
fn normal_function_let(param: String) {return "".to_string()}

let parametrisable_block = (param: Int) {param}

const immutable_function = () {}

// Functional programming
vec.map(v => {
    println("v is", v)
})

fn takes_another_fn(func: std::Function) {
    func()
}

takes_another_fn([]() => {})
@deduce
let anon_fn = [](i) {i}
takes_another_fn(anon_fn)
// note: moves by default, no copy unless Copy is implemented
takes_another_fn(&anon_fn)

// Classes, Objects, Traits

trait Copy {
    // note: self is always mutable unless you do const &self
    fn copy(const &self) -> Self
}

class Cc {
    i: Int
    c: C

    // illegal instruction!
    let k: Int = 5

    // must construct all fields
    construct() {
        self
    }
}

// IN REI, we have two big things
// data
// object

// data is immutable and only exists to be passed around
// objects are immutable and allows polymorphism through traits

// intel are only meant to store data
// they do not have associated functions, though you can use function pointers
data C {
    i: Int
}

// illegal! data cannot be implemented, only classes
impl C (Copy) {
    fn copy(const &self) {
        Self {self.i}
    }
}

```

EVERYTHING IS A FUNCTION IDEAL

```rust
// an Int is an alias for i32 in std
// i32 is simply a function with subfunctions

fn i32(val: numeric) {
    // stack or heap or static depends on the context
    let _val = val

    // when called by its function, returns the function
    fn to_i64() {
        i64(_val)
    }

    // when called by itself, returns its val
    _val
}
```
