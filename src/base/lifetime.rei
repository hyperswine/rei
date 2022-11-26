#*
    Complete Lifetime Elision (CLE)
*#

// ensure all borrows in a scope are valid, mostly to do with mutability
/*
let a = 3

let b = &a

{
    b //valid!

    let c = &a //valid!
}

let d = c //invalid, c does not exist in this scope

f: (a: &Int, b: &String) => ()

main: () {}
*/

export elide_lt: () => ()
