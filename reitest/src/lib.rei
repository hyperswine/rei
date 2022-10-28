#*
    Rei Test Functionalities
*#

export enum Expect {
    Success
    Fail
}

use prei::registry
use reic::Expr

export annotation test(expect: Expect=Success, description: String?) {
    // get the token stream. Note stream is in reic::parse reexported in std
    let test_fn_ast = stream.parse(Expr::Fn)

    // register the fn into the prei registry
    registry.register_test(test_fn_ast)
}
