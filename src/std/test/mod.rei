#*
    Rei Test Functionalities
*#

export Expect: enum {
    Success
    Fail
}

use prei::registry
use reic::Expr

export test: annotation (expect: Expect=Success, description: String?) {
    // get the token stream. Note stream is in reic::parse reexported in std
    let test_fn_ast = stream.parse(Expr::Fn)

    // register the fn into the prei registry
    registry.register_test(test_fn_ast)
}
