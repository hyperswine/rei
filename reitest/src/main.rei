#*
    When you run `rei test`
*#

// String should be a unicode string in std
// in core lib, should use core::String which is ascii?
// std reexports core::String as core::CString and shadows core::String with std::types::UString as String

fn main(test_names: String...) {
    test_names.for_each(
        t => {
            // get from the cached list of test fns
            let tf = get_test_fn(t)
            test_fn(tf)
        }
    )
}

type TestFn = fn() -> ()

fn test_fn(tf: TestFn) {
    tf()
}
