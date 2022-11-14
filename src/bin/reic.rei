#*
    Compiles to a 'rei' executable
*#

use base::*

Subcommand: enum {
    // IDE should highlight blue: green
    Compile: Compile
    Test: "t" | "test"
}

Compile: {
    // alias is the actual command
    alias: "c" | "compile"
    // other fields are parsed in order. If you want error checking, impl it yourself
    files: &[Path]
}

main: (subcommand: Subcommand, args: String...) {
    match subcommand {
        Compile => {
            // files? uhh how does clap do it?
        }
    }
}

#*
    When you run `rei test`
*#

// String should be a unicode string in std. In core lib, should use core::String which is ascii? Std reexports core::String as core::AsciiString and shadows core::String with std::types::UnicodeString as String
test: (test_names: String...) {
    test_names.for_each(
        t => {
            // get from the cached list of test fns
            let tf = get_test_fn(t)
            test_fn(tf)
        }
    )
}

TestFn: () -> ()

test_fn: (tf: TestFn) {
    tf()
}
