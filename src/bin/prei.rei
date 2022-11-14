#*
    When building rei with feature Prei enabled, it should install that directly to your machine and append to PATH
*#

use prei::prelude::*

Command: enum {
    Build: "build"
    Test: "test"
    Run: "run"
    # like cargo check, a seqeunce of queries to ensure the codebase is valid
    Verify: "verify"
}

main: (command: Command) -> Status {
    match command {
        Build => build_project(std::get_working_dir())
        // with .. order matters!
        .. => todo()
    }
}
