#*
    Rei Compiler, by Jasen
*#

# full file compile
main: (file: &str) -> Status {
    let file_contents = std::os::read_to_string(file)

    let res = pkg::compile_expr(file_contents)?

    // phantasm
    pkg::generate_code(res)
}
