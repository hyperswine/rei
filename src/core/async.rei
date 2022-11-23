#*
    Async runtime
*#

// generate code that switches out the thread/returns early and saves the state
// so in terms of spectre, no popping of stack
// interms of cranelift... maybe the same idea. Maybe just inline the fn or treat it like another block
export async: macro {
    (body: Expr) {}
}
