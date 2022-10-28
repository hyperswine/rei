#*
    Async runtime
*#
// only compile + link this module if neutron is the target
@!config(target_os = "neutron")

export Async: {
    body: Expr
}

// normal fns are "parameterised scoped expressions"
// export spawn: () {}

use reic::Parser

// would a vis modifier be required? since the annotation is in parser?
// and executed there?
// I mean wouldnt the annotation be executed here? its imported here and executed locally
// using a global static mutable variable

// maybe an Ok expr (ok_expr) in core?? Same as Err (err_expr)
// if fns small enough, should all be able to fit in local core cache (L1)
// the more a parse expr is used, the more it'll be in cache
// no need to inline. Only inline if its like a short expr, one line immediate value fn usually
// then the compiler would prob inline it anyway unless otherwise configured

// impl async in the std lib
export Parser: extend {
    // its the @priority that actually does the work
    @priority = High
    async_expr: (&mut self) -> ParseRes {
        self.accept(Token::Async)?

        // return async node
        Ok Async(self.expr().unwrap())
    }
}

// example async code
/*
f[T]: async (t: T) -> T {
    t
}

g: () {
    # same as f(()) 
    await f()
}

main: () {
    g()
}
*/
