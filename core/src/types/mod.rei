#*
    Useful core types
*#

export i8: Int8
export i16: Int16
export i32: Int32
export i64: Int64
export i128: Int128

export Range<Index>: {
    start: Index
    end: Index
}

// Duo types can be implicitly casted into Result<T,E> types
// Option types can also be casted into Result<T,E> types

export Debug: trait (&self, formatter: &mut Formatter) -> () | FmtError

export Display: trait (&self, formatter: &mut Formatter) -> () | FmtError

export Clone: trait {}

export Copy: trait {}

# for @derive(Display)
export Display: annotation(derive) (tokens: Tokens) {
    match tokens.parse() {
        Ok(vis, ident, fields...) => {
            add_to_namespace(
                // NOTE: the $operator means expr.embed() which can be impl'd through the Embed trait fn
                impl $ident: Display(&self, formatter: &mut Formatter) -> () | FmtError {
                    @rei {
                        fields.map(f => f.display())!?
                    }
                }
            )
        }
        Err(e) => {
            panic!("Could not derive display, did you do it right?")
        }
    }
}

#*
    Typess
*#

// Result = enum {} or uhh?

export Result<T, E>: enum {
    Ok = T
    Error = E
}

// PARSER HOOKS

// would actually make sense if you just did Parser = {}

// LET LINKER FIGURE IT OUT
// always resolved and shadowed in this order: base -> core -> std -> custom
// rei reflection => the main expr() fn is able to run all registered functions (at link or compile time)

// AST traversal??

// ExprRes = Result<T, E>
use reic::Parser::ExprRes

// add an expr to Expr
extend Expr: enum {
    // extend OperatorExpr::BinaryOp {}
}

extend reic::Parser {}
