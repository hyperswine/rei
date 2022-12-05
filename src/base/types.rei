#*
    Rei Compiler Library
*#

// bitfields? should it just be structs of Bits?
Bit: bool
Bits[N]: [bool: N]

# default data(bits) allows the individual bits of Numeric to be manipulated
Numeric: Bits[u128]
Numeric[T, N]: [Bits[T]; N]

export BASE_NUMERIC_VALUE: 0

# "*". In core, we also allow modifiers like r"*"
BaseString: ()

// Rei types as first class features
ReiType: enum {
    Callable: enum {
        Fn
        Macro
    }
    Object: enum {
        # either key: val or tuples
        Data: Vec[ReiType] | HashMap[String, ReiType]
        Enum
    }
    Primitive: enum {
        # technically, this is the onl type. But base also defines other "types" in a similar primitive way for ergonomics
        Bits: Bits
        Numeric: Numeric
        # maybe in base, the code only knows of literal arrays of chars
        String: std::String
        # 0 for ASCII, 1 for variable UTF8
        Char8: i8
        Char16: i16
    }
}

UnaryType: Prefix | Postfix

Descriptor: {
    one: Bit
    two: Bits[2]
}

# return a list of operators
export Operators: () -> _ {
    Operator.for_each(o => o.to_string())
}

# Not ordered, simply the mechanism, no policy
export Operator: enum {
    @equivable
    Tilde: "~"
    ExclamationMark: "!"
    QuestionMark: "?"
    @equivable
    Ampersand: "&"
    @equivable
    Superscript: "^"
    Backtick: "`"
    At: "@"

    @equivable
    VBar: "|"
    DoubleVBar: "||"
    DollarSign: "$"
    @equivable
    PercentSign: "%"

    @equivable
    Plus: "+"
    @equivable
    Minus: "-"
    @equivable
    Star: "*"
    @equivable
    ForwardSlash: "/"
    BackSlash: "\\"

    Gt: ">"
    Gte: ">="
    Lt: "<"
    Lte: "<="
    Equals: "="
    Equiv: "=="

    Colon: ":"
    Comma: ","
    DoubleColon: "::"
    SemiColon: ";"

    Ellipsis2: ".."
    Ellipsis3: "..."
}

// equivable operators mean something like += is taken as '+=' instead of '+', '='
// and as a "assign to self"
// so &= means specifically expr = expr & expr

// what pattern is better, annotating or fn returning?
// annotating can make it more concise which is good, also reduces duplication
// but fn can make it more customisable and readable in some cases

// expr op= expr
// export operate_equable: () -> &[Operator] {
//     [
//         Tilde, 
//     ]
// }

export trait UnaryOp[T, Rhs, Res]: (self, op_type: T, rhs: Rhs) -> Res
export trait BinaryOp[T, UnaryType, Res]: (self, unary_type: UnaryType, op_type: T) -> Res
