#*
    Rei Compiler Library
*#

// bitfields? should it just be structs of Bits?
Bit: bool
Bits[N]: [bool: N]

# default data(bits) allows the individual bits of Numeric to be manipulated
Numeric: Bits[u128]
Numeric[T, N]: [Bits[T]; N]

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

# Not ordered, simply the mechanism, no policy
export Operator: enum {
    Tilde: "~"
    ExclamationMark
    QuestionMark
    Ampersand: "&"
    Superscript: "^"
    Backtick
    At: "@"
    
    VBar: "|"
    DoubleVBar: "||"
    DollarSign
    PercentSign

    Plus
    Minus
    Star
    ForwardSlash: "/"
    BackSlash: "\\"

    Gt: ">"
    Gte: ">="
    Lt
    Lte
    Equals
    Equiv: "=="

    Colon: ":"
    Comma: ","
    DoubleColon: "::"
    SemiColon: ";"

    Ellipsis2: ".."
    Ellipsis3: "..."
}

// some ops may be only unary some maybe only binary

export UnaryOperator: enum {
    Prefix: enum {
        Star: Deref
        Ampersand: Ref
        Tilde: BitwiseNot
    }
    Postfix: enum {
        QuestionMark: PropagateOk
        ExclamationMark: PropagateErr
        Ellipsis2: Range
        Ellipsis3: Variadic
    }
}

// oh maybe the other way around? how to match a trait then? maybe use annotations?
export BinaryOperator: enum {
    // if impl for Bits, can use for Numeric and String
    Star: Mult
    Plus: Add
    Minus: Sub
    ForwardSlash: Div

    Ampersand: BitwiseAnd
    VBar: BitwiseOr
    Superscript: BitwiseXor

    Gt: Gt
    Lt: Lt
    Gte: Gte
    Lte: Lte
    
    Equiv: Equiv

    Ellipsis2: Range
}
