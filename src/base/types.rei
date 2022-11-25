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
