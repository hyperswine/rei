#*
    Rei Compiler Library
*#

// LEX
use Rust::reic::lex::*
export LEXER: ReiLexer

# incremental compile expr
export compile_expr: (string: String) -> Vec[Instruction] {
    let expr = parse(LEXER.lex(file_contents)).expr()
    lower(expr)
}

/*
    When parsing a program:
    go from top to bottom, high prio to low prio
    the top most expr should be at the top of the tree
    expression priority goes from left to right (index 0..MAX)
*/

use core::types::bits

# data(bits) allows the individual bits of Numeric to be manipulated
Numeric: bits[u128]

// Interpreted Rei Type
// First class objects
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
        String: std::String
    }
}

UnaryType: enum {
    Prefix
    Posfix
}

// bitfields? should it just be structs of Bits?
Bit: bool
Bits[N]: [bool: N]

Descriptor: {
    one: Bit
    two: Bits[2]
}

// kind of like?
// maybe no "position", just ordering?
/*
X: bits {
    one
    two: 4
}
*/

export macro Bits: (ident: Ident) -> Field {
    ident: bool
}

export macro Bits: (ident: Ident, size: Size) -> Field {
    ident: [bool; size]
}

// recursively call another Bits
export macro Bits: (_: Colon, ident: Ident, scope: ScopeExpr) -> Data {
    ident: {
        scope.exprs().map(expr => match expr {
            Ident(id) => Bits(id)
            UniversalDef(id, rhs) => Bits(id, rhs)
            Other(other) => panic("Unexpected expression {other}, expected a bitfield expr!")
        })
    }
}

// process macros
/*
find ident and scope, expect that in code
*/
