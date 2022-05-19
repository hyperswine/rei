#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Bool(bool),
    Num(String),
    Str(String),
    // operators
    Op(String),
    // semicolons, etc
    Ctrl(char),
    Ident(String),
    Fn,
    Let,
    Const,
    If,
    Else,
    For,
    While,
    // use == mod
    Use,
}

/*
Order of precedence (overloadable operators)
Order of precedence matters quite a lot and should be predictable

COMMON
()
::
.
*
/
+
-

*=
/=
+=
-=

**
++
--

COMPARISON
||
&&
<
>
..

LIST
,
|

EQUIVALENCE
==
*/

// NOTE: | means bitwise OR when using numeric. On other types, its free to overload

fn parse(tokens: &[Token]) {
    for token in tokens {
        // no backtracking. LR 1
        // https://en.wikipedia.org/wiki/Canonical_LR_parser#Constructing_LR(1)_parsing_tables

        // https://en.wikipedia.org/wiki/Recursive_descent_parser algorithm
    }
}

fn recursive_descent_parser() {

}
