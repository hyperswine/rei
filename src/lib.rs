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
