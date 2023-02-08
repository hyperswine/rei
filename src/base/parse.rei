#*
    PARSING
*#

// use core::expr::*
// use std::expr::*

Parser[T]: {
    t: T

    
}

ParserError: Token | String
ParseRes: Expr | ParserError

// great idea, just make a new function like split_first: (self, String) -> (String, String)
// and Char is castable to String, and the other way around if len is 0, otherwise UB?
// maybe overload the function to match String; 1

char: (input: String, to_match: Char) -> Parser[CharUtf8, String] => input.split_first(to_match)
string: (input: String, to_match: String) -> Parser[String, String] => input.split_first(to_match)

one_of: (input: String, to_match: [String]) -> Parser[_] => _

// 1+
many: (input: String) => _
// does this work?
many: (self) => many

// 0*
zero_or_many: (input: String) => _

// consumes 1 of by on either side
padded_by: (input: String, to_match: String, by: String) => _

// consumes all of by on either side
padded_by_many: (input: String, to_match: String, by: String) => _

Hs: " \t"
Ws: hs + "\n"

// here, then is just a function rather than a trait
then: (f: Parser) -> Parser[_] => _
// left associative
(~): Unary(then, Postfix, precedence=Precedence::Low)

// idk actually
// then_hs: _
// then_ws: _
// always consume all
// then_maybe_hs: _
// then_maybe_ws: _

ws: many(Ws)
hs: many(Hs)
ws_plus: many1(Ws)
hs_plus: many1(Hs)

// (>>): Unary(then, Postfix, precedence=Precedence::Low)

// e.g. many(ws)

// EXPRS

// many_ws: many(ws)
// many_hs: many(hs)
// padded_by_hs: padded_by_many(hs)
// padded_by_ws: padded_by_many(ws)

// or has a high priority in expr and is left associative with early cutoff
expr: (input: String) -> Expr => def() or context()

def: (input: String) -> Parser[Def] {
    raw_ident(input)
    .hs(generic_param_expr)
    .ws(colon)
    .ws_plus(def_body)
}

def_body: (input: String) -> Parser[DefBody] => parameterised or algebraic_expr or replace_expr

parameterised: (String) => paren_param_list($1).many()
