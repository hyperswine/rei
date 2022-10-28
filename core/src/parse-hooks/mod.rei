#*
    Parser Hooks
*#

use reic::{Parser,Expr,ParserError}

extend Parser {
    @priority(1)
    apply_priority_hooks: (&mut self) -> ParseRes {
        ref_expr()!
        deref_expr()!
        float_expr()!
        literal_expr()!
    }

    @priority(1)
    apply_low_priority_hooks: (&mut self) -> ParseRes {}

    // CUSTOM DEFS
    ref_expr: (&mut self) -> ParseRes {}

    // Shadow
    literal_expr: (&mut self) -> ParserRes {}
}
