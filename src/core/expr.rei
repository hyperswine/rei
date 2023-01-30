// Probably defined in here and integrated into the compiler by prei (higher up software that links core)
// Probably automatically through using core/std functionality since they use these fields

UNARY_OP_PRECENDENCE: [QuestionMark ExclamationMark Star Ampersand]
BITWISE_OP_PRECEDENCE: [Not And Or XOr]
OP_PRECEDENCE: [Paren BITWISE_OP_PRECEDENCE Star LeftSlash Modulo Plus Minus]
