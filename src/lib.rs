use std::vec;

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("//")]
    SinglelineComment,
    // mainly for doc comments, e.g. at the top of the file
    #[token("#")]
    HashComment,
    #[token("/*")]
    MultilineCommentBegin,
    #[token("*/")]
    MultilineCommentEnd,

    #[token("mod")]
    Module,
    #[token("namespace")]
    Namespace,
    #[token("use")]
    Use,
    #[token("class")]
    Class,
    #[token("fn")]
    Function,
    #[token("enum")]
    Enum,
    #[token("self")]
    SelfKeyword,
    #[token("macro")]
    Macro,
    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("static")]
    Static,
    #[token("new")]
    New,
    #[token("unsafe")]
    Unsafe,
    #[token("super")]
    Super,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("match")]
    Match,
    #[token("continue")]
    Continue,
    #[token("loop")]
    Loop,
    #[token("yield")]
    Yield,
    #[token("case")]
    Case,
    #[token("default")]
    Default,
    #[token("switch")]
    Switch,

    // Conditions
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("is")]
    Is,
    #[token("as")]
    As,
    #[token("in")]
    In,

    // @
    #[token("@")]
    OperatorAnnotation,
    // Usually in annotations and lists/tuples
    #[token(",")]
    OperatorComma,

    // Context sensitive
    #[token("(")]
    OperatorBracketLeft,
    #[token(")")]
    OperatorBracketRight,
    #[token("{")]
    OperatorCurlyBraceLeft,
    #[token("}")]
    OperatorCurlyBraceRight,

    // Compiler directive on f-strings
    #[token("$")]
    OperatorDollarSign,

    // OVERLOADABLE or SPECIFIC
    #[token("+")]
    OperatorPlus,
    #[token("-")]
    OperatorMinus,
    #[token("/")]
    OperatorLeftSlash,
    #[token("*")]
    OperatorStar,
    // LArrow [id] RArrow = <id> which means Option<id>
    #[token("<")]
    OperatorLeftArrow,
    #[token(">")]
    OperatorRightArrow,
    #[token("=")]
    OperatorEquals,
    #[token("==")]
    OperatorIdentity,
    #[token("!")]
    OperatorExclamation,
    #[token("?")]
    OperatorQuestion,
    #[token(".")]
    OperatorDot,
    // mainly for range based loops
    #[token("..")]
    OperatorDoubleDot,
    #[token(":")]
    OperatorColon,
    #[token("::")]
    OperatorDoubleColon,
    #[token("'")]
    OperatorSingleQuote,
    #[token("\"")]
    OperatorDoubleQuote,
    #[token("^")]
    OperatorUpArrow,

    // SPECIAL: label deref
    #[token("\\")]
    OperatorRightSlash,

    #[regex("[a-zA-Z]+")]
    Identifier,
    #[regex("[0-9]+")]
    Number,
    // ? I have no idea if this would work
    #[regex(r"\d*\.*+\d+")]
    Fraction,

    // Logos requires one token variant to handle errors,
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

pub fn tokenise(file: &str) -> Vec<Token> {
    let mut lex = Token::lexer(file);

    // let res = lex.filter_map(|t| t.ok()).collect();

    vec![]
}
