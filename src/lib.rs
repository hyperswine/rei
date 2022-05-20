use std::collections::HashMap;

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[regex("//.*")]
    SinglelineComment,
    // mainly for doc comments, e.g. at the top of the file
    #[regex("#.*")]
    HashComment,
    #[regex("/[\\*]([^\\*]|([\\*][^/]))*[\\*]+/")]
    MultilineComment,

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
    // #[token("\\")]
    // OperatorRightSlash,
    #[regex("[a-zA-Z]+")]
    Identifier,
    #[regex("[0-9]+")]
    Number,
    #[regex("-?[0-9]+\\.[0-9]+", |lex| lex.slice().parse())]
    Float(f64),
    // For ascii printable "strings" (without backslash)
    // I dont think it works for \escaped " quotes. \\ doesnt work
    #[regex("\"(?:[^\"]|\\.)*\"")]
    DoubleQuotedString,
    // For 'strings'
    #[regex("'(?:[^\"]|\\.)*'")]
    SingleQuotedString,
    // For `strings`
    #[regex("`(?:[^\"]|\\.)*`")]
    DashQuotedString,

    // Logos requires one token variant to handle errors,
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,
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

pub fn tokenise(file: &str) -> Vec<(Token, std::ops::Range<usize>)> {
    let mut tokens = Token::lexer(file);

    tokens.spanned().collect()
}

pub fn print_tokens(tokens: &Vec<(Token, std::ops::Range<usize>)>) {
    for token in tokens {
        print!("token = {:?}", token.0);
        println!(" range = {:?}", token.1);
    }
}

// ----------------
// PARSING
// ----------------

use serde_derive::{Deserialize, Serialize};

// https://www.tutorialspoint.com/compiler_design/compiler_design_symbol_table.htm symbol table design

// ident : attr
struct Symbol<'sym> {
    ident: &'sym str,
    attr: &'sym str,
}

struct Namespace<'a> {
    elements: HashMap<&'a str, &'a str>,
}

struct SymbolTable<'a> {
    // a symbol must be uniquely identified within its scope
    symbols: HashMap<&'a str, Namespace<'a>>,
}

// NOTE: L-attr SDT with inh and synth attr

// Assumes tree has at least two nodes that is the filename and content
/// An empty file is a valid program. You can compile and run it. It will simply have a minimal _start and empty main function that returns 0 and links to std if specified
/// Its like fn main() {}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ParseTree {
    name: String,
    content: ParseNode<InternalNodeData>,
}

impl ParseTree {
    pub fn new(name: String, content: ParseNode<InternalNodeData>) -> Self {
        Self { name, content }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ParseNode<T> {
    t: T,
    is_leaf: bool,
}

impl<T> ParseNode<T> {
    fn new(t: T, is_leaf: bool) -> Self {
        Self { t, is_leaf }
    }
}

impl ParseNode<InternalNodeData> {
    pub fn new_internal_node(
        inherited_attr: String,
        synthesized_attr: String,
        children: Vec<InternalNodeData>,
    ) -> Self {
        Self {
            t: InternalNodeData::new(inherited_attr, synthesized_attr, children),
            is_leaf: false,
        }
    }
}

impl ParseNode<LeafNodeData> {
    pub fn new_leaf_node(lex_value: String) -> Self {
        Self {
            t: LeafNodeData::new(lex_value),
            is_leaf: true,
        }
    }
}

// should represent a production like T -> T E'
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InternalNodeData {
    inherited_attr: String,
    synthesized_attr: String,
    children: Vec<InternalNodeData>,
}

impl InternalNodeData {
    pub fn new(
        inherited_attr: String,
        synthesized_attr: String,
        children: Vec<InternalNodeData>,
    ) -> Self {
        Self {
            inherited_attr,
            synthesized_attr,
            children,
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LeafNodeData {
    lex_value: String,
}

impl LeafNodeData {
    pub fn new(lex_value: String) -> Self {
        Self { lex_value }
    }
}

/*
COOL PARSING ALGOS:
 1. no backtracking. LR 1
    https://en.wikipedia.org/wiki/Canonical_LR_parser#Constructing_LR(1)_parsing_tables
 2. https://en.wikipedia.org/wiki/Recursive_descent_parser algorithm and https:/
    /www.geeksforgeeks.org/recursive-descent-parser/
*/

/// Parse those tokens boy
fn parse(filename: &str, tokens: &[Token]) {
    // create tree
    let content_node = ParseNode::new_internal_node("".to_owned(), "".to_owned(), vec![]);
    let ast = ParseTree::new(filename.to_owned(), content_node);

    for token in tokens {
        // priority grammar descent


    }
}


