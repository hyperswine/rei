use std::{collections::HashMap, process::exit};

use logos::Logos;

use std::mem::Discriminant;

use lazy_static::lazy_static;

use std::sync::Mutex;

// ---------------
// LEXING
// ---------------

// Symbols
// ident : attr

// Symbols also store comment or metacode info. Reidoc simply takes all hash comment lines and associates them with symbols. I guess I can also go for multiline hash comments

// Remember to derive serde for all structures
// * Can change this to SymbolTable
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum SymbolType {
    Function,
    Class,
    Data,
    Variables,
}

/// The basic foundation for a complex program
/// Can be used to "trickle up" for parse nodes
/// A symbol is associated with a token (lexed) and is non trivial. E.g., not an operator or keyword. An identifier, number, float, double quoted string, single quoted string, enhanced string, document comments
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Symbol {
    symbol_type: SymbolType,
    lex_val: String,
}

impl Symbol {
    pub fn new(symbol_type: SymbolType, lex_val: String) -> Self {
        Self {
            symbol_type,
            lex_val,
        }
    }
}

type Identifier = String;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Namespace {
    elements: HashMap<Identifier, Symbol>,
}

impl Namespace {
    pub fn new(elements: HashMap<Identifier, Symbol>) -> Self {
        Self { elements }
    }

    pub fn get_symbol(&mut self, identifier: &str) -> &mut Symbol {
        self.elements
            .get_mut(identifier)
            .unwrap_or_else(|| panic!("Symbol doesnt exist"))
    }

    pub fn add_symbol(
        &mut self,
        identifier: &str,
        symbol_type: SymbolType,
        lex_val: &str,
    ) -> Result<&Symbol, &'static str> {
        // check if symbol already exists in the namespace
        match self.elements.get(identifier) {
            Some(_) => Err("Symbol already exists"),
            None => {
                let r = Symbol::new(symbol_type, lex_val.to_owned());

                self.elements.insert(identifier.to_owned(), r);

                let res = self.elements.get(identifier).unwrap();

                Ok(res)
            }
        }
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Self {
            elements: Default::default(),
        }
    }
}

/// Symbol table is filled in by the lexer automatically
/// As callbacks for each token
pub struct SymbolTable {
    // a symbol must be uniquely identified within its scope
    namespaces: HashMap<String, Namespace>,
}

impl SymbolTable {
    pub fn new(namespaces: HashMap<String, Namespace>) -> Self {
        Self { namespaces }
    }

    // I think parser or lexer needs to add the namespaces properly
    // Or just have a generic enum SymbolTable
    // Then gets serialised as usual
    pub fn add_namespace(&mut self, name: &str) {
        match self
            .namespaces
            .insert(name.to_owned(), Namespace::default())
        {
            Some(ns) => panic!(
                "Namespace {} already exists, are you sure it was at the right level?",
                name
            ),
            None => {}
        }
    }

    pub fn insert_value(
        &mut self,
        namespace: &str,
        identifier: &str,
        symbol_type: SymbolType,
        lex_val: &str,
    ) -> &Symbol {
        match self.namespaces.get_mut(namespace) {
            Some(ns) => match ns.add_symbol(identifier, symbol_type, lex_val) {
                Ok(symbol) => symbol,
                Err(msg) => panic!("{} in namespace {}", msg, namespace),
            },
            None => panic!("Namespace doesnt exist, did you make it first?"),
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            namespaces: Default::default(),
        }
    }
}

lazy_static! {
    static ref SYMTAB: Mutex<SymbolTable> = Mutex::new(SymbolTable::default());
}

// Use Extras to store extra symbol info. Instead of a symbol table
// Although it would still be good to build when generating IR and assembly
// And to end lexing quickly if the same symbol name is present in the same scope
// So if Hashmap.insert("something") would be off, exit(1)

/// A token in rei
#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[regex("//.*")]
    SinglelineComment,
    // mainly for doc comments, e.g. at the top of the file
    #[regex("#.*")]
    HashComment,
    #[regex("/[\\*]([^\\*]|([\\*][^/]))*[\\*]+/")]
    MultilineComment,
    #[regex("#[\\*]([^\\*]|([\\*][^/]))*[\\*]+#")]
    MultilineHashComment,

    #[token("mod")]
    Module,
    // Not defined in the standard, but pasm
    // #[token("namespace")]
    // Namespace,
    #[token("internal")]
    Internal,
    #[token("pub")]
    Pub,
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
    #[token("super")]
    Super,
    // I was going to make this a core::function
    // #[token("macro")]
    // Macro,
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
    // no such thing as 'default'. Just match each possible case either T or None for enums
    #[token("match")]
    Match,
    #[token("continue")]
    Continue,
    #[token("loop")]
    Loop,
    #[token("yield")]
    Yield,

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
    At,
    // Usually in annotations and lists/tuples
    #[token(",")]
    Comma,

    // Parentheses can be overloaded like C++ classes
    #[token("(")]
    ParenLeft,
    #[token(")")]
    ParenRight,
    // Context sensitive, signifies scoping
    #[token("{")]
    CurlyBraceLeft,
    #[token("}")]
    CurlyBraceRight,

    // Compiler directive on f-strings
    #[token("$")]
    DollarSign,

    // OVERLOADABLE or SPECIFIC
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    LeftSlash,
    #[token("*")]
    Star,
    // LArrow Ident RArrow = <id> which means Option<id>
    #[token("<")]
    LeftArrow,
    #[token(">")]
    RightArrow,
    #[token("=")]
    Equals,
    #[token("==")]
    Identity,
    #[token("!")]
    Exclamation,
    #[token("?")]
    Question,
    #[token(".")]
    Dot,
    // mainly for range based loops
    // Parser: range_expr or range_op
    #[token("..")]
    DoubleDot,
    #[token(":")]
    Colon,
    // usually module qualification
    // Parser: scan the inner namespace for that specific mod Identifier's existence
    #[token("::")]
    DoubleColon,
    #[token("'")]
    SingleQuote,
    #[token("\"")]
    DoubleQuote,
    #[token("^")]
    UpArrow,

    // SPECIAL: label deref
    // #[token("\\")]
    // RightSlash,

    // slice().parse() should be good for most things?
    #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
    Identifier,
    #[regex("[-][0-9]+", |lex| lex.slice().parse())]
    Int(i64),
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

    // * If an lexer error is detected, panic. Maybe just use Whitespace?

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
    // Build symbol table (DONE WITH LAZY STATIC)
    // let symtab = SymbolTable::default();

    let mut tokens = Token::lexer(file);

    let mut str_ = tokens.source();
    log::info!("\n====SOURCE====\n{}\n==============", str_);

    // Generating labels for scoped blocks. In the end, you cant use namespaces because ELF doesnt have an idea of what that is. Later just generate random names for all the labels
    // 1. collect all the explicitly labelled blocks and add them to the symtab

    let res = tokens.spanned().collect();

    res
}

pub fn print_tokens(tokens: &Vec<(Token, std::ops::Range<usize>)>) {
    for token in tokens {
        log::info!("token = {:?}", token.0);
        log::info!(" range = {:?}", token.1);
    }
}

// ----------------
// PARSING
// ----------------

use serde_derive::{Deserialize, Serialize};

// https://www.tutorialspoint.com/compiler_design/compiler_design_symbol_table.htm symbol table design

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ParseTree {
    Leaf(LeafData),
    Internal(InternalData, Vec<ParseTree>),
    Root(RootData, Vec<ParseTree>),
}

// NOTE: L-attr SDT with inh and synth attr

// Assumes tree has at least two nodes that is the filename and content
/// An empty file is a valid program. You can compile and run it. It will simply have a minimal _start and empty main function that returns 0 and links to std if specified
/// Its like fn main() {}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct RootData {
    name: String,
}

const DEFAULT_NAME: &'static str = "content";

impl RootData {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
        }
    }
}

impl Default for RootData {
    fn default() -> Self {
        Self {
            name: DEFAULT_NAME.to_owned(),
        }
    }
}

// Do not expose to main
impl ParseTree {
    fn new_leaf(lex_value: &str) -> Self {
        Self::Leaf(LeafData {
            lex_value: lex_value.to_owned(),
        })
    }

    fn new_internal(data: InternalData, children: Vec<ParseTree>) -> Self {
        Self::Internal(data, children)
    }

    fn new_default_internal() -> Self {
        Self::Internal(InternalData::default(), vec![])
    }

    fn new_root(name: &str, children: Vec<ParseTree>) -> Self {
        Self::Root(RootData::new(name), children)
    }

    fn new_default_root() -> Self {
        Self::Root(RootData::default(), vec![])
    }

    /// Insert a node (subtree) as this node's child
    /// Would be the last child
    fn insert_child(&mut self, parsetree: ParseTree) {
        // if root or internal, append it, else dont do anything
        match self {
            ParseTree::Leaf(_) => {}
            ParseTree::Internal(_, children) => {
                children.push(parsetree);
            }
            ParseTree::Root(_, children) => {
                children.push(parsetree);
            }
        }
    }
}

// IDK if inherited and synthd attrs should be in the node or symtab
// I think it makes more sense to put most of it in the symtab. But inh/synthd attr are built during parsing

/// Represents a production like T -> T E'
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InternalData {
    inherited_attr: String,
    synthesized_attr: String,
    children: Vec<InternalData>,
}

impl InternalData {
    pub fn new(inherited_attr: &str, synthesized_attr: &str, children: Vec<InternalData>) -> Self {
        Self {
            inherited_attr: inherited_attr.to_owned(),
            synthesized_attr: synthesized_attr.to_owned(),
            children,
        }
    }
}

impl Default for InternalData {
    fn default() -> Self {
        Self {
            inherited_attr: Default::default(),
            synthesized_attr: Default::default(),
            children: Default::default(),
        }
    }
}

/// Represents a nonterminal production like ident -> [_|ASCII_ALPHA]{[_|ASCII_ALPHANUMERIC]}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LeafData {
    lex_value: String,
}

impl LeafData {
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

// BASIC IDEA:
// For each level, build a node, call any recursive functions then add that to your current node. Then return your node

/// Parse those tokens boy
/// Dont need ranges for increment lookahead. Just use token seq directly
/// Though good to have for error messages
fn parse<T>(tokens: &[Token]) -> ParseTree {
    // Create tree
    let content_node = ParseTree::new_root("content", vec![]);

    // DEFINE FUNCTIONS (Leaf -> Root)

    // Should be k lookahead
    let mut lookahead = 0;

    // I think also a curr_token
    let curr_token: &Token = tokens.first().unwrap_or(&Token::ERROR);

    // Match function
    let mut match_token = |t: Token| {
        if t == tokens[lookahead] {
            lookahead += 1;
        }
    };

    let mut use_stmt = || {
        // entry
        let mut res = ParseTree::new_internal(InternalData::default(), vec![]);

        // use -> ident
        if tokens[lookahead] == Token::Use {
            // 1 ident
            if tokens[lookahead + 1] == Token::Identifier {
                // take value of identifier
                // match tokens[lookahead].into() {
                //     Ok(_) => todo!(),
                //     Err(_) => todo!(),
                // }

                // 0 or more :: ident
                let mut i = 2;
                while tokens[lookahead + i] == Token::DoubleColon
                    && tokens[lookahead + i + 1] == Token::DoubleColon
                {
                    i += 2;
                    // TODO: build leaf nodes for each token and add to res
                    // use symbol table if needed to link the attrs with the `Ident` type of each token. I THINK WE NEED A UNIQUE ID with each token so symboltable {id: Symbol}
                    // something like SymbolTable.search(token_id)

                    let leaf = ParseTree::new_leaf("");
                    res.insert_child(leaf);
                }

                // * push res

                return Some(res);
            } else {
                // error! stop program. Could also panic or try to resolve it by converting chars (push error onto stack). But not really bothered to do that
                panic!("Error! Undefined `use` expression");
            }
        }
        None
    };

    // * return (true, subtree) if possible. Just return true if one of them is right. Otherwise (false, ()). Or just Option<ParseTree>
    let mut stmt = || {
        // stmt entry
        // * should prob have a ParseTree::new_internal_default()
        let mut res = ParseTree::new_internal(InternalData::default(), vec![]);

        // use statement
        match use_stmt() {
            Some(r) => {
                res.insert_child(r);
                return Some(res);
            }
            None => return None,
        }

        // class def

        // function def

        // data def

        // variable def

        // control def

        // SKIP any Error token and Comment tokens

        // no more statements
        None
    };

    // Start symbol
    let mut content = || {
        // zero or more statements
        while let res = stmt() {
            match res {
                Some(r) => {
                    println!("Found a statement")
                    // * add as a child
                }
                None => break,
            }
        }
    };

    content();

    content_node
}
