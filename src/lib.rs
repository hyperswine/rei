extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::error::Error;
use pest::Parser;
use std::ffi::CString;

#[derive(Parser)]
#[grammar = "grammar/rei.pest"]
struct ReiParser;

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Integer(i32),
    Float(f64),
    Ident(String),
    Str(CString),
}

use self::AstNode::*;

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = ReiParser::parse(Rule::program, source)?;
    for pair in pairs {
        match pair.as_rule() {
            Rule::expr => {
                ast.push(Print(Box::new(build_ast_from_expr(pair))));
            }
            _ => {}
        }
    }

    Ok(ast)
}
