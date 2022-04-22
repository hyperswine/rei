#[cfg(not(test))]
fn main() {
    // 1. handle reic file.rei
    use std::fs::read_to_string;
    use std::env;

    // Take in command line args
    let args: Vec<String> = env::args().collect();

    // Check if there are enough args
    if args.len() != 2 {
        println!("Usage: reic <file.rei>");
        return;
    }

    println!("file = {}", &args[1]);

    // Read the file
    let file_str = read_to_string(&args[1]).unwrap();
}

pub mod ast;
pub mod tok;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub calc);

#[test]
fn calc() {
    let expr = calc::ExprParser::new()
        .parse("22 * 44 + 66")
        .unwrap();
    assert_eq!(&format!("{:?}", expr), "((22 * 44) + 66)");
}
