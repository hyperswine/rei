use reic::{tokenise, print_tokens};

fn main() {
    // 1. handle reic file.rei
    use std::env;
    use std::fs::read_to_string;

    // Take in command line args
    let args: Vec<String> = env::args().collect();

    // Check if there are enough args
    if args.len() != 2 {
        println!("Usage: reic <file.rei>");
        return;
    }

    // Read the file
    let file_str = read_to_string(&args[1]).expect("cannot read file");

    // Tokenise
    let res = tokenise(&file_str);

    // Print tokens
    print_tokens(&res);

    // Parse the file contents
    // let astnode = parse(&file_str).expect("unsuccessful parse");
    // println!("{:?}", &astnode);
}
