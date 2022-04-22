use reic::Token;

fn main() {
    // 1. handle reic file.rei
    use std::fs::read_to_string;
    use std::env;

    // Take in command line args
    let args: Vec<String> = env::args().collect();

    // Check if there are enough args
    if args.len() != 1 {
        println!("Usage: reic <file.rei>");
        return;
    }

    // Read the file
    let file = read_to_string(&args[1]).unwrap();

    // 2. parse the file
    let tokens = reic::tokenise(&file);
}
