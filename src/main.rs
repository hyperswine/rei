use reic::parse;

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
    let file = read_to_string(&args[1]).unwrap();

    let res = parse(&file);
}
