use reic::{print_tokens, tokenise};
use std::path::Path;

/// Usage: reic <files.rei>
/// Output: YamlAST | pasm | lib.a
fn main() {
    // INIT LOGGING
    env_logger::init();

    // 1. handle reic file.rei
    use std::env;
    use std::fs::read_to_string;

    // Take in command line args
    let args: Vec<String> = env::args().collect();

    // Check if there are enough args
    if args.len() < 2 {
        println!("Usage: reic <file.rei>");
        return;
    }

    // Read the file
    let mut files = vec![];

    for i in 1..args.len() {
        let res = read_to_string(&args[i]).expect("cannot read file");
        // get filename from path
        let path = &args[i];
        let path = Path::new(&path);
        let mut filename = path.file_stem().unwrap().to_str().unwrap().to_owned();

        // for each file string, attach mod filename { to the start and append } to the end
        // NOTE: doesnt work for subdirectories, yet. For subdirs, maybe just prei
        // I guess we can also take the full path into account
        filename = format!("mod {} {{\n", filename) + &res;
        filename = filename + "\n}";

        files.push(filename);
    }

    // Combine file strings
    let res = combine_files(&files);

    // Tokenise
    let res = tokenise(&res);

    // Print tokens
    print_tokens(&res);

    // Parse the file contents
    // let astnode = parse(&file_str).expect("unsuccessful parse");
    // println!("{:?}", &astnode);
}

/// Combine files into a single output file
/// NOTE: without prei, theres no distinction between `main()`. So there must be a file called main.rei to make an executable as preproc is done for the root{} module
/// Otherwise just compiles as a static library
fn combine_files(files: &Vec<String>) -> String {
    files.iter().fold("".to_owned(), |acc, next| acc + next)
}
