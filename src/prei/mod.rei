#*
    Prei is a vital tool for managing rei projects
*#

use pkg::base::[Symbol Symtab]

Symtab: {
    items: Vec[Symbol]
}

// symbol should be declared in rei base?
// also the base symtab?
// a symbol like a function may have references to other symbols or own them
Symbol: extend {
    // module symbol tables have a export modifier for each defined item
    // not sure if this should be here or as an enum of the symbol table
    // yea hold sub symbol tables and not symbols? maybe just the sub symbol table (if defined) or a reference to the node
    // and higher logic can deal with that
    Module
}

# symbols defined in a prei project
PreiSymbols: {
    root: Symtab
}

Symtab: extend {
    Module: {
        
    }
}

/*
    Rei + Prei Defines:

    : means         general definition
    => means        "one line" general definition
    = means         sequential definition
    export means    universal definition (prei)
    no export       means local definition (prei)
*/

// to prevent circular dependencies, keep this macro local
// since you wont be linking prei with prei? wait but you are
// maybe it wont highlight it if it detects the AST? because export here isnt a macro but an ident
export: macro {}

/*
    Preventing circular dependencies:

    Rei is structured as a self defining language
    Syntax highlighting is defined externally based on configuration options.
        For example: prei defines a few macros that are meant to be interpreted as key functionalities like mod and export
        The language server interface expects a bunch of {token: ViewOptions}. Note it is a dictionary so no conflicts are allowed, or the latest one is replaced
        The rei source code includes that interface and exports its definitions via @ViewOption() annotations, which are seen by the user of the interface on a higher level, i.e. rein
        Rein includes lang_server::view_options and calls it on a prei project. The prei library should then return the view options
        Rein is then free to choose whatever colors, squiggles, etc. to show based on what definitions are considered as keywords

    What happens when you open the prei source in rein with prei itself?
        We want it should just work. However, `export` may be seen as a keyword instead of an identifier in rein
        There are a few things we can do, one of the being an extra annotation telling you that it is defined here? (what about previous and other versions?)
        Another one being smart detection somehow. It detects a conflict and assumes the default behavior
*/
