#*
    Prei is a vital tool for managing rei projects
*#

use pkg::base::prelude::[Symbol Symtab]

# symbols defined in a prei project
PreiSymbols: {
    root: Symtab
}

// a variant
Module: extend Symbol {
    // children
    fields: Vec[Symbol]
}

/*
    Rei + Prei Defines:

    : means         general definition
    => means        "one line" general definition
    = means         sequential definition
    means    universal definition (prei)
    no       means local definition (prei)
*/

export: macro {}

# A live lowerer of a package
Lowerer: {
    root_symbol: Symbol
}
