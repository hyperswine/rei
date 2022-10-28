#*
    REI LSP FUNCTIONALITY
*#

Line: Int

Context: complex {
    pub {
        curr_line: Line
        curr_char: Int
    }

    pub {
        open_files: Vec<File>
        proj: prei::Project
    }
}

/*
    The below this can be wrapped with core::state that makes things as safe as possible

    let context, set_context = State(Context())
    OR JUST:
    @state context = Context()
    which generates a : set_context if there isnt a definition already
*/

mut context = Context

context: () -> &Context {
    &context
}

// you can call this as set_context(...Context) to unpack Context
set_context: unsafe (line_number: _, char_number: _) {
    context.curr_char = char_number
    context.curr_line = line_number
}

// What else? Link parser

/*
    Frontend: snippets, syntax highlighting, open files, strings, filesystem
    Backend: goto def, error and warning squiggles, early checks
*/

// open file independent
@state
mut cached_defs = Vec<File>()

export goto_def: (item: Item) -> File? {
    // try to find the definition in the AST or list of open files or cache

    // go to the item with the same "kind" or type. Dont return a bunch of guesses? the proj should be setup properly
    // not necessarily if you want to "use x", find x from somewhere
    // If found, return immediately
    cached_defs
    .filter(file => file.kind = item.kind)
    .first()!
}

// Vec can be a stack object. The IDE reveals whether it is with <stack> or <heap>
// similar to dynamic dispatch, but dynamic dispatch is explicit

#*
    $params:
        $1: a manageable item
    $return: A list of candidates that match the query
*#
export find_item: (item: Item) -> Vec<File> {
    // otherwise find it in the list of open files

    // otherwise query the entire project prei from the root
    let files = prei::files(proj)

    // for each file belonging to the project (registered module), if we didnt already search through them, open it and try to find the item

    // MAYBE: when you start rein, you "semi open" the files. Or you keep them in cache as you start opening and maybe closing them. Closing them in rein doesnt actually close them until you close the IDE process

    // NOTE: collect() and Vec<T> are great tools for this use case
    files.filter(file => file.kind = item.kind).collect()
}

// get compiler checks as you add/remove code => prei check => reic --check
export prei_check: () {
    prei::check(proj)
}
