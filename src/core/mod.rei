#*
    Contains useful primitives for writing low level software on supported platforms
*#

// complete homoiconicity
// always resolved and shadowed in this order: custom -> std -> core -> base
// rei reflection => the main expr() fn is able to run all registered functions (at link or compile time)
// through string to symbol lookup during late AST descent or later on
