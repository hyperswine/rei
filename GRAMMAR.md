# Grammar

Pretty much based on scala. Pest cant parse EBNF since it is PEG. So use PEG instead.

## PEG

```rust
stmt = expr

expr = assignment_expr | 
```

## Types of expressions

Most things are an expression like rust. Statements evaluate to a value like an expression, but does not return it. Instead it prob assigns it.

But unlike rust we dont have semicolons and instead rely on whitespace (default style = 2 spaces of inlining, 1 space between clauses). We need a clear way of distinguishing between expressions and statements.

So rei's keywords and design allows us to separate between them. Cues like `let` should delineate a statement.

## Types of statements

- macro define statements
- function definition statements
- class definition statements
- variable definition statements
