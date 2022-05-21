# Grammar

Pretty much based on scala. Pest cant parse EBNF since it is PEG. So use PEG instead.

In rei, expressions and statements have a pretty blurry line. The compiler is needs to ensure good things happen.

## PEG

Note there is an implicit `filename` included in the AST generated. Its basically any valid filename on your OS and reic that doesnt include certain special chars.

- Each production is associated with an internal node
- Each terminal production is associated with a leaf node

```rust
content: stmt* 

// -------------
// STATEMENTS
// -------------

stmt: function_def | class_def | data_def | variable_def | operator_stmt | if_stmt | for_stmt | while_stmt | return_stmt | trivial_stmt | use_stmt

return_stmt: "return" ~ [expr]

trivial_stmt: "yield" | "continue" | "break"

variable_def: ("let" ~ ident ~ "=" ~ expr) | ("let" ~ expr ~ "=" ~ expr)

// same as mod something::something
use_stmt: "use" ~ ("::" ~ ident)*

// Function Statements

function_def: {
  anonymous_function_stmt | standard_function_stmt
}

// just a let/const but function like
anonymous_function_stmt: {
  "(" ~ param_list ~ ")" ~ single_or_scoped_block
}

// note: function_body is basically any_statement*
// but apparently no left recursive
standard_function_stmt: {
  fn_keyword ~ ident ~ "(" ~ param_list ~ ")" ~ "{" ~ stmt* ~ "}"
}

// Control Statements

// should technically be a boolean expr, should be parseable
if_stmt: {
  if_block ~ (elif_block* ~ [else_block])
}

if_block: "if" ~ stmt* ~ single_or_scoped_block

elif_block: {
  "elif" ~ stmt* ~ if_block
}

else_block: {
  "else" ~ if_block
}

for_stmt: {
  standard_for_stmt | range_based_for_stmt
}

standard_for_stmt: {
  "for" ~ for_expr_cond ~ single_or_scoped_block
}

// for i in start..end
for_expr_cond: {
  ident ~ "in" ~ expr ~ ".." ~ expr
}

single_or_scoped_block: {
  scoped_block | stmt*
}

// -------------
// EXPRESSIONS
// -------------

expr: ["+"|"-"] ~ term ~ {op ~ term}

term: ident | number | "(" expr ")"

// -------------
// OPERATORS
// -------------

op: "<" | ...

// -------------
// SCOPED BLOCKS
// -------------

scoped_block = {
  "{" ~ stmt ~ "}"
}
```

## Types of expressions

Most things are an expression like rust. Statements evaluate to a value like an expression, but does not return it. Instead it prob assigns it.

But unlike rust we dont have semicolons and instead rely on whitespace (default style = 2 spaces of inlining, 1 space between clauses). We need a clear way of distinguishing between expressions and statements.

So rei's keywords and design allows us to separate between them. Cues like `let` should delineate a statement.

All statements are expressions. All expressions evaluate to a value.

## Types of statements

- function definition statements
  - macro define statements -> core library function
  - anonymous function defines -> `() {}` that can be assigned
  - lambda function defines -> `[]() {}`
- class definition statements -> `class ident() {}`
- data definition statements -> `data ident {}`
- variable definition statements -> `let ident = expr`, `const ident = expr`

<!-- ? Maybe not actually -->
<!-- - operator statements -> `expr op expr`, `ident op expr`, `expr op ident` based on l/r-value semantics -->
