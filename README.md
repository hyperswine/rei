# Rei

Rei Toolchains (compilers) + Libraries.

![Rei](/docs/ReiLogo.png)

A programming language that just works.

## Compiler Process

Rei follows a similar pattern to rust's compilation process. First, prei converts all source files to modules and all modules to a hierarchy of namespaces ending with the root `pkg` namespace. It does this in parallel for each source file, and parses each file in parallel. The resulting AST is combined together and sent to the analyser.

The analyser does a few things. First, note that a few artifacts were generated: the AST and Symbol Table. The symtab provides a unique identifier enforcement for each namespace and a cache for nodes of the AST given a namespace, identifier and rei type. The analyser first descends the AST, converting certain expressions to values (constexpr) and overall trying to reduce expressions to constants. Its main job however, is to "lower" the AST into a more generic form of instruction flow that can be more readily converted into assembly.

### Lowering

The lowering process involves:

- converting all "sugared" expression nodes into the same form in MIR. E.g. all ternary, if-else, match, elvis statements are converted to the same universal hashed branch block for PIR or jump table or branch statements (depends on size) in cranelift
- converting all local definitions aka variable definitions into stack slots in cranelift or recursive definitions in PIR
- converting all boxed values or external memory definitions into syscalls on unix or just API fns. Note boxed values are just pointers, basically u64 values. In cranelift, I think there are ways to point to external addresses. In PIR its even simpler

### Macro Expansion

Macros can be seen as compiler plugins that are treated as meta programs. Macros are parsed in the usual style just like any other function. But unlike regular functions, they are always "constexpr". So during descent pass 1, each macro invocation is evaluated lazily. A macro fn itself may also be reduced like any other expr. Its like reflection. When you first bump into the macro invocation, you go to that macro in the symtab and use that fn to evaluate the inputs. But unlike usual params, the params directly target the compiler's types like Expr.

E.g.

```rust
x: macro {
    (x: Ident y: Expr) {}

    (x: Ident "," y: Expr) {}
}
```
