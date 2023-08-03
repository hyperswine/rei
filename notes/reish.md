---
layout: default
title: Rei Scripting Language
---

`reish` is pretty much rei's scripting language. You can embed it in your code, or use it as a library. `reish` is good for many things like shell commands. Unlike bash where there is no versioning, we have a syntax include versioning in `reish` files:

```bash
@!reis v1.1

echo "Hello World!"
```

`reish` includes bash like features as namespaced functions (overloadable) to do stuff like `cd`, `ls`, `curl`, and many other GNU/Unix commands.

```rust
@!reish v2.2

// defined in rei::reis
echo: macro {
    (x: Expr) => {
        // call function to output to
        // stdout or fd

        // write is a core function for fd
        write_volatile(fd, $x)
    }
}

// in /sys/scripts
use fs
use env

// overload 1
ls: () -> Opcode {
    let dir = fs::get_cwd()
    echo "$dir"

    OP_SUCCESS
}

// overload 2
ls: (args: String...) -> Opcode {
    // reis treats repeated args on a case by case basis, though --args and -args should only be singular
    for arg in args {
        if arg.contains("-") {
            // take all the flags

            continue
        }
        if arg.contains("--") {
            // take the = val if there is any
            // or just the arg itself

            continue
        }
        match arg {
            "l"
        }
        
        // otherwise, arg not recognised and exit with ARG_NOT_RECOGNISED
        exit(FAIL, ARG_NOT_RECOGNISED)        
    }

    OP_SUCCESS
}

// if compiling with neutron, can simply include its API as a module instead of using syscalls or primitives like write
pwd: () -> Opcode {
    // call primitive rei function
    let res = service::pwd()
    echo "res"

    OP_SUCCESS
}
```
