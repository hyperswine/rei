---
layout: default
title: Rei Scripting Language
---

`reis` is pretty much rei's scripting language.

You can embed it in your code, or use it as a library.

`reis` is good for many things like shell commands. Unlike bash where there is no versioning, we have a syntax include versioning in `reis` files:

```bash
!reis v1.1

echo "Hello World!"
```

`reis` includes bash like features as namespaced functions (overloadable) to do stuff like `cd`, `ls`, `curl`, and many other GNU/Unix commands.

```rust
!reis v2.2

// defined in rei::reis
echo: macro {
    ($x: expr) => {
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
ls: () -> opcode {
    let dir = fs::get_cwd()
    echo "$dir"

    OP_SUCCESS
}

// overload 2
ls: (args: &[str]) -> opcode {
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
pwd: () -> opcode {
    // call primitive rei function
    let res = service::pwd()
    echo "res"

    OP_SUCCESS
}
```
