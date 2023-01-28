# Rei

Query-based Compilation (lexing, parsing, codegen API), Project Management, Standard Libraries.

![Rei](/docs/ReiLogo.png)

A programming language that just works.

[Semi thorough documentation can be found in here](https://hyperswine.github.io/rei)

## Bootstrapping

If you are building rei from scratch, you have a couple of options:

1. use the rust implemention of rei base to build rei base, then use that to build `rei`: base, core, std, prei. Then use the built prei and rei suite to rebuild rei
2. download the prebuilt binaries `reic` and `prei` via an installer, e.g. for neutron
