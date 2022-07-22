# Rei

A programming language that just works.

## Types

In rei, the only compiler defined types are Int (64bit), Float (64bit), String, enum, class, function.

Rei core builds on these types to result in core::prelude::i32,i64,u32,u64, etc. Bytes can be specified as well. Or binary with b"". Regex with core::regex r"". Raw strings with core::raw raw"". F (formatted) strings with f"". With f strings, you use $ instead of {}.

## Dependencies

Logos for lexing. Serde for serialising an AST to StrictYAML.

## Design

- Simple
- Low level and High level depending on abstraction level of choice. Core/Std/Arrkia, etc.
- Abstraction friendly, `std` and official libs highly recommended. Like C++, uses zero overhead principle. If you're building something, please use `std` for as many things as possible and reduce OS dependent code, unless building an OS

Being a highly compile time based language like Rust. We can perform a lot of static analysis. With LSP integration to highlight common tokenisation/parsing errors as well as `reic --check` errors. Runtime errors can be minimised as long as the programmer follows fairly strict guidelines and builds ontop rather than bandage around bad workarounds.

[Here is the coolest thing](https://docs.python.org/3/library/ast.html).

## Rei-LSP

Like rust-analyzer, RLS (rei language server) connects to an IDE with a RCP/UDP socket. Once connection is established. The IDE sends the file as a memory mapped file to RLS. Each time an update is made on the IDE (a change to the file), it does a zero copy move over the socket by writing the pointer to the location and the size. As the RLS reads from the file, the user may change the file, which will change the RLS' view of the file. To prevent this, the file is CoW'd so that the readers still have the original copy. And the writers have the new copy. If a change has been made since the original copy, the RLS response is ditched and the IDE waits for the next response. Until the user stops making changes to that file.

On the same machine. Neutron may be able to skip the UDP process and directly transfer the JSON file to the RLS process. E.g. through a two way pipe. The EOF signals the end like usual and everything is async'd and scheduled on the fly. As long as theres nothing too annoying on the system, it should be pretty snappy.

For the syntax highlighting. Need to get rid of all `;` and `end` stuff.

## LSP

Good reference [here](https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample).

The LSP server can look like [this](https://github.com/microsoft/vscode-extension-samples/blob/main/lsp-sample/server/src/server.ts). Basically, you need to accept a JSON that contains a bunch of fields like the requested file, the requested function (e.g. hover, find definition, documentation). And maybe provide active syntax highlighting to the current active file. And any parsing checks on the entire project (configured by the language extension as a bunch of active files). Perhaps even pre compile checks, like [cargo check](https://doc.rust-lang.org/cargo/commands/cargo-check.html).

For syntax highlighting, someone did it for rust [here](https://github.com/dustypomerleau/rust-syntax/blob/master/syntaxes/rust.tmLanguage.json).

To use Rei-LSP locally. Just run `copy_files.sh` which rm -rf the stuff in .vscode/extensions/Rei-LSP and copies the new version in.

## The Greatest Animal

Manta ray.
