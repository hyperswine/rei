# Rei

A programming language that just works.

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

## Found this Algorithm for Building a Parse Tree

From [here](https://runestone.academy/ns/books/published/pythonds/Trees/ParseTree.html).

```python
from pythonds.basic import Stack
from pythonds.trees import BinaryTree

def buildParseTree(fpexp):
    fplist = fpexp.split()
    pStack = Stack()
    eTree = BinaryTree('')
    pStack.push(eTree)
    currentTree = eTree

    for i in fplist:
        if i == '(':
            currentTree.insertLeft('')
            pStack.push(currentTree)
            currentTree = currentTree.getLeftChild()

        elif i in ['+', '-', '*', '/']:
            currentTree.setRootVal(i)
            currentTree.insertRight('')
            pStack.push(currentTree)
            currentTree = currentTree.getRightChild()

        elif i == ')':
            currentTree = pStack.pop()

        elif i not in ['+', '-', '*', '/', ')']:
            try:
                currentTree.setRootVal(int(i))
                parent = pStack.pop()
                currentTree = parent

            except ValueError:
                raise ValueError("token '{}' is not a valid integer".format(i))

    return eTree

pt = buildParseTree("( ( 10 + 5 ) * 3 )")
pt.postorder()  #defined and explained in the next section
```
