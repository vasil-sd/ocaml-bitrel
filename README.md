bitrel — Library for work with relations
-------------------------------------------------------------------------------
%%VERSION%%

Bitrel is module for work with relations.


Bitrel is distributed under the ISC license.

Homepage: https://github.com/vasil-sd/ocaml-bitrel

## Installation

Bitrel can be installed with `opam`:

    git clone https://github.com/vasil-sd/ocaml-bitrel.git
    opam pin add bits ocaml-bitrel

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
bitrel`.

[doc]: https://vasil-sd.github.io/ocaml-bitrel/doc

## Sample programs

If you installed bits with `opam` sample programs are located in
the directory `opam var bitrel:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
