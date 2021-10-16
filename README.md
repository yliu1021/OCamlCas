# OCaml CAS

## Quick Start
*OCaml CAS* requires the opam packages `async`, `base`, `core`.

To get started, run
```
dune exec -- ./main.exe <port>
```
and to connect, (if you have `nc` installed), run
```
nc localhost <port>
> 3+4
7.
> ...
```

## Project Layout
Most of the project lies within the `./cas` directory.
```
cas
├── expr
│   ├── dune
│   ├── expr.ml
│   └── expr.mli
├── parser
│   ├── dune
│   ├── parser.ml
│   └── parser.mli
├── repl
│   ├── dune
│   ├── engine.ml
│   ├── engine.mli
│   ├── eval.ml
│   ├── eval.mli
│   ├── repl.ml
│   ├── repl_state.ml
│   └── repl_state.mli
└── tokenizer
    ├── dune
    ├── tokenizer.ml
    └── tokenizer.mli
```

The `parser` and `tokenizer` moduels are used to parse input strings into syntax trees. The `expr` module defines an expression tree and a function to convert a syntax tree into an expression tree. Finally, the `repl` module contains `eval.ml`, which takes an expression tree and evaluates it.

## Extending Functionality
* To add a new function or constant, simply add it into `repl_state.ml` (with the right classification of `Function` or `Constant`), and in the `eval.ml` module, implement how it should be evaluated.

* To add a new gramamr rule, go into `parser.ml` and modify the `get_substitution` function to add a new grammar rule. The available substitutions are defined in `type substitution`. If a new substitution type is added, make sure to implement how it should be matched in the `perform_sub` function (as well as how the resulting syntax tree should be constructed).

## Future Work
* Support user defined constants and functions
* Symbolic calculations (partially implemented)
* Integrate arbitrary precision numbers
* Expression tree simplifications
