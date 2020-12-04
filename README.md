CS 4110 Final Project
==================

The Files
---------

- `ast.ml`:
  Defines the data types for the abstract syntax trees (ASTs).

- `eval.ml`:
  The interpreter for the ASTs.

- `kripke.ml`:
  The data structure for Kripke models.

- `main.ml`:
  The top level code that parses in an input file, and executes it.

- `lexer.mll` and `parser.mly`:
  The lexer and parser specs for MODAL programs. These are fed through [OCaml's
  lexer and parser generators][ocamlyacc] to generate source code.

- `pprint.ml`:
  A pretty printer for the ASTs.

- `test1.modal, test2.modal, test3.modal, test4.modal`:
  Test MODAL programs.

[ocamlyacc]: http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html


Compile and Execute
-------------------

You'll need to [install OCaml][] if you don't have it already. See the course website for instructions. 

If you have Make (i.e., on any Unix), you can simply type `make` at the
command line. Compilation will produce an executable file
called `modal`.

Now you can run the executable on a test program: `./modal test.modal`.

This will parse the file test.modal and evaluate the program.

Test examples are given by `test1.modal`, `test2.modal`, `test3.modal`, `test4.modal`.

Tests 1-3 show the propositional logic functionality. 

`test3.modal` is a simple example of dynamic scope in our language.

`test4.modal` is an example of modal logic in our language.

I will explain each of the new commands for modal logic.

- `[create k]` creates a new Kripke model with an empty set of worlds and empty relations
- `[k add world w]` adds world w to the Kripke model [k], assuming [k] was created
- `[k add valuation w a]` sets propositional variable [a] to be true in world [w] in model [k]
- `[k w ||- b]` returns the truth value of modal logic formula [b] in world [w] in model [k]
    - one can only assign the result to another variable or print out the result
- `[]a` represents the square notation for "[a] is necessary"
- `<>a` represents the diamond notation for "[a] is possible"