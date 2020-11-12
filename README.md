CS 4110 Final Project
==================

The Files
---------

- `ast.ml`:
  Defines the data types for the abstract syntax trees (ASTs).

- `eval.ml`:
  The interpreter for the ASTs.

- `main.ml`:
  The top level code that parses in an input file, and executes it.

- `lexer.mll` and `parser.mly`:
  The lexer and parser specs for MODAL programs. These are fed through [OCaml's
  lexer and parser generators][ocamlyacc] to generate source code.

- `pprint.ml`:
  A pretty printer for the ASTs.

- `test.modal`:
  A test MODAL program.

[ocamlyacc]: http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html


Compile and Execute
-------------------

You'll need to [install OCaml][] if you don't have it already. See the course website for instructions. 

If you have Make (i.e., on any Unix), you can simply type `make` at the
command line. Compilation will produce an executable file
called `modal`.

Now you can run the executable on a test program: `./modal test.modal`.

This will parse the file test.modal and evaluate the program.