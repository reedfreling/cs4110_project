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

- `test1.modal, test2.modal, test3.modal, test4.modal ... test9.modal`:
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

Test examples are given by `test1.modal`, `test2.modal`, `test3.modal`, `test4.modal`, ..., `test9.modal`.

Tests 1-3 show the propositional logic functionality. 

`test3.modal` is a simple example of dynamic scope in our language.

`test4.modal` is an example of modal logic in our language.

Test 5 is test 4 written with new commands for convenience.

Tests 6 through 9 test modal logic on a more complex level.

I will explain each of the new commands for modal logic.

- `[create k]` creates a new Kripke model with an empty set of worlds and empty relations
- `[k add world w]` adds world w to the Kripke model [k], assuming [k] was created
- `[k add worlds { w1, w2 }]` adds worlds [w1] and [w2] to the Kripke model [k], assuming [k] was created
- `[k add accessibility w1 w2]` adds an edge between worlds [w1] and [w2] in Kripke model [k]
- `[k add accessibilities { (w1, w2); (w2, w1) }]` adds edges between world [w1] [w2] and one between [w2] and [w1] 
- `[k add valuation w a]` sets propositional variable [a] to be true in world [w] in model [k]
- `[k add valuations w { a, c }]` sets propositional variables [a] and [c] to be true in world [w] in model [k]
- `[k w ||- b]` returns the truth value of modal logic formula [b] in world [w] in model [k]
    - one can only assign the result to another variable or print out the result
- `[]a` represents the square notation for "[a] is necessary"
- `<>a` represents the diamond notation for "[a] is possible"
- `latexit k mymodel.txt` writes LaTeX code Kripke model [k] to the file `mymodel.txt`