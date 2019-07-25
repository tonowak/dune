Variaous regression tests fixed by ocaml/dune#992

Interaction of (menhir ...) and -p
----------------------------------

This used to fail because dune couldn't associate a compilation
context to the menhir files when package bar was hidden.

  $ cd menhir-and-dash-p && dune build -p foo

package field without public_name field
---------------------------------------

This used to fail because the parser for the "package" field when
there is no "public_name"/"public_names" field used to not parse the
argument of "package".

  $ cd package-without-pub-name && dune build -p foo
  File "dune", line 3, characters 1-14:
  3 |  (package foo))
       ^^^^^^^^^^^^^
  Error: This field is useless without a (public_name ...) field.
  [1]

  $ cd package-without-pub-name-jbuild && dune build -p foo
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild", line 2, characters 2-12:
  2 |  ((name foo)
        ^^^^^^^^^^
  Error: Atom expected
  [1]
