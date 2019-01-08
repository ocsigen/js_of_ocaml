== Performances of Js_of_ocaml compiled programs ==

We have compared the running time of OCaml programs executed natively, interpreted by the bytecode interpreter, and executed by a Javascript engine after compilation by Js_of_ocaml.
With a state of the art Javascript engine (such as Google's V8, Apple's Nitro, or Mozilla's Jaegermonkey), programs often run faster when compiled to Javascript than with the bytecode interpreter. Benchmarks were run in march 2011, with current development versions of each of the engines.

Exceptions are very expansive (boyer and kb).  String operations are also slow (splay), as OCaml strings cannot be mapped directly to Javascript immutable UTF-16 strings.  Memory allocation might also be less efficient (binary_trees, splay, taku).

<<a_img src="performances/perf.png" | Relative execution times >>

Most of the time, the generated Javascript code is smaller than the bytecode file.  The outlier (boyer) contains many constant values which are currently not compiled in a compact way.  The benchmarks are all small programs that does not make much use of any library.  The generated code remains smaller than the bytecode file for large programs (ocamlc, unison).  The size gain can be much larger for programs that rely more on external libraries, thanks to dead code elimination.

<<a_img src="performances/size.png" | Relative size >>
