This directory contains a collection of bytecode files.

Generated with ocaml/ocaml#1568
===============================

`$ cat match_with_exn1.ml`
```ocaml
exception A of int

let y =
  match Random.int 2 with
  | 0 as i | exception A (2 as i) -> i
  | i -> i+1
  | exception A i -> i+2
```
`$ cat match_with_exn2.ml`
```ocaml
exception A of int

let y =
  match Random.int 2 with
  | 0 as i | exception A (2 as i) -> i
  | i -> i+1
```
