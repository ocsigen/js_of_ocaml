# Module `Ppx_devtools_expect_runtime.Ops`

The operators used by reified predicates. The generated code refers to them through this module, which, unlike `Stdlib` and its operators, the test module cannot shadow.

```ocaml
val not : bool -> bool
```
```ocaml
val (&&) : bool -> bool -> bool
```
```ocaml
val (||) : bool -> bool -> bool
```
```ocaml
val (=) : 'a -> 'a -> bool
```
```ocaml
val (<>) : 'a -> 'a -> bool
```
```ocaml
val (<) : 'a -> 'a -> bool
```
```ocaml
val (>) : 'a -> 'a -> bool
```
```ocaml
val (<=) : 'a -> 'a -> bool
```
```ocaml
val (>=) : 'a -> 'a -> bool
```
```ocaml
val string_compare : string -> string -> int
```
