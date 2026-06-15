
# Module type `Js.OPT`

Signatures of a set of standard functions for manipulating optional values.

```ocaml
type 'a t
```
```ocaml
val empty : 'a t
```
No value.

```ocaml
val return : 'a -> 'a t
```
Consider a value as an optional value.

```ocaml
val map : 'a t -> ('a -> 'b) -> 'b t
```
Apply a function to an optional value if it is available. Returns the result of the application.

```ocaml
val bind : 'a t -> ('a -> 'b t) -> 'b t
```
Apply a function returning an optional value to an optional value

```ocaml
val test : 'a t -> bool
```
Returns `true` if a value is available, `false` otherwise.

```ocaml
val iter : 'a t -> ('a -> unit) -> unit
```
Apply a function to an optional value if it is available.

```ocaml
val case : 'a t -> (unit -> 'b) -> ('a -> 'b) -> 'b
```
Pattern matching on optional values.

```ocaml
val get : 'a t -> (unit -> 'a) -> 'a
```
Get the value. If no value available, an alternative function is called to get a default value.

```ocaml
val option : 'a option -> 'a t
```
Convert option type.

```ocaml
val to_option : 'a t -> 'a option
```
Convert to option type.

```ocaml
val equals : _ t -> _ t -> bool
```
Javascript `==` equality operator.

```ocaml
val strict_equals : _ t -> _ t -> bool
```
Javascript `===` equality operator.
