
# Module `Deriving_Json`

Typesafe IO (based on the *deriving* library).

**This module is provided by the `js_of_ocaml.deriving` library, part of the `js_of_ocaml` opam package.**

see [https://github.com/ocsigen/deriving](https://github.com/ocsigen/deriving) the source code of deriving
see [http://code.google.com/p/deriving/](http://code.google.com/p/deriving/) the documentation of the original deriving library by Jeremy Yallop.
```ocaml
type 'a t
```
The type of JSON parser/printer for value of type `'a`.

```ocaml
val make : 
  (Stdlib.Buffer.t -> 'a -> unit) ->
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  'a t
```
```ocaml
val write : 'a t -> Stdlib.Buffer.t -> 'a -> unit
```
```ocaml
val read : 'a t -> Deriving_Json_lexer.lexbuf -> 'a
```
```ocaml
val to_string : 'a t -> 'a -> string
```
`to_string Json.t<ty> v` marshal the `v` of type `ty` to a JSON string.

```ocaml
val from_string : 'a t -> string -> 'a
```
`from_string Json.t<ty> s` safely unmarshal the JSON `s` into an OCaml value of type `ty`. Throws `Failure` if the received value isn't the javascript representation of a value of type `ty`.

```ocaml
module type Json = sig ... end
```
The signature of the JSON class.

Deriver

```ocaml
module type Json_min = sig ... end
```
```ocaml
module type Json_min' = sig ... end
```
```ocaml
module type Json_min'' = sig ... end
```
```ocaml
module Defaults (J : Json_min) : Json with type a = J.a
```
```ocaml
module Defaults' (J : Json_min') : Json with type a = J.a
```
```ocaml
module Defaults'' (J : Json_min'') : Json with type a = J.a
```
```ocaml
module Json_char : Json with type a = char
```
```ocaml
module Json_bool : Json with type a = bool
```
```ocaml
module Json_unit : Json with type a = unit
```
```ocaml
module Json_int : Json with type a = int
```
```ocaml
module Json_int32 : Json with type a = int32
```
```ocaml
module Json_int64 : Json with type a = int64
```
```ocaml
module Json_nativeint : Json with type a = nativeint
```
```ocaml
module Json_float : Json with type a = float
```
```ocaml
module Json_string : Json with type a = string
```
```ocaml
module Json_list (A : Json) : Json with type a = A.a list
```
```ocaml
module Json_ref (A : Json) : Json with type a = A.a Stdlib.ref
```
```ocaml
module Json_option (A : Json) : Json with type a = A.a option
```
```ocaml
module Json_array (A : Json) : Json with type a = A.a array
```
```ocaml
val read_list : 
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  Deriving_Json_lexer.lexbuf ->
  'a list
```
```ocaml
val write_list : 
  (Stdlib.Buffer.t -> 'a -> unit) ->
  Stdlib.Buffer.t ->
  'a list ->
  unit
```
```ocaml
val read_ref : 
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  Deriving_Json_lexer.lexbuf ->
  'a Stdlib.ref
```
```ocaml
val write_ref : 
  (Stdlib.Buffer.t -> 'a -> unit) ->
  Stdlib.Buffer.t ->
  'a Stdlib.ref ->
  unit
```
```ocaml
val read_option : 
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  Deriving_Json_lexer.lexbuf ->
  'a option
```
```ocaml
val write_option : 
  (Stdlib.Buffer.t -> 'a -> unit) ->
  Stdlib.Buffer.t ->
  'a option ->
  unit
```
```ocaml
val read_array : 
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  Deriving_Json_lexer.lexbuf ->
  'a array
```
```ocaml
val write_array : 
  (Stdlib.Buffer.t -> 'a -> unit) ->
  Stdlib.Buffer.t ->
  'a array ->
  unit
```