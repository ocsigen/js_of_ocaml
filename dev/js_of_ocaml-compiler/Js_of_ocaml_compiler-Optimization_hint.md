# Module `Js_of_ocaml_compiler.Optimization_hint`

```ocaml
type boxed_integer = 
  | Int32
  | Int64
  | Nativeint
```
```ocaml
type array_kind = 
  | Generic
  | Value
  | Float
```
```ocaml
module Bigarray : sig ... end
```
```ocaml
type repr = 
  | Value
  | Float
  | Int32
  | Nativeint
  | Int64
  | Int
```
```ocaml
type primitive = {
  name : string;
  args : repr list;
  res : repr;
}
```
```ocaml
type ccall = 
  | Hint_unsafe
  | Hint_int of boxed_integer
  | Hint_bigarray of Bigarray.t
  | Hint_primitive of primitive
  | Hint_immediate_result (* The value returned by the C function is an immediate *)
```
```ocaml
type inline_attribute = 
  | Always_inline
  | Never_inline
  | Hint_inline
  | Unroll of int
  | Default_inline
```
```ocaml
type specialise_attribute = 
  | Always_specialise
  | Never_specialise
  | Default_specialise
```
```ocaml
type closure_hint = {
  params : repr list;
  return : repr;
  inline : inline_attribute;
  specialise : specialise_attribute;
  is_a_functor : bool;
}
```
```ocaml
type t = 
  | Hint_immutable_block
  | Hint_arraylength of array_kind
  | Hint_closures of closure_hint list
  | Hint_ccall of ccall
  | Hint_immediate (* The value produced by the instruction is an immediate *)
  | Hint_variant (* The argument of the instruction is a value of a variant type: if it is an immediate, it is a constant constructor *)
```
```ocaml
val print_ccall : Stdlib.Format.formatter -> ccall -> unit
```
```ocaml
val print : Stdlib.Format.formatter -> t -> unit
```
```ocaml
val print_closure_hint : Stdlib.Format.formatter -> closure_hint -> unit
```
