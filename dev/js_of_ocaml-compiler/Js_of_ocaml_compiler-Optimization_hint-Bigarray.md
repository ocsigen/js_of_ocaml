
# Module `Optimization_hint.Bigarray`

```ocaml
type kind = 
  | Float16
  | Float32
  | Float32_t
  | Float64
  | Int8_signed
  | Int8_unsigned
  | Int16_signed
  | Int16_unsigned
  | Int32
  | Int64
  | Int
  | Nativeint
  | Complex32
  | Complex64
```
```ocaml
type layout = 
  | C
  | Fortran
```
```ocaml
type t = {
  unsafe : bool;
  kind : kind;
  layout : layout;
}
```