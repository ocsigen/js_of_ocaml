
# Module `Js_of_ocaml_tyxml.Tyxml_cast`

Cast to and from Tyxml types

```ocaml
module MakeTo
  (C : sig ... end) : 
  Tyxml_cast_sigs.TO with type 'a elt = 'a C.elt
```
```ocaml
module MakeOf
  (C : sig ... end) : 
  Tyxml_cast_sigs.OF with type 'a elt = 'a C.elt
```