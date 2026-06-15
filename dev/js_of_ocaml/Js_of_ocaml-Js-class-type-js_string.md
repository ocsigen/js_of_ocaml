
# Class type `Js.js_string`

Specification of Javascript string objects.

```ocaml
method toString : js_string t meth
```
```ocaml
method valueOf : js_string t meth
```
```ocaml
method charAt : int -> js_string t meth
```
```ocaml
method charCodeAt : int -> number t meth
```
```ocaml
method codePointAt : int -> number t optdef meth
```
```ocaml
method concat : js_string t -> js_string t meth
```
```ocaml
method concat_2 : js_string t -> js_string t -> js_string t meth
```
```ocaml
method concat_3 : js_string t -> js_string t -> js_string t -> js_string t meth
```
```ocaml
method concat_4 : js_string t ->
  js_string t ->
  js_string t ->
  js_string t ->
  js_string t meth
```
```ocaml
method indexOf : js_string t -> int meth
```
```ocaml
method indexOf_from : js_string t -> int -> int meth
```
```ocaml
method lastIndexOf : js_string t -> int meth
```
```ocaml
method lastIndexOf_from : js_string t -> int -> int meth
```
```ocaml
method localeCompare : js_string t -> number t meth
```
```ocaml
method _match : regExp t -> match_result_handle t opt meth
```
```ocaml
method normalize : js_string t meth
```
```ocaml
method normalize_form : normalization t -> js_string t meth
```
```ocaml
method replace : regExp t -> js_string t -> js_string t meth
```
```ocaml
method replace_string : js_string t -> js_string t -> js_string t meth
```
```ocaml
method search : regExp t -> int meth
```
```ocaml
method slice : int -> int -> js_string t meth
```
```ocaml
method slice_end : int -> js_string t meth
```
```ocaml
method split : js_string t -> string_array t meth
```
```ocaml
method split_limited : js_string t -> int -> string_array t meth
```
```ocaml
method split_regExp : regExp t -> string_array t meth
```
```ocaml
method split_regExpLimited : regExp t -> int -> string_array t meth
```
```ocaml
method substring : int -> int -> js_string t meth
```
```ocaml
method substring_toEnd : int -> js_string t meth
```
```ocaml
method toLowerCase : js_string t meth
```
```ocaml
method toLocaleLowerCase : js_string t meth
```
```ocaml
method toUpperCase : js_string t meth
```
```ocaml
method toLocaleUpperCase : js_string t meth
```
```ocaml
method trim : js_string t meth
```
```ocaml
method length : int readonly_prop
```