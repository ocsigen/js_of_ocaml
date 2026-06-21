
# Class type `Js.regExp`

Specification of Javascript regular expression objects.

```ocaml
method exec : js_string t -> match_result_handle t opt meth
```
```ocaml
method test : js_string t -> bool t meth
```
```ocaml
method toString : js_string t meth
```
```ocaml
method source : js_string t readonly_prop
```
```ocaml
method flags : js_string t readonly_prop
```
```ocaml
method global : bool t readonly_prop
```
```ocaml
method ignoreCase : bool t readonly_prop
```
```ocaml
method multiline : bool t readonly_prop
```
```ocaml
method lastIndex : int prop
```