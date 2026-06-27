
# Class type `Crypto.cryptoKey`

```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
The key type: `"secret"`, `"private"` or `"public"`.

```ocaml
method extractable : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method algorithm : algorithm Js_of_ocaml__.Js.readonly_prop
```
The algorithm the key is for; pass to `of_algorithm` to inspect it.

```ocaml
method usages : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.js_array
                  Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```