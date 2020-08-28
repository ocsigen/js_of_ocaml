---
id: export-ocaml-to-js
title: Export OCaml code to JavaScript
sidebar_label: Export OCaml code to JavaScript
---


The easiest way to export OCaml values (e.g., functions) to JavaScript is to
create a JavaScript object containing all values to export
and to make the object reachable.

```ocaml
let _ =
  Js.export "myMathLib"
    (object%js
       method add x y = x +. y
       method abs x = abs_float x
       val zero = 0.
     end)
```

```javascript
myMathLib.add(3,4)
```

## Using the Node.js module system

`Js.export` and `Js.export_all` will export a value to `module.exports` if it exists.

```ocaml
(* cat math.ml *)
let _ =
  Js.export_all
    (object%js
      method add x y = x +. y
      method abs x = abs_float x
      val zero = 0.
     end)
```

```shell
ocamlfind ocamlc -verbose \
  -package js_of_ocaml -package js_of_ocaml-ppx \
  -linkpkg math.ml -o math.byte
js_of_ocaml math.byte
node
> var math = require("./math.js");
> math.add(2,3)
```
