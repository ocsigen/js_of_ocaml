
# Export OCaml code to JavaScript

This page explains how to make OCaml values (functions, objects, constants) accessible from JavaScript code.


## Basic export with `Js.export`

Use [Js.export](./Js_of_ocaml-Js.md#val-export) to export a single named value:

```ocaml
open Js_of_ocaml

let () =
  Js.export "myMathLib"
    (object%js
       method add x y = x + y
       method abs x = abs x
       val zero = 0
     end)
```
From JavaScript:

```javascript
myMathLib.add(3, 4);    // 7
myMathLib.abs(-5);      // 5
myMathLib.zero;         // 0
```

## Exporting multiple values with `Js.export_all`

Use [Js.export\_all](./Js_of_ocaml-Js.md#val-export_all) to export all properties of an object as top-level exports:

```ocaml
let () =
  Js.export_all
    (object%js
       method add x y = x + y
       method sub x y = x - y
       val version = Js.string "1.0.0"
     end)
```
From JavaScript:

```javascript
add(3, 4);        // 7 (directly accessible)
sub(5, 2);        // 3
version;          // "1.0.0"
```

## Exporting individual functions

You can export plain OCaml functions. They are automatically wrapped for JavaScript:

```ocaml
let greet name =
  "Hello, " ^ name ^ "!"

let factorial n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (acc * n) (n - 1)
  in aux 1 n

let () =
  Js.export "greet" greet;
  Js.export "factorial" factorial
```
From JavaScript:

```javascript
greet("World");    // "Hello, World!"
factorial(5);      // 120
```

## Type considerations

Exported functions are automatically wrapped with [Js.wrap\_callback](./Js_of_ocaml-Js.md#val-wrap_callback). However, you must convert OCaml values to their JavaScript equivalents (strings with `Js.string`, booleans with `Js.bool`, etc.). See [type conversions](./javascript-interop.md#conversions) for the full list.


## Handling callbacks from JavaScript

When JavaScript passes a callback to your OCaml code, you receive a JavaScript function. Use [Js.Unsafe.fun\_call](./Js_of_ocaml-Js-Unsafe.md#val-fun_call) to invoke it:

```ocaml
let () =
  Js.export "utils"
    (object%js
       method forEach arr callback =
         let len = arr##.length in
         for i = 0 to len - 1 do
           let item = Js.array_get arr i in
           Js.Optdef.iter item (fun v ->
             ignore (Js.Unsafe.fun_call callback [| Js.Unsafe.inject v |]))
         done
     end)
```
From JavaScript:

```javascript
utils.forEach([1, 2, 3], function(x) { console.log(x); });
```

## Using with Node.js

[Js.export](./Js_of_ocaml-Js.md#val-export) and [Js.export\_all](./Js_of_ocaml-Js.md#val-export_all) automatically use `module.exports` when running in Node.js:

```ocaml
(* math.ml *)
open Js_of_ocaml

let () =
  Js.export_all
    (object%js
       method add x y = x + y
       method mul x y = x * y
     end)
```
Build and use:

```
$ ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml-ppx \
    -linkpkg math.ml -o math.byte
$ js_of_ocaml math.byte -o math.js
$ node
> var math = require("./math.js");
> math.add(2, 3)
5
> math.mul(4, 5)
20
```

## Using in browsers

In browsers, exports are added to the global object (`window`):

```html
<script src="math.js"></script>
<script>
  console.log(myMathLib.add(2, 3));
</script>
```
To avoid polluting the global namespace, you can export a single namespace object:

```ocaml
let () =
  Js.export "MyApp"
    (object%js
       val math = object%js
         method add x y = x + y
       end
       val utils = object%js
         method log msg = Console.console##log msg
       end
     end)
```