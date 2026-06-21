
# Module `Js_of_ocaml.CSS`

This module contains a few types and values to ease the use of CSS properties and such. If you think a feature is missing, consider sending a patch or an RFE to the mailing list.

This module contain submodules each with a signature similar to:

```ocaml
  type t            (* type the module is focused on *)
  type js_t         (* valid js representation of values of type t *)
  val js: t -> js_t (* conversion *)
  val ml: js_t -> t (* conversion *)
```
Additional functions (string conversion, standard operation, etc.) are sometime available. Some module have several different types instead of just one.

```ocaml
module Color : sig ... end
```
All about CSS colors. MDC documentation here: https://developer.mozilla.org/en/CSS/color\_value . Specifications here: http://www.w3.org/TR/css3-color/\#svg-color .

```ocaml
module Length : sig ... end
```
```ocaml
module Angle : sig ... end
```