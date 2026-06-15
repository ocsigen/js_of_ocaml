
# Module `CSS.Length`

```ocaml
type t = 
  | Zero (* For 0, unit is optional *)
  | Em of float (* Relative to the font size *)
  | Ex of float (* Relative to the x-height *)
  | Px of float (* Relative to the viewing device *)
  | Gd of float (* Relative to the grid *)
  | Rem of float (* Relative to the font size of the root *)
  | Vw of float (* Relative to the viewport's width *)
  | Vh of float (* Relative to the viewport's height *)
  | Vm of float (* Relative to the smallest of the viewport's width or height *)
  | Ch of float (* Relative to the width of a char '0' *)
  | Mm of float (* in Milimeter *)
  | Cm of float (* in Centimeter *)
  | In of float (* in Inch *)
  | Pt of float (* in Points (72pt = 1in) *)
  | Pc of float (* in Picas (1pc = 12pt) *)
```
The type of length attributes. Mdc documentation: https://developer.mozilla.org/en/CSS/length and specification: http://www.w3.org/TR/css3-values/\#lengths

```ocaml
type js_t = private Js.js_string Js.t
```
Js representation of lengths.

Conversion functions

```ocaml
val string_of_t : t -> string
```
```ocaml
val js : t -> js_t
```
```ocaml
val ml : js_t -> t
```