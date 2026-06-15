
# Module `CSS.Color`

All about CSS colors. MDC documentation here: https://developer.mozilla.org/en/CSS/color\_value . Specifications here: http://www.w3.org/TR/css3-color/\#svg-color .

```ocaml
type name = 
  | Aliceblue
  | Antiquewhite
  | Aqua
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Black
  | Blanchedalmond
  | Blue
  | Blueviolet
  | Brown
  | Burlywood
  | Cadetblue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflowerblue
  | Cornsilk
  | Crimson
  | Cyan
  | Darkblue
  | Darkcyan
  | Darkgoldenrod
  | Darkgray
  | Darkgreen
  | Darkgrey
  | Darkkhaki
  | Darkmagenta
  | Darkolivegreen
  | Darkorange
  | Darkorchid
  | Darkred
  | Darksalmon
  | Darkseagreen
  | Darkslateblue
  | Darkslategray
  | Darkslategrey
  | Darkturquoise
  | Darkviolet
  | Deeppink
  | Deepskyblue
  | Dimgray
  | Dimgrey
  | Dodgerblue
  | Firebrick
  | Floralwhite
  | Forestgreen
  | Fuchsia
  | Gainsboro
  | Ghostwhite
  | Gold
  | Goldenrod
  | Gray
  | Grey
  | Green
  | Greenyellow
  | Honeydew
  | Hotpink
  | Indianred
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavenderblush
  | Lawngreen
  | Lemonchiffon
  | Lightblue
  | Lightcoral
  | Lightcyan
  | Lightgoldenrodyellow
  | Lightgray
  | Lightgreen
  | Lightgrey
  | Lightpink
  | Lightsalmon
  | Lightseagreen
  | Lightskyblue
  | Lightslategray
  | Lightslategrey
  | Lightsteelblue
  | Lightyellow
  | Lime
  | Limegreen
  | Linen
  | Magenta
  | Maroon
  | Mediumaquamarine
  | Mediumblue
  | Mediumorchid
  | Mediumpurple
  | Mediumseagreen
  | Mediumslateblue
  | Mediumspringgreen
  | Mediumturquoise
  | Mediumvioletred
  | Midnightblue
  | Mintcream
  | Mistyrose
  | Moccasin
  | Navajowhite
  | Navy
  | Oldlace
  | Olive
  | Olivedrab
  | Orange
  | Orangered
  | Orchid
  | Palegoldenrod
  | Palegreen
  | Paleturquoise
  | Palevioletred
  | Papayawhip
  | Peachpuff
  | Peru
  | Pink
  | Plum
  | Powderblue
  | Purple
  | Red
  | Rosybrown
  | Royalblue
  | Saddlebrown
  | Salmon
  | Sandybrown
  | Seagreen
  | Seashell
  | Sienna
  | Silver
  | Skyblue
  | Slateblue
  | Slategray
  | Slategrey
  | Snow
  | Springgreen
  | Steelblue
  | Tan
  | Teal
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White
  | Whitesmoke
  | Yellow
  | Yellowgreen
```
The colors by name.

```ocaml
val string_of_name : name -> string
```
Gives the string equivalent of the argument.

```ocaml
val rgb_of_name : name -> int * int * int
```
Converts a color name into three integers representing the Red, Green and Blue channels. Channel values are in between `0` and `255`.

```ocaml
val hex_of_rgb : (int * int * int) -> string
```
Convert a tuple of three integers between `0` and `255` into a hex string

```ocaml
type t = 
  | Name of name (* A color by name *)
  | RGB of int * int * int (* Red, Green and Blue values. Clipped to 0..255 by most (All?) browsers. *)
  | RGB_percent of int * int * int (* RBG channels are specified as a percentage of their maximal value. *)
  | RGBA of int * int * int * float (* Same as RGB with additional transparency argument. Opacity should be between 0. (completely transparent) and 1. (completely opaque). *)
  | RGBA_percent of int * int * int * float (* RGB channels specified as percentage of their maximal value. Alpha channel (opacity) is still a 0. to 1. float. *)
  | HSL of int * int * int (* Hue, Saturation and Lightness values. Hue is an angle in degree (in interval 0..360). Saturation is a percentage (0..100) with 0 being colorless. Lightness is also a percentage (0..100) with 0 being black. *)
  | HSLA of int * int * int * float (* Same as HSL with an opacity argument between 0. and 1.. *)
```
The type of colors, either by name, by Red-Green-Blue constructor or by Hue-Saturation-Lightness constructors.

```ocaml
val rgb : ?a:float -> int -> int -> int -> t
```
build a color from the values of red, green, and blue channels. optional `a` argument can be used to specify alpha channel (aka opacity).

```ocaml
val hsl : ?a:float -> int -> int -> int -> t
```
build a color from the values of hue, saturation, and lightness channels. optional `a` argument can be used to specify alpha channel (aka opacity).

```ocaml
type js_t = private Js.js_string Js.t
```
A `js_t` is a valid string representation of a CSS color

A few conversion functions

```ocaml
val string_of_t : t -> string
```
Convert to a string representation (for debugging purpose mainly).

```ocaml
val js : t -> js_t
```
Projection from OCaml to Js. `js c` is equivalent to `Js.string (string_of_t c)` but with a `js_t` return type.

```ocaml
val ml : js_t -> t
```
Projection from Js to OCaml. The function is the dual of `js`.

```ocaml
val js_t_of_js_string : Js.js_string Js.t -> js_t
```
Checks the well-formedness of a string or fails with `Invalid_argument`
