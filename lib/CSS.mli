(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(**This module contains a few types and values to ease the use of CSS properties
   and such. If you think a feature is missing, consider sending a patch or an
   RFE to the mailing list.
*)

(**All about CSS colors. MDC documentation here:
  https://developer.mozilla.org/en/CSS/color_value . Specifications here:
  http://www.w3.org/TR/css3-color/#svg-color .*)

(**The colors by name.*)
type color_name =
  | Aliceblue | Antiquewhite | Aqua | Aquamarine | Azure
  | Beige | Bisque | Black | Blanchedalmond | Blue | Blueviolet | Brown
  | Burlywood
  | Cadetblue | Chartreuse | Chocolate | Coral | Cornflowerblue | Cornsilk
  | Crimson | Cyan
  | Darkblue | Darkcyan | Darkgoldenrod | Darkgray | Darkgreen | Darkgrey
  | Darkkhaki | Darkmagenta | Darkolivegreen | Darkorange | Darkorchid | Darkred
  | Darksalmon | Darkseagreen | Darkslateblue | Darkslategray | Darkslategrey
  | Darkturquoise | Darkviolet | Deeppink | Deepskyblue | Dimgray | Dimgrey
  | Dodgerblue
  | Firebrick | Floralwhite | Forestgreen | Fuchsia
  | Gainsboro | Ghostwhite | Gold | Goldenrod | Gray | Grey | Green
  | Greenyellow
  | Honeydew | Hotpink
  | Indianred | Indigo | Ivory
  | Khaki
  | Lavender | Lavenderblush | Lawngreen | Lemonchiffon | Lightblue | Lightcoral
  | Lightcyan | Lightgoldenrodyellow | Lightgray | Lightgreen | Lightgrey
  | Lightpink | Lightsalmon | Lightseagreen | Lightskyblue | Lightslategray
  | Lightslategrey | Lightsteelblue | Lightyellow | Lime | Limegreen | Linen
  | Magenta | Maroon | Mediumaquamarine | Mediumblue | Mediumorchid
  | Mediumpurple | Mediumseagreen | Mediumslateblue | Mediumspringgreen
  | Mediumturquoise | Mediumvioletred | Midnightblue | Mintcream | Mistyrose
  | Moccasin
  | Navajowhite | Navy
  | Oldlace | Olive | Olivedrab | Orange | Orangered | Orchid
  | Palegoldenrod | Palegreen | Paleturquoise | Palevioletred | Papayawhip
  | Peachpuff | Peru | Pink | Plum | Powderblue | Purple
  | Red | Rosybrown | Royalblue
  | Saddlebrown | Salmon | Sandybrown | Seagreen | Seashell | Sienna | Silver
  | Skyblue | Slateblue | Slategray | Slategrey | Snow | Springgreen | Steelblue
  | Tan | Teal | Thistle | Tomato | Turquoise
  | Violet
  | Wheat | White | Whitesmoke
  | Yellow | Yellowgreen

(**Gives the string equivalent of the argument.*)
val string_of_color_name : color_name -> string

(**Converts a color name into three integers representing the Red, Green and
  Blue channels. Channel values are in between [0] and [255].*)
val rgb_of_color_name : color_name -> (int * int * int)


(**The type of colors, either by name, by Red-Green-Blue constructor or by
   Hue-Saturation-Lightness constructors.*)
type color =

  | Color_name of color_name
  (**A color by name*)

  | RGB of (int * int * int)
  (**Red, Green and Blue values. Clipped to [0..255] by most (All?)
     browsers.*)

  | RGB_percent of (int * int * int)
  (**RBG channels are specified as a percentage of their maximal value.*)

  | RGBA of (int * int * int * float)
  (**Same as RGB with additionnal transparency argument. Opacity should be
    between [0.] (completely transparent) and [1.] (completely opaque).*)

  | RGBA_percent of (int * int * int * float)
  (**RGB channels specified as percentage of their maximal value. Alpha
    channel (opacity) is still a [0.] to [1.] float.*)

  | HSL of (int * int * int)
  (**Hue, Saturation and Lightness values. Hue is an angle in degree (in
     interval [0..360]). Saturation is a percentage ([0..100]) with [0]
     being colorless. Lightness is also a percentage ([0..100]) with [0]
     being black.*)

  | HSLA of (int * int * int * float)
  (**Same as HSL with an opacity argument between [0.] and [1.].*)


(**build a color from the values of red, green, and blue channels. optional [a]
   argument can be used to specify alpha channel (aka opacity).*)
val rgb : ?a:float -> int -> int -> int -> color

(**build a color from the values of hue, saturation, and lightness channels.
   optional [a] argument can be used to specify alpha channel (aka opacity).*)
val hsl : ?a:float -> int -> int -> int -> color

(**Return a string representation of a color.*)
val string_of_color : color -> string

(**A [js_color] is a valid string representation of a CSS color*)
type js_color = private Js.js_string Js.t

(**A few conversion functions*)

(**Projection from OCam to Js. [color c] is equivalent
   to [Js.string (string_of_color c)].*)
val color : color -> js_color

(**Checks the well-formedness of a string or fails with [Invalid_argument]*)
val js_color_of_js_string : Js.js_string Js.t -> js_color
