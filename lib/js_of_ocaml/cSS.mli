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

(** This module contains a few types and values to ease the use of CSS properties and
    such. If you think a feature is missing, consider sending a patch or an RFE to the
    mailing list.

    This module contain submodules each with a signature similar to:
    {[
      type t (* type the module is focused on *)

      type js_t (* valid js representation of values of type t *)

      val js : t -> js_t (* conversion *)

      val ml : js_t -> t (* conversion *)
    ]}
    Additional functions (string conversion, standard operation, etc.) are sometime
    available. Some module have several different types instead of just one. *)

module Color : sig
  (**All about CSS colors. MDC documentation here:
     https://developer.mozilla.org/en/CSS/color_value . Specifications here:
     http://www.w3.org/TR/css3-color/#svg-color .*)

  (**The colors by name.*)
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

  val string_of_name : name -> string
  (**Gives the string equivalent of the argument.*)

  val rgb_of_name : name -> int * int * int
  (**Converts a color name into three integers representing the Red, Green and Blue
     channels. Channel values are in between [0] and [255].*)

  val hex_of_rgb : int * int * int -> string
  (** Convert a tuple of three integers between [0] and [255] into a hex string *)

  (**The type of colors, either by name, by Red-Green-Blue constructor or by
     Hue-Saturation-Lightness constructors.*)
  type t =
    | Name of name  (**A color by name*)
    | RGB of (int * int * int)
        (**Red, Green and Blue values. Clipped to [0..255] by most (All?) browsers.*)
    | RGB_percent of (int * int * int)
        (**RBG channels are specified as a percentage of their maximal value.*)
    | RGBA of (int * int * int * float)
        (**Same as RGB with additional transparency argument. Opacity should be between
           [0.] (completely transparent) and [1.] (completely opaque).*)
    | RGBA_percent of (int * int * int * float)
        (**RGB channels specified as percentage of their maximal value. Alpha channel
           (opacity) is still a [0.] to [1.] float.*)
    | HSL of (int * int * int)
        (**Hue, Saturation and Lightness values. Hue is an angle in degree (in interval
           [0..360]). Saturation is a percentage ([0..100]) with [0] being colorless.
           Lightness is also a percentage ([0..100]) with [0] being black.*)
    | HSLA of (int * int * int * float)
        (**Same as HSL with an opacity argument between [0.] and [1.].*)

  val rgb : ?a:float -> int -> int -> int -> t
  (**build a color from the values of red, green, and blue channels. optional [a] argument
     can be used to specify alpha channel (aka opacity).*)

  val hsl : ?a:float -> int -> int -> int -> t
  (**build a color from the values of hue, saturation, and lightness channels. optional
     [a] argument can be used to specify alpha channel (aka opacity).*)

  type js_t = private Js.js_string Js.t
  (**A [js_t] is a valid string representation of a CSS color*)

  (**A few conversion functions*)

  val string_of_t : t -> string
  (**Convert to a string representation (for debugging purpose mainly).*)

  val js : t -> js_t
  (**Projection from OCaml to Js. [js c] is equivalent to [Js.string (string_of_t c)] but
     with a [js_t] return type.*)

  val ml : js_t -> t
  (**Projection from Js to OCaml. The function is the dual of [js].*)

  val js_t_of_js_string : Js.js_string Js.t -> js_t
  (**Checks the well-formedness of a string or fails with [Invalid_argument]*)
end

module Length : sig
  (**The type of length attributes. Mdc documentation:
     https://developer.mozilla.org/en/CSS/length and specification:
     http://www.w3.org/TR/css3-values/#lengths *)
  type t =
    | Zero  (**For 0, unit is optional*)
    | Em of float  (**Relative to the font size *)
    | Ex of float  (**Relative to the x-height *)
    | Px of float  (**Relative to the viewing device *)
    | Gd of float  (**Relative to the grid *)
    | Rem of float  (**Relative to the font size of the root *)
    | Vw of float  (**Relative to the viewport's width *)
    | Vh of float  (**Relative to the viewport's height *)
    | Vm of float  (**Relative to the smallest of the viewport's width or height *)
    | Ch of float  (**Relative to the width of a char '0' *)
    | Mm of float  (** in Milimeter *)
    | Cm of float  (** in Centimeter *)
    | In of float  (** in Inch *)
    | Pt of float  (** in Points (72pt = 1in)*)
    | Pc of float  (** in Picas (1pc = 12pt)*)

  type js_t = private Js.js_string Js.t
  (** Js representation of lengths. *)

  (**Conversion functions*)

  val string_of_t : t -> string

  val js : t -> js_t

  val ml : js_t -> t
end

module Angle : sig
  type t =
    | Deg of float
    | Grad of float
    | Rad of float
    | Turns of float

  type js_t = private Js.js_string Js.t

  val string_of_t : t -> string

  val js : t -> js_t

  val ml : js_t -> t
end
