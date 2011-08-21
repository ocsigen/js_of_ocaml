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


(*The type of CSS colors. First by name and then by constructor.*)
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

let string_of_color_name = function
  | Aliceblue              -> "aliceblue"
  | Antiquewhite           -> "antiquewhite"
  | Aqua                   -> "aqua"
  | Aquamarine             -> "aquamarine"
  | Azure                  -> "azure"
  | Beige                  -> "beige"
  | Bisque                 -> "bisque"
  | Black                  -> "black"
  | Blanchedalmond         -> "blanchedalmond"
  | Blue                   -> "blue"
  | Blueviolet             -> "blueviolet"
  | Brown                  -> "brown"
  | Burlywood              -> "burlywood"
  | Cadetblue              -> "cadetblue"
  | Chartreuse             -> "chartreuse"
  | Chocolate              -> "chocolate"
  | Coral                  -> "coral"
  | Cornflowerblue         -> "cornflowerblue"
  | Cornsilk               -> "cornsilk"
  | Crimson                -> "crimson"
  | Cyan                   -> "cyan"
  | Darkblue               -> "darkblue"
  | Darkcyan               -> "darkcyan"
  | Darkgoldenrod          -> "darkgoldenrod"
  | Darkgray               -> "darkgray"
  | Darkgreen              -> "darkgreen"
  | Darkgrey               -> "darkgrey"
  | Darkkhaki              -> "darkkhaki"
  | Darkmagenta            -> "darkmagenta"
  | Darkolivegreen         -> "darkolivegreen"
  | Darkorange             -> "darkorange"
  | Darkorchid             -> "darkorchid"
  | Darkred                -> "darkred"
  | Darksalmon             -> "darksalmon"
  | Darkseagreen           -> "darkseagreen"
  | Darkslateblue          -> "darkslateblue"
  | Darkslategray          -> "darkslategray"
  | Darkslategrey          -> "darkslategrey"
  | Darkturquoise          -> "darkturquoise"
  | Darkviolet             -> "darkviolet"
  | Deeppink               -> "deeppink"
  | Deepskyblue            -> "deepskyblue"
  | Dimgray                -> "dimgray"
  | Dimgrey                -> "dimgrey"
  | Dodgerblue             -> "dodgerblue"
  | Firebrick              -> "firebrick"
  | Floralwhite            -> "floralwhite"
  | Forestgreen            -> "forestgreen"
  | Fuchsia                -> "fuchsia"
  | Gainsboro              -> "gainsboro"
  | Ghostwhite             -> "ghostwhite"
  | Gold                   -> "gold"
  | Goldenrod              -> "goldenrod"
  | Gray                   -> "gray"
  | Green                  -> "green"
  | Greenyellow            -> "greenyellow"
  | Grey                   -> "grey"
  | Honeydew               -> "honeydew"
  | Hotpink                -> "hotpink"
  | Indianred              -> "indianred"
  | Indigo                 -> "indigo"
  | Ivory                  -> "ivory"
  | Khaki                  -> "khaki"
  | Lavender               -> "lavender"
  | Lavenderblush          -> "lavenderblush"
  | Lawngreen              -> "lawngreen"
  | Lemonchiffon           -> "lemonchiffon"
  | Lightblue              -> "lightblue"
  | Lightcoral             -> "lightcoral"
  | Lightcyan              -> "lightcyan"
  | Lightgoldenrodyellow   -> "lightgoldenrodyellow"
  | Lightgray              -> "lightgray"
  | Lightgreen             -> "lightgreen"
  | Lightgrey              -> "lightgrey"
  | Lightpink              -> "lightpink"
  | Lightsalmon            -> "lightsalmon"
  | Lightseagreen          -> "lightseagreen"
  | Lightskyblue           -> "lightskyblue"
  | Lightslategray         -> "lightslategray"
  | Lightslategrey         -> "lightslategrey"
  | Lightsteelblue         -> "lightsteelblue"
  | Lightyellow            -> "lightyellow"
  | Lime                   -> "lime"
  | Limegreen              -> "limegreen"
  | Linen                  -> "linen"
  | Magenta                -> "magenta"
  | Maroon                 -> "maroon"
  | Mediumaquamarine       -> "mediumaquamarine"
  | Mediumblue             -> "mediumblue"
  | Mediumorchid           -> "mediumorchid"
  | Mediumpurple           -> "mediumpurple"
  | Mediumseagreen         -> "mediumseagreen"
  | Mediumslateblue        -> "mediumslateblue"
  | Mediumspringgreen      -> "mediumspringgreen"
  | Mediumturquoise        -> "mediumturquoise"
  | Mediumvioletred        -> "mediumvioletred"
  | Midnightblue           -> "midnightblue"
  | Mintcream              -> "mintcream"
  | Mistyrose              -> "mistyrose"
  | Moccasin               -> "moccasin"
  | Navajowhite            -> "navajowhite"
  | Navy                   -> "navy"
  | Oldlace                -> "oldlace"
  | Olive                  -> "olive"
  | Olivedrab              -> "olivedrab"
  | Orange                 -> "orange"
  | Orangered              -> "orangered"
  | Orchid                 -> "orchid"
  | Palegoldenrod          -> "palegoldenrod"
  | Palegreen              -> "palegreen"
  | Paleturquoise          -> "paleturquoise"
  | Palevioletred          -> "palevioletred"
  | Papayawhip             -> "papayawhip"
  | Peachpuff              -> "peachpuff"
  | Peru                   -> "peru"
  | Pink                   -> "pink"
  | Plum                   -> "plum"
  | Powderblue             -> "powderblue"
  | Purple                 -> "purple"
  | Red                    -> "red"
  | Rosybrown              -> "rosybrown"
  | Royalblue              -> "royalblue"
  | Saddlebrown            -> "saddlebrown"
  | Salmon                 -> "salmon"
  | Sandybrown             -> "sandybrown"
  | Seagreen               -> "seagreen"
  | Seashell               -> "seashell"
  | Sienna                 -> "sienna"
  | Silver                 -> "silver"
  | Skyblue                -> "skyblue"
  | Slateblue              -> "slateblue"
  | Slategray              -> "slategray"
  | Slategrey              -> "slategrey"
  | Snow                   -> "snow"
  | Springgreen            -> "springgreen"
  | Steelblue              -> "steelblue"
  | Tan                    -> "tan"
  | Teal                   -> "teal"
  | Thistle                -> "thistle"
  | Tomato                 -> "tomato"
  | Turquoise              -> "turquoise"
  | Violet                 -> "violet"
  | Wheat                  -> "wheat"
  | White                  -> "white"
  | Whitesmoke             -> "whitesmoke"
  | Yellow                 -> "yellow"
  | Yellowgreen            -> "yellowgreen"

let rgb_of_color_name = function
  | Aliceblue              -> (240,248,255)
  | Antiquewhite           -> (250,235,215)
  | Aqua                   -> (0,255,255)
  | Aquamarine             -> (127,255,212)
  | Azure                  -> (240,255,255)
  | Beige                  -> (245,245,220)
  | Bisque                 -> (255,228,196)
  | Black                  -> (0,0,0)
  | Blanchedalmond         -> (255,235,205)
  | Blue                   -> (0,0,255)
  | Blueviolet             -> (138,43,226)
  | Brown                  -> (165,42,42)
  | Burlywood              -> (222,184,135)
  | Cadetblue              -> (95,158,160)
  | Chartreuse             -> (127,255,0)
  | Chocolate              -> (210,105,30)
  | Coral                  -> (255,127,80)
  | Cornflowerblue         -> (100,149,237)
  | Cornsilk               -> (255,248,220)
  | Crimson                -> (220,20,60)
  | Cyan                   -> (0,255,255)
  | Darkblue               -> (0,0,139)
  | Darkcyan               -> (0,139,139)
  | Darkgoldenrod          -> (184,134,11)
  | Darkgray               -> (169,169,169)
  | Darkgreen              -> (0,100,0)
  | Darkgrey               -> (169,169,169)
  | Darkkhaki              -> (189,183,107)
  | Darkmagenta            -> (139,0,139)
  | Darkolivegreen         -> (85,107,47)
  | Darkorange             -> (255,140,0)
  | Darkorchid             -> (153,50,204)
  | Darkred                -> (139,0,0)
  | Darksalmon             -> (233,150,122)
  | Darkseagreen           -> (143,188,143)
  | Darkslateblue          -> (72,61,139)
  | Darkslategray          -> (47,79,79)
  | Darkslategrey          -> (47,79,79)
  | Darkturquoise          -> (0,206,209)
  | Darkviolet             -> (148,0,211)
  | Deeppink               -> (255,20,147)
  | Deepskyblue            -> (0,191,255)
  | Dimgray                -> (105,105,105)
  | Dimgrey                -> (105,105,105)
  | Dodgerblue             -> (30,144,255)
  | Firebrick              -> (178,34,34)
  | Floralwhite            -> (255,250,240)
  | Forestgreen            -> (34,139,34)
  | Fuchsia                -> (255,0,255)
  | Gainsboro              -> (220,220,220)
  | Ghostwhite             -> (248,248,255)
  | Gold                   -> (255,215,0)
  | Goldenrod              -> (218,165,32)
  | Gray                   -> (128,128,128)
  | Green                  -> (0,128,0)
  | Greenyellow            -> (173,255,47)
  | Grey                   -> (128,128,128)
  | Honeydew               -> (240,255,240)
  | Hotpink                -> (255,105,180)
  | Indianred              -> (205,92,92)
  | Indigo                 -> (75,0,130)
  | Ivory                  -> (255,255,240)
  | Khaki                  -> (240,230,140)
  | Lavender               -> (230,230,250)
  | Lavenderblush          -> (255,240,245)
  | Lawngreen              -> (124,252,0)
  | Lemonchiffon           -> (255,250,205)
  | Lightblue              -> (173,216,230)
  | Lightcoral             -> (240,128,128)
  | Lightcyan              -> (224,255,255)
  | Lightgoldenrodyellow   -> (250,250,210)
  | Lightgray              -> (211,211,211)
  | Lightgreen             -> (144,238,144)
  | Lightgrey              -> (211,211,211)
  | Lightpink              -> (255,182,193)
  | Lightsalmon            -> (255,160,122)
  | Lightseagreen          -> (32,178,170)
  | Lightskyblue           -> (135,206,250)
  | Lightslategray         -> (119,136,153)
  | Lightslategrey         -> (119,136,153)
  | Lightsteelblue         -> (176,196,222)
  | Lightyellow            -> (255,255,224)
  | Lime                   -> (0,255,0)
  | Limegreen              -> (50,205,50)
  | Linen                  -> (250,240,230)
  | Magenta                -> (255,0,255)
  | Maroon                 -> (128,0,0)
  | Mediumaquamarine       -> (102,205,170)
  | Mediumblue             -> (0,0,205)
  | Mediumorchid           -> (186,85,211)
  | Mediumpurple           -> (147,112,219)
  | Mediumseagreen         -> (60,179,113)
  | Mediumslateblue        -> (123,104,238)
  | Mediumspringgreen      -> (0,250,154)
  | Mediumturquoise        -> (72,209,204)
  | Mediumvioletred        -> (199,21,133)
  | Midnightblue           -> (25,25,112)
  | Mintcream              -> (245,255,250)
  | Mistyrose              -> (255,228,225)
  | Moccasin               -> (255,228,181)
  | Navajowhite            -> (255,222,173)
  | Navy                   -> (0,0,128)
  | Oldlace                -> (253,245,230)
  | Olive                  -> (128,128,0)
  | Olivedrab              -> (107,142,35)
  | Orange                 -> (255,165,0)
  | Orangered              -> (255,69,0)
  | Orchid                 -> (218,112,214)
  | Palegoldenrod          -> (238,232,170)
  | Palegreen              -> (152,251,152)
  | Paleturquoise          -> (175,238,238)
  | Palevioletred          -> (219,112,147)
  | Papayawhip             -> (255,239,213)
  | Peachpuff              -> (255,218,185)
  | Peru                   -> (205,133,63)
  | Pink                   -> (255,192,203)
  | Plum                   -> (221,160,221)
  | Powderblue             -> (176,224,230)
  | Purple                 -> (128,0,128)
  | Red                    -> (255,0,0)
  | Rosybrown              -> (188,143,143)
  | Royalblue              -> (65,105,225)
  | Saddlebrown            -> (139,69,19)
  | Salmon                 -> (250,128,114)
  | Sandybrown             -> (244,164,96)
  | Seagreen               -> (46,139,87)
  | Seashell               -> (255,245,238)
  | Sienna                 -> (160,82,45)
  | Silver                 -> (192,192,192)
  | Skyblue                -> (135,206,235)
  | Slateblue              -> (106,90,205)
  | Slategray              -> (112,128,144)
  | Slategrey              -> (112,128,144)
  | Snow                   -> (255,250,250)
  | Springgreen            -> (0,255,127)
  | Steelblue              -> (70,130,180)
  | Tan                    -> (210,180,140)
  | Teal                   -> (0,128,128)
  | Thistle                -> (216,191,216)
  | Tomato                 -> (255,99,71)
  | Turquoise              -> (64,224,208)
  | Violet                 -> (238,130,238)
  | Wheat                  -> (245,222,179)
  | White                  -> (255,255,255)
  | Whitesmoke             -> (245,245,245)
  | Yellow                 -> (255,255,0)
  | Yellowgreen            -> (154,205,50)

type color =
  | Color_name of color_name

  | RGB of (int * int * int)
    (**Red, Green and Blue values. Clipped to [[0..255]] by most (All?)
       browsers.*)

  | RGB_percent of (int * int * int)
    (**RGB channels are specified as a percentage of their maximal value.*)

  | RGBA of (int * int * int * float)
    (**Same as RGB with additionnal transparency argument. Opacity should be in
       [0.] (completely transparent) and [1.] (completely opaque).*)

  | RGBA_percent of (int * int * int * float)
    (**RGB channels specified as percentage of their maximal value. Alpha
        channel (opacity) is still a [0.] to [1.] float.*)

  | HSL of (int * int * int)
    (**Hue, Saturation and Lightness values. Hue is an angle in degree (in
       interval [[0..360[]). Saturation is a percentage ([[0..100]]) with [0]
       being colorless. Lightness is also a percentage ([[0..100]]) with [0]
       being black.*)

  | HSLA of (int * int * int * float)
  (**Same as HSL with an opacity argument between [0.] and [1.].*)

let rgb ?a r g b = match a with
  | None   -> RGB(r,g,b)
  | Some a -> RGBA(r,g,b,a)

let hsl ?a h s l = match a with
  | None   -> HSL(h,s,l)
  | Some a -> HSLA(h,s,l,a)

let string_of_color = function
  | Color_name n           -> string_of_color_name n
  | RGB (r,g,b)            -> Printf.sprintf "rgb(%d,%d,%d)"           r g b
  | RGB_percent (r,g,b)    -> Printf.sprintf "rgb(%d%%,%d%%,%d%%)"     r g b
  | RGBA (r,g,b,a)         -> Printf.sprintf "rgba(%d,%d,%d,%f)"       r g b a
  | RGBA_percent (r,g,b,a) -> Printf.sprintf "rgba(%d%%,%d%%,%d%%,%f)" r g b a
  | HSL (h,s,l)            -> Printf.sprintf "hsl(%d,%d%%,%d%%)"       h s l
  | HSLA (h,s,l,a)         -> Printf.sprintf "hsla(%d,%d%%,%d%%,%f)"   h s l a

(*Ocaml <-> JS representation*)
type js_color = Js.js_string Js.t

(*TODO? be more restrictive, clip values into standard range*)
let js_color_of_js_string s =
  let rgb_re = jsnew Js.regExp
    (Js.bytestring "^rgb\\(\\s*\\d*,\\s*\\d*,\\s*\\d*\\)$")
  in
  let rgb_pct_re = jsnew Js.regExp
    (Js.bytestring "^rgb\\(\\s*\\d*%,\\s*\\d*%,\\s*\\d*%\\)$")
  in
  let rgba_re = jsnew Js.regExp
    (Js.bytestring
      "^rgba\\(\\s*\\d*,\\s*\\d*,\\s*\\d*,\\d*\\.?\\d*\\)$")
  in
  let rgba_pct_re = jsnew Js.regExp
    (Js.bytestring
      "^rgba\\(\\s*\\d*%,\\s*\\d*%,\\s*\\d*%,\\d*\\.?\\d*\\)$")
  in
  let hsl_re = jsnew Js.regExp
    (Js.bytestring "^hsl\\(\\s*\\d*,\\s*\\d*%,\\s*\\d*%\\)$")
  in
  let hsla_re = jsnew Js.regExp
    (Js.bytestring "^hsla\\(\\s*\\d*,\\s*\\d*%,\\s*\\d*%,\\d*\\.?\\d*\\)$")
  in
  if Js.to_bool (rgb_re##test(s)) || Js.to_bool (rgba_re##test(s))
    || Js.to_bool (rgb_pct_re##test(s)) || Js.to_bool (rgba_pct_re##test(s))
    || Js.to_bool (hsl_re##test(s)) || Js.to_bool (hsla_re##test(s))
  then
    s
  else
    if List.mem (Js.to_string s)
    [ "aliceblue"; "antiquewhite"; "aqua"; "aquamarine"; "azure"; "beige";
    "bisque"; "black"; "blanchedalmond"; "blue"; "blueviolet"; "brown";
    "burlywood"; "cadetblue"; "chartreuse"; "chocolate"; "coral";
    "cornflowerblue"; "cornsilk"; "crimson"; "cyan"; "darkblue"; "darkcyan";
    "darkgoldenrod"; "darkgray"; "darkgreen"; "darkgrey"; "darkkhaki";
    "darkmagenta"; "darkolivegreen"; "darkorange"; "darkorchid"; "darkred";
    "darksalmon"; "darkseagreen"; "darkslateblue"; "darkslategray";
    "darkslategrey"; "darkturquoise"; "darkviolet"; "deeppink"; "deepskyblue";
    "dimgray"; "dimgrey"; "dodgerblue"; "firebrick"; "floralwhite";
    "forestgreen"; "fuchsia"; "gainsboro"; "ghostwhite"; "gold"; "goldenrod";
    "gray"; "green"; "greenyellow"; "grey"; "honeydew"; "hotpink"; "indianred";
    "indigo"; "ivory"; "khaki"; "lavender"; "lavenderblush"; "lawngreen";
    "lemonchiffon"; "lightblue"; "lightcoral"; "lightcyan";
    "lightgoldenrodyellow"; "lightgray"; "lightgreen"; "lightgrey"; "lightpink";
    "lightsalmon"; "lightseagreen"; "lightskyblue"; "lightslategray";
    "lightslategrey"; "lightsteelblue"; "lightyellow"; "lime"; "limegreen";
    "linen"; "magenta"; "maroon"; "mediumaquamarine"; "mediumblue";
    "mediumorchid"; "mediumpurple"; "mediumseagreen"; "mediumslateblue";
    "mediumspringgreen"; "mediumturquoise"; "mediumvioletred"; "midnightblue";
    "mintcream"; "mistyrose"; "moccasin"; "navajowhite"; "navy"; "oldlace";
    "olive"; "olivedrab"; "orange"; "orangered"; "orchid"; "palegoldenrod";
    "palegreen"; "paleturquoise"; "palevioletred"; "papayawhip"; "peachpuff";
    "peru"; "pink"; "plum"; "powderblue"; "purple"; "red"; "rosybrown";
    "royalblue"; "saddlebrown"; "salmon"; "sandybrown"; "seagreen"; "seashell";
    "sienna"; "silver"; "skyblue"; "slateblue"; "slategray"; "slategrey";
    "snow"; "springgreen"; "steelblue"; "tan"; "teal"; "thistle"; "tomato";
    "turquoise"; "violet"; "wheat"; "white"; "whitesmoke"; "yellow";
    "yellowgreen"; ]
    then
      s
    else raise (Invalid_argument (Js.to_string s ^ " is not a valid color"))

let color_name cn = Js.string (string_of_color_name cn)

(*TODO? inline function calls*)
let color = function
  | Color_name n -> color_name n
  | RGB _ | RGB_percent _ | RGBA _ | RGBA_percent _
  | HSL _ | HSLA _ as c
  -> Js.string (string_of_color c)

