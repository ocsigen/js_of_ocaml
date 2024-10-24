// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Format

//Provides: caml_parse_format
//Requires: caml_jsbytes_of_string, caml_invalid_argument
function caml_parse_format(fmt) {
  fmt = caml_jsbytes_of_string(fmt);
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f = {
    justify: "+",
    signstyle: "-",
    filler: " ",
    alternate: false,
    base: 0,
    signedconv: false,
    width: 0,
    uppercase: false,
    sign: 1,
    prec: -1,
    conv: "f",
  };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
      case "-":
        f.justify = "-";
        break;
      case "+":
      case " ":
        f.signstyle = c;
        break;
      case "0":
        f.filler = "0";
        break;
      case "#":
        f.alternate = true;
        break;
      case "1":
      case "2":
      case "3":
      case "4":
      case "5":
      case "6":
      case "7":
      case "8":
      case "9":
        f.width = 0;
        while (((c = fmt.charCodeAt(i) - 48), c >= 0 && c <= 9)) {
          f.width = f.width * 10 + c;
          i++;
        }
        i--;
        break;
      case ".":
        f.prec = 0;
        i++;
        while (((c = fmt.charCodeAt(i) - 48), c >= 0 && c <= 9)) {
          f.prec = f.prec * 10 + c;
          i++;
        }
        i--;
        break;
      case "d":
      case "i":
        f.signedconv = true;
        f.base = 10;
        break;
      case "u":
        f.base = 10;
        break;
      case "x":
        f.base = 16;
        break;
      case "X":
        f.base = 16;
        f.uppercase = true;
        break;
      case "o":
        f.base = 8;
        break;
      case "e":
      case "f":
      case "g":
        f.signedconv = true;
        f.conv = c;
        break;
      case "E":
      case "F":
      case "G":
        f.signedconv = true;
        f.uppercase = true;
        f.conv = c.toLowerCase();
        break;
    }
  }
  return f;
}

//Provides: caml_finish_formatting
//Requires: caml_string_of_jsbytes
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  /* Adjust len to reflect additional chars (sign, etc) */
  if (f.signedconv && (f.sign < 0 || f.signstyle !== "-")) len++;
  if (f.alternate) {
    if (f.base === 8) len += 1;
    if (f.base === 16) len += 2;
  }
  /* Do the formatting */
  var buffer = "";
  if (f.justify === "+" && f.filler === " ")
    for (var i = len; i < f.width; i++) buffer += " ";
  if (f.signedconv) {
    if (f.sign < 0) buffer += "-";
    else if (f.signstyle !== "-") buffer += f.signstyle;
  }
  if (f.alternate && f.base === 8) buffer += "0";
  if (f.alternate && f.base === 16) buffer += f.uppercase ? "0X" : "0x";
  if (f.justify === "+" && f.filler === "0")
    for (var i = len; i < f.width; i++) buffer += "0";
  buffer += rawbuffer;
  if (f.justify === "-") for (var i = len; i < f.width; i++) buffer += " ";
  return caml_string_of_jsbytes(buffer);
}
