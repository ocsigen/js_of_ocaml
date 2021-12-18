(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

open! Stdlib

let keyword =
  List.fold_left
    ~f:(fun acc x -> StringSet.add x acc)
    ~init:StringSet.empty
    [ (* keyword *)
      "break"
    ; "case"
    ; "catch"
    ; "continue"
    ; "debugger"
    ; "default"
    ; "delete"
    ; "do"
    ; "else"
    ; "finally"
    ; "for"
    ; "function"
    ; "if"
    ; "in"
    ; "instanceof"
    ; "new"
    ; "return"
    ; "switch"
    ; "this"
    ; "throw"
    ; "try"
    ; "typeof"
    ; "var"
    ; "void"
    ; "while"
    ; "with"
    ; (* reserved in ECMAScript 5 *)
      "class"
    ; "enum"
    ; "export"
    ; "extends"
    ; "import"
    ; "super"
    ; "implements"
    ; "interface"
    ; "let"
    ; "package"
    ; "private"
    ; "protected"
    ; "public"
    ; "static"
    ; "yield"
    ; (* other *)
      "null"
    ; "true"
    ; "false"
    ; "NaN"
    ; "undefined"
    ; "this"
    ; (* Unexpected eval or arguments in strict mode *)
      "eval"
    ; "arguments"
    ; (* also reserved in ECMAScript 3 *)
      "abstract"
    ; "boolean"
    ; "byte"
    ; "char"
    ; "const"
    ; "double"
    ; "final"
    ; "float"
    ; "goto"
    ; "int"
    ; "long"
    ; "native"
    ; "short"
    ; "synchronized"
    ; "throws"
    ; "transient"
    ; "volatile"
    ; (* also reserved in ECMAScript 6 *)
      "await"
    ]

let provided =
  List.fold_left
    ~f:(fun acc x -> StringSet.add x acc)
    ~init:StringSet.empty
    [ "event"
    ; "location"
    ; "window"
    ; "document"
    ; "eval"
    ; "navigator"
    ; "self"
    ; "Array"
    ; "Function"
    ; "Date"
    ; "Math"
    ; "JSON"
    ; "Object"
    ; "globalThis"
    ; "RegExp"
    ; "String"
    ; "Boolean"
    ; "Number"
    ; "BigInt"
    ; "Infinity"
    ; "isFinite"
    ; "ActiveXObject"
    ; "XMLHttpRequest"
    ; "XDomainRequest"
    ; "DOMException"
    ; "Error"
    ; "SyntaxError"
    ; "TypeError"
    ; "arguments"
    ; "decodeURI"
    ; "decodeURIComponent"
    ; "encodeURI"
    ; "encodeURIComponent"
    ; "escape"
    ; "unescape"
    ; "isNaN"
    ; "parseFloat"
    ; "parseInt"
    ; "module"
    ; "require"
    ; "Symbol"
    ]
