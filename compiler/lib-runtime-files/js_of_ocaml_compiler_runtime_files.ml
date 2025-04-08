(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

let runtime =
  Files.
    [ array
    ; backtrace
    ; bigarray
    ; bigstring
    ; compare
    ; fail
    ; format
    ; fs
    ; fs_fake
    ; fs_node
    ; gc
    ; graphics
    ; hash
    ; ieee_754
    ; int64
    ; ints
    ; io
    ; jslib
    ; jslib_js_of_ocaml
    ; lexing
    ; marshal
    ; md5
    ; mlBytes
    ; nat
    ; obj
    ; parsing
    ; stdlib
    ; sys
    ; str
    ; unix
    ; weak
    ; domain
    ; prng
    ; sync
    ; effect_
    ; zstd
    ; runtime_events
    ; blake2
    ]

include Files
