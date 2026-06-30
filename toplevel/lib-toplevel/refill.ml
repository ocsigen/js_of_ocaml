(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

open! Js_of_ocaml_compiler
open! Js_of_ocaml_compiler.Stdlib

let trim_end s =
  let ws = function
    | ' ' | '\t' | '\n' -> true
    | _ -> false
  in
  let len = String.length s in
  let stop = ref (len - 1) in
  while !stop >= 0 && ws s.[!stop] do
    decr stop
  done;
  String.sub s ~pos:0 ~len:(!stop + 1)

(* Ensure the source ends with [;;] so the incremental
   [Toploop.parse_toplevel_phrase] driven from this refill function (used by
   [execute]) does not drop a trailing, unterminated phrase. The [use] paths
   parse whole buffers with [parse_use_file] and do not go through here. *)
let normalize code =
  let content = trim_end code in
  if String.is_empty content
  then content
  else if String.ends_with ~suffix:";;" content
  then content ^ "\n"
  else content ^ ";;\n"

let lexbuf s p ppf =
  let s = normalize s in
  fun buffer len ->
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in
    String.blit ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    (match ppf with
    | Some ppf ->
        Format.fprintf ppf "%s" (Bytes.sub_string buffer ~pos:0 ~len:len'');
        (* Only emit the synthetic newline once the whole final (newline-less)
           segment has been echoed: when the segment is larger than [len] it
           is fed in several chunks, and a newline after a partial chunk would
           split a line mid-source. *)
        if nl && len'' = len' then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''
