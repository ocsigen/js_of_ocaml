(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

(* Canonicalisation of expect output used for comparison and rendering. The
   rule: drop trailing whitespace on each line, drop leading/trailing blank
   lines, and remove the common leading indentation. This makes the comparison
   insensitive to how the [%expect] snapshot is indented in the source, so
   snapshots written by the upstream ppx_expect keep comparing equal. *)

let rstrip s =
  let n = ref (String.length s) in
  while
    !n > 0
    &&
    match s.[!n - 1] with
    | ' ' | '\t' | '\r' -> true
    | _ -> false
  do
    decr n
  done;
  String.sub s 0 !n

let leading_spaces s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && Char.equal s.[!i] ' ' do
    incr i
  done;
  !i

let drop_leading_blanks lines =
  let rec loop = function
    | "" :: tl -> loop tl
    | l -> l
  in
  loop lines

(* Returns the canonical list of lines (no trailing newline). *)
let canonical_lines s =
  let lines = String.split_on_char '\n' s in
  let lines = List.map rstrip lines in
  let lines = drop_leading_blanks lines in
  let lines = List.rev (drop_leading_blanks (List.rev lines)) in
  match lines with
  | [] -> []
  | _ ->
      let indent =
        List.fold_left
          (fun acc l -> if String.length l = 0 then acc else min acc (leading_spaces l))
          max_int
          lines
      in
      let indent = if indent = max_int then 0 else indent in
      List.map
        (fun l ->
          if String.length l <= indent
          then ""
          else String.sub l indent (String.length l - indent))
        lines

let normalize s = String.concat "\n" (canonical_lines s)
