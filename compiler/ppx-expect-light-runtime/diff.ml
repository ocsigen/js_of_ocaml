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

(* A minimal line-oriented diff for the failure report. Lines present only in
   the expected output are prefixed with ["-"], lines only in the actual output
   with ["+"], and common lines with [" "]. The alignment is a longest-common-
   subsequence so unchanged regions line up. *)

let lcs a b =
  let na = Array.length a and nb = Array.length b in
  let m = Array.make_matrix (na + 1) (nb + 1) 0 in
  for i = na - 1 downto 0 do
    for j = nb - 1 downto 0 do
      m.(i).(j) <-
        (if String.equal a.(i) b.(j)
         then m.(i + 1).(j + 1) + 1
         else max m.(i + 1).(j) m.(i).(j + 1))
    done
  done;
  let buf = Buffer.create 256 in
  let add prefix line = Buffer.add_string buf (prefix ^ line ^ "\n") in
  let rec loop i j =
    if i >= na && j >= nb
    then ()
    else if i >= na
    then (
      add "+" b.(j);
      loop i (j + 1))
    else if j >= nb
    then (
      add "-" a.(i);
      loop (i + 1) j)
    else if String.equal a.(i) b.(j)
    then (
      add " " a.(i);
      loop (i + 1) (j + 1))
    else if m.(i + 1).(j) >= m.(i).(j + 1)
    then (
      add "-" a.(i);
      loop (i + 1) j)
    else (
      add "+" b.(j);
      loop i (j + 1))
  in
  loop 0 0;
  Buffer.contents buf

let make ~description ~expected ~actual =
  let lines s = Array.of_list (String.split_on_char '\n' s) in
  Printf.sprintf "FAILED: %s\n%s" description (lcs (lines expected) (lines actual))
