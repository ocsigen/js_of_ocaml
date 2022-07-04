(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

let () =
  let files = ref [] in
  let offset = ref None in
  let len = ref None in
  Arg.parse
    [ "-offset", Int (fun i -> offset := Some i), "<OFFSET>"
    ; "-length", Int (fun i -> len := Some i), "<LENGTH>"
    ]
    (fun f -> files := f :: !files)
    "";
  let files =
    match List.rev !files with
    | [] -> [ "-" ]
    | l -> l
  in
  List.iter
    (fun file ->
      try
        let c =
          match file with
          | "-" -> stdin
          | fname -> open_in_bin fname
        in
        (match !offset, file with
        | None, _ -> ()
        | Some o, "-" ->
            let buf = Bytes.create (1024 * 1024) in
            let rec skip n =
              let read = input c buf 0 (min (Bytes.length buf) n) in
              if read = 0
              then raise End_of_file
              else
                let n = n - read in
                if n = 0 then () else skip n
            in
            skip o
        | Some o, _ -> seek_in c o);
        let size =
          match !len with
          | None -> -1
          | Some l -> l
        in
        let digest = Digest.channel c size in
        Printf.printf "%s\t%s\n" (Digest.to_hex digest) file
      with e -> Printf.printf "Error: %s: %s\n" file (Printexc.to_string e))
    files
