(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

open Stdlib

module Wasm_binary = struct
  let header = "\000asm\001\000\000\000"

  let check_header file ch =
    let s = really_input_string ch 8 in
    if not (String.equal s header)
    then failwith (file ^ " is not a Wasm binary file (bad magic)")

  type t =
    { ch : in_channel
    ; limit : int
    }

  let open_in f =
    let ch = open_in_bin f in
    check_header f ch;
    { ch; limit = in_channel_length ch }

  let rec read_uint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 16);
    if i < 128 then i else i - 128 + (read_uint ~n:(n - 1) ch lsl 7)

  let rec read_sint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 8 || (i > 120 && i < 128));
    if i < 64
    then i
    else if i < 128
    then i - 128
    else i - 128 + (read_sint ~n:(n - 1) ch lsl 7)

  type section =
    { id : int
    ; size : int
    }

  let next_section ch =
    if pos_in ch.ch = ch.limit
    then None
    else
      let id = input_byte ch.ch in
      let size = read_uint ch.ch in
      Some { id; size }

  let skip_section ch { size; _ } = seek_in ch.ch (pos_in ch.ch + size)

  let vec f ch =
    let rec loop acc n = if n = 0 then List.rev acc else loop (f ch :: acc) (n - 1) in
    loop [] (read_uint ch)

  let name ch =
    let n = read_uint ch in
    really_input_string ch n

  let heaptype ch = ignore (read_sint ch)

  let reftype' i ch =
    match i with
    | 0x6a | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 -> ()
    | 0x63 | 0x64 -> heaptype ch
    | _ ->
        Format.eprintf "Unknown reftype %x@." i;
        assert false

  let reftype ch = reftype' (input_byte ch) ch

  let valtype ch =
    let i = read_uint ch in
    match i with
    | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f -> ()
    | _ -> reftype' i ch

  let limits ch =
    match input_byte ch with
    | 0 -> ignore (read_uint ch)
    | 1 ->
        ignore (read_uint ch);
        ignore (read_uint ch)
    | _ -> assert false

  let memtype = limits

  let tabletype ch =
    reftype ch;
    limits ch

  type import =
    { module_ : string
    ; name : string
    }

  let import ch =
    let module_ = name ch in
    let name = name ch in
    let d = read_uint ch in
    let _ =
      match d with
      | 0 -> ignore (read_uint ch)
      | 1 -> tabletype ch
      | 2 -> memtype ch
      | 3 ->
          let _typ = valtype ch in
          let _mut = input_byte ch in
          ()
      | 4 ->
          assert (read_uint ch = 0);
          ignore (read_uint ch)
      | _ ->
          Format.eprintf "Unknown import %x@." d;
          assert false
    in
    { module_; name }

  let read_imports ~file =
    let ch = open_in file in
    let rec find_section () =
      match next_section ch with
      | None -> false
      | Some s ->
          s.id = 2
          ||
          (skip_section ch s;
           find_section ())
    in
    let res = if find_section () then vec import ch.ch else [] in
    close_in ch.ch;
    res
end
