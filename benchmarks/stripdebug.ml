(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Copy a bytecode executable, removing debugging information
   and #! header from the copy.
   Usage: stripdebug <source file> <dest file>
*)

open Printf
open Misc

module Bytesections : sig
  module Name : sig
    type raw_name = private string

    type t =
      | CODE  (** bytecode *)
      | CRCS  (** crcs for modules *)
      | DATA  (** global data (constant) *)
      | DBUG  (** debug info *)
      | DLLS  (** dll names *)
      | DLPT  (** dll paths *)
      | PRIM  (** primitives names *)
      | RNTM  (** The path to the bytecode interpreter (use_runtime mode) *)
      | SYMB  (** global identifiers *)
      | Other of raw_name
  end

  (** Recording sections written to a bytecode executable file *)

  type toc_writer

  val init_record : out_channel -> toc_writer
  (** Start recording sections from the current position in out_channel *)

  val record : toc_writer -> Name.t -> unit
  (** Record the current position in the out_channel as the end of the section with the
      given name. *)

  val write_toc_and_trailer : toc_writer -> unit
  (** Write the table of contents and the standard trailer for bytecode executable files
  *)

  (** Reading sections from a bytecode executable file *)

  type section_entry =
    { name : Name.t  (** name of the section. *)
    ; pos : int  (** byte offset at which the section starts. *)
    ; len : int  (** length of the section. *)
    }

  type section_table

  exception Bad_magic_number

  val read_toc : in_channel -> section_table
  (** Read the table of sections from a bytecode executable. Raise [Bad_magic_number] if
      magic number doesn't match *)

  val all : section_table -> section_entry list
  (** Returns all [section_entry] from a [section_table] in increasing position order. *)

  val pos_first_section : section_table -> int
  (** Return the position of the beginning of the first section *)
end = struct
  module Name = struct
    type raw_name = string

    type t =
      | CODE  (** bytecode *)
      | CRCS  (** crcs for modules *)
      | DATA  (** global data (constant) *)
      | DBUG  (** debug info *)
      | DLLS  (** dll names *)
      | DLPT  (** dll paths *)
      | PRIM  (** primitives names *)
      | RNTM  (** The path to the bytecode interpreter (use_runtime mode) *)
      | SYMB  (** global identifiers *)
      | Other of raw_name

    let of_string name =
      match name with
      | "CODE" -> CODE
      | "DLPT" -> DLPT
      | "DLLS" -> DLLS
      | "DATA" -> DATA
      | "PRIM" -> PRIM
      | "SYMB" -> SYMB
      | "DBUG" -> DBUG
      | "CRCS" -> CRCS
      | "RNTM" -> RNTM
      | name ->
          if String.length name <> 4
          then invalid_arg "Bytesections.Name.of_string: must be of size 4";
          Other name

    let to_string = function
      | CODE -> "CODE"
      | DLPT -> "DLPT"
      | DLLS -> "DLLS"
      | DATA -> "DATA"
      | PRIM -> "PRIM"
      | SYMB -> "SYMB"
      | DBUG -> "DBUG"
      | CRCS -> "CRCS"
      | RNTM -> "RNTM"
      | Other n -> n
  end

  type section_entry =
    { name : Name.t
    ; pos : int
    ; len : int
    }

  type section_table =
    { sections : section_entry list
    ; first_pos : int
    }

  (* Recording sections *)
  type toc_writer =
    { (* List of all sections, in reverse order *)
      mutable section_table_rev : section_entry list
    ; mutable section_prev : int
    ; outchan : out_channel
    }

  let init_record outchan : toc_writer =
    let pos = pos_out outchan in
    { section_prev = pos; section_table_rev = []; outchan }

  let record t name =
    let pos = pos_out t.outchan in
    if pos < t.section_prev
    then invalid_arg "Bytesections.record: out_channel offset moved backward";
    let entry = { name; pos = t.section_prev; len = pos - t.section_prev } in
    t.section_table_rev <- entry :: t.section_table_rev;
    t.section_prev <- pos

  let write_toc_and_trailer t =
    let section_table = List.rev t.section_table_rev in
    List.iter
      (fun { name; pos = _; len } ->
        let name = Name.to_string name in
        assert (String.length name = 4);
        output_string t.outchan name;
        output_binary_int t.outchan len)
      section_table;
    output_binary_int t.outchan (List.length section_table);
    output_string t.outchan Config.exec_magic_number

  (* Read the table of sections from a bytecode executable *)

  exception Bad_magic_number

  let read_toc ic =
    let pos_trailer = in_channel_length ic - 16 in
    seek_in ic pos_trailer;
    let num_sections = input_binary_int ic in
    let header = really_input_string ic (String.length Config.exec_magic_number) in
    if header <> Config.exec_magic_number then raise Bad_magic_number;
    let toc_pos = pos_trailer - (8 * num_sections) in
    seek_in ic toc_pos;
    let section_table_rev = ref [] in
    for _i = 1 to num_sections do
      let name = Name.of_string (really_input_string ic 4) in
      let len = input_binary_int ic in
      section_table_rev := (name, len) :: !section_table_rev
    done;
    let first_pos, sections =
      List.fold_left
        (fun (pos, l) (name, len) ->
          let section = { name; pos = pos - len; len } in
          pos - len, section :: l)
        (toc_pos, [])
        !section_table_rev
    in
    { sections; first_pos }

  let all t = t.sections

  let pos_first_section t = t.first_pos
end

let stripdebug infile outfile =
  let ic = open_in_bin infile in
  let toc = Bytesections.read_toc ic in
  let pos_first_section = Bytesections.pos_first_section toc in
  let oc =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o777 outfile
  in
  (* Skip the #! header, going straight to the first section. *)
  seek_in ic pos_first_section;
  (* Copy each section except DBUG *)
  let writer = Bytesections.init_record oc in
  List.iter
    (fun { Bytesections.name; pos; len } ->
      match name with
      | Bytesections.Name.DBUG -> ()
      | name ->
          seek_in ic pos;
          copy_file_chunk ic oc len;
          Bytesections.record writer name)
    (Bytesections.all toc);
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer writer;
  (* Done *)
  close_in ic;
  close_out oc

let _ =
  if Array.length Sys.argv = 3
  then stripdebug Sys.argv.(1) Sys.argv.(2)
  else (
    eprintf "Usage: stripdebug <source file> <destination file>\n";
    exit 2)
