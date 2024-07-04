(* Wasm_of_ocaml compiler
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
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let stdlib_close_out = close_out

open Stdlib

module type CRC = sig
  type t

  val start : t

  val update_from_bytes : bytes -> int -> int -> t -> t

  val update_from_string : string -> int -> int -> t -> t

  val finish : t -> int32
end

module CRC32 : CRC = struct
  let compute_table () =
    let open Int32 in
    let tbl = Array.make 256 zero in
    let poly = 0xedb88320l in
    for i = 0 to 255 do
      let n = ref (of_int i) in
      for _ = 0 to 7 do
        if logand !n one = one
        then n := logxor (shift_right_logical !n 1) poly
        else n := shift_right_logical !n 1
      done;
      tbl.(i) <- !n
    done;
    tbl

  module CRC32 : CRC with type t = int32 = struct
    type t = int32

    let table = lazy (compute_table ())

    let start = 0xffffffffl

    let update_from_bytes s pos len crc =
      assert (pos >= 0 && len >= 0 && pos <= Bytes.length s - len);
      let open Int32 in
      let tbl = Lazy.force table in
      let crc = ref crc in
      for i = pos to pos + len - 1 do
        crc :=
          logxor
            (shift_right_logical !crc 8)
            (Array.unsafe_get
               tbl
               (to_int !crc land 0xff lxor Char.code (Bytes.unsafe_get s i)))
      done;
      !crc

    let update_from_string s pos len crc =
      assert (pos >= 0 && len >= 0 && pos <= String.length s - len);
      let open Int32 in
      let tbl = Lazy.force table in
      let crc = ref crc in
      for i = pos to pos + len - 1 do
        crc :=
          logxor
            (shift_right_logical !crc 8)
            (Array.unsafe_get tbl (to_int !crc land 0xff lxor Char.code s.[i]))
      done;
      !crc

    let finish crc = Int32.(logxor crc start)
  end

  module CRC64 : CRC with type t = int = struct
    type t = int

    let start = (1 lsl 32) - 1

    let next_table tbl tbl' =
      lazy
        (let tbl = Lazy.force tbl in
         let tbl' = Lazy.force tbl' in
         Array.init 256 ~f:(fun i -> (tbl'.(i) lsr 8) lxor tbl.(tbl'.(i) land 0xFF)))

    let table1 =
      lazy (Array.map ~f:(fun i -> Int32.to_int i land start) (compute_table ()))

    let table2 = next_table table1 table1

    let table3 = next_table table1 table2

    let table4 = next_table table1 table3

    let table5 = next_table table1 table4

    let table6 = next_table table1 table5

    let table7 = next_table table1 table6

    let table8 = next_table table1 table7

    let update_from_bytes s pos len crc =
      assert (pos >= 0 && len >= 0 && pos <= Bytes.length s - len);
      let tbl1 = Lazy.force table1 in
      let tbl2 = Lazy.force table2 in
      let tbl3 = Lazy.force table3 in
      let tbl4 = Lazy.force table4 in
      let tbl5 = Lazy.force table5 in
      let tbl6 = Lazy.force table6 in
      let tbl7 = Lazy.force table7 in
      let tbl8 = Lazy.force table8 in
      let crc = ref crc in
      for i = 0 to (len / 8) - 1 do
        let pos = pos + (i lsl 3) in
        crc :=
          let crc = !crc in
          Array.unsafe_get tbl8 (crc lxor Char.code (Bytes.unsafe_get s pos) land 0xff)
          lxor Array.unsafe_get
                 tbl7
                 ((crc lsr 8) lxor Char.code (Bytes.unsafe_get s (pos + 1)) land 0xff)
          lxor (Array.unsafe_get
                  tbl6
                  ((crc lsr 16) lxor Char.code (Bytes.unsafe_get s (pos + 2)) land 0xff)
               lxor Array.unsafe_get
                      tbl5
                      ((crc lsr 24) lxor Char.code (Bytes.unsafe_get s (pos + 3))))
          lxor (Array.unsafe_get tbl4 (Char.code (Bytes.unsafe_get s (pos + 4)))
               lxor Array.unsafe_get tbl3 (Char.code (Bytes.unsafe_get s (pos + 5)))
               lxor Array.unsafe_get tbl2 (Char.code (Bytes.unsafe_get s (pos + 6)))
               lxor Array.unsafe_get tbl1 (Char.code (Bytes.unsafe_get s (pos + 7))))
      done;
      for i = pos + (len land -8) to pos + len - 1 do
        crc :=
          (!crc lsr 8)
          lxor Array.unsafe_get tbl1 (!crc land 0xff lxor Char.code (Bytes.unsafe_get s i))
      done;
      !crc

    let update_from_string s pos len crc =
      assert (pos >= 0 && len >= 0 && pos <= String.length s - len);
      let tbl = Lazy.force table1 in
      let crc = ref crc in
      for i = pos to pos + len - 1 do
        crc := (!crc lsr 8) lxor Array.unsafe_get tbl (!crc land 0xff lxor Char.code s.[i])
      done;
      !crc

    let finish crc = Int32.of_int (crc lxor start)
  end

  module Repr = Sys.Immediate64.Make (Int) (Int32)

  include
    (val match Repr.repr with
         | Immediate -> (module CRC64 : CRC)
         | Non_immediate -> (module CRC32 : CRC)
        : CRC)
end

let buffer = lazy (Bytes.create 65536)

let copy in_ch out_ch ?(iter = fun _ _ _ -> ()) len =
  let buffer = Lazy.force buffer in
  let buffer_len = Bytes.length buffer in
  let rec copy rem =
    if rem > 0
    then (
      let n = input in_ch buffer 0 (min buffer_len rem) in
      if n = 0 then raise End_of_file;
      iter buffer 0 n;
      output out_ch buffer 0 n;
      copy (rem - n))
  in
  copy len

type file =
  { name : string
  ; pos : int
  ; len : int
  ; mutable crc : int32
  }

type output =
  { ch : out_channel
  ; mutable files : file list
  }

let open_out name = { ch = open_out_bin name; files = [] }

let output_16 ch c =
  output_byte ch c;
  output_byte ch (c lsr 8)

let output_32 ch c =
  output_16 ch c;
  output_16 ch (c lsr 16)

let output_crc ch crc =
  output_16 ch (Int32.to_int crc);
  output_16 ch (Int32.to_int (Int32.shift_right_logical crc 16))

let output_local_file_header ch ?(crc = 0l) { name; len; _ } =
  output_32 ch 0x04034b50;
  (* version needed to extract *)
  output_16 ch 10;
  (* general purpose but flag *)
  output_16 ch 0x0;
  (* compression method *)
  output_16 ch 0x0;
  (* time / date *)
  output_16 ch 0x0;
  output_16 ch 0x5821;
  (* CRC *)
  let crc_pos = pos_out ch in
  output_crc ch crc;
  (* compressed / uncompressed size *)
  output_32 ch len;
  output_32 ch len;
  (* file name length *)
  output_16 ch (String.length name);
  (* extra field length *)
  output_16 ch 0;
  (* file name *)
  output_string ch name;
  crc_pos

let add_file z ~name ~file =
  let ch = open_in_bin file in
  let pos = pos_out z.ch in
  let len = in_channel_length ch in
  let file = { name; pos; len; crc = 0l } in
  z.files <- file :: z.files;
  let crc_pos = output_local_file_header z.ch file in
  let crc = ref CRC32.start in
  copy ch z.ch ~iter:(fun b pos len -> crc := CRC32.update_from_bytes b pos len !crc) len;
  let crc = CRC32.finish !crc in
  file.crc <- crc;
  let pos = pos_out z.ch in
  seek_out z.ch crc_pos;
  output_crc z.ch crc;
  seek_out z.ch pos

let add_entry z ~name ~contents =
  let pos = pos_out z.ch in
  let len = String.length contents in
  let crc = CRC32.start |> CRC32.update_from_string contents 0 len |> CRC32.finish in
  let file = { name; pos; len; crc } in
  z.files <- file :: z.files;
  let _crc_pos = output_local_file_header z.ch ~crc file in
  output_string z.ch contents

let output_file_header ch { name; pos; len; crc } =
  output_32 ch 0x02014b50;
  (* versions: made by / needed to extract *)
  output_16 ch 10;
  output_16 ch 10;
  (* general purpose but flag *)
  output_16 ch 0x0;
  (* compression method *)
  output_16 ch 0x0;
  (* time / date *)
  output_16 ch 0x0;
  output_16 ch 0x5821;
  (* CRC *)
  output_crc ch crc;
  (* compressed / uncompressed size *)
  output_32 ch len;
  output_32 ch len;
  (* file name length *)
  output_16 ch (String.length name);
  (* extra field length *)
  output_16 ch 0;
  (* file comment length *)
  output_16 ch 0;
  (* disk number start *)
  output_16 ch 0;
  (* file attributes *)
  output_16 ch 0;
  output_32 ch 0;
  (* relative offset of local header *)
  output_32 ch pos;
  (* file name *)
  output_string ch name

let output_end_of_directory z pos len =
  let ch = z.ch in
  output_32 ch 0x06054b50;
  (* disk numbers *)
  output_16 ch 0;
  output_16 ch 0;
  (* number of entries *)
  let n = List.length z.files in
  output_16 ch n;
  output_16 ch n;
  (* size of the central directory *)
  output_32 ch len;
  (* offset of the central directory *)
  output_32 ch pos;
  (* comment length *)
  output_16 ch 0

let output_directory z =
  let pos = pos_out z.ch in
  List.iter ~f:(output_file_header z.ch) (List.rev z.files);
  let pos' = pos_out z.ch in
  output_end_of_directory z pos (pos' - pos)

let close_out z =
  output_directory z;
  close_out z.ch

(****)

type entry =
  { pos : int
  ; len : int
  ; crc : int32
  }

let input_16 ch =
  let c = input_byte ch in
  c lor (input_byte ch lsl 8)

let input_32 ch =
  let c = input_16 ch in
  c lor (input_16 ch lsl 16)

let input_32' ch =
  let c = input_16 ch in
  Int32.(logor (of_int c) (shift_left (of_int (input_16 ch)) 16))

let read_local_file_header ch pos =
  let pos = pos + 14 in
  seek_in ch pos;
  let crc = input_32' ch in
  let _ = input_32 ch in
  let len = input_32 ch in
  let name_len = input_16 ch in
  let extra_len = input_16 ch in
  { pos = pos + 16 + name_len + extra_len; len; crc }

let read_file_header ch =
  let signature = input_32' ch in
  if not (Int32.equal signature 0x02014b50l) then failwith "bad signature";
  (* versions: made by / needed to extract *)
  ignore (input_16 ch);
  let v = input_16 ch in
  if v > 10 then failwith "unsupported file format";
  (* general purpose but flag *)
  ignore (input_16 ch);
  (* compression method *)
  ignore (input_16 ch);
  (* time / date *)
  ignore (input_32 ch);
  (* CRC *)
  ignore (input_32' ch);
  (* compressed / uncompressed size *)
  ignore (input_32 ch);
  ignore (input_32 ch);
  (* file name length *)
  let name_len = input_16 ch in
  (* extra field length *)
  let extra_len = input_16 ch in
  (* file comment length *)
  let comment_len = input_16 ch in
  (* disk number start *)
  ignore (input_16 ch);
  (* file attributes *)
  ignore (input_16 ch);
  ignore (input_32 ch);
  (* relative offset of local header *)
  let pos = input_32 ch in
  (* file name *)
  let name = really_input_string ch name_len in
  ignore (really_input_string ch extra_len);
  ignore (really_input_string ch comment_len);
  name, pos

type input =
  { ch : in_channel
  ; files : int StringMap.t
  }

let open_in name =
  let ch = open_in_bin name in
  let len = in_channel_length ch in
  let find_directory_end offset =
    seek_in ch (len - 22 - offset);
    let c = ref 0l in
    let p = ref (-1) in
    for i = 0 to offset + 3 do
      (c := Int32.(add (shift_left !c 8) (of_int (input_byte ch))));
      if Int32.equal !c 0x504b0506l then p := 22 + 3 + offset - i
    done;
    !p
  in
  let p = find_directory_end 0 in
  let p = if p = -1 then find_directory_end 65535 else p in
  if p = -1 then failwith "not a ZIP file";
  seek_in ch (len - p + 10);
  (* number of entries *)
  let n = input_16 ch in
  (* size of the directory *)
  ignore (input_32 ch);
  (* offset of the directory *)
  let offset = input_32 ch in
  seek_in ch offset;
  let m = ref StringMap.empty in
  for _ = 0 to n - 1 do
    let name, entry = read_file_header ch in
    m := StringMap.add name entry !m
  done;
  { ch; files = !m }

let with_open_in name f =
  let z = open_in name in
  Fun.protect ~finally:(fun () -> close_in_noerr z.ch) (fun () -> f z)

let get_pos z ~name =
  try StringMap.find name z.files
  with Not_found -> failwith (Printf.sprintf "File %s not found in archive" name)

let has_entry z ~name = StringMap.mem name z.files

let read_entry z ~name =
  let pos = get_pos z ~name in
  let { pos; len; _ } = read_local_file_header z.ch pos in
  seek_in z.ch pos;
  really_input_string z.ch len

let get_entry z ~name =
  let pos = get_pos z ~name in
  let { pos; len; crc } = read_local_file_header z.ch pos in
  z.ch, pos, len, crc

let extract_file z ~name ~file =
  let pos = get_pos z ~name in
  let { pos; len; _ } = read_local_file_header z.ch pos in
  seek_in z.ch pos;
  let ch = open_out_bin file in
  copy z.ch ch len;
  stdlib_close_out ch

let close_in z = close_in z.ch

let copy_file z (z' : output) ~src_name ~dst_name =
  let pos = StringMap.find src_name z.files in
  let { pos; len; crc } = read_local_file_header z.ch pos in
  seek_in z.ch pos;
  let pos' = pos_out z'.ch in
  let file = { name = dst_name; pos = pos'; len; crc } in
  z'.files <- file :: z'.files;
  let _ = output_local_file_header z'.ch ~crc file in
  copy z.ch z'.ch len
