(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Shachar Itzhaky
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

(* https://github.com/ocsigen/js_of_ocaml/pull/814 *)

let%expect_test _ =
  let module M = struct
    [@@@warning "-37"]

    type sign =
      | SPlus
      | SMinus

    type raw_numeral = int

    type prim_token =
      | Numeral of sign * raw_numeral
      | String of string

    type operator_token =
      | Add
      | Sub
      | Times
      | PlusPlus

    type expr =
      | Literal of prim_token
      | Op of operator_token * expr list

    let write_out chan v =
      let start = pos_out chan in
      Marshal.to_channel chan v [];
      pos_out chan - start

    let write_out_noshare chan v =
      let start = pos_out chan in
      Marshal.to_channel chan v [ Marshal.No_sharing ];
      pos_out chan - start
  end in
  let open M in
  let tmp_filename = Filename.temp_file "out" "txt" in
  let chan = open_out_bin tmp_filename in
  let v1 = Op (Add, [ Literal (Numeral (SPlus, 5)); Literal (Numeral (SMinus, 7)) ]) in
  let v2 =
    Op (Times, [ v1; v1 ])
    (* shared *)
  in
  let v1_sz = write_out chan v1 in
  let v2_sz = write_out chan v2 in
  let v2_ns_sz = write_out_noshare chan v2 in
  flush chan;
  Format.printf
    "sizes = %d %d %d (|v2| %s |v2_ns|)\n%!"
    v1_sz
    v2_sz
    v2_ns_sz
    (if v2_sz < v2_ns_sz then "<" else ">=");

  let chan = open_in_bin tmp_filename in
  let v1' = Marshal.from_channel chan in
  (let v2' = Marshal.from_channel chan in
   Format.printf "readback = %B %B\n%!" (v1 = v1') (v2 = v2'));
  [%expect {|
   sizes = 33 40 51 (|v2| < |v2_ns|)
   readback = true true |}]

(* https://github.com/ocsigen/js_of_ocaml/issues/359 *)

let%expect_test _ =
  let module M = struct
    type loop = { mutable pointer : loop option }

    let l = { pointer = None }

    let () =
      assert (l.pointer = None);
      l.pointer <- Some l

    let _ =
      let s = Marshal.to_string l [] in
      Format.printf "%d\n%S\n%!" (String.length s) s
  end in
  let open! M in
  [%expect
    {|
    24
    "\132\149\166\190\000\000\000\004\000\000\000\002\000\000\000\004\000\000\000\004\144\144\004\002" |}]

let%expect_test _ =
  Printf.printf "%S" (Marshal.to_string [| 0L; 1L |] []);
  [%expect
    {| "\132\149\166\190\000\000\000\025\000\000\000\003\000\000\000\011\000\000\000\t\160\025_j\000\000\000\000\000\000\000\000\000\025_j\000\000\000\000\000\000\000\000\001" |}];
  let data =
    "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001"
  in
  let v = Marshal.from_string data 0 in
  let () = assert (1L = v) in
  [%expect {||}];
  let data =
    "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\018_j\000\000\000\000\000\000\000\000\001"
  in
  (if String.split_on_char '.' Sys.ocaml_version |> List.hd |> int_of_string >= 5
   then ()
   else
     let v = Marshal.from_string data 0 in
     let () = assert (1L = v) in
     ());
  [%expect {||}]

let%expect_test _ =
  Printf.printf "%S" (Marshal.to_string 1L []);
  [%expect
    {| "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001" |}]

let%expect_test _ =
  let ba = Bigarray.Array1.create Int16_unsigned C_layout 3 in
  ba.{0} <- 3;
  ba.{1} <- 1;
  ba.{2} <- 2;
  Printf.printf "%S" (Marshal.to_string ba []);
  [%expect
    {| "\132\149\166\190\000\000\000'\000\000\000\001\000\000\000\007\000\000\000\007\024_bigarr02\000\000\000\000\020\000\000\000\000\000\000\000(\000\000\000\001\000\000\000\005\000\003\000\003\000\001\000\002" |}]

let%expect_test "test sharing of string" =
  let s = "AString" in
  let p = s, s in
  let obj = [ p; p ] in
  Printf.printf "%S" (Marshal.to_string s []);
  [%expect
    {| "\132\149\166\190\000\000\000\b\000\000\000\001\000\000\000\003\000\000\000\002'AString" |}];
  Printf.printf "%S" (Marshal.to_string obj []);
  [%expect
    {| "\132\149\166\190\000\000\000\016\000\000\000\004\000\000\000\012\000\000\000\011\160\160'AString\004\001\160\004\003@" |}]

let%expect_test "test float" =
  let s = 3.14 in
  let p = s, s in
  let obj = [ p; p ] in
  let r1 = Marshal.to_string s [ No_sharing ] in
  Printf.printf "%S\n" r1;
  Printf.printf "%f" (Marshal.from_string r1 0);
  [%expect
    {|
      "\132\149\166\190\000\000\000\t\000\000\000\000\000\000\000\003\000\000\000\002\012\031\133\235Q\184\030\t@"
      3.140000 |}];
  let r2 = Marshal.to_string obj [] in
  Printf.printf "%S\n" r2;
  let a, b, c, d =
    match Marshal.from_string r2 0 with
    | [ (a, b); (c, d) ] -> a, b, c, d
    | _ -> assert false
  in
  Printf.printf "%f %f %f %f" a b c d;
  [%expect
    {|
    "\132\149\166\190\000\000\000\017\000\000\000\004\000\000\000\012\000\000\000\011\160\160\012\031\133\235Q\184\030\t@\004\001\160\004\003@"
    3.140000 3.140000 3.140000 3.140000 |}]

let%expect_test "md5 digest via channel" =
  (* The MD5 buffer offset bug only triggers when MD5Update is called with
     in_buf != 0, i.e. when a previous update left a partial 64-byte block.
     Digest.string does a single update from in_buf=0, so it can't trigger it.
     To trigger it: write a file larger than the IO buffer (65536 bytes in wasm),
     read a non-64-aligned prefix so that the first caml_getblock returns a
     non-64-aligned amount. Then the second MD5Update call has in_buf != 0. *)
  let total = 100_000 in
  let skip = 37 in
  let data = String.init total (fun i -> Char.chr (i mod 256)) in
  let expected = Digest.to_hex (Digest.string (String.sub data skip (total - skip))) in
  let tmp = Filename.temp_file "md5" "test" in
  let oc = open_out_bin tmp in
  output_string oc data;
  close_out oc;
  let ic = open_in_bin tmp in
  ignore (really_input_string ic skip);
  let got = Digest.to_hex (Digest.channel ic (-1)) in
  close_in ic;
  Sys.remove tmp;
  Printf.printf "match: %b\n" (expected = got);
  [%expect {| match: true |}]

let%expect_test "test input with offset" =
  let s = 3.14 in
  let b = Bytes.create 1000 in
  let prefix = "not-marshal" in
  Bytes.blit_string prefix 0 b 0 (String.length prefix);
  let _ =
    Marshal.to_buffer
      b
      (String.length prefix)
      (Bytes.length b - String.length prefix)
      s
      []
  in
  Printf.printf "%f" (Marshal.from_bytes b (String.length prefix));
  [%expect {| 3.140000 |}]

let%expect_test "block with 2^21 fields" =
  (* Blocks with 2^21..2^22-1 fields have bit 31 of their BLOCK32
     header set; the reader must extract the size with an unsigned
     shift. *)
  let n = 1 lsl 21 in
  let a = Array.make n 0 in
  a.(0) <- 1;
  a.(n - 1) <- 2;
  (try
     let a' : int array = Marshal.from_string (Marshal.to_string a []) 0 in
     Printf.printf "%d %d %d\n" (Array.length a') a'.(0) a'.(Array.length a' - 1)
   with e -> print_endline (Printexc.to_string e));
  [%expect {| 2097152 1 2 |}]

let%expect_test "shared reference after a big-endian double array" =
  (* Hand-crafted stream (the jsoo writer only emits little-endian
     codes; native writers on big-endian platforms emit
     CODE_DOUBLE_ARRAY32_BIG): the value is (s, [| 1.5 |], s) with the
     second occurrence of s shared. The double array must be registered
     in the object table for the back-reference to resolve. *)
  let buf =
    "\x84\x95\xa6\xbe" (* magic *)
    ^ "\x00\x00\x00\x13" (* data length: 19 *)
    ^ "\x00\x00\x00\x03" (* number of shared objects *)
    ^ "\x00\x00\x00\x09" (* size_32 *)
    ^ "\x00\x00\x00\x08" (* size_64 *)
    ^ "\xb0" (* small block, tag 0, size 3 *)
    ^ "\x22hi" (* small string "hi" *)
    ^ "\x0f\x00\x00\x00\x01" (* DOUBLE_ARRAY32_BIG, 1 element *)
    ^ "\x3f\xf8\x00\x00\x00\x00\x00\x00" (* 1.5, big-endian *)
    ^ "\x04\x02" (* SHARED8, offset 2 *)
  in
  let (a, d, b) : string * float array * string = Marshal.from_string buf 0 in
  Printf.printf "%s %g %b\n" a d.(0) (a == b);
  [%expect {| hi 1.5 false |}]
