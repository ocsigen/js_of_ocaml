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
  Util.compile_and_run
    {|
    type sign = SPlus | SMinus
    type raw_numeral = int

    type prim_token =
      | Numeral of sign * raw_numeral
      | String of string

    type operator_token = Add | Sub | Times | PlusPlus

    type expr =
      | Literal of prim_token
      | Op of operator_token * expr list

    let write_out chan v =
        let start = pos_out chan in
        Marshal.to_channel chan v [];
        pos_out chan - start

    let write_out_noshare chan v =
        let start = pos_out chan in
        Marshal.to_channel chan v [Marshal.No_sharing];
        pos_out chan - start

    let _ =
      let tmp_filename = Filename.temp_file "out" "txt" in
      let chan = open_out tmp_filename in
      let v1 = Op (Add, [Literal (Numeral (SPlus, 5)); Literal (Numeral (SMinus, 7))]) in
      let v2 = Op (Times, [v1; v1]) (* shared *) in
      let v1_sz = write_out chan v1 in
      let v2_sz = write_out chan v2 in
      let v2_ns_sz = write_out_noshare chan v2 in
      flush chan;
      Format.printf "sizes = %d %d %d (|v2| %s |v2_ns|)\n%!" v1_sz v2_sz v2_ns_sz
        (if v2_sz < v2_ns_sz then "<" else ">=");

      let chan = open_in tmp_filename in
      let v1' = Marshal.from_channel chan in
      let v2' = Marshal.from_channel chan in
      Format.printf "readback = %B %B\n%!" (v1 = v1') (v2 = v2')
|};
  [%expect {|
   sizes = 33 40 51 (|v2| < |v2_ns|)
   readback = true true |}]

(* https://github.com/ocsigen/js_of_ocaml/issues/359 *)

let%expect_test _ =
  let module M = struct
    type loop = { mutable pointer : loop option }

    let l = { pointer = None }

    let () = l.pointer <- Some l

    let _ =
      let s = Marshal.to_string l [] in
      Format.printf "%d\n%S\n%!" (String.length s) s
  end in
  [%expect
    {|
    24
    "\132\149\166\190\000\000\000\004\000\000\000\002\000\000\000\004\000\000\000\004\144\144\004\002" |}]

let%expect_test _ =
  Util.compile_and_run
    {|
    type loop = { mutable pointer : loop option }
    let l = { pointer = None }
    let () = l.pointer <- Some l

    let _ =
      let s = Marshal.to_string l [] in
      Format.printf "%d\n%S\n%!" (String.length s) s
|};
  [%expect
    {|
    24
    "\132\149\166\190\000\000\000\004\000\000\000\002\000\000\000\004\000\000\000\004\144\144\004\002" |}]

let%expect_test _ =
  Util.compile_and_run {|Printf.printf "%S" (Marshal.to_string [| 0L; 1L |] [])|};
  [%expect
    {| "\132\149\166\190\000\000\000\025\000\000\000\003\000\000\000\011\000\000\000\t\160\025_j\000\000\000\000\000\000\000\000\000\025_j\000\000\000\000\000\000\000\000\001" |}];
  Util.compile_and_run
    {|
     let data = "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001"
     let v = Marshal.from_string data 0
     let () =  assert (1L = v) |};
  [%expect {||}];
  Util.compile_and_run
    {|
     let data = "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\018_j\000\000\000\000\000\000\000\000\001"
     let v = Marshal.from_string data 0
     let () = assert (1L = v) |};
  [%expect {||}]

let%expect_test _ =
  Util.compile_and_run {|Printf.printf "%S" (Marshal.to_string 1L [])|};
  [%expect
    {| "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001" |}];
  Printf.printf "%S" (Marshal.to_string 1L []);
  [%expect
    {| "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001" |}]

let%expect_test _ =
  Util.compile_and_run {|Printf.printf "%S" (Marshal.to_string 1L [])|};
  [%expect
    {| "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001" |}];
  Printf.printf "%S" (Marshal.to_string 1L []);
  [%expect
    {| "\132\149\166\190\000\000\000\012\000\000\000\001\000\000\000\004\000\000\000\003\025_j\000\000\000\000\000\000\000\000\001" |}]

let%expect_test _ =
  Util.compile_and_run
    {|
let ba = Bigarray.Array1.create Int16_unsigned C_layout 3 in
ba.{0} <- 3;
ba.{1} <- 1;
ba.{2} <- 2;
Printf.printf "%S" (Marshal.to_string ba [])
|};
  [%expect
    {| "\132\149\166\190\000\000\000)\000\000\000\001\000\000\000\007\000\000\000\007\024_bigarray\000\000\000\000\020\000\000\000\000\000\000\000(\000\000\000\001\000\000\000\005\000\000\000\003\000\003\000\001\000\002" |}];
  let ba = Bigarray.Array1.create Int16_unsigned C_layout 3 in
  ba.{0} <- 3;
  ba.{1} <- 1;
  ba.{2} <- 2;
  Printf.printf "%S" (Marshal.to_string ba []);
  [%expect
    {| "\132\149\166\190\000\000\000)\000\000\000\001\000\000\000\007\000\000\000\007\024_bigarray\000\000\000\000\020\000\000\000\000\000\000\000(\000\000\000\001\000\000\000\005\000\000\000\003\000\003\000\001\000\002" |}]
