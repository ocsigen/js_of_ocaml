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
