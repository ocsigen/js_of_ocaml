(* Js_of_ocaml
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

open Js_of_ocaml
open Typed_array
open! Bigarray

type ('a, 'b) ba = ('a, 'b, c_layout) Genarray.t

type ('a, 'b, 'c) ta = ('a, 'b, 'c) typedArray

module Setup = struct
  type (_, _, _) t =
    | Int8 : (int, int, Bigarray.int8_signed_elt) t
    | Uint8 : (int, int, Bigarray.int8_unsigned_elt) t
    | Int16 : (int, int, Bigarray.int16_signed_elt) t
    | Uint16 : (int, int, Bigarray.int16_unsigned_elt) t
    | Int32 : (Js.number Js.t, Int32.t, Bigarray.int32_elt) t
    | Float32 : (Js.number Js.t, float, Bigarray.float32_elt) t
    | Float64 : (Js.number Js.t, float, Bigarray.float64_elt) t
end

let kind_of_setup : type a b c. (a, b, c) Setup.t -> (b, c) kind = function
  | Setup.Int8 -> Int8_signed
  | Setup.Uint8 -> Int8_unsigned
  | Setup.Int16 -> Int16_signed
  | Setup.Uint16 -> Int16_unsigned
  | Setup.Int32 -> Int32
  | Setup.Float32 -> Float32
  | Setup.Float64 -> Float64

let convert : type a b c. (a, b, c) Setup.t -> a -> b = function
  | Setup.Int8 -> Fun.id
  | Setup.Uint8 -> Fun.id
  | Setup.Int16 -> Fun.id
  | Setup.Uint16 -> Fun.id
  | Setup.Int32 -> fun f -> Int32.of_float (Js.to_float f)
  | Setup.Float32 -> Js.to_float
  | Setup.Float64 -> Js.to_float

let type_of_setup : type a b c. (a, b, c) Setup.t -> (a, b, c) type' = function
  | Setup.Int8 -> Int8_signed
  | Setup.Uint8 -> Int8_unsigned
  | Setup.Int16 -> Int16_signed
  | Setup.Uint16 -> Int16_unsigned
  | Setup.Int32 -> Int32_signed
  | Setup.Float32 -> Float32
  | Setup.Float64 -> Float64

let ta_type_is_correct : type a b c. (a, b, c) Setup.t -> (a, b, c) ta Js.t -> bool =
 fun setup a ->
  let get_prop prop obj = Js.Unsafe.get obj (Js.string prop) in
  let name = a |> get_prop "constructor" |> get_prop "name" |> Js.to_string in
  match setup, name with
  | Setup.Float32, "Float32Array" -> true
  | Setup.Float64, "Float64Array" -> true
  | Setup.Int8, "Int8Array" -> true
  | Setup.Uint8, "Uint8Array" -> true
  | Setup.Int16, "Int16Array" -> true
  | Setup.Uint16, "Uint16Array" -> true
  | Setup.Int32, "Int32Array" -> true
  | _, _ -> false

let kind_field_is_correct : type a b c. (a, b, c) Setup.t -> (b, c) ba -> bool =
 fun setup a ->
  (* To trigger a `false`, modify the `kind` integer hard coded in the
   * `caml_ba_kind_of_typed_array` stub
   *)
  match kind_of_setup setup, Genarray.kind a with
  | Float32, Float32 -> true
  | Float64, Float64 -> true
  | Int8_signed, Int8_signed -> true
  | Int8_unsigned, Int8_unsigned -> true
  | Int16_signed, Int16_signed -> true
  | Int16_unsigned, Int16_unsigned -> true
  | Int32, Int32 -> true
  | _, _ -> false

let ba_of_array : type a b c. (a, b, c) Setup.t -> b array -> (b, c) ba =
 fun setup a -> Array1.of_array (kind_of_setup setup) c_layout a |> genarray_of_array1

let array_of_ba : type a b. (a, b) ba -> a array =
 fun a ->
  let a = Bigarray.array1_of_genarray a in
  let len = Bigarray.Array1.dim a in
  let rec aux i =
    if i == len then [] else Bigarray.Array1.unsafe_get a i :: aux (i + 1)
  in
  aux 0 |> Array.of_list

let array_of_ta : type a b c. (a, b, c) Setup.t -> (a, b, c) ta Js.t -> b array =
 fun setup a ->
  let len = a##.length in
  let rec aux i =
    if i == len then [] else convert setup (unsafe_get a i) :: aux (i + 1)
  in
  aux 0 |> Array.of_list

let test : type a b c. (a, b, c) Setup.t -> b array -> unit =
 fun setup a0 ->
  let a1 = ba_of_array setup a0 in

  let a2 = from_genarray (type_of_setup setup) a1 in
  if not (array_of_ta setup a2 = a0) then print_endline "`a2` doesnt match `a0`";
  if not (ta_type_is_correct setup a2) then print_endline "corrupted typedArray type";

  let a3 = to_genarray a2 in
  if not (array_of_ba a3 = a0) then print_endline "`a3` doesnt match `a0`";
  if not (kind_field_is_correct setup a3) then print_endline "corrupted `kind`";
  ()

let%expect_test "float32" =
  test Setup.Float32 [| Float.neg_infinity; -1.; 0.; 1.; Float.infinity |];
  [%expect {||}]

let%expect_test "float64" =
  test Setup.Float64 [| Float.neg_infinity; -1.; 0.; 1.; Float.infinity |];
  [%expect {||}]

let%expect_test "int8" =
  test Setup.Int8 [| -128; -1; 0; 1; 127 |];
  [%expect {||}]

let%expect_test "uint8" =
  test Setup.Uint8 [| 0; 255 |];
  [%expect {||}]

let%expect_test "int16" =
  test Setup.Int16 [| -32768; -1; 0; 1; 32767 |];
  [%expect {||}]

let%expect_test "uint16" =
  test Setup.Uint16 [| 0; 65535 |];
  [%expect {||}]

let%expect_test "int32" =
  test Setup.Int32 [| -2147483648l; -1l; 0l; 1l; 2147483647l |];
  [%expect {||}]
