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

(* With OCaml 5.6 bytecode hints, the JavaScript backend accesses bigarrays
   whose kind and layout are statically known with specialized primitives,
   and compares int64 values with dedicated primitives. These tests exercise
   these accesses and comparisons (with older OCaml versions, they go through
   the generic primitives). *)

open Bigarray

let pr = Printf.printf

let catch f =
  match f () with
  | v -> v
  | exception Invalid_argument s -> "<" ^ s ^ ">"

(* The accessors must be used at known kinds and layouts for OCaml to emit
   bigarray hints: they are passed in records built at these types. *)
type ('a, 'b, 'l) ops1 =
  { get : ('a, 'b, 'l) Array1.t -> int -> 'a
  ; unsafe_get : ('a, 'b, 'l) Array1.t -> int -> 'a
  ; set : ('a, 'b, 'l) Array1.t -> int -> 'a -> unit
  ; unsafe_set : ('a, 'b, 'l) Array1.t -> int -> 'a -> unit
  }

let test1
    (type a b)
    name
    (k : (a, b) kind)
    (ops_c : (a, b, c_layout) ops1)
    (ops_f : (a, b, fortran_layout) ops1)
    (conv : a -> string)
    (vals : a array) =
  let go (type l) (l : l layout) (ops : (a, b, l) ops1) lo =
    let a = Array1.create k l 5 in
    Array.iteri (fun i v -> ops.set a (lo + (i mod 5)) v) vals;
    List.iter
      (fun i -> pr "%s " (catch (fun () -> conv (ops.get a i))))
      [ min_int; lo - 1; lo; lo + 1; lo + 2; lo + 3; lo + 4; lo + 5; max_int ];
    for i = lo to lo + 4 do
      pr "%s " (conv (ops.unsafe_get a i))
    done;
    pr
      "%s "
      (catch (fun () ->
           ops.set a (lo + 5) vals.(0);
           "ok"));
    pr
      "%s "
      (catch (fun () ->
           ops.set a (lo - 1) vals.(0);
           "ok"));
    ops.unsafe_set a lo vals.(1);
    pr "%s\n" (conv (ops.get a lo))
  in
  pr "%s C: " name;
  go c_layout ops_c 0;
  pr "%s F: " name;
  go fortran_layout ops_f 1

type ('a, 'b, 'l) ops23 =
  { get2 : ('a, 'b, 'l) Array2.t -> int -> int -> 'a
  ; unsafe_get2 : ('a, 'b, 'l) Array2.t -> int -> int -> 'a
  ; set2 : ('a, 'b, 'l) Array2.t -> int -> int -> 'a -> unit
  ; unsafe_set2 : ('a, 'b, 'l) Array2.t -> int -> int -> 'a -> unit
  ; get3 : ('a, 'b, 'l) Array3.t -> int -> int -> int -> 'a
  ; unsafe_get3 : ('a, 'b, 'l) Array3.t -> int -> int -> int -> 'a
  ; set3 : ('a, 'b, 'l) Array3.t -> int -> int -> int -> 'a -> unit
  ; unsafe_set3 : ('a, 'b, 'l) Array3.t -> int -> int -> int -> 'a -> unit
  }

let test23
    (type a b)
    name
    (k : (a, b) kind)
    (ops_c : (a, b, c_layout) ops23)
    (ops_f : (a, b, fortran_layout) ops23)
    (conv : a -> string)
    (v : int -> a) =
  let go (type l) (l : l layout) (ops : (a, b, l) ops23) lo =
    let a = Array2.create k l 3 4 in
    for i = 0 to 2 do
      for j = 0 to 3 do
        ops.set2 a (i + lo) (j + lo) (v ((i * 4) + j))
      done
    done;
    for i = lo - 1 to lo + 3 do
      for j = lo - 1 to lo + 4 do
        pr "%s " (catch (fun () -> conv (ops.get2 a i j)))
      done
    done;
    pr "%s " (conv (ops.unsafe_get2 a (lo + 2) (lo + 1)));
    ops.unsafe_set2 a lo (lo + 3) (v 99);
    pr "%s\n" (conv (ops.get2 a lo (lo + 3)));
    let b = Array3.create k l 2 3 4 in
    for i = 0 to 1 do
      for j = 0 to 2 do
        for m = 0 to 3 do
          ops.set3 b (i + lo) (j + lo) (m + lo) (v ((i * 12) + (j * 4) + m))
        done
      done
    done;
    for i = lo - 1 to lo + 2 do
      for j = lo - 1 to lo + 3 do
        for m = lo - 1 to lo + 4 do
          pr "%s " (catch (fun () -> conv (ops.get3 b i j m)))
        done
      done
    done;
    pr "%s " (conv (ops.unsafe_get3 b (lo + 1) (lo + 2) (lo + 3)));
    ops.unsafe_set3 b lo lo lo (v 77);
    pr "%s\n" (conv (ops.get3 b lo lo lo))
  in
  pr "%s C: " name;
  go c_layout ops_c 0;
  pr "%s F: " name;
  go fortran_layout ops_f 1

let f64_c : (float, float64_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let f64_fortran : (float, float64_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let f32_c : (float, float32_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let f32_fortran : (float, float32_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i8_c : (int, int8_signed_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i8_fortran : (int, int8_signed_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let u8_c : (int, int8_unsigned_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let u8_fortran : (int, int8_unsigned_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i16_c : (int, int16_signed_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i16_fortran : (int, int16_signed_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let u16_c : (int, int16_unsigned_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let u16_fortran : (int, int16_unsigned_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i32_c : (int32, int32_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i32_fortran : (int32, int32_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let int_c : (int, int_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let int_fortran : (int, int_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let nat_c : (nativeint, nativeint_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let nat_fortran : (nativeint, nativeint_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let char_c : (char, int8_unsigned_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let char_fortran : (char, int8_unsigned_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i64_c : (int64, int64_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let i64_fortran : (int64, int64_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let c64_c : (Complex.t, complex64_elt, c_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let c64_fortran : (Complex.t, complex64_elt, fortran_layout) ops1 =
  { get = Array1.get
  ; unsafe_get = Array1.unsafe_get
  ; set = Array1.set
  ; unsafe_set = Array1.unsafe_set
  }

let f64_c_23 : (float, float64_elt, c_layout) ops23 =
  { get2 = Array2.get
  ; unsafe_get2 = Array2.unsafe_get
  ; set2 = Array2.set
  ; unsafe_set2 = Array2.unsafe_set
  ; get3 = Array3.get
  ; unsafe_get3 = Array3.unsafe_get
  ; set3 = Array3.set
  ; unsafe_set3 = Array3.unsafe_set
  }

let f64_fortran_23 : (float, float64_elt, fortran_layout) ops23 =
  { get2 = Array2.get
  ; unsafe_get2 = Array2.unsafe_get
  ; set2 = Array2.set
  ; unsafe_set2 = Array2.unsafe_set
  ; get3 = Array3.get
  ; unsafe_get3 = Array3.unsafe_get
  ; set3 = Array3.set
  ; unsafe_set3 = Array3.unsafe_set
  }

let u8_c_23 : (int, int8_unsigned_elt, c_layout) ops23 =
  { get2 = Array2.get
  ; unsafe_get2 = Array2.unsafe_get
  ; set2 = Array2.set
  ; unsafe_set2 = Array2.unsafe_set
  ; get3 = Array3.get
  ; unsafe_get3 = Array3.unsafe_get
  ; set3 = Array3.set
  ; unsafe_set3 = Array3.unsafe_set
  }

let u8_fortran_23 : (int, int8_unsigned_elt, fortran_layout) ops23 =
  { get2 = Array2.get
  ; unsafe_get2 = Array2.unsafe_get
  ; set2 = Array2.set
  ; unsafe_set2 = Array2.unsafe_set
  ; get3 = Array3.get
  ; unsafe_get3 = Array3.unsafe_get
  ; set3 = Array3.set
  ; unsafe_set3 = Array3.unsafe_set
  }

let i32_c_23 : (int32, int32_elt, c_layout) ops23 =
  { get2 = Array2.get
  ; unsafe_get2 = Array2.unsafe_get
  ; set2 = Array2.set
  ; unsafe_set2 = Array2.unsafe_set
  ; get3 = Array3.get
  ; unsafe_get3 = Array3.unsafe_get
  ; set3 = Array3.set
  ; unsafe_set3 = Array3.unsafe_set
  }

let i32_fortran_23 : (int32, int32_elt, fortran_layout) ops23 =
  { get2 = Array2.get
  ; unsafe_get2 = Array2.unsafe_get
  ; set2 = Array2.set
  ; unsafe_set2 = Array2.unsafe_set
  ; get3 = Array3.get
  ; unsafe_get3 = Array3.unsafe_get
  ; set3 = Array3.set
  ; unsafe_set3 = Array3.unsafe_set
  }

let%expect_test "bigarray accesses, 1 dimension" =
  let fl = Printf.sprintf "%h" in
  test1 "f64" float64 f64_c f64_fortran fl [| 1.5; -2.25; 1e300; -0.; 0.1 |];
  test1 "f32" float32 f32_c f32_fortran fl [| 1.5; -2.25; 0.1; 1e40; 3.4e38 |];
  test1 "i8" int8_signed i8_c i8_fortran string_of_int [| 1; -1; 127; 128; 300 |];
  test1 "u8" int8_unsigned u8_c u8_fortran string_of_int [| 1; -1; 255; 256; 300 |];
  test1
    "i16"
    int16_signed
    i16_c
    i16_fortran
    string_of_int
    [| 1; -1; 32767; 32768; 70000 |];
  test1
    "u16"
    int16_unsigned
    u16_c
    u16_fortran
    string_of_int
    [| 1; -1; 65535; 65536; 70000 |];
  test1
    "i32"
    int32
    i32_c
    i32_fortran
    Int32.to_string
    [| 1l; -1l; Int32.max_int; Int32.min_int; 7l |];
  test1
    "int"
    int
    int_c
    int_fortran
    string_of_int
    [| 1; -1; (1 lsl 30) - 1; -(1 lsl 30); 7 |];
  test1
    "nat"
    nativeint
    nat_c
    nat_fortran
    Nativeint.to_string
    [| 1n; -1n; 2147483647n; -2147483648n; 7n |];
  test1 "char" char char_c char_fortran Char.escaped [| 'a'; 'z'; '\000'; '\255'; 'q' |];
  test1
    "i64"
    int64
    i64_c
    i64_fortran
    Int64.to_string
    [| 1L; -1L; Int64.max_int; Int64.min_int; 7L |];
  test1
    "c64"
    complex64
    c64_c
    c64_fortran
    (fun c -> Printf.sprintf "%h+%hi" c.Complex.re c.im)
    [| Complex.one; Complex.i; Complex.zero; Complex.one; Complex.i |];
  [%expect
    {|
           f64 C: <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.7e43c8800759cp+996 -0x0p+0 0x1.999999999999ap-4 <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.7e43c8800759cp+996 -0x0p+0 0x1.999999999999ap-4 <index out of bounds> <index out of bounds> -0x1.2p+1
           f64 F: <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.7e43c8800759cp+996 -0x0p+0 0x1.999999999999ap-4 <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.7e43c8800759cp+996 -0x0p+0 0x1.999999999999ap-4 <index out of bounds> <index out of bounds> -0x1.2p+1
           f32 C: <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.99999ap-4 infinity 0x1.ff933cp+127 <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.99999ap-4 infinity 0x1.ff933cp+127 <index out of bounds> <index out of bounds> -0x1.2p+1
           f32 F: <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.99999ap-4 infinity 0x1.ff933cp+127 <index out of bounds> <index out of bounds> 0x1.8p+0 -0x1.2p+1 0x1.99999ap-4 infinity 0x1.ff933cp+127 <index out of bounds> <index out of bounds> -0x1.2p+1
           i8 C: <index out of bounds> <index out of bounds> 1 -1 127 -128 44 <index out of bounds> <index out of bounds> 1 -1 127 -128 44 <index out of bounds> <index out of bounds> -1
           i8 F: <index out of bounds> <index out of bounds> 1 -1 127 -128 44 <index out of bounds> <index out of bounds> 1 -1 127 -128 44 <index out of bounds> <index out of bounds> -1
           u8 C: <index out of bounds> <index out of bounds> 1 255 255 0 44 <index out of bounds> <index out of bounds> 1 255 255 0 44 <index out of bounds> <index out of bounds> 255
           u8 F: <index out of bounds> <index out of bounds> 1 255 255 0 44 <index out of bounds> <index out of bounds> 1 255 255 0 44 <index out of bounds> <index out of bounds> 255
           i16 C: <index out of bounds> <index out of bounds> 1 -1 32767 -32768 4464 <index out of bounds> <index out of bounds> 1 -1 32767 -32768 4464 <index out of bounds> <index out of bounds> -1
           i16 F: <index out of bounds> <index out of bounds> 1 -1 32767 -32768 4464 <index out of bounds> <index out of bounds> 1 -1 32767 -32768 4464 <index out of bounds> <index out of bounds> -1
           u16 C: <index out of bounds> <index out of bounds> 1 65535 65535 0 4464 <index out of bounds> <index out of bounds> 1 65535 65535 0 4464 <index out of bounds> <index out of bounds> 65535
           u16 F: <index out of bounds> <index out of bounds> 1 65535 65535 0 4464 <index out of bounds> <index out of bounds> 1 65535 65535 0 4464 <index out of bounds> <index out of bounds> 65535
           i32 C: <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> -1
           i32 F: <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> -1
           int C: <index out of bounds> <index out of bounds> 1 -1 1073741823 -1073741824 7 <index out of bounds> <index out of bounds> 1 -1 1073741823 -1073741824 7 <index out of bounds> <index out of bounds> -1
           int F: <index out of bounds> <index out of bounds> 1 -1 1073741823 -1073741824 7 <index out of bounds> <index out of bounds> 1 -1 1073741823 -1073741824 7 <index out of bounds> <index out of bounds> -1
           nat C: <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> -1
           nat F: <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> 1 -1 2147483647 -2147483648 7 <index out of bounds> <index out of bounds> -1
           char C: <index out of bounds> <index out of bounds> a z \000 \255 q <index out of bounds> <index out of bounds> a z \000 \255 q <index out of bounds> <index out of bounds> z
           char F: <index out of bounds> <index out of bounds> a z \000 \255 q <index out of bounds> <index out of bounds> a z \000 \255 q <index out of bounds> <index out of bounds> z
           i64 C: <index out of bounds> <index out of bounds> 1 -1 9223372036854775807 -9223372036854775808 7 <index out of bounds> <index out of bounds> 1 -1 9223372036854775807 -9223372036854775808 7 <index out of bounds> <index out of bounds> -1
           i64 F: <index out of bounds> <index out of bounds> 1 -1 9223372036854775807 -9223372036854775808 7 <index out of bounds> <index out of bounds> 1 -1 9223372036854775807 -9223372036854775808 7 <index out of bounds> <index out of bounds> -1
           c64 C: <index out of bounds> <index out of bounds> 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i 0x0p+0+0x0p+0i 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i <index out of bounds> <index out of bounds> 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i 0x0p+0+0x0p+0i 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i <index out of bounds> <index out of bounds> 0x0p+0+0x1p+0i
           c64 F: <index out of bounds> <index out of bounds> 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i 0x0p+0+0x0p+0i 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i <index out of bounds> <index out of bounds> 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i 0x0p+0+0x0p+0i 0x1p+0+0x0p+0i 0x0p+0+0x1p+0i <index out of bounds> <index out of bounds> 0x0p+0+0x1p+0i
           |}]

let%expect_test "bigarray accesses, 2 and 3 dimensions" =
  test23 "f64" float64 f64_c_23 f64_fortran_23 (Printf.sprintf "%h") float;
  test23 "u8" int8_unsigned u8_c_23 u8_fortran_23 string_of_int (fun i -> i * 7);
  test23 "i32" int32 i32_c_23 i32_fortran_23 Int32.to_string (fun i ->
      Int32.of_int (i * 1000003));
  [%expect
    {|
           f64 C: <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x0p+0 0x1p+0 0x1p+1 0x1.8p+1 <index out of bounds> <index out of bounds> 0x1p+2 0x1.4p+2 0x1.8p+2 0x1.cp+2 <index out of bounds> <index out of bounds> 0x1p+3 0x1.2p+3 0x1.4p+3 0x1.6p+3 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x1.2p+3 0x1.8cp+6
           <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x0p+0 0x1p+0 0x1p+1 0x1.8p+1 <index out of bounds> <index out of bounds> 0x1p+2 0x1.4p+2 0x1.8p+2 0x1.cp+2 <index out of bounds> <index out of bounds> 0x1p+3 0x1.2p+3 0x1.4p+3 0x1.6p+3 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x1.8p+3 0x1.ap+3 0x1.cp+3 0x1.ep+3 <index out of bounds> <index out of bounds> 0x1p+4 0x1.1p+4 0x1.2p+4 0x1.3p+4 <index out of bounds> <index out of bounds> 0x1.4p+4 0x1.5p+4 0x1.6p+4 0x1.7p+4 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x1.7p+4 0x1.34p+6
           f64 F: <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x0p+0 0x1p+0 0x1p+1 0x1.8p+1 <index out of bounds> <index out of bounds> 0x1p+2 0x1.4p+2 0x1.8p+2 0x1.cp+2 <index out of bounds> <index out of bounds> 0x1p+3 0x1.2p+3 0x1.4p+3 0x1.6p+3 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x1.2p+3 0x1.8cp+6
           <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x0p+0 0x1p+0 0x1p+1 0x1.8p+1 <index out of bounds> <index out of bounds> 0x1p+2 0x1.4p+2 0x1.8p+2 0x1.cp+2 <index out of bounds> <index out of bounds> 0x1p+3 0x1.2p+3 0x1.4p+3 0x1.6p+3 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x1.8p+3 0x1.ap+3 0x1.cp+3 0x1.ep+3 <index out of bounds> <index out of bounds> 0x1p+4 0x1.1p+4 0x1.2p+4 0x1.3p+4 <index out of bounds> <index out of bounds> 0x1.4p+4 0x1.5p+4 0x1.6p+4 0x1.7p+4 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0x1.7p+4 0x1.34p+6
           u8 C: <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 7 14 21 <index out of bounds> <index out of bounds> 28 35 42 49 <index out of bounds> <index out of bounds> 56 63 70 77 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 63 181
           <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 7 14 21 <index out of bounds> <index out of bounds> 28 35 42 49 <index out of bounds> <index out of bounds> 56 63 70 77 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 84 91 98 105 <index out of bounds> <index out of bounds> 112 119 126 133 <index out of bounds> <index out of bounds> 140 147 154 161 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 161 27
           u8 F: <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 7 14 21 <index out of bounds> <index out of bounds> 28 35 42 49 <index out of bounds> <index out of bounds> 56 63 70 77 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 63 181
           <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 7 14 21 <index out of bounds> <index out of bounds> 28 35 42 49 <index out of bounds> <index out of bounds> 56 63 70 77 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 84 91 98 105 <index out of bounds> <index out of bounds> 112 119 126 133 <index out of bounds> <index out of bounds> 140 147 154 161 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 161 27
           i32 C: <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 1000003 2000006 3000009 <index out of bounds> <index out of bounds> 4000012 5000015 6000018 7000021 <index out of bounds> <index out of bounds> 8000024 9000027 10000030 11000033 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 9000027 99000297
           <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 1000003 2000006 3000009 <index out of bounds> <index out of bounds> 4000012 5000015 6000018 7000021 <index out of bounds> <index out of bounds> 8000024 9000027 10000030 11000033 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 12000036 13000039 14000042 15000045 <index out of bounds> <index out of bounds> 16000048 17000051 18000054 19000057 <index out of bounds> <index out of bounds> 20000060 21000063 22000066 23000069 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 23000069 77000231
           i32 F: <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 1000003 2000006 3000009 <index out of bounds> <index out of bounds> 4000012 5000015 6000018 7000021 <index out of bounds> <index out of bounds> 8000024 9000027 10000030 11000033 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 9000027 99000297
           <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 0 1000003 2000006 3000009 <index out of bounds> <index out of bounds> 4000012 5000015 6000018 7000021 <index out of bounds> <index out of bounds> 8000024 9000027 10000030 11000033 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 12000036 13000039 14000042 15000045 <index out of bounds> <index out of bounds> 16000048 17000051 18000054 19000057 <index out of bounds> <index out of bounds> 20000060 21000063 22000066 23000069 <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> <index out of bounds> 23000069 77000231
           |}]

let%expect_test "int64 comparisons" =
  let l =
    [ 0L
    ; 1L
    ; -1L
    ; 2L
    ; Int64.max_int
    ; Int64.min_int
    ; 0x1000000L
    ; 0xffffffL
    ; 0x1000000000000L
    ; 0xffffffffffffL
    ; -0x1000000L
    ; Int64.add Int64.min_int 1L
    ; 0x7fff000000000000L
    ; -0x7fff000000000000L
    ]
  in
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          pr
            "%d%d%d%d%d%d "
            (Bool.to_int (x = y))
            (Bool.to_int (x <> y))
            (Bool.to_int (x < y))
            (Bool.to_int (x > y))
            (Bool.to_int (x <= y))
            (Bool.to_int (x >= y)))
        l;
      pr "\n")
    l;
  [%expect
    {|
           100011 011010 010101 011010 011010 010101 011010 011010 011010 011010 010101 010101 011010 010101
           010101 100011 010101 011010 011010 010101 011010 011010 011010 011010 010101 010101 011010 010101
           011010 011010 100011 011010 011010 010101 011010 011010 011010 011010 010101 010101 011010 010101
           010101 010101 010101 100011 011010 010101 011010 011010 011010 011010 010101 010101 011010 010101
           010101 010101 010101 010101 100011 010101 010101 010101 010101 010101 010101 010101 010101 010101
           011010 011010 011010 011010 011010 100011 011010 011010 011010 011010 011010 011010 011010 011010
           010101 010101 010101 010101 011010 010101 100011 010101 011010 011010 010101 010101 011010 010101
           010101 010101 010101 010101 011010 010101 011010 100011 011010 011010 010101 010101 011010 010101
           010101 010101 010101 010101 011010 010101 010101 010101 100011 010101 010101 010101 011010 010101
           010101 010101 010101 010101 011010 010101 010101 010101 011010 100011 010101 010101 011010 010101
           011010 011010 011010 011010 011010 010101 011010 011010 011010 011010 100011 010101 011010 010101
           011010 011010 011010 011010 011010 010101 011010 011010 011010 011010 011010 100011 011010 011010
           010101 010101 010101 010101 011010 010101 010101 010101 010101 010101 010101 010101 100011 010101
           011010 011010 011010 011010 011010 010101 011010 011010 011010 011010 011010 010101 011010 100011
           |}]
