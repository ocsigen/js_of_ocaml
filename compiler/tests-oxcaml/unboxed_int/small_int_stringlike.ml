(* TEST *)

(* The small-int stringlike accessors: [caml_string_geti8],
   [caml_string_geti16], [caml_bytes_geti8], [caml_bytes_geti16],
   [caml_bytes_set8], [caml_ba_uint8_geti8], [caml_ba_uint8_geti16] and
   [caml_ba_uint8_set8].

   Unlike [get16], which zero-extends, [geti8] and [geti16] sign-extend their
   result; this is what the tests below pin down, together with the bounds
   checks. *)

module Int8 = Stdlib_stable.Int8
module Int16 = Stdlib_stable.Int16

external string_geti8 : string -> int -> int8 = "%caml_string_geti8"

external string_geti16 : string -> int -> int16 = "%caml_string_geti16"

external bytes_geti8 : bytes -> int -> int8 = "%caml_bytes_geti8"

external bytes_geti16 : bytes -> int -> int16 = "%caml_bytes_geti16"

external bytes_set8 : bytes -> int -> int8 -> unit = "%caml_bytes_set8"

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external bigstring_geti8 : bigstring -> int -> int8 = "%caml_bigstring_geti8"

external bigstring_geti16 : bigstring -> int -> int16 = "%caml_bigstring_geti16"

external bigstring_set8 : bigstring -> int -> int8 -> unit
  = "%caml_bigstring_set8"

let i8 = Int8.of_int

let i16 = Int16.of_int

let bigstring_of_string s =
  let a = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (String.length s) in
  String.iteri (fun i c -> a.{i} <- c) s;
  a

let out_of_bounds f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

(* "\x00\x7f\x80\xff": the two middle bytes straddle the sign boundary. *)
let s = "\x00\x7f\x80\xff"

let test_string () =
  assert (Int8.equal (string_geti8 s 0) (i8 0));
  assert (Int8.equal (string_geti8 s 1) (i8 127));
  (* 0x80 sign-extends to -128, where [String.get_uint8] would give 128. *)
  assert (Int8.equal (string_geti8 s 2) (i8 (-128)));
  assert (Int8.equal (string_geti8 s 3) (i8 (-1)));
  (* Little-endian pairs: 0x7f00 = 32512, 0xff80 = -128. *)
  assert (Int16.equal (string_geti16 s 0) (i16 32512));
  assert (Int16.equal (string_geti16 s 2) (i16 (-128)));
  assert (out_of_bounds (fun () -> string_geti8 s 4));
  assert (out_of_bounds (fun () -> string_geti8 s (-1)));
  assert (out_of_bounds (fun () -> string_geti16 s 3));
  assert (out_of_bounds (fun () -> string_geti16 s (-1)))

let test_bytes () =
  let b = Bytes.of_string s in
  assert (Int8.equal (bytes_geti8 b 2) (i8 (-128)));
  assert (Int8.equal (bytes_geti8 b 3) (i8 (-1)));
  assert (Int16.equal (bytes_geti16 b 0) (i16 32512));
  assert (Int16.equal (bytes_geti16 b 2) (i16 (-128)));
  (* [set8] truncates to the low byte, so -1 and 255 write the same byte. *)
  bytes_set8 b 0 (i8 (-1));
  assert (Bytes.get_uint8 b 0 = 255);
  assert (Int8.equal (bytes_geti8 b 0) (i8 (-1)));
  bytes_set8 b 1 (i8 (-128));
  assert (Bytes.get_uint8 b 1 = 128);
  assert (out_of_bounds (fun () -> bytes_geti8 b 4));
  assert (out_of_bounds (fun () -> bytes_geti8 b (-1)));
  assert (out_of_bounds (fun () -> bytes_geti16 b 3));
  assert (out_of_bounds (fun () -> bytes_set8 b 4 (i8 0)));
  assert (out_of_bounds (fun () -> bytes_set8 b (-1) (i8 0)))

let test_bigstring () =
  let a = bigstring_of_string s in
  assert (Int8.equal (bigstring_geti8 a 0) (i8 0));
  assert (Int8.equal (bigstring_geti8 a 2) (i8 (-128)));
  assert (Int8.equal (bigstring_geti8 a 3) (i8 (-1)));
  assert (Int16.equal (bigstring_geti16 a 0) (i16 32512));
  assert (Int16.equal (bigstring_geti16 a 2) (i16 (-128)));
  bigstring_set8 a 0 (i8 (-1));
  assert (Char.code a.{0} = 255);
  assert (Int8.equal (bigstring_geti8 a 0) (i8 (-1)));
  assert (out_of_bounds (fun () -> bigstring_geti8 a 4));
  assert (out_of_bounds (fun () -> bigstring_geti8 a (-1)));
  assert (out_of_bounds (fun () -> bigstring_geti16 a 3));
  assert (out_of_bounds (fun () -> bigstring_set8 a 4 (i8 0)))

let () =
  test_string ();
  test_bytes ();
  test_bigstring ();
  print_endline "OK"
