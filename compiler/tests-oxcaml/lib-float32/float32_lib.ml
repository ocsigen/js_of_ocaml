[@@@ocaml.warning "-unused-value-declaration"]

[@@@ocaml.warning "-unused-module"]

(* Tests for the float32 otherlib *)

module F32 = Stdlib_stable.Float32

module CF32 = struct
  open F32
  let check_float32s f =
    Random.set_state (Random.State.make [| 123456789 |]);
    let neg_one = -1.s in
    let neg_zero = -0.s in
    f zero zero;
    f zero one;
    f one one;
    f zero neg_one;
    f neg_one neg_one;
    f one neg_one;
    f zero neg_zero;
    f neg_zero zero;
    f nan zero;
    f infinity zero;
    f neg_infinity zero;
    f nan nan;
    f infinity infinity;
    f neg_infinity neg_infinity;
    f neg_infinity infinity;
    f infinity nan;
    f neg_infinity nan;
    f max_float infinity;
    f max_float neg_infinity;
    f min_float infinity;
    f min_float neg_infinity;
    f max_float max_float;
    f min_float min_float;
    f max_float min_float;
    for _ = 0 to 100_000 do
      let f0 = Random.int32 Int32.max_int in
      let f1 = Random.int32 Int32.max_int in
      f
        ((if Random.bool () then f0 else Int32.neg f0)
        |> Int32.float_of_bits |> F32.of_float)
        ((if Random.bool () then f1 else Int32.neg f1)
        |> Int32.float_of_bits |> F32.of_float)
    done
end

let bit_eq f1 f2 =
  assert (F32.to_bits f1 = F32.to_bits f2 || (F32.is_nan f1 && F32.is_nan f2))

let () =
  (* In glibc 2.25+, powf(nan, zero) returns one if the nan is non-signaling. *)
  bit_eq (F32.pow F32.nan F32.zero) F32.one;
  bit_eq (F32.pow F32.quiet_nan F32.zero) F32.one

let () =
  CF32.check_float32s (fun f _ ->
      assert (F32.seeded_hash 42 f = Hashtbl.seeded_hash 42 f);
      assert (F32.hash f = Hashtbl.hash f))

let () =
  try
    ignore (F32.of_string "");
    assert false
  with Failure msg -> (
    assert (msg = "float32_of_string");
    try
      ignore (F32.of_string "a");
      assert false
    with Failure msg -> (
      assert (msg = "float32_of_string");
      try
        ignore (F32.of_string "0.0.0");
        assert false
      with Failure msg -> (
        assert (msg = "float32_of_string");
        try
          ignore (F32.of_string "0xzz");
          assert false
        with Failure msg -> (
          assert (msg = "float32_of_string");
          try
            ignore (F32.of_string "1e10.0");
            assert false
          with Failure msg ->
            assert (msg = "float32_of_string");
            assert (Option.is_none (F32.of_string_opt ""));
            assert (Option.is_none (F32.of_string_opt "a"));
            assert (Option.is_none (F32.of_string_opt "0.0.0"));
            assert (Option.is_none (F32.of_string_opt "0xzz"));
            assert (Option.is_none (F32.of_string_opt "1e10.0"))))))

external format : string -> float32 -> string = "caml_format_float32"

(* [to_string] calls format with "%.9g"; these are some additional format string
   tests. *)
let () =
  assert (format "%.0g" 0.1234s = "0.1");
  assert (format "%.1g" 0.1234s = "0.1");
  assert (format "%.2g" 0.1234s = "0.12");
  assert (format "%.3g" 0.1234s = "0.123");
  assert (format "%f" 0.1234s = "0.123400");
  assert (format "%f" 1024.s = "1024.000000");
  assert (format "%f" 1e10s = "10000000000.000000");
  assert (format "%g" 1e20s = "1e+20");
  assert (format "%g" 1e-20s = "1e-20");
  assert (format "%f" F32.infinity = "inf");
  assert (format "%f" F32.neg_infinity = "-inf");
  assert (format "%f" F32.nan = "nan")

let () =
  CF32.check_float32s (fun f _ ->
      bit_eq (F32.of_string (F32.to_string f)) f;
      match F32.of_string_opt (F32.to_string f) with
      | None -> assert false
      | Some f' -> bit_eq f f');
  let check s f = bit_eq (F32.of_string s) f in
  check "0.0" 0.0s;
  check "1.0" 1.0s;
  check "0.5" 0.5s;
  check "1234.1234" 1234.1234s;
  check "0." 0.s;
  check "1e10" 1e10s;
  check "1e-9_8" 1e-9_8s;
  check "1e+1" 1e+1s;
  check "1.12345e+12" 1.12345e+12s;
  check "0x2_2p+0" 0x22p+0s;
  check "0x2p+0" 0x2p+0s;
  check "0x3p+0" 0x3p+0s;
  check "0x5p+0" 0x5p+0s;
  check "0x1.4p+0" 0x1.4p+0s;
  check "0xcp-4" 0xcp-4s;
  check "0x1p-4" 0x1p-4s;
  check "0x1p+0" 0x1p+0s;
  check "0x0p+0" 0x0p+0s;
  check "0xf.f___ffffp+124" 0xf.fffffp+124s;
  check "0xf.ffffffffffff8p+1020" 0xf.ffffffffffff8p+1020s;
  check "0x4p-128" 0x4p-128s;
  check "0x1p-252" 0x1p-252s;
  check "0x4p-1024" 0x4p-1024s;
  check "0x8p-972" 0x8p-972s;
  check "0xf.fff_f_e_000001p+252" 0xf.ff_ffe_00_0001p+252s;
  check "0x2.fffp+12" 0x2.fffp+12s;
  check "0x1.ffffp-24" 0x1.ffffp-24s;
  check "0x2._fff006p+12" 0x2._fff006p+12s;
  check "0x1.fffp+0" 0x1.fffp+0s;
  check "0x1.00001p+0" 0x1.00001p+0s;
  check "0xc.d5e6fp+1_24" 0xc.d5e6fp+1_24s;
  check "0x2.6af378p-128" 0x2.6af378p-128s;
  check "0x5p-128" 0x5p-128s;
  check "0x1____p-128" 0x1p-128s;
  check "0x8p-152" 0x8p-152s;
  check "0x8p-4" 0x8p-4s;
  check "0x8p+124" 0x8p+124s;
  check "0x1000002p+0" 0x1000002p+0s;
  check "0x1000003p+0" 0x1000003p+0s;
  check "0x100000fp+0" 0x100000fp+0s;
  check "0x10000001p+0" 0x10000001p+0s;
  check "0x10000002p+0" 0x10000002p+0s;
  check "0x10000003p+0" 0x10000003p+0s;
  check "0x1000000fp+0" 0x1000000fp+0s;
  check "0x1000003fp+0" 0x1000003fp+0s;
  check "0x1000002fp+0" 0x1000002fp+0s;
  check "0x1.000002p+0" 0x1.000002p+0s;
  check "0x1.000003p+0" 0x1.000003p+0s;
  check "0x1.00000fp+0" 0x1.00000fp+0s;
  check "0x1.0000001p+0" 0x1.0000001p+0s;
  check "0x1.0000002p+0" 0x1.0000002p+0s;
  check "0x1.0000003p+0" 0x1.0000003p+0s;
  check "0x1.000000fp+0" 0x1.000000fp+0s;
  check "0x1.000002fp+0" 0x1.000002fp+0s;
  check "0x1.00000200p+0" 0x1.00000200p+0s;
  check "0x1.000002001p+0" 0x1.000002001p+0s;
  check "0x1.000002000000000p+0" 0x1.000002000000000p+0s;
  check "0x1.000002000000001p+0" 0x1.000002000000001p+0s;
  check "0x1.00000200000000000000p+0" 0x1.00000200000000000000p+0s;
  check "0x1.00000200000000000001p+0" 0x1.00000200000000000001p+0s;
  check "0x1.000003p+0" 0x1.000003p+0s

module Bytes = struct
  let data = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = F32.of_bits 0x03020100l

  let high = F32.of_bits 0x07060504l

  (* Getters *)

  let () =
    let v = F32.Bytes.get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bytes.unsafe_get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bytes.get data ~pos:4 in
    bit_eq high v;
    let v = F32.Bytes.unsafe_get data ~pos:4 in
    bit_eq high v

  let () =
    for bad = -4 to -1 do
      try
        let _ = F32.Bytes.get data ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bytes.get data ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done

  (* Setters *)

  let set f pos =
    F32.Bytes.set data f ~pos;
    let v = F32.Bytes.get data ~pos in
    bit_eq f v

  let set_unsafe f pos =
    F32.Bytes.unsafe_set data f ~pos;
    let v = F32.Bytes.get data ~pos in
    bit_eq f v

  let () =
    set (F32.of_bits 0x10101010l) 0;
    set (F32.of_bits 0x20202020l) 4;
    set_unsafe (F32.of_bits 0x10101010l) 0;
    set_unsafe (F32.of_bits 0x20202020l) 4;
    Random.init 1234;
    for _ = 1 to 1000 do
      set (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5);
      set_unsafe (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5)
    done

  let () =
    let set = F32.of_bits 0xFFFFFFFFl in
    for bad = -4 to -1 do
      try
        let _ = F32.Bytes.set data set ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bytes.set data set ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done
end

module String = struct
  let data = "\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = F32.of_bits 0x03020100l

  let high = F32.of_bits 0x07060504l

  (* Getters *)

  let () =
    let v = F32.String.get data ~pos:0 in
    bit_eq low v;
    let v = F32.String.unsafe_get data ~pos:0 in
    bit_eq low v;
    let v = F32.String.get data ~pos:4 in
    bit_eq high v;
    let v = F32.String.unsafe_get data ~pos:4 in
    bit_eq high v

  let () =
    for bad = -4 to -1 do
      try
        let _ = F32.String.get data ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.String.get data ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done
end

module Bigstring = struct
  open Bigarray

  let bigstring_of_string s =
    let open Stdlib in
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let data = bigstring_of_string "\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = F32.of_bits 0x03020100l

  let high = F32.of_bits 0x07060504l

  (* Getters *)

  let () =
    let v = F32.Bigstring.get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bigstring.unsafe_get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bigstring.get data ~pos:4 in
    bit_eq high v;
    let v = F32.Bigstring.unsafe_get data ~pos:4 in
    bit_eq high v

  let () =
    for bad = -4 to -1 do
      try
        let _ = F32.Bigstring.get data ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bigstring.get data ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done

  (* Setters *)

  let set f pos =
    F32.Bigstring.set data f ~pos;
    let v = F32.Bigstring.get data ~pos in
    bit_eq f v

  let set_unsafe f pos =
    F32.Bigstring.unsafe_set data f ~pos;
    let v = F32.Bigstring.get data ~pos in
    bit_eq f v

  let () =
    set (F32.of_bits 0x10101010l) 0;
    set (F32.of_bits 0x20202020l) 4;
    set_unsafe (F32.of_bits 0x10101010l) 0;
    set_unsafe (F32.of_bits 0x20202020l) 4;
    Random.init 1234;
    for _ = 1 to 1000 do
      set (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5);
      set_unsafe (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5)
    done

  let () =
    let set = F32.of_bits 0xFFFFFFFFl in
    for bad = -4 to -1 do
      try
        let _ = F32.Bigstring.set data set ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bigstring.set data set ~pos:bad in
        assert false
      with Invalid_argument s when s = "index out of bounds" -> ()
    done
end

module Bigarray = struct
  open Stdlib.Bigarray

  module A1 = struct
    let c_array = Array1.init Float32 C_layout 4 Float.of_int

    let f_array = Array1.init Float32 Fortran_layout 4 Float.of_int

    let () =
      let v = F32.Bigarray.Array1.get c_array 0 in
      bit_eq 0.0s v;
      let v = F32.Bigarray.Array1.unsafe_get c_array 0 in
      bit_eq 0.0s v;
      let v = F32.Bigarray.Array1.get c_array 3 in
      bit_eq 3.0s v;
      let v = F32.Bigarray.Array1.unsafe_get c_array 3 in
      bit_eq 3.0s v

    let () =
      let v = F32.Bigarray.Array1.get f_array 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array1.unsafe_get f_array 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array1.get f_array 4 in
      bit_eq 4.0s v;
      let v = F32.Bigarray.Array1.unsafe_get f_array 4 in
      bit_eq 4.0s v

    let set array f pos =
      F32.Bigarray.Array1.set array pos f;
      let v = F32.Bigarray.Array1.get array pos in
      bit_eq f v

    let set_unsafe array f pos =
      F32.Bigarray.Array1.unsafe_set array pos f;
      let v = F32.Bigarray.Array1.get array pos in
      bit_eq f v

    let () =
      set c_array (F32.of_bits 0x10101010l) 0;
      set c_array (F32.of_bits 0x20202020l) 1;
      set_unsafe c_array (F32.of_bits 0x10101010l) 2;
      set_unsafe c_array (F32.of_bits 0x20202020l) 3;
      Random.init 1234;
      for _ = 1 to 1000 do
        set c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4);
        set_unsafe c_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (Random.int 4)
      done;
      set f_array (F32.of_bits 0x10101010l) 1;
      set f_array (F32.of_bits 0x20202020l) 2;
      set_unsafe f_array (F32.of_bits 0x10101010l) 3;
      set_unsafe f_array (F32.of_bits 0x20202020l) 4;
      Random.init 1234;
      for _ = 1 to 1000 do
        set f_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (1 + Random.int 4);
        set_unsafe f_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (1 + Random.int 4)
      done

    let () =
      let check f =
        try
          f () |> ignore;
          assert false
        with Invalid_argument s when s = "index out of bounds" -> ()
      in
      check (fun () -> F32.Bigarray.Array1.get c_array (-1));
      check (fun () -> F32.Bigarray.Array1.set c_array (-1) 0.0s);
      check (fun () -> F32.Bigarray.Array1.get c_array 4);
      check (fun () -> F32.Bigarray.Array1.set c_array 4 0.0s);
      check (fun () -> F32.Bigarray.Array1.get f_array 0);
      check (fun () -> F32.Bigarray.Array1.set f_array 0 0.0s);
      check (fun () -> F32.Bigarray.Array1.get f_array 5);
      check (fun () -> F32.Bigarray.Array1.set f_array 5 0.0s)
  end

  module A2 = struct
    let c_array =
      Array2.init Float32 C_layout 4 4 (fun i j -> Float.of_int ((i * 4) + j))

    let f_array =
      Array2.init Float32 Fortran_layout 4 4 (fun i j ->
          Float.of_int ((i * 4) + j))

    let () =
      let v = F32.Bigarray.Array2.get c_array 0 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array2.unsafe_get c_array 0 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array2.get c_array 3 2 in
      bit_eq 14.0s v;
      let v = F32.Bigarray.Array2.unsafe_get c_array 3 2 in
      bit_eq 14.0s v

    let () =
      let v = F32.Bigarray.Array2.get f_array 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array2.unsafe_get f_array 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array2.get f_array 4 3 in
      bit_eq 19.0s v;
      let v = F32.Bigarray.Array2.unsafe_get f_array 4 3 in
      bit_eq 19.0s v

    let set array f i j =
      F32.Bigarray.Array2.set array i j f;
      let v = F32.Bigarray.Array2.get array i j in
      bit_eq f v

    let set_unsafe array f i j =
      F32.Bigarray.Array2.unsafe_set array i j f;
      let v = F32.Bigarray.Array2.get array i j in
      bit_eq f v

    let () =
      set c_array (F32.of_bits 0x10101010l) 0 1;
      set c_array (F32.of_bits 0x20202020l) 1 0;
      set_unsafe c_array (F32.of_bits 0x10101010l) 2 3;
      set_unsafe c_array (F32.of_bits 0x20202020l) 3 2;
      Random.init 1234;
      for _ = 1 to 1000 do
        set c_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (Random.int 4) (Random.int 4);
        set_unsafe c_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (Random.int 4) (Random.int 4)
      done;
      set f_array (F32.of_bits 0x10101010l) 1 2;
      set f_array (F32.of_bits 0x20202020l) 2 1;
      set_unsafe f_array (F32.of_bits 0x10101010l) 3 4;
      set_unsafe f_array (F32.of_bits 0x20202020l) 4 3;
      Random.init 1234;
      for _ = 1 to 1000 do
        set f_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (1 + Random.int 4)
          (1 + Random.int 4);
        set_unsafe f_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (1 + Random.int 4)
          (1 + Random.int 4)
      done

    let () =
      let check f =
        try
          f () |> ignore;
          assert false
        with Invalid_argument s when s = "index out of bounds" -> ()
      in
      check (fun () -> F32.Bigarray.Array2.get c_array (-1) 0);
      check (fun () -> F32.Bigarray.Array2.set c_array (-1) 0 0.0s);
      check (fun () -> F32.Bigarray.Array2.get c_array 4 0);
      check (fun () -> F32.Bigarray.Array2.set c_array 4 0 0.0s);
      check (fun () -> F32.Bigarray.Array2.get c_array 0 (-1));
      check (fun () -> F32.Bigarray.Array2.set c_array 0 (-1) 0.0s);
      check (fun () -> F32.Bigarray.Array2.get c_array 0 4);
      check (fun () -> F32.Bigarray.Array2.set c_array 0 4 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 0 1);
      check (fun () -> F32.Bigarray.Array2.set f_array 0 1 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 5 1);
      check (fun () -> F32.Bigarray.Array2.set f_array 5 1 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 1 0);
      check (fun () -> F32.Bigarray.Array2.set f_array 1 0 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 1 5);
      check (fun () -> F32.Bigarray.Array2.set f_array 1 5 0.0s)
  end

  module A3 = struct
    let c_array =
      Array3.init Float32 C_layout 4 4 4 (fun i j k ->
          Float.of_int ((i * 16) + (j * 4) + k))

    let f_array =
      Array3.init Float32 Fortran_layout 4 4 4 (fun i j k ->
          Float.of_int ((i * 16) + (j * 4) + k))

    let () =
      let v = F32.Bigarray.Array3.get c_array 0 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array3.unsafe_get c_array 0 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array3.get c_array 3 2 1 in
      bit_eq 57.0s v;
      let v = F32.Bigarray.Array3.unsafe_get c_array 3 2 1 in
      bit_eq 57.0s v

    let () =
      let v = F32.Bigarray.Array3.get f_array 1 2 3 in
      bit_eq 27.0s v;
      let v = F32.Bigarray.Array3.unsafe_get f_array 1 2 3 in
      bit_eq 27.0s v;
      let v = F32.Bigarray.Array3.get f_array 4 3 2 in
      bit_eq 78.0s v;
      let v = F32.Bigarray.Array3.unsafe_get f_array 4 3 2 in
      bit_eq 78.0s v

    let set array f i j k =
      F32.Bigarray.Array3.set array i j k f;
      let v = F32.Bigarray.Array3.get array i j k in
      bit_eq f v

    let set_unsafe array f i j k =
      F32.Bigarray.Array3.unsafe_set array i j k f;
      let v = F32.Bigarray.Array3.get array i j k in
      bit_eq f v

    let () =
      set c_array (F32.of_bits 0x10101010l) 0 1 2;
      set c_array (F32.of_bits 0x20202020l) 2 1 0;
      set_unsafe c_array (F32.of_bits 0x10101010l) 1 2 3;
      set_unsafe c_array (F32.of_bits 0x20202020l) 3 2 1;
      Random.init 1234;
      for _ = 1 to 1000 do
        set c_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (Random.int 4) (Random.int 4) (Random.int 4);
        set_unsafe c_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (Random.int 4) (Random.int 4) (Random.int 4)
      done;
      set f_array (F32.of_bits 0x10101010l) 1 2 3;
      set f_array (F32.of_bits 0x20202020l) 3 2 1;
      set_unsafe f_array (F32.of_bits 0x10101010l) 2 3 4;
      set_unsafe f_array (F32.of_bits 0x20202020l) 4 3 2;
      Random.init 1234;
      for _ = 1 to 1000 do
        set f_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (1 + Random.int 4)
          (1 + Random.int 4)
          (1 + Random.int 4);
        set_unsafe f_array
          (Random.int32 Int32.max_int |> F32.of_bits)
          (1 + Random.int 4)
          (1 + Random.int 4)
          (1 + Random.int 4)
      done

    let () =
      let check f =
        try
          f () |> ignore;
          assert false
        with Invalid_argument s when s = "index out of bounds" -> ()
      in
      check (fun () -> F32.Bigarray.Array3.get c_array (-1) 0 0);
      check (fun () -> F32.Bigarray.Array3.set c_array (-1) 0 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 4 0 0);
      check (fun () -> F32.Bigarray.Array3.set c_array 4 0 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 (-1) 0);
      check (fun () -> F32.Bigarray.Array3.set c_array 0 (-1) 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 4 0);
      check (fun () -> F32.Bigarray.Array3.set c_array 0 4 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 0 (-1));
      check (fun () -> F32.Bigarray.Array3.set c_array 0 0 (-1) 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 0 4);
      check (fun () -> F32.Bigarray.Array3.set c_array 0 0 4 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 0 1 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 0 1 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 5 1 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 5 1 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 0 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 0 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 5 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 5 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 1 0);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 1 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 1 5);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 1 5 0.0s)
  end
end

let () =
  let s = Marshal.to_string 1.234s [] in
  assert (Marshal.from_string s 0 = 1.234s)
