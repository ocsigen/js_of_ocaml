(* Regression tests for the wasm marshalling fixes (marshal.wat,
   bigarray.wat).

   - [Marshal.to_buffer] returned 0 instead of the number of bytes
     written.
   - [bigarray_deserialize] now validates the dimension count and size;
     this checks that valid bigarrays still round-trip. *)

let () =
  (* to_buffer must return the number of bytes written. *)
  let v = 1, "hello", [ 2; 3; 4 ] in
  let s = Marshal.to_string v [] in
  let buf = Bytes.create (String.length s + 16) in
  let n = Marshal.to_buffer buf 0 (Bytes.length buf) v [] in
  assert (n = String.length s);
  (* and the written prefix must round-trip. *)
  assert (Marshal.from_bytes buf 0 = v);

  (* A valid bigarray must still round-trip through marshalling. *)
  let a = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 5 in
  for i = 0 to 4 do
    Bigarray.Array1.set a i (i * i)
  done;
  let b : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t =
    Marshal.from_string (Marshal.to_string a []) 0
  in
  assert (Bigarray.Array1.dim b = 5);
  for i = 0 to 4 do
    assert (Bigarray.Array1.get b i = i * i)
  done
