(* Regression tests for the bound-check bugs fixed in the wasm marshal
   runtime. *)

(* [caml_input_value_from_bytes] must use an unsigned comparison so that a
   data_len that wraps to negative in i32 cannot bypass the buffer-overflow
   check.  We craft a 20-byte buffer holding a small-magic header with
   data_len = 0x80000000.  Sum [ofs + header_len + data_len] is negative as
   i32 (so the previous signed compare bypassed the check) but huge as u32. *)
let test_bad_length () =
  let bad =
    Bytes.of_string
      "\x84\x95\xA6\xBE\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
  in
  match Marshal.from_bytes bad 0 with
  | _ -> assert false
  | exception Failure _ -> ()

(* [extern_custom] (CUSTOM_LEN) must write the [size_64] header field as a
   zero-extended 8-byte big-endian value.  Previously only 4 of its 8 bytes
   were written, so with a user-provided output buffer the high half of
   [size_64] leaked pre-existing buffer contents.  Marshal a Bigarray (which
   uses CUSTOM_LEN) into two buffers prefilled with different bytes and check
   that the marshaled output is identical, then verify it round-trips. *)
let test_custom_len_size_64 () =
  let ba = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 4 in
  for i = 0 to 3 do
    ba.{i} <- i + 1
  done;
  let canonical = Marshal.to_bytes ba [] in
  let n = Bytes.length canonical in
  let dirty = Bytes.make (n + 16) '\xFF' in
  Marshal.to_buffer dirty 0 (n + 16) ba [] |> ignore;
  assert (Bytes.sub_string canonical 0 n = Bytes.sub_string dirty 0 n);
  let ba' : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t =
    Marshal.from_bytes dirty 0
  in
  assert (Bigarray.Array1.dim ba' = 4);
  for i = 0 to 3 do
    assert (ba'.{i} = i + 1)
  done

let () =
  test_bad_length ();
  test_custom_len_size_64 ()
