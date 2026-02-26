let max = (1 lsl 16) - 1

let ba =
  Bigarray.Array1.init Bigarray.int16_unsigned Bigarray.c_layout (max + 1) (fun x -> x)

let s = Marshal.to_string ba []

let b = Bytes.of_string s

let s =
  Bytes.to_string
    (Bytes.set b 50 '\r';
     b)

let ba' : (float, Bigarray.float16_elt, Bigarray.c_layout) Bigarray.Array1.t =
  Marshal.from_string s 0

let () =
  for i = 0 to max do
    let old = ba'.{i} in
    Printf.printf "%h\n%!" (if Float.is_nan old then Float.nan else old);
    ba'.{i} <- old;
    if old = old && not (old = ba'.{i}) then Printf.printf "%g <> %g\n%!" old ba'.{i}
  done;
  (* Test hashing of NaN values — exercises the NaN canonical value path *)
  let nan_ba = Bigarray.Array1.create Bigarray.float16 Bigarray.c_layout 4 in
  nan_ba.{0} <- Float.nan;
  nan_ba.{1} <- Float.nan;
  nan_ba.{2} <- 1.0;
  nan_ba.{3} <- Float.nan;
  Printf.printf "hash_nan: %08x\n%!" (Hashtbl.hash nan_ba)
