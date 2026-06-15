(* Regression test for the js/wasm BLAKE2 runtime: a key longer than 64
   bytes must be accepted and truncated to its first 64 bytes.

   The [caml_blake2_create] primitive used to compute [key.subarray(0, 64)]
   but discard the result, so the over-long key reached the BLAKE2b
   initialisation unchanged and the JS runtime threw "Illegal key" instead
   of producing a digest.

   BLAKE2b keys are at most 64 bytes; the underlying state only copies the
   first 64. The jsoo runtime truncates the key to that prefix, so hashing
   with a long key is equivalent to hashing with its 64-byte prefix. (This
   equivalence is jsoo-specific: the native runtime keeps the original key
   length in the parameter block, hence [modes js wasm] only.) *)

type state

external create_gen : int -> string -> state = "caml_blake2_create"
external update : state -> bytes -> int -> int -> unit = "caml_blake2_update"
external final : state -> int -> string = "caml_blake2_final"

let digest_keyed key msg =
  let st = create_gen 64 key in
  let b = Bytes.of_string msg in
  update st b 0 (Bytes.length b);
  final st 64

let () =
  let key100 = String.init 100 (fun i -> Char.chr (i land 0xff)) in
  let key64 = String.sub key100 0 64 in
  let msg = "the quick brown fox" in
  (* Used to raise "Illegal key" in the JS runtime before the fix. *)
  let h_long = digest_keyed key100 msg in
  let h_trunc = digest_keyed key64 msg in
  assert (String.length h_long = 64);
  assert (String.equal h_long h_trunc);
  print_string "ok\n"
