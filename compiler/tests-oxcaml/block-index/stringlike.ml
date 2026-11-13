(* TEST *)

(* String-like block indices: a block of tag 2 (resp. 3, 4) whose only field
   is an offset indexes into a string (resp. bytes, bigstring).  This is how
   slices backed by strings, bytes and bigstrings are represented on
   JavaScript and Wasm.  Reads sign-extend the byte and writes keep its low
   byte, as on native; out-of-bounds accesses raise.  These go through
   [caml_get_idx_bytecode] and [caml_set_idx_bytecode] in the JavaScript and
   Wasm runtimes. *)

external get : 'base -> Obj.t -> int = "caml_get_idx_bytecode"

external set : 'base -> Obj.t -> int -> unit = "caml_set_idx_bytecode"

let idx tag i =
  let b = Obj.new_block tag 1 in
  Obj.set_field b 0 (Obj.repr i);
  b

let s = "\x00\x01\x7f\x80\x81\xfe\xff"

let test f =
  List.init (String.length s) f
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline

(* Byte [s.[i]] as a signed 8-bit integer *)
let expected i =
  let c = Char.code s.[i] in
  if c < 0x80 then c else c - 0x100

let bigstring_of_string s =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (String.length s) (String.get s)

let reads_sign_extend () =
  print_endline "reads sign-extend";
  test expected;
  (* String *)
  test (fun i -> get s (idx 2 i));
  (* Bytes *)
  let b = Bytes.of_string s in
  test (fun i -> get b (idx 3 i));
  (* Bigstring *)
  let bs = bigstring_of_string s in
  test (fun i -> get bs (idx 4 i))

let writes_keep_the_low_byte () =
  print_endline "writes keep the low byte";
  (* Bytes *)
  let b = Bytes.make (String.length s) '\000' in
  for i = 0 to String.length s - 1 do
    set b (idx 3 i) (expected i)
  done;
  test (fun i -> Char.code (Bytes.get b i));
  test (fun i -> get b (idx 3 i));
  (* Bigstring *)
  let bs = bigstring_of_string (String.make (String.length s) '\000') in
  for i = 0 to String.length s - 1 do
    set bs (idx 4 i) (expected i)
  done;
  test (fun i -> Char.code bs.{i});
  test (fun i -> get bs (idx 4 i))

let print_result f =
  match f () with
  | v -> print_endline (string_of_int v)
  | exception e -> print_endline (Printexc.to_string e)

let out_of_bounds_reads_raise () =
  print_endline "out-of-bounds reads raise";
  let len = String.length s in
  let b = Bytes.of_string s in
  let bs = bigstring_of_string s in
  List.iter
    (fun i ->
      print_result (fun () -> get s (idx 2 i));
      print_result (fun () -> get b (idx 3 i));
      print_result (fun () -> get bs (idx 4 i)))
    [ -1; len ]

let () =
  reads_sign_extend ();
  writes_keep_the_low_byte ();
  out_of_bounds_reads_raise ()
