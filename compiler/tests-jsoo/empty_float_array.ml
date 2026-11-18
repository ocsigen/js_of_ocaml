let empty = [||]

let check_empty_array (a : float array) =
  let a = Sys.opaque_identity a in
  assert (Array.length a = 0);
  assert (Obj.tag (Obj.repr a) = 0);
  assert (a = empty)

let%expect_test "empty array constant at float array type" = check_empty_array empty

let%expect_test "empty float array literal" = check_empty_array [||]

let%expect_test "recursive empty float array" =
  let rec a : float array =
    let _ = a in
    [||]
  in
  check_empty_array a

let%expect_test "empty Array.concat" =
  check_empty_array (Array.concat []);
  check_empty_array (Array.concat [ empty; empty ])

(* From OCaml 5.3 to 5.4, the C runtime gives empty results of
   [Float.Array.sub], [append] and, from 5.4, [concat] tag 254, so that they
   are not equal to [Float.Array.create 0] (fixed upstream by
   ocaml/ocaml#14635, in 5.5.0). The OxCaml runtime is not affected. *)
let buggy_native_concat = false
[@@if not (ocaml_version >= (5, 4, 0) && ocaml_version < (5, 5, 0) && not oxcaml)]

let buggy_native_concat = true
[@@if ocaml_version >= (5, 4, 0) && ocaml_version < (5, 5, 0) && not oxcaml]

let%expect_test "empty Float.Array.concat" =
  let empty = Float.Array.create 0 in
  let check a =
    assert (Float.Array.length a = 0);
    match Sys.backend_type with
    | (Sys.Native | Sys.Bytecode) when buggy_native_concat -> ()
    | Sys.Native | Sys.Bytecode | Sys.Other _ ->
        assert (a = empty);
        assert (Obj.tag (Obj.repr a) = 0)
  in
  check (Float.Array.concat []);
  check (Float.Array.concat [ empty; empty ])
