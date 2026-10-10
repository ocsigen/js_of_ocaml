(* As in OCaml, arrays have at most [Max_wosize] elements, and float arrays
   half as many: the runtime functions creating an array of arbitrary
   length check this bound, rather than letting the engine fail to allocate
   the array. *)

(* An opaque integer *)
let o x = Sys.opaque_identity x

let test name f =
  match f () with
  | s -> Printf.printf "%s: %s\n" name s
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %s\n" name s
  | exception Out_of_memory -> Printf.printf "%s: Out_of_memory\n" name

(* Without the check of [Weak.create] *)
external weak_create : int -> 'a Weak.t = "caml_weak_create"

let () =
  test "limits" (fun () ->
      string_of_bool (Sys.max_floatarray_length = Sys.max_array_length / 2));
  test "Array.append" (fun () ->
      string_of_int (Array.length (Array.append (Array.make (o 3) 0) [| 1; 2 |])));
  test "Array.concat" (fun () ->
      string_of_int (Array.length (Array.concat [ [| 1. |]; [||]; [| 2.; 3. |] ])));
  test "Obj.new_block" (fun () -> string_of_int (Obj.size (Obj.new_block 0 (o 5))));
  test "too large array" (fun () ->
      string_of_int (Array.length (Array.make (o (Sys.max_array_length + 1)) 0)));
  test "too large float array" (fun () ->
      (* The JavaScript runtime cannot tell a float from an integer, so
         [Array.make] builds a generic array there *)
      let max =
        match Sys.backend_type with
        | Other "js_of_ocaml" -> Sys.max_array_length
        | _ -> Sys.max_floatarray_length
      in
      string_of_int (Array.length (Array.make (o (max + 1)) 0.)));
  test "too large Float.Array" (fun () ->
      string_of_int
        (Float.Array.length (Float.Array.create (o (Sys.max_floatarray_length + 1)))));
  test "too large block" (fun () ->
      string_of_int (Obj.size (Obj.new_block 0 (o (Sys.max_array_length + 1)))));
  test "too large float block" (fun () ->
      string_of_int
        (Obj.size
           (Obj.new_block Obj.double_array_tag (o (Sys.max_floatarray_length + 1)))));
  test "too large weak array" (fun () ->
      string_of_int (Weak.length (weak_create (o (Sys.max_array_length - 1)))))
