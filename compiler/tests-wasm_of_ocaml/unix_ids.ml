(* Regression test for the wasm unix.wat id primitives.

   - There was no [caml_unix_getegid] stub, so [Unix.getegid] was a
     missing primitive.
   - [Unix.getgrgid] was exported under the typo [caml_unix_getgruid],
     so it too was a missing primitive (it should resolve and, with no
     group database in the Wasm runtime, raise [Not_found]). *)

let () =
  let _ : int = Unix.getegid () in
  match Unix.getgrgid (Unix.getgid ()) with
  | (_ : Unix.group_entry) -> assert false
  | exception Not_found -> ()
