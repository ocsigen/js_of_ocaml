
(*XXX Should be extensible... *)
let pure_prims =
  ["caml_int64_float_of_bits"; "caml_sys_get_argv"; "caml_sys_get_config";
   "caml_obj_dup"; "caml_ml_open_descriptor_in"; "caml_ml_open_descriptor_out";
   "caml_nativeint_sub"; "caml_nativeint_shift_left"]

let is_pure f = List.mem f pure_prims

(****)

let primitives = ref Util.StringSet.empty

let mark_used nm =
  primitives := Util.StringSet.add nm !primitives;
  Code.add_reserved_name nm  (*XXX HACK *)

let list_used () =
  Format.eprintf "Primitives:@.";
  Util.StringSet.iter (fun nm -> Format.eprintf "  %s@." nm) !primitives
