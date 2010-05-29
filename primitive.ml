
type kind = [ `Const | `Mutable | `Mutator ]

let kinds = Hashtbl.create 37

let _ =
  List.iter (fun (nm, k) -> Hashtbl.add kinds nm k)
    [("caml_int64_float_of_bits", `Const);
     ("caml_sys_get_argv", `Const);
     ("caml_sys_get_config", `Const);
     ("caml_obj_dup", `Mutable);
     ("caml_ml_open_descriptor_in", `Mutable);
     ("caml_ml_open_descriptor_out", `Mutable);
     ("caml_nativeint_sub", `Const);
     ("caml_nativeint_shift_left", `Const);
     ("caml_js_var", `Mutable);
     ("caml_ensure_stack_capacity", `Const)]

let register p k = Hashtbl.add kinds p k

let kind nm = try Hashtbl.find kinds nm with Not_found -> `Mutator

let is_pure nm = kind nm <> `Mutator

(****)

let primitives = ref Util.StringSet.empty

let mark_used nm =
  primitives := Util.StringSet.add nm !primitives

let list_used () =
  Format.eprintf "Primitives:@.";
  Util.StringSet.iter (fun nm -> Format.eprintf "  %s@." nm) !primitives

(****)

let aliases = Hashtbl.create 17

let alias nm nm' = Hashtbl.add aliases nm nm'
let resolve nm = try Hashtbl.find aliases nm with Not_found -> nm
