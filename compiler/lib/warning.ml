open StdLabels

type t =
  [ (* Parsing bytecode *)
    `Integer_overflow
  | `Missing_debug_event
  | `Missing_cmi
  | `Effect_handlers_without_effect_backend
  | (* runtime *)
    `Missing_primitive
  | `Missing_define
  | `Missing_deps
  | `Deprecated_joo_global_object
  | `Overriding_primitive
  | `Overriding_primitive_purity
  | `Deprecated_primitive
  | `Unused_js_variable
  | `Free_variables_in_primitive
  ]

module StringTable = Hashtbl.Make (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end)

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash

  let equal (a : t) b = a = b
end)

let state = Table.create 0

let enable t = Table.add state t true

let disable t = Table.add state t false

let default = function
  (* Parsing bytecode *)
  | `Integer_overflow | `Missing_debug_event | `Missing_cmi -> true
  (* effects *)
  | `Effect_handlers_without_effect_backend -> true
  (* runtime *)
  | `Missing_primitive | `Missing_define | `Missing_deps | `Free_variables_in_primitive ->
      true
  | `Deprecated_joo_global_object -> true
  | `Overriding_primitive | `Overriding_primitive_purity -> true
  | `Deprecated_primitive -> true
  | `Unused_js_variable -> false

let all =
  [ (* Parsing bytecode *)
    `Integer_overflow
  ; `Missing_debug_event
  ; `Missing_cmi
  ; `Effect_handlers_without_effect_backend
  ; (* runtime *)
    `Missing_primitive
  ; `Missing_define
  ; `Missing_deps
  ; `Deprecated_joo_global_object
  ; `Overriding_primitive
  ; `Overriding_primitive_purity
  ; `Deprecated_primitive
  ; `Unused_js_variable
  ; `Free_variables_in_primitive
  ]

let name = function
  (* Parsing bytecode *)
  | `Integer_overflow -> "integer-overflow"
  | `Missing_debug_event -> "missing-debug-event"
  | `Missing_cmi -> "missing-cmi"
  (* effects *)
  | `Effect_handlers_without_effect_backend -> "missing-effects-backend"
  (* runtime *)
  | `Missing_primitive -> "missing-primitive"
  | `Missing_define -> "missing-define"
  | `Missing_deps -> "missing-deps"
  | `Free_variables_in_primitive -> "free-variables"
  | `Deprecated_joo_global_object -> "deprecated-joo-global-object"
  | `Overriding_primitive -> "overriding-primitive"
  | `Overriding_primitive_purity -> "overriding-primitive-purity"
  | `Deprecated_primitive -> "deprecated-primitive"
  | `Unused_js_variable -> "unused-js-vars"

let parse : string -> t option =
  let h = StringTable.create 18 in
  List.iter all ~f:(fun t ->
      let name = name t in
      (* We use the no- prefix to disable warnings *)
      assert (not (String.starts_with ~prefix:"no-" name));
      StringTable.add h name t);
  fun s -> StringTable.find_opt h s

let enabled t =
  match Table.find_opt state t with
  | Some b -> b
  | None -> default t

let quiet = ref false

let werror = ref false

let warnings = ref 0

let warn (t : t) fmt =
  Format.kasprintf
    (fun s ->
      if enabled t && not !quiet
      then (
        incr warnings;
        Format.eprintf "Warning%s: %s%!" (Printf.sprintf " [%s]" (name t)) s))
    fmt

let process_warnings () =
  if !warnings > 0 && !werror
  then (
    Format.eprintf "%s: all warnings being treated as errors@." Sys.argv.(0);
    exit 1)
