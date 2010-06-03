
let aliases = Hashtbl.create 17

let alias nm nm' = Hashtbl.add aliases nm nm'
let resolve nm = try Hashtbl.find aliases nm with Not_found -> nm

(****)

type kind = [ `Const | `Mutable | `Mutator ]

let kinds = Hashtbl.create 37

let register p k = Hashtbl.add kinds p k

let kind nm = try Hashtbl.find kinds (resolve nm) with Not_found -> `Mutator

let is_pure nm = kind nm <> `Mutator

(****)

let primitives = ref Util.StringSet.empty

let mark_used nm =
  primitives := Util.StringSet.add nm !primitives

let list_used () =
  Format.eprintf "Primitives:@.";
  Util.StringSet.iter (fun nm -> Format.eprintf "  %s@." nm) !primitives

let get_used () = Util.StringSet.elements !primitives
