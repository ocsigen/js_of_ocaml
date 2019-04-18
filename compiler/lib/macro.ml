open Stdlib

let macro recurse fallthrough =
  let module J = Javascript in
  let zero, one = J.ENum "0", J.ENum "1" in
  function
  | "BLOCK", tag :: args when List.length args > 0 ->
      let tag = Some tag in
      let args = List.map ~f:(fun a -> Some (recurse a)) args in
      J.EArr (tag :: args)
  | "TAG", [e] -> J.EAccess (recurse e, zero)
  | "LENGTH", [e] ->
      let underlying = J.EDot (recurse e, "length") in
      J.EBin (J.Minus, underlying, one)
  | "FIELD", [e; J.ENum n] ->
      let idx = int_of_string n in
      let adjusted = J.ENum (string_of_int (idx + 1)) in
      J.EAccess (recurse e, adjusted)
  | "FIELD", [_; J.EUn (J.Neg, _)] -> failwith "Negative field indexes are not allowed"
  | "FIELD", [e; idx] ->
      let adjusted = J.EBin (J.Plus, one, recurse idx) in
      J.EAccess (recurse e, adjusted)
  | "ISBLOCK", [e] ->
      J.EBin (J.NotEqEq, J.EUn (J.Typeof, recurse e), J.EStr ("number", `Utf8))
  | ("BLOCK", _ | "TAG", _ | "LENGTH", _ | "FIELD", _ | "ISBLOCK", _) as s ->
      let s, _ = s in
      failwith (Format.sprintf "macro %s called with inappropriate arguments" s)
  | _ -> fallthrough ()

class macro_mapper =
  object (m)
    inherit Js_traverse.map as super

    method expression x =
      let module J = Javascript in
      let fallthrough () = super#expression x in
      let recurse = m#expression in
      match x with
      | J.ECall (J.EVar (J.S {name; _}), args, _) ->
          macro recurse fallthrough (name, args)
      | _ -> fallthrough ()
  end

let f js =
  let trav = new macro_mapper in
  trav#program js
