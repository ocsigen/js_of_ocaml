open Stdlib

let zero, one = Javascript.ENum "0", Javascript.ENum "1"

class macro_mapper =
  object (m)
    inherit Js_traverse.map as super

    method expression x =
      let module J = Javascript in
      match x with
      | J.ECall (J.EVar (J.S {name; _}), args, _) -> (
        match name, args with
        | "BLOCK", tag :: (_ :: _ as args) ->
            let tag = Some tag in
            let args = List.map ~f:(fun a -> Some (m#expression a)) args in
            J.EArr (tag :: args)
        | "TAG", [e] -> J.EAccess (m#expression e, zero)
        | "LENGTH", [e] ->
            let underlying = J.EDot (m#expression e, "length") in
            J.EBin (J.Minus, underlying, one)
        | "FIELD", [e; J.ENum n] ->
            let idx = int_of_string n in
            let adjusted = J.ENum (string_of_int (idx + 1)) in
            J.EAccess (m#expression e, adjusted)
        | "FIELD", [_; J.EUn (J.Neg, _)] ->
            failwith "Negative field indexes are not allowed"
        | "FIELD", [e; idx] ->
            let adjusted = J.EBin (J.Plus, one, m#expression idx) in
            J.EAccess (m#expression e, adjusted)
        | "ISBLOCK", [e] ->
            J.EBin (J.NotEqEq, J.EUn (J.Typeof, m#expression e), J.EStr ("number", `Utf8))
        | ("BLOCK" | "TAG" | "LENGTH" | "FIELD" | "ISBLOCK"), _ ->
            failwith (Format.sprintf "macro %s called with inappropriate arguments" name)
        | _ -> super#expression x)
      | _ -> super#expression x
  end

let f js =
  let trav = new macro_mapper in
  trav#program js
