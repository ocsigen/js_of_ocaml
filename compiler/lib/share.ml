open! Stdlib
open Code

module T = Hashtbl.Make (struct
  type t = constant

  let equal = Poly.equal

  let hash = Hashtbl.hash
end)

let f (p : program) : program =
  let size = Code.Var.count () in
  let subst = Array.init size ~f:(fun s -> Var.of_idx s) in
  Addr.Map.iter
    (fun _ block ->
      let t = T.create 7 in
      List.iter block.body ~f:(function
        | Let (x, Constant c) -> (
            match T.find_opt t c with
            | None -> T.add t c x
            | Some y -> subst.(Var.idx x) <- y)
        | _ -> ()))
    p.blocks;
  Subst.Excluding_Binders.program (Subst.from_array subst) p
