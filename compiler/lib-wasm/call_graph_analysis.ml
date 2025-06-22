open! Stdlib
open Code

let debug = Debug.find "call-graph"

let times = Debug.find "times"

let block_deps ~info ~non_escaping ~ambiguous ~blocks pc =
  let block = Addr.Map.find pc blocks in
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Apply { f; _ }) -> (
          try
            match Var.Tbl.get info.Global_flow.info_approximation f with
            | Top -> ()
            | Values { known; others } ->
                if others || Var.Set.cardinal known > 1
                then Var.Set.iter (fun x -> Var.Hashtbl.replace ambiguous x ()) known;
                if debug ()
                then
                  Format.eprintf
                    "CALL others:%b known:%d@."
                    others
                    (Var.Set.cardinal known)
          with Invalid_argument _ -> ())
      | Let (x, Closure _) -> (
          match Var.Tbl.get info.Global_flow.info_approximation x with
          | Top -> ()
          | Values { known; others } ->
              if Var.Set.cardinal known = 1 && (not others) && Var.Set.mem x known
              then (
                let may_escape = Var.ISet.mem info.Global_flow.info_may_escape x in
                if debug () then Format.eprintf "CLOSURE may-escape:%b@." may_escape;
                if not may_escape then Var.Hashtbl.replace non_escaping x ()))
      | Let (_, (Prim _ | Block _ | Constant _ | Field _ | Special _))
      | Event _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ())

type t =
  { unambiguous_non_escaping : unit Var.Hashtbl.t
  ; has_tail_calls : unit Var.Hashtbl.t
  }

let direct_calls_only info f =
  Config.Flag.optcall () && Var.Hashtbl.mem info.unambiguous_non_escaping f

let has_tail_calls info f = Var.Hashtbl.mem info.has_tail_calls f

let f p info =
  let t = Timer.make () in
  let non_escaping = Var.Hashtbl.create 128 in
  let ambiguous = Var.Hashtbl.create 128 in
  let has_tail_calls = Var.Hashtbl.create 128 in
  fold_closures
    p
    (fun name_opt _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () -> block_deps ~info ~non_escaping ~ambiguous ~blocks:p.blocks pc)
        pc
        p.blocks
        ();
      Option.iter
        ~f:(fun f ->
          traverse
            { fold = Code.fold_children }
            (fun pc () ->
              let block = Addr.Map.find pc p.blocks in
              match block.branch with
              | Return x -> (
                  match last_instr block.body with
                  | Some (Let (x', Apply _)) when Code.Var.equal x x' ->
                      Var.Hashtbl.replace has_tail_calls f ()
                  | _ -> ())
              | _ -> ())
            pc
            p.blocks
            ())
        name_opt)
    ();
  if debug ()
  then Format.eprintf "SUMMARY non-escaping:%d" (Var.Hashtbl.length non_escaping);
  Var.Hashtbl.iter (fun x () -> Var.Hashtbl.remove non_escaping x) ambiguous;
  if debug ()
  then Format.eprintf " unambiguous-non-escaping:%d@." (Var.Hashtbl.length non_escaping);
  if times () then Format.eprintf "  call graph analysis: %a@." Timer.print t;
  { unambiguous_non_escaping = non_escaping; has_tail_calls }
