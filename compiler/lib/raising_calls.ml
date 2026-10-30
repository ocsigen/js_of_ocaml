(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Stdlib
open Code

let debug = Debug.find "raising-calls"

let times = Debug.find "times"

type t =
  { raising_funcs : Var.Set.t
  ; raising_calls : Var.Set.t
  }

let empty = { raising_funcs = Var.Set.empty; raising_calls = Var.Set.empty }

let enabled () =
  match Config.target (), Config.effects () with
  | `JavaScript, `Disabled -> Config.Flag.exn_sentinel () && Config.Flag.optcall ()
  | `JavaScript, (`Cps | `Double_translation | `Jspi | `Native)
  | `Wasm, (`Disabled | `Cps | `Double_translation | `Jspi | `Native) -> false

let f p info =
  let t = Timer.make () in
  let call_info = Call_graph_analysis.f p info in
  let known_calls = ref [] in
  let raising =
    Call_graph_analysis.raising_functions
      ~on_call:(fun h pc x ~f g ~in_handler ->
        known_calls := (h, pc, x, f, g, in_handler) :: !known_calls)
      p
      info
      call_info
      ~wrappable:(fun _ -> true)
      ~tail_calls:false
      (fun _ -> true)
  in
  (* The bodies of the raising functions which can be called through
     closures *)
  let bodies = Var.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun f () ->
      if not (Call_graph_analysis.direct_calls_only call_info f)
      then Var.Hashtbl.replace bodies f (Var.fork f))
    raising;
  (* The calls to raising functions, with the function called and
     where the call is performed, and the blocks containing them *)
  let calls = Var.Hashtbl.create 128 in
  let call_blocks = Addr.Hashtbl.create 128 in
  (* Raising functions only called directly get a wrapper as well
     when it is called directly at enough call sites outside handlers,
     in non-raising functions: this is shorter than testing the result
     at each call site. *)
  let throw_counts = Var.Hashtbl.create 16 in
  (* Wrapped functions whose body is retrieved through the wrapper at
     some call site *)
  let body_props = Var.Hashtbl.create 16 in
  List.iter !known_calls ~f:(fun (h, pc, x, f, g, in_handler) ->
      if Var.Hashtbl.mem raising g
      then (
        let context =
          if in_handler
          then `Handler
          else
            match h with
            | Some h when Var.Hashtbl.mem raising h -> (
                let block = Addr.Map.find pc p.blocks in
                match block.branch, last_instr block.body with
                | Return y, Some (Let (x', _)) when Var.equal x y && Var.equal x x' ->
                    `Tail
                | _ -> `Return)
            | Some _ | None -> `Throw
        in
        Var.Hashtbl.replace calls x (g, context);
        Addr.Hashtbl.replace call_blocks pc ();
        if Poly.equal context `Throw
        then (
          if Var.equal f g && not (Var.Hashtbl.mem bodies g)
          then
            Var.Hashtbl.replace
              throw_counts
              g
              (1 + Option.value ~default:0 (Var.Hashtbl.find_opt throw_counts g)))
        else if (not (Var.equal f g)) && Var.Hashtbl.mem bodies g
        then Var.Hashtbl.replace body_props g ()));
  let throw_wrappers = Var.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun f n -> if n >= 3 then Var.Hashtbl.replace throw_wrappers f (Var.fork f))
    throw_counts;
  let raising_calls = ref Var.Set.empty in
  let free_pc = ref p.free_pc in
  let new_blocks = ref [] in
  let rewrite_call x f args exact =
    match Var.Hashtbl.find_opt calls x with
    | Some (g, context) -> (
        match context, Var.Hashtbl.find_opt bodies g with
        | `Throw, Some _ ->
            (* The wrapper throws the exception *)
            [ Let (x, Apply { f; args; exact }) ]
        | `Throw, None when Var.Hashtbl.mem throw_wrappers f ->
            [ Let (x, Apply { f = Var.Hashtbl.find throw_wrappers f; args; exact }) ]
        | (`Throw | `Return | `Handler | `Tail), None ->
            (* A sentinel value returned by a tail call is returned as is *)
            if not (Poly.equal context `Tail)
            then raising_calls := Var.Set.add x !raising_calls;
            [ Let (x, Apply { f; args; exact }) ]
        | (`Return | `Handler | `Tail), Some body ->
            if not (Poly.equal context `Tail)
            then raising_calls := Var.Set.add x !raising_calls;
            if Var.equal f g
            then [ Let (x, Apply { f = body; args; exact }) ]
            else
              (* The body is only in scope where the function is: get
                 it from the wrapper *)
              let f' = Var.fork f in
              [ Let (f', Prim (Extern ("%caml_raising_body", None), [ Pv f ]))
              ; Let (x, Apply { f = f'; args; exact })
              ])
    | None -> [ Let (x, Apply { f; args; exact }) ]
  in
  let wrapper params cloc callee =
    let params' = List.map ~f:Var.fork params in
    let res = Var.fresh () in
    let pc = !free_pc in
    incr free_pc;
    new_blocks :=
      ( pc
      , { params = []
        ; body = [ Let (res, Apply { f = callee; args = params'; exact = true }) ]
        ; branch = Return res
        } )
      :: !new_blocks;
    raising_calls := Var.Set.add res !raising_calls;
    Closure (params', (pc, []), cloc)
  in
  (* The wrappers are given their bodies after a whole group of
     closures, which may be mutually recursive (an event may occur
     between two closures of a group, see [Generate.collect_closures]).
     The rewritten instructions [acc] are in reverse order, as are
     [closures] and [setters]. *)
  let rec rewrite_body l closures setters acc =
    let flush acc = setters @ closures @ acc in
    match l with
    | (Let (g, Closure (params, cont, cloc)) as i) :: rem -> (
        match Var.Hashtbl.find_opt bodies g, Var.Hashtbl.find_opt throw_wrappers g with
        | Some body, _ ->
            let setters =
              if Var.Hashtbl.mem body_props g
              then
                Let
                  ( Var.fresh ()
                  , Prim (Extern ("%caml_set_raising_body", None), [ Pv g; Pv body ]) )
                :: setters
              else setters
            in
            rewrite_body
              rem
              (Let (g, wrapper params cloc body)
              :: Let (body, Closure (params, cont, cloc))
              :: closures)
              setters
              acc
        | None, Some g' ->
            rewrite_body
              rem
              (Let (g', wrapper params cloc g) :: i :: closures)
              setters
              acc
        | None, None -> rewrite_body rem (i :: closures) setters acc)
    | (Event _ as i) :: (Let (_, Closure _) :: _ as rem) ->
        rewrite_body rem (i :: closures) setters acc
    | Let (x, Apply { f; args; exact }) :: rem ->
        rewrite_body rem [] [] (List.rev_append (rewrite_call x f args exact) (flush acc))
    | i :: rem -> rewrite_body rem [] [] (i :: flush acc)
    | [] -> List.rev (flush acc)
  in
  let blocks =
    if Var.Hashtbl.length raising = 0
    then p.blocks
    else
      (* Only the blocks containing calls to raising functions or
         wrapped functions are changed *)
      let wrapped g = Var.Hashtbl.mem bodies g || Var.Hashtbl.mem throw_wrappers g in
      Addr.Map.fold
        (fun pc block blocks ->
          if
            Addr.Hashtbl.mem call_blocks pc
            || List.exists block.body ~f:(fun i ->
                match i with
                | Let (g, Closure _) -> wrapped g
                | _ -> false)
          then
            Addr.Map.add pc { block with body = rewrite_body block.body [] [] [] } blocks
          else blocks)
        p.blocks
        p.blocks
  in
  let blocks =
    List.fold_left
      ~f:(fun blocks (pc, block) -> Addr.Map.add pc block blocks)
      ~init:blocks
      !new_blocks
  in
  let raising_funcs =
    Var.Hashtbl.fold
      (fun f () s ->
        Var.Set.add
          (match Var.Hashtbl.find_opt bodies f with
          | Some body -> body
          | None -> f)
          s)
      raising
      Var.Set.empty
  in
  if debug ()
  then
    Format.eprintf
      "raising functions: %d (wrapped: %d), raising calls: %d@."
      (Var.Set.cardinal raising_funcs)
      (Var.Hashtbl.length bodies)
      (Var.Set.cardinal !raising_calls);
  if times () then Format.eprintf "  raising calls: %a@." Timer.print t;
  { p with blocks; free_pc = !free_pc }, { raising_funcs; raising_calls = !raising_calls }
