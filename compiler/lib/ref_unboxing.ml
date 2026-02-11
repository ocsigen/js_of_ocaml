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

(*
ocamlc does not perform reference unboxing when emitting debugging
information. Inlining can also enable additional reference unboxing.

We currently do not unbox references which are used within the scope
of an exception handler. This should often not result in significant
performance improvements, and is tricky to get right. Indeed, we would
need to introduce variables for these references right before the
[Pushtrap], and then add [Assign] instructions to keep their contents
up to date whenever a reference is updated.
*)

let debug = Debug.find "unbox-refs"

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

let rewrite_body unboxed_refs body ref_contents subst =
  let ref_contents, subst, l =
    List.fold_left
      ~f:(fun (ref_contents, subst, acc) i ->
        match i with
        | Let (x, Block (0, [| y |], (NotArray | Unknown), Maybe_mutable))
          when Var.Set.mem x unboxed_refs -> Var.Map.add x y ref_contents, subst, acc
        | Let (y, Field (x, 0, Non_float)) when Var.Map.mem x ref_contents ->
            ref_contents, Var.Map.add y (Var.Map.find x ref_contents) subst, acc
        | Offset_ref (x, n) when Var.Map.mem x ref_contents ->
            let y = Var.fork x in
            ( Var.Map.add x y ref_contents
            , subst
            , Let
                ( y
                , Prim
                    ( Extern "%int_add"
                    , [ Pv (Var.Map.find x ref_contents)
                      ; Pc (Int (Targetint.of_int_exn n))
                      ] ) )
              :: acc )
        | Set_field (x, 0, Non_float, y) when Var.Map.mem x ref_contents ->
            Var.Map.add x y ref_contents, subst, acc
        | Event _ -> (
            ( ref_contents
            , subst
            , match acc with
              | Event _ :: prev ->
                  (* Avoid consecutive events (keep just the last one) *)
                  i :: prev
              | _ -> i :: acc ))
        | _ -> ref_contents, subst, i :: acc)
      body
      ~init:(ref_contents, subst, [])
  in
  ref_contents, subst, List.rev l

let rewrite_cont relevant_vars ref_contents (pc', args) =
  let refs, _ = Int.Hashtbl.find relevant_vars pc' in
  let vars = Var.Map.filter (fun x _ -> Var.Set.mem x refs) ref_contents in
  pc', List.map ~f:snd (Var.Map.bindings vars) @ args

let rewrite_branch relevant_vars ref_contents branch =
  match branch with
  | Return _ | Raise _ | Stop -> branch
  | Branch cont -> Branch (rewrite_cont relevant_vars ref_contents cont)
  | Cond (x, cont, cont') ->
      Cond
        ( x
        , rewrite_cont relevant_vars ref_contents cont
        , rewrite_cont relevant_vars ref_contents cont' )
  | Switch (x, a) ->
      Switch (x, Array.map ~f:(fun cont -> rewrite_cont relevant_vars ref_contents cont) a)
  | Pushtrap (cont, x, cont') ->
      Pushtrap
        ( rewrite_cont relevant_vars ref_contents cont
        , x
        , rewrite_cont relevant_vars ref_contents cont' )
  | Poptrap cont -> Poptrap (rewrite_cont relevant_vars ref_contents cont)

let rewrite_function p ~unboxed_refs pc subst =
  let g = Structure.(dominator_tree (build_graph p.blocks pc)) in
  let relevant_vars =
    let relevant_vars = Int.Hashtbl.create 16 in
    let rec traverse_tree g pc refs =
      let block = Addr.Map.find pc p.blocks in
      let refs' =
        List.fold_left
          ~f:(fun s i ->
            match i with
            | Let (x, Block (0, [| _ |], (NotArray | Unknown), Maybe_mutable))
              when Var.Hashtbl.mem unboxed_refs x -> Var.Set.add x s
            | _ -> s)
          ~init:refs
          block.body
      in
      Int.Hashtbl.add relevant_vars pc (refs, refs');
      Addr.Set.iter (fun pc' -> traverse_tree g pc' refs') (Structure.get_edges g pc)
    in
    traverse_tree g pc Var.Set.empty;
    relevant_vars
  in
  let rec traverse_tree' g pc blocks subst =
    let block = Addr.Map.find pc p.blocks in
    let refs, refs' = Int.Hashtbl.find relevant_vars pc in
    let ref_contents =
      Var.Set.fold (fun x m -> Var.Map.add x (Var.fork x) m) refs Var.Map.empty
    in
    let params = List.map ~f:snd (Var.Map.bindings ref_contents) @ block.params in
    let ref_contents, subst, body = rewrite_body refs' block.body ref_contents subst in
    let branch = rewrite_branch relevant_vars ref_contents block.branch in
    let blocks = Addr.Map.add pc { params; body; branch } blocks in
    Addr.Set.fold
      (fun pc' (blocks, subst) -> traverse_tree' g pc' blocks subst)
      (Structure.get_edges g pc)
      (blocks, subst)
  in
  let blocks, subst = traverse_tree' g pc p.blocks subst in
  { p with blocks }, subst

let f p =
  let previous_p = p in
  let t = Timer.make () in
  let candidates = Var.Hashtbl.create 128 in
  let updated = Var.Hashtbl.create 128 in
  let visited = BitSet.create' p.free_pc in
  let discard x = Var.Hashtbl.remove candidates x in
  let check_field_access depth x =
    match Var.Hashtbl.find candidates x with
    | exception Not_found -> false
    | depth' ->
        if depth' = depth
        then true
        else (
          Var.Hashtbl.remove candidates x;
          false)
  in
  (* A reference can be defined within the scope of an exception
     handler and used within the scope of another exception handler.
     So exception handlers should have strictly increasing depths
     within a function. [max_depth] is the largest depth used so far
     inside a function. This way, we know which depth to use when
     entering the scope of an exception handler. We use [depth_stack]
     to restore the previous depth when leaving the scope of an
     exception handler. *)
  let rec traverse depth_stack max_depth depth start_pc pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, Block (0, [| _ |], (NotArray | Unknown), Maybe_mutable)) ->
              Freevars.iter_instr_free_vars discard i;
              Var.Hashtbl.replace candidates x depth
          | Let (_, Closure (_, (pc', _), _)) ->
              traverse [] (max_depth + 1) (max_depth + 1) pc' pc'
          | Let (_, Field (x, 0, Non_float)) -> ignore (check_field_access depth x)
          | Offset_ref (x, _) ->
              if check_field_access depth x then Var.Hashtbl.replace updated x start_pc
          | Set_field (x, _, Non_float, y) ->
              discard y;
              if check_field_access depth x then Var.Hashtbl.replace updated x start_pc
          | _ -> Freevars.iter_instr_free_vars discard i)
        block.body;
      Freevars.iter_last_free_var discard block.branch;
      match block.branch with
      | Pushtrap ((pc', _), _, (pc'', _)) ->
          traverse (depth :: depth_stack) (max_depth + 1) (max_depth + 1) start_pc pc';
          traverse depth_stack max_depth depth start_pc pc''
      | Poptrap (pc', _) ->
          traverse (List.tl depth_stack) max_depth (List.hd depth_stack) start_pc pc'
      | _ ->
          Code.fold_children
            p.blocks
            pc
            (fun pc' () -> traverse depth_stack max_depth depth start_pc pc')
            ())
  in
  traverse [] 0 0 p.start p.start;
  if debug ()
  then
    Print.program
      Format.err_formatter
      (fun _ i ->
        match i with
        | Instr (Let (x, _))
          when Var.Hashtbl.mem candidates x && Var.Hashtbl.mem updated x -> "REF"
        | _ -> "")
      p;
  Var.Hashtbl.filter_map_inplace
    (fun x _depth -> Var.Hashtbl.find_opt updated x)
    candidates;
  let functions =
    Var.Hashtbl.fold (fun _ pc s -> Addr.Set.add pc s) candidates Addr.Set.empty
  in
  let p, subst =
    Addr.Set.fold
      (fun pc (p, subst) -> rewrite_function p ~unboxed_refs:candidates pc subst)
      functions
      (p, Var.Map.empty)
  in
  let p =
    if Var.Map.is_empty subst
    then p
    else Subst.Excluding_Binders.program (Subst.from_map subst) p
  in
  if times () then Format.eprintf "  reference unboxing: %a@." Timer.print t;
  let updates = Var.Hashtbl.length candidates in
  if stats ()
  then (
    Format.eprintf "Stats - reference unboxing: %d@." updates;
    Code.print_block_sharing ~name:"ref_unboxing" previous_p p);
  if debug_stats ()
  then Code.check_updates ~name:"ref_unboxing" previous_p p ~updates;
  p
