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

let debug = Debug.find "lifting_simple"

let baseline = 0 (* Depth to which the functions are lifted *)

let rec compute_depth program pc =
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc d ->
      let block = Code.Addr.Map.find pc program.blocks in
      List.fold_left block.body ~init:d ~f:(fun d i ->
          match i with
          | Let (_, Closure (_, (pc', _), _)) ->
              let d' = compute_depth program pc' in
              max d (d' + 1)
          | _ -> d))
    pc
    program.blocks
    0

let collect_free_vars program var_depth depth pc =
  let vars = ref Var.Set.empty in
  let rec traverse pc =
    Code.preorder_traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Code.Addr.Map.find pc program.blocks in
        Freevars.iter_block_free_vars
          (fun x ->
            let idx = Var.idx x in
            if idx < Array.length var_depth
            then (
              let d = var_depth.(idx) in
              assert (d >= 0);
              if d > baseline && d < depth then vars := Var.Set.add x !vars))
          block;
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (_, Closure (_, (pc', _), _)) -> traverse pc'
            | _ -> ()))
      pc
      program.blocks
      ()
  in
  traverse pc;
  !vars

let mark_bound_variables var_depth block depth =
  Freevars.iter_block_bound_vars (fun x -> var_depth.(Var.idx x) <- depth) block;
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Closure (params, _, _)) ->
          List.iter params ~f:(fun x -> var_depth.(Var.idx x) <- depth + 1)
      | _ -> ())

let starts_with_closure = function
  | Let (_, Closure _) :: _ -> true
  | _ :: _ | [] -> false

(* Replace closures to lift by lifter applications; returns definitions and names of the
   lifter functions (to be defined before the new body). *)
let rec rewrite_blocks
    ~to_lift
    ~inside_lifted
    ~var_depth
    ~st:(program, (functions : instr list), lifters)
    ~pc
    ~depth : _ * _ * Var.t Var.Map.t =
  assert (depth > 0);
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc (program, functions, lifters) ->
      let block = Code.Addr.Map.find pc program.blocks in
      mark_bound_variables var_depth block depth;
      let body, (program, functions, lifters) =
        rewrite_body
          ~to_lift
          ~inside_lifted
          ~var_depth
          ~current_contiguous:[]
          ~st:(program, functions, lifters)
          ~depth
          ~acc_instr:[]
          block.body
      in
      ( { program with blocks = Addr.Map.add pc { block with body } program.blocks }
      , functions
      , lifters ))
    pc
    program.blocks
    (program, functions, lifters)

and rewrite_body
    ~to_lift
    ~inside_lifted
    ~depth
    ~var_depth
    ~current_contiguous
    ~acc_instr
    ~(st : Code.program * instr list * Var.t Var.Map.t)
    body =
  (* We lift possibly mutually recursive closures (that are created by contiguous
     statements) together. Isolated closures are lambda-lifted normally. *)
  match body with
  | Let (f, (Closure (_, (pc', _), _) as cl)) :: rem
    when List.is_empty current_contiguous
         && (inside_lifted || Var.Set.mem f to_lift)
         && not (starts_with_closure rem) ->
      (* We lift an isolated closure *)
      if debug () then Format.eprintf "@[<v>lifting isolated closure %a@,@]" Var.print f;
      let program, functions, lifters =
        rewrite_blocks
          ~to_lift
          ~inside_lifted:(Var.Set.mem f to_lift)
          ~var_depth
          ~st
          ~pc:pc'
          ~depth:(depth + 1)
      in
      let free_vars = collect_free_vars program var_depth (depth + 1) pc' in
      if debug ()
      then (
        Format.eprintf "@[<v>free variables:@,";
        free_vars |> Var.Set.iter (fun v -> Format.eprintf "%a,@ " Var.print v);
        Format.eprintf "@]");
      let s =
        Var.Set.fold (fun x m -> Var.Map.add x (Var.fork x) m) free_vars Var.Map.empty
      in
      let program = Subst.Excluding_Binders.cont (Subst.from_map s) pc' program in
      let f' = try Var.Map.find f s with Not_found -> Var.fork f in
      let s = Var.Map.bindings (Var.Map.remove f s) in
      let f'' = Var.fork f in
      if debug ()
      then
        Format.eprintf
          "LIFT %a (depth:%d free_vars:%d inner_depth:%d)@."
          Code.Var.print
          f''
          depth
          (Var.Set.cardinal free_vars)
          (compute_depth program pc');
      let pc'' = program.free_pc in
      let bl = { params = []; body = [ Let (f', cl) ]; branch = Return f' } in
      let program =
        { program with free_pc = pc'' + 1; blocks = Addr.Map.add pc'' bl program.blocks }
      in
      (* Add to returned list of lifter functions definitions *)
      let functions =
        Let (f'', Closure (List.map s ~f:snd, (pc'', []), None)) :: functions
      in
      let lifters = Var.Map.add f f' lifters in
      rewrite_body
        ~to_lift
        ~inside_lifted
        ~current_contiguous:[]
        ~st:(program, functions, lifters)
        ~var_depth
        ~acc_instr:
          (* Replace closure with application of the lifter function *)
          (Let (f, Apply { f = f''; args = List.map ~f:fst s; exact = true }) :: acc_instr)
        ~depth
        rem
  | Let (cname, Closure (params, (pc', args), cloc)) :: rem ->
      (* More closure definitions follow: accumulate and lift later *)
      let st =
        rewrite_blocks
          ~to_lift
          ~inside_lifted:(Var.Set.mem cname to_lift)
          ~var_depth
          ~st
          ~pc:pc'
          ~depth:(depth + 1)
      in
      rewrite_body
        ~to_lift
        ~inside_lifted
        ~var_depth
        ~current_contiguous:((cname, params, pc', args, cloc) :: current_contiguous)
        ~st
        ~acc_instr
        ~depth
        rem
  | _ :: _ | [] -> (
      (* Process the accumulated closure definitions *)
      assert (
        match current_contiguous with
        | [ (f, _, _, _, _) ] -> not (Var.Set.mem f to_lift)
        | _ -> true);
      let st, acc_instr =
        match current_contiguous with
        | [] -> st, acc_instr
        | _ :: _
          when inside_lifted
               || List.exists
                    ~f:(fun (f, _, _, _, _) -> Var.Set.mem f to_lift)
                    current_contiguous ->
            (* Lift several closures at once *)
            let program, functions, lifters = st in
            let free_vars =
              List.fold_left
                current_contiguous
                ~f:(fun acc (_, _, pc, _, _) ->
                  Var.Set.union acc @@ collect_free_vars program var_depth (depth + 1) pc)
                ~init:Var.Set.empty
            in
            let s =
              Var.Set.fold
                (fun x m -> Var.Map.add x (Var.fork x) m)
                free_vars
                Var.Map.empty
            in
            let program =
              List.fold_left
                current_contiguous
                ~f:(fun program (_, _, pc, _, _) ->
                  Subst.Excluding_Binders.cont (Subst.from_map s) pc program)
                ~init:program
            in
            let f's =
              List.map current_contiguous ~f:(fun (f, _, _, _, _) ->
                  Var.(try Map.find f s with Not_found -> fork f))
            in
            let s =
              List.fold_left
                current_contiguous
                ~f:(fun s (f, _, _, _, _) -> Var.Map.remove f s)
                ~init:s
              |> Var.Map.bindings
            in
            let f_tuple = Var.fresh_n "recfuncs" in
            (if debug ()
             then
               Format.(
                 eprintf
                   "LIFT %a in tuple %a (depth:%d free_vars:%d)@,"
                   (pp_print_list ~pp_sep:pp_print_space Code.Var.print)
                   f's
                   Code.Var.print
                   f_tuple
                   depth
                   (Var.Set.cardinal free_vars)));
            let pc_tuple = program.free_pc in
            let lifted_block =
              let tuple = Var.fresh_n "tuple" in
              { params = []
              ; body =
                  List.rev_map2
                    f's
                    current_contiguous
                    ~f:(fun f' (_, params, pc, args, cloc) ->
                      Let (f', Closure (params, (pc, args), cloc)))
                  @ [ Let (tuple, Block (0, Array.of_list f's, NotArray, Immutable)) ]
              ; branch = Return tuple
              }
            in
            let program =
              { program with
                free_pc = pc_tuple + 1
              ; blocks = Addr.Map.add pc_tuple lifted_block program.blocks
              }
            in
            let functions =
              Let (f_tuple, Closure (List.map s ~f:snd, (pc_tuple, []), None))
              :: functions
            in
            let lifters =
              Var.Map.add_seq
                (List.to_seq
                @@ List.combine
                     (List.map current_contiguous ~f:(fun (f, _, _, _, _) -> f))
                     f's)
                lifters
            in
            let tuple = Var.fresh_n "tuple" in
            let rev_decl =
              List.mapi current_contiguous ~f:(fun i (f, _, _, _, _) ->
                  Let (f, Field (tuple, i, Non_float)))
            in
            ( (program, functions, lifters)
            , rev_decl
              @ Let (tuple, Apply { f = f_tuple; args = List.map ~f:fst s; exact = true })
                :: acc_instr )
        | _ :: _ ->
            (* No need to lift the accumulated closures: just keep their definitions
               unchanged *)
            let rev_decls =
              List.map current_contiguous ~f:(fun (f, params, pc, args, cloc) ->
                  Let (f, Closure (params, (pc, args), cloc)))
            in
            st, rev_decls @ acc_instr
      in
      match body with
      | [] -> List.rev acc_instr, st
      | i :: rem ->
          rewrite_body
            ~to_lift
            ~inside_lifted
            ~var_depth
            ~depth
            ~current_contiguous:[]
            ~st
            ~acc_instr:(i :: acc_instr)
            rem)

let lift ~to_lift ~pc program : program * Var.t Var.Map.t =
  let nv = Var.count () in
  let var_depth = Array.make nv (-1) in
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc (program, lifter_map) ->
      let block = Code.Addr.Map.find pc program.blocks in
      mark_bound_variables var_depth block 0;
      let program, body, lifter_map' =
        List.fold_right
          block.body
          ~init:(program, [], Var.Map.empty)
          ~f:(fun i (program, rem, lifters) ->
            match i with
            | Let (f, Closure (_, (pc', _), _)) as i ->
                let program, functions, lifters =
                  rewrite_blocks
                    ~to_lift
                    ~inside_lifted:(Var.Set.mem f to_lift)
                    ~var_depth
                    ~st:(program, [], lifters)
                    ~pc:pc'
                    ~depth:1
                in
                program, List.rev_append functions (i :: rem), lifters
            | i -> program, i :: rem, lifters)
      in
      ( { program with blocks = Addr.Map.add pc { block with body } program.blocks }
      , Var.Map.union (fun _ _ -> assert false) lifter_map lifter_map' ))
    pc
    program.blocks
    (program, Var.Map.empty)

let f ~to_lift program =
  if debug ()
  then (
    Format.eprintf "@[<v>Program before lambda lifting:@,";
    Code.Print.program Format.err_formatter (fun _ _ -> "") program;
    Format.eprintf "@]");
  let t = Timer.make () in
  let program, liftings = lift ~to_lift ~pc:program.start program in
  if Debug.find "times" () then Format.eprintf "  lambda lifting: %a@." Timer.print t;
  program, liftings
