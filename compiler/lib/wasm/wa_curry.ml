open! Stdlib
open Code
module W = Wa_ast
open Wa_code_generation

module Make (Target : Wa_target_sig.S) = struct
  open Target

  let func_type n =
    { W.params = List.init ~len:(n + 1) ~f:(fun _ -> Value.value)
    ; result = [ Value.value ]
    }

  let bind_parameters l =
    List.fold_left
      ~f:(fun l x ->
        let* _ = l in
        let* _ = add_var x in
        return ())
      ~init:(return ())
      l

  let call ?typ ~arity closure args =
    let funct = Var.fresh () in
    let* closure = tee ?typ funct closure in
    let args = args @ [ closure ] in
    let* kind, funct =
      Memory.load_function_pointer ~arity ~skip_cast:(Option.is_some typ) (load funct)
    in
    match kind with
    | `Index -> return (W.Call_indirect (func_type (List.length args), funct, args))
    | `Ref ty -> return (W.Call_ref (ty, funct, args))

  let curry_app_name n m = Printf.sprintf "curry_app %d_%d" n m

  (* ZZZ
          curry_app: load m arguments from the env;
          get (m - n) arguments as parameters;
          apply to f
     parameters : closure_{n - m}

     local.set closure_(n -1) (field 4 (local.get closure_n))

     local.set closure_(n - 1) (field 4 (local.get closure_n))
     call
        (load_func (local.get closure_0)) (field 3 (local.get closure_1)) (field 3 (local.get closure_2)) ... (local.get closure_{n - m})) (local.get x1) ... (local.get xm) (local.get closure_0))
  *)
  let curry_app ~context ~arity m ~name =
    let body =
      let args =
        List.init ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x_%d" i)) ~len:m
      in
      let* () = bind_parameters args in
      let f = Code.Var.fresh_n "f" in
      let* _ = add_var f in
      let* args' = expression_list load args in
      let* _f = load f in
      let rec loop m args closure closure_typ =
        if m = arity
        then
          let* e = call ?typ:closure_typ ~arity (load closure) (List.append args args') in
          instr (W.Push e)
        else
          let* load_arg, load_closure, closure_typ =
            Closure.curry_load ~arity m closure
          in
          let* x = load_arg in
          let closure' = Code.Var.fresh_n "f" in
          let* () = store ?typ:closure_typ closure' load_closure in
          loop (m + 1) (x :: args) closure' closure_typ
      in
      loop m [] f None
    in
    let locals, body =
      function_body ~context ~value_type:Value.value ~param_count:2 ~body
    in
    W.Function { name; exported_name = None; typ = func_type 1; locals; body }

  let curry_name n m = Printf.sprintf "curry_%d_%d" n m

  let rec curry ~context ~arity m ~name =
    assert (m > 1);
    let name', functions =
      if m = 2
      then
        let nm = Var.fresh_n (curry_app_name arity 1) in
        let func = curry_app ~context ~arity 1 ~name:nm in
        nm, [ func ]
      else
        let nm = Var.fresh_n (curry_name arity (m - 1)) in
        let functions = curry ~context ~arity (m - 1) ~name:nm in
        nm, functions
    in
    let body =
      let x = Code.Var.fresh_n "x" in
      let* _ = add_var x in
      let f = Code.Var.fresh_n "f" in
      let* _ = add_var f in
      let res = Code.Var.fresh_n "res" in
      let stack_info, stack =
        Stack.make_info ()
        |> fun info ->
        Stack.add_spilling
          info
          ~location:res
          ~stack:[]
          ~live_vars:Var.Set.empty
          ~spilled_vars:(Var.Set.of_list [ x; f ])
      in
      let ret = Code.Var.fresh_n "ret" in
      let stack_info, _ =
        Stack.add_spilling
          stack_info
          ~location:ret
          ~stack
          ~live_vars:Var.Set.empty
          ~spilled_vars:Var.Set.empty
      in
      let stack_ctx = Stack.start_function ~context stack_info in
      let* () =
        push
          (Closure.curry_allocate ~stack_ctx ~x:res ~arity m ~f:name' ~closure:f ~arg:x)
      in
      Stack.perform_spilling stack_ctx (`Instr ret)
    in
    let locals, body =
      function_body ~context ~value_type:Value.value ~param_count:2 ~body
    in
    W.Function { name; exported_name = None; typ = func_type 1; locals; body }
    :: functions

  let curry ~arity ~name = curry ~arity arity ~name

  let apply ~context ~arity ~name =
    assert (arity > 1);
    let body =
      let l =
        List.rev
          (List.init ~len:arity ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x%d" i)))
      in
      let* () = bind_parameters l in
      let f = Code.Var.fresh_n "f" in
      let* _ = add_var f in
      Memory.check_function_arity
        f
        arity
        (fun ~typ closure ->
          let* l = expression_list load l in
          call ?typ ~arity closure l)
        (let rec build_spilling_info stack_info stack live_vars acc l =
           match l with
           | [] -> stack_info, List.rev acc
           | x :: rem ->
               let live_vars = Var.Set.remove x live_vars in
               let y = Var.fresh () in
               let stack_info, stack =
                 Stack.add_spilling
                   stack_info
                   ~location:y
                   ~stack
                   ~live_vars
                   ~spilled_vars:
                     (if List.is_empty stack then live_vars else Var.Set.empty)
               in
               build_spilling_info stack_info stack live_vars ((x, y) :: acc) rem
         in
         let stack_info, l =
           build_spilling_info (Stack.make_info ()) [] (Var.Set.of_list l) [] l
         in
         let stack_ctx = Stack.start_function ~context stack_info in
         let rec build_applies y l =
           match l with
           | [] ->
               let* y = y in
               instr (Push y)
           | (x, y') :: rem ->
               let* () = Stack.perform_reloads stack_ctx (`Vars (Var.Set.singleton x)) in
               let* () = Stack.perform_spilling stack_ctx (`Instr y') in
               let* x = load x in
               Stack.kill_variables stack_ctx;
               let* () = store y' (call ~arity:1 y [ x ]) in
               build_applies (load y') rem
         in
         build_applies (load f) l)
    in
    let locals, body =
      function_body ~context ~value_type:Value.value ~param_count:(arity + 1) ~body
    in
    W.Function { name; exported_name = None; typ = func_type arity; locals; body }

  let dummy ~context ~arity ~name =
    let body =
      let l =
        List.rev
          (List.init ~len:arity ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x%d" i)))
      in
      let* () = bind_parameters l in
      let f = Code.Var.fresh_n "f" in
      let* _ = add_var f in
      let* typ, closure = Memory.load_real_closure ~arity (load f) in
      let* l = expression_list load l in
      let* e =
        call ~typ:(W.Ref { nullable = false; typ = Type typ }) ~arity (return closure) l
      in
      instr (W.Return (Some e))
    in
    let locals, body =
      function_body ~context ~value_type:Value.value ~param_count:(arity + 1) ~body
    in
    W.Function { name; exported_name = None; typ = func_type arity; locals; body }

  let f ~context =
    IntMap.iter
      (fun arity name ->
        let f = apply ~context ~arity ~name in
        context.other_fields <- f :: context.other_fields)
      context.apply_funs;
    IntMap.iter
      (fun arity name ->
        let l = curry ~context ~arity ~name in
        context.other_fields <- List.rev_append l context.other_fields)
      context.curry_funs;
    IntMap.iter
      (fun arity name ->
        let f = dummy ~context ~arity ~name in
        context.other_fields <- f :: context.other_fields)
      context.dummy_funs
end
