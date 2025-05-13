(* Wasm_of_ocaml compiler
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
module W = Wasm_ast
open Code_generation

module Make (Target : Target_sig.S) = struct
  open Target

  let bind_parameters l =
    List.fold_left
      ~f:(fun l x ->
        let* _ = l in
        let* _ = add_var x in
        return ())
      ~init:(return ())
      l

  let call ?typ ~cps ~arity closure args =
    let funct = Var.fresh () in
    let* closure = tee ?typ funct closure in
    let args = args @ [ closure ] in
    let* ty, funct =
      Memory.load_function_pointer
        ~cps
        ~arity
        ~skip_cast:(Option.is_some typ)
        (load funct)
    in
    return (W.Call_ref (ty, funct, args))

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
    let args =
      List.init ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x_%d" i)) ~len:m
    in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* () = bind_parameters args in
      let* _ = add_var f in
      let* args' = expression_list load args in
      let* _f = load f in
      let rec loop m args closure closure_typ =
        if m = arity
        then
          let* e =
            call
              ?typ:closure_typ
              ~cps:false
              ~arity
              (load closure)
              (List.append args args')
          in
          instr (W.Push e)
        else
          let* load_arg, load_closure, closure_typ =
            Closure.curry_load ~cps:false ~arity m closure
          in
          let* x = load_arg in
          let closure' = Code.Var.fresh_n "f" in
          let* () = store ?typ:closure_typ closure' load_closure in
          loop (m + 1) (x :: args) closure' closure_typ
      in
      loop m [] f None
    in
    let param_names = args @ [ f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.func_type 1
      ; param_names
      ; locals
      ; body
      }

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
    let x = Code.Var.fresh_n "x" in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* _ = add_var x in
      let* _ = add_var f in
      push (Closure.curry_allocate ~cps:false ~arity m ~f:name' ~closure:f ~arg:x)
    in
    let param_names = [ x; f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.func_type 1
      ; param_names
      ; locals
      ; body
      }
    :: functions

  let curry ~arity ~name = curry ~arity arity ~name

  let cps_curry_app_name n m = Printf.sprintf "cps_curry_app %d_%d" n m

  let cps_curry_app ~context ~arity m ~name =
    let args =
      List.init ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x_%d" i)) ~len:(m + 1)
    in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* () = bind_parameters args in
      let* _ = add_var f in
      let* args' = expression_list load args in
      let* _f = load f in
      let rec loop m args closure closure_typ =
        if m = arity
        then
          let* e =
            call
              ?typ:closure_typ
              ~cps:true
              ~arity:(arity + 1)
              (load closure)
              (List.append args args')
          in
          instr (W.Push e)
        else
          let* load_arg, load_closure, closure_typ =
            Closure.curry_load ~cps:true ~arity m closure
          in
          let* x = load_arg in
          let closure' = Code.Var.fresh_n "f" in
          let* () = store ?typ:closure_typ closure' load_closure in
          loop (m + 1) (x :: args) closure' closure_typ
      in
      loop m [] f None
    in
    let param_names = args @ [ f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.func_type 2
      ; param_names
      ; locals
      ; body
      }

  let cps_curry_name n m = Printf.sprintf "cps_curry_%d_%d" n m

  let rec cps_curry ~context ~arity m ~name =
    assert (m > 1);
    let name', functions =
      if m = 2
      then
        let nm = Var.fresh_n (cps_curry_app_name arity 1) in
        let func = cps_curry_app ~context ~arity 1 ~name:nm in
        nm, [ func ]
      else
        let nm = Var.fresh_n (cps_curry_name arity (m - 1)) in
        let functions = cps_curry ~context ~arity (m - 1) ~name:nm in
        nm, functions
    in
    let x = Code.Var.fresh_n "x" in
    let cont = Code.Var.fresh_n "cont" in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* _ = add_var x in
      let* _ = add_var cont in
      let* _ = add_var f in
      let* e = Closure.curry_allocate ~cps:true ~arity m ~f:name' ~closure:f ~arg:x in
      let* c = call ~cps:false ~arity:1 (load cont) [ e ] in
      instr (W.Return (Some c))
    in
    let param_names = [ x; cont; f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.func_type 2
      ; param_names
      ; locals
      ; body
      }
    :: functions

  let cps_curry ~arity ~name = cps_curry ~arity arity ~name

  let apply ~context ~arity ~name =
    assert (arity > 1);
    let l =
      List.rev
        (List.init ~len:arity ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x%d" i)))
    in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* () = bind_parameters l in
      let* _ = add_var f in
      Memory.check_function_arity
        f
        ~cps:false
        ~arity
        (fun ~typ closure ->
          let* l = expression_list load l in
          call ?typ ~cps:false ~arity closure l)
        (let rec build_applies y l =
           match l with
           | [] ->
               let* y = y in
               instr (Push y)
           | x :: rem ->
               let* x = load x in
               build_applies (call ~cps:false ~arity:1 y [ x ]) rem
         in
         build_applies (load f) l)
    in
    let param_names = l @ [ f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.primitive_type (arity + 1)
      ; param_names
      ; locals
      ; body
      }

  let cps_apply ~context ~arity ~name =
    assert (arity > 2);
    let l =
      List.rev
        (List.init ~len:arity ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x%d" i)))
    in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* () = bind_parameters l in
      let* _ = add_var f in
      Memory.check_function_arity
        f
        ~cps:true
        ~arity:(arity - 1)
        (fun ~typ closure ->
          let* l = expression_list load l in
          call ?typ ~cps:true ~arity closure l)
        (let* args =
           (* We don't need the deadcode sentinal when the tag is 0 *)
           Memory.allocate
             ~tag:0
             ~deadcode_sentinal:(Code.Var.fresh ())
             (List.map ~f:(fun x -> `Var x) (List.tl l))
         in
         let* make_iterator =
           register_import ~name:"caml_apply_continuation" (Fun (Type.primitive_type 1))
         in
         let iterate = Var.fresh_n "iterate" in
         let* () = store iterate (return (W.Call (make_iterator, [ args ]))) in
         let x = List.hd l in
         let* x = load x in
         let* iterate = load iterate in
         push (call ~cps:true ~arity:2 (load f) [ x; iterate ]))
    in
    let param_names = l @ [ f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.primitive_type (arity + 1)
      ; param_names
      ; locals
      ; body
      }

  let dummy ~context ~cps ~arity ~name =
    let arity = if cps then arity + 1 else arity in
    let l =
      List.rev
        (List.init ~len:arity ~f:(fun i -> Code.Var.fresh_n (Printf.sprintf "x%d" i)))
    in
    let f = Code.Var.fresh_n "f" in
    let body =
      let* () = no_event in
      let* () = bind_parameters l in
      let* _ = add_var f in
      let* typ, closure = Memory.load_real_closure ~cps ~arity (load f) in
      let* l = expression_list load l in
      let* e =
        call
          ~typ:(W.Ref { nullable = false; typ = Type typ })
          ~cps
          ~arity
          (return closure)
          l
      in
      instr (W.Return (Some e))
    in
    let param_names = l @ [ f ] in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name
      ; exported_name = None
      ; typ = None
      ; signature = Type.func_type arity
      ; param_names
      ; locals
      ; body
      }

  let f ~context =
    IntMap.iter
      (fun arity name ->
        let f = apply ~context ~arity ~name in
        context.other_fields <- f :: context.other_fields)
      context.apply_funs;
    IntMap.iter
      (fun arity name ->
        let f = cps_apply ~context ~arity ~name in
        context.other_fields <- f :: context.other_fields)
      context.cps_apply_funs;
    IntMap.iter
      (fun arity name ->
        let l = curry ~context ~arity ~name in
        context.other_fields <- List.rev_append l context.other_fields)
      context.curry_funs;
    IntMap.iter
      (fun arity name ->
        let l = cps_curry ~context ~arity ~name in
        context.other_fields <- List.rev_append l context.other_fields)
      context.cps_curry_funs;
    IntMap.iter
      (fun arity name ->
        let f = dummy ~context ~cps:false ~arity ~name in
        context.other_fields <- f :: context.other_fields)
      context.dummy_funs;
    IntMap.iter
      (fun arity name ->
        let f = dummy ~context ~cps:true ~arity ~name in
        context.other_fields <- f :: context.other_fields)
      context.cps_dummy_funs
end
