
open Code
module J = Javascript
open Js_traverse
open Javascript


class tailcall = object(m)
  inherit map as super

  val mutable tc = VarSet.empty

  method expression e =
    match e with
    | EFun _ -> e
    | _ -> super#expression e

  method statement s =
    let s = super#statement s in
    match s with
    | Return_statement( Some e,nid ) ->
      ignore(m#last_e e);
      s
    | _ -> s

  method source s =
    match s with
    | Function_declaration _ -> s
    | Statement s -> Statement (m#statement s)

  method get = tc

  method clear = tc <- VarSet.empty

  method last_e e =
    match e with
      | ECall (EVar (V var), args) -> tc <- VarSet.add var tc
      | ESeq (_,e) -> m#last_e e
      | ECond (_,e1,e2) -> m#last_e e1;m#last_e e2
      | _ -> ()
end

class tailcall_rewrite f = object(m)
  inherit map as super
  method expression e =
    match e with
    | EFun _ -> e
    | _ -> super#expression e

  method statement s =
    let s = super#statement s in
    match s with
    | Return_statement(Some e,nid) -> begin match m#last_e e with
        | None -> s
        | Some s -> s
      end
    | _ -> s

  method last_e e =
    match e with
    | ECall (EVar var,args) -> f var args
    | ECond (cond,e1,e2) ->
      let e1' = m#last_e e1 in
      let e2' = m#last_e e2 in
      begin match e1',e2' with
        | None,None -> None
        | Some s,None ->
          Some (If_statement(cond,s,Some (Return_statement (Some e2,N)),N))
        | None,Some s ->
          Some (If_statement(cond,Return_statement (Some e1,N),Some s,N))
        | Some s1,Some s2 ->
          Some (If_statement(cond,s1,Some (s2),N))
      end
    | ESeq (e1,e2) ->
      begin match m#last_e e2 with
        | None -> None
        | Some s2 -> Some (Block ([Expression_statement (e1,N);s2],N))
      end
    | _ -> None
  method source s =
    match s with
    | Statement st -> Statement (m#statement st)
    | Function_declaration _ -> s


end


module type TC = sig
  val rewrite : (Code.Var.t * Javascript.expression * VarSet.t) list -> Javascript.statement list
end

module Ident : TC = struct
  let rewrite closures =
    [J.Variable_statement (
      List.map (fun (name, cl,_) ->
          J.V name, Some cl
          ) closures, J.N)]

end

module While : TC = struct
  let rewrite closures = failwith "todo"
end

module Tramp : TC = struct

  let rewrite cls =
    match cls with
    | [x,cl,req_tc] when not (VarSet.mem x req_tc) -> Ident.rewrite cls
    | _ ->
    let counter = Var.fresh () in
    let m2old,m2new = List.fold_right (fun (v,_,_) (m2old,m2new) ->
        let v' = Var.fresh () in
        VarMap.add v' v m2old, VarMap.add v v' m2new
      ) cls (VarMap.empty,VarMap.empty)in
    let rewrite v args =
      try
        match v with
        | J.S _ -> None
        | J.V v ->
          let n = J.V (VarMap.find v m2new) in
          let st = J.Return_statement (
            Some (
              J.ECond (
                J.EBin (J.Lt,
                        J.EVar (J.V counter),
                        J.ENum (float_of_int (Option.Tailcall.maximum()))),
                J.ECall(J.EVar n,(J.EBin (J.Plus,J.ENum 1.,J.EVar (J.V counter)))::args),
                J.ECall (
                  J.EVar (J.S {J.name="caml_trampoline_return";var = None} ),
                  J.EVar n :: J.ENum 0. :: args
                ))),J.N)
          in Some st
      with Not_found -> None
    in
    let rw = new tailcall_rewrite rewrite in
    let wrappers = List.map (fun (v,clo,_) ->
        match clo with
        | J.EFun (_, args, _, nid) ->
          let b = J.ECall(
              J.EVar (J.S ({J.name="caml_trampoline" ;var = None})),
              (J.EVar (J.V (VarMap.find v m2new)) :: J.ENum 0. :: List.map (fun i -> J.EVar i) args)) in
          let b = J.Statement (J.Return_statement (Some b,J.N)) in
          v,J.EFun (None, args,[b],nid )
        | _ -> assert false) cls in
    let reals = List.map (fun (v,clo,_) ->
        VarMap.find v m2new,
        match clo with
        | J.EFun (nm,args,body,nid) ->
          J.EFun (nm,(J.V counter)::args,rw#sources body, nid)
        | _ -> assert false
      ) cls in
    let make binds = [J.Variable_statement (
        List.map (fun (name, ex) ->
            J.V (name), Some ex
          ) binds, J.N)] in
    make (reals@wrappers)

end

let rewrite l =
  let open Option.Tailcall in
  match get () with
  | TcNone -> Ident.rewrite l
  | TcTrampoline -> Tramp.rewrite l
  | TcWhile -> While.rewrite l
