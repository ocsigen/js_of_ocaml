(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(* An OCaml loop [for i = lo to hi do ... done] is compiled to

   {[
     if(lo <= hi){
       var i = lo;
       for(;;){...; if(hi === i) break; i = i + 1 | 0;}
     }
   ]}

   We turn it into [for(var i = lo; i <= hi; i++){...}].

   The loop variable ends up being incremented once more, without
   wrapping around. This is fine when it is only used in the loop body,
   where its value is unchanged, and not captured by a closure. *)

open! Stdlib
module J = Javascript
module Var = Code.Var

let is_assign (op : J.binop) =
  match op with
  | Eq
  | StarEq
  | SlashEq
  | ModEq
  | PlusEq
  | MinusEq
  | LslEq
  | AsrEq
  | LsrEq
  | BandEq
  | BxorEq
  | BorEq
  | OrEq
  | AndEq
  | ExpEq
  | CoalesceEq -> true
  | _ -> false

(* Number of occurrences of each variable in the program *)
class count tbl =
  object
    inherit Js_traverse.iter

    method! ident i =
      match i with
      | V x -> tbl.(Var.idx x) <- tbl.(Var.idx x) + 1
      | S _ -> ()
  end

(* Scan some statements, to find how many times each variable occurs
   outside of nested functions, which variables occur in nested
   functions, and which variables are assigned. *)
class scan =
  object
    inherit Js_traverse.iter as super

    val mutable in_fun = false

    val counts = Var.Hashtbl.create 16

    val mutable captured = Var.Set.empty

    val mutable assigned = Var.Set.empty

    val mutable unknown = false

    method count x = try Var.Hashtbl.find counts x with Not_found -> 0

    method captured x = Var.Set.mem x captured

    method assigned x = Var.Set.mem x assigned

    (* Some assignments were not understood *)
    method unknown = unknown

    method! ident id =
      match id with
      | V x ->
          if in_fun
          then captured <- Var.Set.add x captured
          else
            Var.Hashtbl.replace
              counts
              x
              (1 + try Var.Hashtbl.find counts x with Not_found -> 0)
      | S _ -> ()

    method! fun_decl d =
      let old = in_fun in
      in_fun <- true;
      super#fun_decl d;
      in_fun <- old

    method! class_decl d =
      let old = in_fun in
      in_fun <- true;
      super#class_decl d;
      in_fun <- old

    method! variable_declaration k d =
      (match d with
      | DeclIdent (V x, _) -> assigned <- Var.Set.add x assigned
      | DeclIdent (S _, _) -> ()
      | DeclPattern _ -> unknown <- true);
      super#variable_declaration k d

    method! statement s =
      (match s with
      | ForIn_statement _ | ForOf_statement _ | ForAwaitOf_statement _ -> unknown <- true
      | _ -> ());
      super#statement s

    method! expression e =
      (match e with
      | EBin (op, EVar (V x), _) when is_assign op -> assigned <- Var.Set.add x assigned
      | EBin (op, (EVar (S _) | EAccess _ | EDot _), _) when is_assign op -> ()
      | EBin (op, _, _) when is_assign op -> unknown <- true
      | EUn ((IncrA | DecrA | IncrB | DecrB), EVar (V x)) ->
          assigned <- Var.Set.add x assigned
      | EUn ((IncrA | DecrA | IncrB | DecrB), (EVar (S _) | EAccess _ | EDot _)) -> ()
      | EUn ((IncrA | DecrA | IncrB | DecrB), _) -> unknown <- true
      | _ -> ());
      super#expression e
  end

let simple (e : J.expression) =
  match e with
  | EVar (V _) | ENum _ -> true
  | _ -> false

let same (e : J.expression) (e' : J.expression) =
  match e, e' with
  | EVar (V x), EVar (V y) -> Var.equal x y
  | ENum n, ENum n' -> J.Num.equal n n'
  | _ -> false

let minus_one = J.Num.neg (J.Num.of_targetint Targetint.one)

(* The end of the loop body: the exit test and the update of the loop
   variable. Returns the loop variable, its direction, the two sides of
   the test, and the rest of the body. *)
let split_body ~label body =
  match List.rev body with
  | ( J.Expression_statement
        (EBin (Eq, EVar (V i), EBin (Bor, EBin (op, EVar (V i'), ENum n), ENum zero)))
    , _ )
    :: rem
    when Var.equal i i' && J.Num.is_zero zero -> (
      let up =
        match op with
        | Plus when J.Num.is_one n -> Some true
        | Plus when J.Num.equal n minus_one -> Some false
        | Minus when J.Num.is_one n -> Some false
        | _ -> None
      in
      match up, rem with
      | Some up, (If_statement (test, (Break_statement lab', _), None), _) :: rem
        when match lab' with
             | None -> true
             | Some _ -> Option.equal J.Label.equal lab' label -> (
          match test with
          | EBin (EqEqEq, a, b) -> Some (i, up, a, b, List.rev rem)
          | EUn (Not, (EVar (V _) as a)) ->
              (* [!i] is a test against zero *)
              Some (i, up, a, J.ENum (J.Num.of_targetint Targetint.zero), List.rev rem)
          | _ -> None)
      | _ -> None)
  | _ -> None

let rewrite tbl ((s : J.statement), loc) =
  let ( let* ) o f = Option.bind o ~f in
  let guard b = if b then Some () else None in
  let* test, decls, label, body, loop_loc =
    match s with
    | If_statement (test, (Block l, _), None) -> (
        let rec split acc (l : J.statement_list) =
          match l with
          | (Variable_statement (Var, decls), _) :: rem -> split (decls :: acc) rem
          | [ (loop, loop_loc) ] -> Some (List.concat (List.rev acc), loop, loop_loc)
          | _ -> None
        in
        let* decls, loop, loop_loc = split [] l in
        match loop with
        | For_statement (Left None, None, None, (Block body, _)) ->
            Some (test, decls, None, body, loop_loc)
        | Labelled_statement
            (lab, (For_statement (Left None, None, None, (Block body, _)), loop_loc)) ->
            Some (test, decls, Some lab, body, loop_loc)
        | _ -> None)
    | _ -> None
  in
  let* i, up, a, b, body = split_body ~label body in
  let var_i = J.EVar (V i) in
  let* hi = if same a var_i then Some b else if same b var_i then Some a else None in
  let* () = guard (simple hi && not (same hi var_i)) in
  let* lo =
    List.find_map decls ~f:(fun (d : J.variable_declaration) ->
        match d with
        | DeclIdent (V x, Some (e, _)) when Var.equal x i -> Some e
        | _ -> None)
  in
  (* The declarations are now performed even if the loop is not entered *)
  let* () =
    guard
      (List.for_all decls ~f:(fun (d : J.variable_declaration) ->
           match d with
           | DeclIdent (_, None) -> true
           | DeclIdent (_, Some (e, _)) -> simple e
           | DeclPattern _ -> false))
  in
  let* () =
    guard
      (match test, up with
      | EBin ((Le | LeInt), l, h), true
      | EBin ((Ge | GeInt), h, l), true
      | EBin ((Ge | GeInt), l, h), false
      | EBin ((Le | LeInt), h, l), false -> same l lo && same h hi
      | _ -> false)
  in
  (* A [continue] would now perform the update *)
  let* () = guard (not (snd (Js_simpl.loop_jumps ~label body))) in
  let* () = guard (not (Js_simpl.has_scoped_declaration body)) in
  let scan_body = new scan in
  scan_body#statements body;
  let scan_rest = new scan in
  scan_rest#statements [ J.Variable_statement (Var, decls), J.N ];
  let* () = guard (not scan_body#unknown) in
  (* The loop variable and the bound are not modified *)
  let frozen x = not (scan_body#assigned x) in
  let* () =
    guard
      (frozen i
      &&
      match hi with
      | EVar (V h) -> frozen h
      | _ -> true)
  in
  (* The loop variable is only used by the loop. Besides the body, it
     occurs in its declaration, the test and the update. *)
  let* () =
    guard
      ((not (scan_body#captured i))
      && tbl.(Var.idx i) = scan_body#count i + scan_rest#count i + 3)
  in
  (* Keep the table up to date: we remove the initial test, and one
     occurrence of the loop variable. *)
  let forget (e : J.expression) =
    match e with
    | EVar (V x) -> tbl.(Var.idx x) <- tbl.(Var.idx x) - 1
    | _ -> ()
  in
  forget lo;
  forget hi;
  forget var_i;
  let test = J.EBin ((if up then LeInt else GeInt), var_i, hi) in
  let update = J.EUn ((if up then IncrA else DecrA), var_i) in
  let loop =
    J.For_statement (Right (Var, decls), Some test, Some update, (J.Block body, J.N))
  in
  Some
    (match label with
    | None -> loop, loc
    | Some lab -> J.Labelled_statement (lab, (loop, loop_loc)), loc)

class for_loops tbl =
  object
    inherit Js_traverse.map as super

    method! statements l =
      let l = super#statements l in
      List.map l ~f:(fun s ->
          match rewrite tbl s with
          | Some s -> s
          | None -> s)
  end

let f p =
  let tbl = Array.make (Var.count ()) 0 in
  (new count tbl)#program p;
  (new for_loops tbl)#program p
