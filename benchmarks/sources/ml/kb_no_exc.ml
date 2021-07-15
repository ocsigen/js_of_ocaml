let print_string _ = ()

let print_int _ = ()

let print_newline _ = ()

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: terms.ml 2553 1999-11-17 18:59:06Z xleroy $ *)

(****************** Term manipulations *****************)

type term =
  | Var of int
  | Term of string * term list

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | a :: r -> if List.mem a l2 then union r l2 else a :: union r l2

let rec vars = function
  | Var n -> [ n ]
  | Term (_, l) -> vars_of_list l

and vars_of_list = function
  | [] -> []
  | t :: r -> union (vars t) (vars_of_list r)

let rec substitute subst = function
  | Term (oper, sons) -> Term (oper, List.map (substitute subst) sons)
  | Var n as t -> ( try List.assoc n subst with Not_found -> t)

(* Term replacement: replace M u N is M[u<-N]. *)

let rec replace m u n =
  match u, m with
  | [], _ -> n
  | i :: u, Term (oper, sons) -> Term (oper, replace_nth i sons u n)
  | _ -> failwith "replace"

and replace_nth i sons u n =
  match sons with
  | s :: r -> if i = 1 then replace s u n :: r else s :: replace_nth (i - 1) r u n
  | [] -> failwith "replace_nth"

(* Term matching. *)

let rec fold_left2_opt f accu l1 l2 =
  match l1, l2 with
  | [], [] -> Some accu
  | a1 :: l1, a2 :: l2 -> (
      match f accu a1 a2 with
      | None -> None
      | Some accu' -> fold_left2_opt f accu' l1 l2)
  | _, _ -> invalid_arg "List.fold_left2"

let rec match_rec subst t1 t2 =
  match t1, t2 with
  | Var v, _ ->
      if List.mem_assoc v subst
      then if t2 = List.assoc v subst then Some subst else None
      else Some ((v, t2) :: subst)
  | Term (op1, sons1), Term (op2, sons2) ->
      if op1 = op2 then fold_left2_opt match_rec subst sons1 sons2 else None
  | _ -> None

let matching term1 term2 = match_rec [] term1 term2

(* A naive unification algorithm. *)

let compsubst subst1 subst2 =
  List.map (fun (v, t) -> v, substitute subst1 t) subst2 @ subst1

let rec occurs n = function
  | Var m -> m = n
  | Term (_, sons) -> List.exists (occurs n) sons

let rec unify term1 term2 =
  match term1, term2 with
  | Var n1, _ ->
      if term1 = term2
      then []
      else if occurs n1 term2
      then failwith "unify"
      else [ n1, term2 ]
  | term1, Var n2 -> if occurs n2 term1 then failwith "unify" else [ n2, term1 ]
  | Term (op1, sons1), Term (op2, sons2) ->
      if op1 = op2
      then
        List.fold_left2
          (fun s t1 t2 -> compsubst (unify (substitute s t1) (substitute s t2)) s)
          []
          sons1
          sons2
      else failwith "unify"

(* We need to print terms with variables independently from input terms
   obtained by parsing. We give arbitrary names v1,v2,... to their variables.
*)

let infixes = [ "+"; "*" ]

let rec pretty_term = function
  | Var n ->
      print_string "v";
      print_int n
  | Term (oper, sons) ->
      if List.mem oper infixes
      then
        match sons with
        | [ s1; s2 ] ->
            pretty_close s1;
            print_string oper;
            pretty_close s2
        | _ -> failwith "pretty_term : infix arity <> 2"
      else (
        print_string oper;
        match sons with
        | [] -> ()
        | t :: lt ->
            print_string "(";
            pretty_term t;
            List.iter
              (fun t ->
                print_string ",";
                pretty_term t)
              lt;
            print_string ")")

and pretty_close = function
  | Term (oper, _) as m ->
      if List.mem oper infixes
      then (
        print_string "(";
        pretty_term m;
        print_string ")")
      else pretty_term m
  | m -> pretty_term m

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: equations.ml 2553 1999-11-17 18:59:06Z xleroy $ *)

(****************** Equation manipulations *************)

type rule =
  { number : int
  ; numvars : int
  ; lhs : term
  ; rhs : term
  }

(* standardizes an equation so its variables are 1,2,... *)

let mk_rule num m n =
  let all_vars = union (vars m) (vars n) in
  let counter = ref 0 in
  let subst =
    List.map
      (fun v ->
        incr counter;
        v, Var !counter)
      (List.rev all_vars)
  in
  { number = num; numvars = !counter; lhs = substitute subst m; rhs = substitute subst n }

(* checks that rules are numbered in sequence and returns their number *)

let check_rules rules =
  let counter = ref 0 in
  List.iter
    (fun r ->
      incr counter;
      if r.number <> !counter then failwith "Rule numbers not in sequence")
    rules;
  !counter

let pretty_rule rule =
  print_int rule.number;
  print_string " : ";
  pretty_term rule.lhs;
  print_string " = ";
  pretty_term rule.rhs;
  print_newline ()

let pretty_rules rules = List.iter pretty_rule rules

(****************** Rewriting **************************)

(* Top-level rewriting. Let eq:L=R be an equation, M be a term such that L<=M.
   With sigma = matching L M, we define the image of M by eq as sigma(R) *)
let reduce l m r =
  match matching l m with
  | Some s -> Some (substitute s r)
  | None -> None

(* Test whether m can be reduced by l, i.e. m contains an instance of l. *)

let can_match l m =
  match matching l m with
  | Some _ -> true
  | None -> false

let rec reducible l m =
  can_match l m
  ||
  match m with
  | Term (_, sons) -> List.exists (reducible l) sons
  | _ -> false

(* Top-level rewriting with multiple rules. *)

let rec mreduce rules m =
  match rules with
  | [] -> None
  | rule :: rest -> (
      match reduce rule.lhs m rule.rhs with
      | Some _ as v -> v
      | None -> mreduce rest m)

(* One step of rewriting in leftmost-outermost strategy,
   with multiple rules. Fails if no redex is found *)

let rec mrewrite1 rules m =
  match mreduce rules m with
  | Some v -> v
  | None -> (
      match m with
      | Var _ -> failwith "mrewrite1"
      | Term (f, sons) -> Term (f, mrewrite1_sons rules sons))

and mrewrite1_sons rules = function
  | [] -> failwith "mrewrite1"
  | son :: rest -> (
      try mrewrite1 rules son :: rest with Failure _ -> son :: mrewrite1_sons rules rest)

(* Iterating rewrite1. Returns a normal form. May loop forever *)

let rec mrewrite_all rules m =
  try mrewrite_all rules (mrewrite1 rules m) with Failure _ -> m

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: orderings.ml 2553 1999-11-17 18:59:06Z xleroy $ *)

(*********************** Recursive Path Ordering ****************************)

type ordering =
  | Greater
  | Equal
  | NotGE

let ge_ord order pair =
  match order pair with
  | NotGE -> false
  | _ -> true

and gt_ord order pair =
  match order pair with
  | Greater -> true
  | _ -> false

and eq_ord order pair =
  match order pair with
  | Equal -> true
  | _ -> false

let rec rem_eq equiv x = function
  | [] -> failwith "rem_eq"
  | y :: l -> if equiv (x, y) then l else y :: rem_eq equiv x l

let diff_eq equiv (x, y) =
  let rec diffrec = function
    | ([], _) as p -> p
    | h :: t, y -> (
        try diffrec (t, rem_eq equiv h y)
        with Failure _ ->
          let x', y' = diffrec (t, y) in
          h :: x', y')
  in
  if List.length x > List.length y then diffrec (y, x) else diffrec (x, y)

(* Multiset extension of order *)

let mult_ext order = function
  | Term (_, sons1), Term (_, sons2) -> (
      match diff_eq (eq_ord order) (sons1, sons2) with
      | [], [] -> Equal
      | l1, l2 ->
          if List.for_all (fun n -> List.exists (fun m -> gt_ord order (m, n)) l1) l2
          then Greater
          else NotGE)
  | _ -> failwith "mult_ext"

(* Lexicographic extension of order *)

let lex_ext order = function
  | (Term (_, sons1) as m), (Term (_, sons2) as n) ->
      let rec lexrec = function
        | [], [] -> Equal
        | [], _ -> NotGE
        | _, [] -> Greater
        | x1 :: l1, x2 :: l2 -> (
            match order (x1, x2) with
            | Greater ->
                if List.for_all (fun n' -> gt_ord order (m, n')) l2
                then Greater
                else NotGE
            | Equal -> lexrec (l1, l2)
            | NotGE ->
                if List.exists (fun m' -> ge_ord order (m', n)) l1 then Greater else NotGE
            )
      in
      lexrec (sons1, sons2)
  | _ -> failwith "lex_ext"

(* Recursive path ordering *)

let rpo op_order ext =
  let rec rporec (m, n) =
    if m = n
    then Equal
    else
      match m with
      | Var _ -> NotGE
      | Term (op1, sons1) -> (
          match n with
          | Var vn -> if occurs vn m then Greater else NotGE
          | Term (op2, sons2) -> (
              match op_order op1 op2 with
              | Greater ->
                  if List.for_all (fun n' -> gt_ord rporec (m, n')) sons2
                  then Greater
                  else NotGE
              | Equal -> ext rporec (m, n)
              | NotGE ->
                  if List.exists (fun m' -> ge_ord rporec (m', n)) sons1
                  then Greater
                  else NotGE))
  in
  rporec

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: kb.ml 2553 1999-11-17 18:59:06Z xleroy $ *)

(****************** Critical pairs *********************)

(* All (u,subst) such that N/u (&var) unifies with M,
   with principal unifier subst *)

let rec super m = function
  | Term (_, sons) as n -> (
      let rec collate n = function
        | [] -> []
        | son :: rest ->
            List.map (fun (u, subst) -> n :: u, subst) (super m son)
            @ collate (n + 1) rest
      in
      let insides = collate 1 sons in
      try ([], unify m n) :: insides with Failure _ -> insides)
  | _ -> []

(* Ex :
   let (m,_) = <<F(A,B)>>
   and (n,_) = <<H(F(A,x),F(x,y))>> in super m n
   ==> [[1],[2,Term ("B",[])];                      x <- B
        [2],[2,Term ("A",[]); 1,Term ("B",[])]]     x <- A  y <- B
*)

(* All (u,subst), u&[], such that n/u unifies with m *)

let super_strict m = function
  | Term (_, sons) ->
      let rec collate n = function
        | [] -> []
        | son :: rest ->
            List.map (fun (u, subst) -> n :: u, subst) (super m son)
            @ collate (n + 1) rest
      in
      collate 1 sons
  | _ -> []

(* Critical pairs of l1=r1 with l2=r2 *)
(* critical_pairs : term_pair -> term_pair -> term_pair list *)
let critical_pairs (l1, r1) (l2, r2) =
  let mk_pair (u, subst) = substitute subst (replace l2 u r1), substitute subst r2 in
  List.map mk_pair (super l1 l2)

(* Strict critical pairs of l1=r1 with l2=r2 *)
(* strict_critical_pairs : term_pair -> term_pair -> term_pair list *)
let strict_critical_pairs (l1, r1) (l2, r2) =
  let mk_pair (u, subst) = substitute subst (replace l2 u r1), substitute subst r2 in
  List.map mk_pair (super_strict l1 l2)

(* All critical pairs of eq1 with eq2 *)
let mutual_critical_pairs eq1 eq2 = strict_critical_pairs eq1 eq2 @ critical_pairs eq2 eq1

(* Renaming of variables *)

let rename n (t1, t2) =
  let rec ren_rec = function
    | Var k -> Var (k + n)
    | Term (op, sons) -> Term (op, List.map ren_rec sons)
  in
  ren_rec t1, ren_rec t2

(************************ Completion ******************************)

let deletion_message rule =
  print_string "Rule ";
  print_int rule.number;
  print_string " deleted";
  print_newline ()

(* Generate failure message *)
let non_orientable (m, n) =
  pretty_term m;
  print_string " = ";
  pretty_term n;
  print_newline ()

let rec partition p = function
  | [] -> [], []
  | x :: l ->
      let l1, l2 = partition p l in
      if p x then x :: l1, l2 else l1, x :: l2

let rec get_rule n = function
  | [] -> raise Not_found
  | r :: l -> if n = r.number then r else get_rule n l

(* Improved Knuth-Bendix completion procedure *)

let kb_completion greater =
  let rec kbrec j rules =
    let rec process failures (k, l) eqs =
      (* {[
            print_string "***kb_completion "; print_int j; print_newline();
            pretty_rules rules;
            List.iter non_orientable failures;
            print_int k; print_string " "; print_int l; print_newline();
            List.iter non_orientable eqs;
         ]}
      *)
      match eqs with
      | [] -> (
          if k < l
          then next_criticals failures (k + 1, l)
          else if l < j
          then next_criticals failures (1, l + 1)
          else
            match failures with
            | [] -> rules (* successful completion *)
            | _ ->
                print_string "Non-orientable equations :";
                print_newline ();
                List.iter non_orientable failures;
                failwith "kb_completion")
      | (m, n) :: eqs ->
          let m' = mrewrite_all rules m
          and n' = mrewrite_all rules n
          and enter_rule (left, right) =
            let new_rule = mk_rule (j + 1) left right in
            pretty_rule new_rule;
            let left_reducible rule = reducible left rule.lhs in
            let redl, irredl = partition left_reducible rules in
            List.iter deletion_message redl;
            let right_reduce rule =
              mk_rule rule.number rule.lhs (mrewrite_all (new_rule :: rules) rule.rhs)
            in
            let irreds = List.map right_reduce irredl in
            let eqs' = List.map (fun rule -> rule.lhs, rule.rhs) redl in
            kbrec (j + 1) (new_rule :: irreds) [] (k, l) (eqs @ eqs' @ failures)
          in
          (* {[
                print_string "--- Considering "; non_orientable (m', n');
             ]}
          *)
          if m' = n'
          then process failures (k, l) eqs
          else if greater (m', n')
          then enter_rule (m', n')
          else if greater (n', m')
          then enter_rule (n', m')
          else process ((m', n') :: failures) (k, l) eqs
    and next_criticals failures (k, l) =
      (*
         {[
            print_string "***next_criticals ";
            print_int k; print_string " "; print_int l ; print_newline();
         ]}
       *)
      try
        let rl = get_rule l rules in
        let el = rl.lhs, rl.rhs in
        if k = l
        then process failures (k, l) (strict_critical_pairs el (rename rl.numvars el))
        else
          try
            let rk = get_rule k rules in
            let ek = rk.lhs, rk.rhs in
            process failures (k, l) (mutual_critical_pairs el (rename rl.numvars ek))
          with Not_found -> next_criticals failures (k + 1, l)
      with Not_found -> next_criticals failures (1, l + 1)
    in
    process
  in
  kbrec

(* complete_rules is assumed locally confluent, and checked Noetherian with
   ordering greater, rules is any list of rules *)

let kb_complete greater complete_rules rules =
  let n = check_rules complete_rules
  and eqs = List.map (fun rule -> rule.lhs, rule.rhs) rules in
  let completed_rules = kb_completion greater n complete_rules [] (n, n) eqs in
  print_string "Canonical set found :";
  print_newline ();
  pretty_rules (List.rev completed_rules)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: kbmain.ml 7017 2005-08-12 09:22:04Z xleroy $ *)

(*
  {[
      let group_rules = [
        { number = 1; numvars = 1;
          lhs = Term("*", [Term("U",[]); Var 1]); rhs = Var 1 };
        { number = 2; numvars = 1;
          lhs = Term("*", [Term("I",[Var 1]); Var 1]); rhs = Term("U",[]) };
        { number = 3; numvars = 3;
          lhs = Term("*", [Term("*", [Var 1; Var 2]); Var 3]);
          rhs = Term("*", [Var 1; Term("*", [Var 2; Var 3])]) }
      ]
  ]}
*)

let geom_rules =
  [ { number = 1; numvars = 1; lhs = Term ("*", [ Term ("U", []); Var 1 ]); rhs = Var 1 }
  ; { number = 2
    ; numvars = 1
    ; lhs = Term ("*", [ Term ("I", [ Var 1 ]); Var 1 ])
    ; rhs = Term ("U", [])
    }
  ; { number = 3
    ; numvars = 3
    ; lhs = Term ("*", [ Term ("*", [ Var 1; Var 2 ]); Var 3 ])
    ; rhs = Term ("*", [ Var 1; Term ("*", [ Var 2; Var 3 ]) ])
    }
  ; { number = 4
    ; numvars = 0
    ; lhs = Term ("*", [ Term ("A", []); Term ("B", []) ])
    ; rhs = Term ("*", [ Term ("B", []); Term ("A", []) ])
    }
  ; { number = 5
    ; numvars = 0
    ; lhs = Term ("*", [ Term ("C", []); Term ("C", []) ])
    ; rhs = Term ("U", [])
    }
  ; { number = 6
    ; numvars = 0
    ; lhs =
        Term
          ( "*"
          , [ Term ("C", [])
            ; Term ("*", [ Term ("A", []); Term ("I", [ Term ("C", []) ]) ])
            ] )
    ; rhs = Term ("I", [ Term ("A", []) ])
    }
  ; { number = 7
    ; numvars = 0
    ; lhs =
        Term
          ( "*"
          , [ Term ("C", [])
            ; Term ("*", [ Term ("B", []); Term ("I", [ Term ("C", []) ]) ])
            ] )
    ; rhs = Term ("B", [])
    }
  ]

let group_rank = function
  | "U" -> 0
  | "*" -> 1
  | "I" -> 2
  | "B" -> 3
  | "C" -> 4
  | "A" -> 5
  | _ -> assert false

let group_precedence op1 op2 =
  let r1 = group_rank op1 and r2 = group_rank op2 in
  if r1 = r2 then Equal else if r1 > r2 then Greater else NotGE

let group_order = rpo group_precedence lex_ext

let greater pair =
  match group_order pair with
  | Greater -> true
  | _ -> false

let _ =
  for _ = 1 to 20 do
    kb_complete greater [] geom_rules
  done
