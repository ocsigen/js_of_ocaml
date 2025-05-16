(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Jérôme Vouillon
 * Copyright (C) 2013 Hugo Heuzard
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
open Javascript

let debug_shortvar = Debug.find "shortvar"

let debug = Debug.find "js_assign"

module S = Code.Var.Set
module Var = Code.Var

module type Strategy = sig
  type t

  val create : int -> t

  val record_block : t -> Js_traverse.t -> Js_traverse.block -> unit

  val allocate_variables : t -> count:int array -> string array
end

module Min : Strategy = struct
  (*
We are trying to achieve the following goals:
(1) variable names should be as short as possible
(2) one should reuse as much as possible a small subsets of variable
    names
(3) function parameters should be: function(a,b,...){...}
(4) for longer variable names, variable which are closed from one
    another should share a same prefix

Point (1) minimizes the size of uncompressed files, while point (2) to
(4) improve compression.

We use the following strategy. We maintain the constraint that
variables occurring in a function should keep different names.
We first assign names a, b, ... (in order) to function parameters,
starting from inner functions, skipping variables which have a
conflict with a previously names variable (goal 3). Then, we order
the remaining variables by their number of occurrences, then by
their index (goal 4), and greedily assigned name to them. For that,
we use for each variable the smallest possible name still available
(goal 1/2).

This algorithm seems effective. Here are some statistics gathered
while compiling the OCaml toplevel:
(1) We get 132025 occurrences of one-char variables out of 169728
    occurrences while the optimal number (determined using a mixed
    integer linear programming solver) is 132105 occurrences (80 more
    occurrences).
(2) Variable names are heavily biased toward character a: among
    variables, we have about 34000 occurrences of character a, less
    than 5000 occurrences of character i (9th character, out of the 54
    characters that can start an identifier), and about 1500
    occurrences of character A.
(3) About 6% of the function parameters are not assigned as wanted;
    it is not clear we can do any better: there are a lot of nested
    functions.
(4) We save 8181 bytes on the compressed file (1.8%) by sorting
    variables using their index as a secondary key rather that just
    based on their weights (the size of the uncompressed file remains
    unchanged)
*)

  type alloc =
    { mutable first_free : int
    ; used : BitSet.t
    ; existing_names : StringSet.t
    }

  let make_alloc_table existing_names =
    { first_free = 0; used = BitSet.create (); existing_names }

  let next_available a i = BitSet.next_free a.used (max i a.first_free)

  let allocate a i =
    BitSet.set a.used i;
    if a.first_free = i then a.first_free <- BitSet.next_free a.used a.first_free

  let is_available l name i =
    List.for_all l ~f:(fun a ->
        (not (BitSet.mem a.used i)) && not (StringSet.mem name a.existing_names))

  let first_available l =
    let rec find_rec n l =
      let n' = List.fold_left l ~init:n ~f:(fun n a -> next_available a n) in
      if n = n' then n else find_rec n' l
    in
    find_rec 0 l

  let mark_allocated l i = List.iter l ~f:(fun a -> allocate a i)

  type t =
    { constr : alloc list array (* Constraints on variables *)
    ; mutable parameters : Var.t list array (* Function parameters *)
    ; printer : Var_printer.t
    }

  let create nv =
    { constr = Array.make nv []
    ; parameters = [| [] |]
    ; printer = Var_printer.create Var_printer.Alphabet.javascript
    }

  let allocate_variables t ~count =
    let weight v = count.(v) in
    let constr = t.constr in
    let len = Array.length constr in
    let idx = Array.make len 0 in
    for i = 0 to len - 1 do
      idx.(i) <- i
    done;
    Array.stable_sort idx ~cmp:(fun i j -> compare (weight j) (weight i));
    let names = Array.make len "" in
    let n0 = ref 0 in
    let n1 = ref 0 in
    let n2 = ref 0 in
    let n3 = ref 0 in
    let stats i n =
      incr n0;
      if n < 54
      then (
        incr n1;
        n2 := !n2 + weight i);
      n3 := !n3 + weight i
    in
    let total = ref 0 in
    let bad = ref 0 in
    for i = 0 to Array.length t.parameters - 1 do
      let name = Var_printer.to_string t.printer i in
      List.iter
        (List.rev t.parameters.(i))
        ~f:(fun x ->
          incr total;
          let idx = Var.idx x in
          let l = constr.(idx) in
          if is_available l name i
          then (
            names.(idx) <- name;
            mark_allocated l i;
            stats idx i)
          else incr bad)
    done;
    if debug_shortvar ()
    then
      Format.eprintf
        "Function parameter properly assigned: %d/%d@."
        (!total - !bad)
        !total;
    for i = 0 to len - 1 do
      let idx = idx.(i) in
      let l = constr.(idx) in
      (if (not (List.is_empty l)) && String.length names.(idx) = 0
       then
         let rec loop () =
           let n = first_available l in
           let name = Var_printer.to_string t.printer n in
           if List.for_all l ~f:(fun a -> not (StringSet.mem name a.existing_names))
           then (
             names.(idx) <- name;
             mark_allocated l n;
             stats idx n)
           else (
             mark_allocated l n;
             loop ())
         in
         loop ());
      if List.is_empty l then assert (weight idx = 0)
    done;
    if debug_shortvar ()
    then (
      Format.eprintf "short variable count: %d/%d@." !n1 !n0;
      Format.eprintf "short variable occurrences: %d/%d@." !n2 !n3);
    names

  let add_constraints global u ~offset (params : ident list) =
    let constr = global.constr in
    let existing_names =
      Javascript.IdentSet.fold
        (fun x acc ->
          match x with
          | V _ -> acc
          | S { name = Utf8 name; _ } -> StringSet.add name acc)
        u
        StringSet.empty
    in
    let c = make_alloc_table existing_names in
    Javascript.IdentSet.iter
      (function
        | S _ -> ()
        | V v ->
            let i = Var.idx v in
            constr.(i) <- c :: constr.(i))
      u;
    let params = Array.of_list params in
    let len = Array.length params in
    let len_max = len + offset in
    if Array.length global.parameters < len_max
    then (
      let a = Array.make (2 * len_max) [] in
      Array.blit
        ~src:global.parameters
        ~src_pos:0
        ~dst:a
        ~dst_pos:0
        ~len:(Array.length global.parameters);
      global.parameters <- a);
    for i = 0 to len - 1 do
      match params.(i) with
      | V x -> global.parameters.(i + offset) <- x :: global.parameters.(i + offset)
      | _ -> ()
    done

  let record_block state scope (block : Js_traverse.block) =
    let all =
      Javascript.IdentSet.union
        (Javascript.IdentSet.union scope.Js_traverse.def_var scope.Js_traverse.def_local)
        scope.Js_traverse.use
    in
    let all =
      match block with
      | Normal -> all
      | Params _ -> all
      | Catch (p, _) ->
          let ids = bound_idents_of_binding p in
          List.fold_left ids ~init:all ~f:(fun all i -> Javascript.IdentSet.add i all)
    in
    match block with
    | Normal -> add_constraints state all ~offset:0 []
    | Catch (v, _) -> add_constraints state all ~offset:5 (bound_idents_of_binding v)
    | Params p -> add_constraints state all ~offset:0 (bound_idents_of_params p)
end

module Preserve : Strategy = struct
  (* Try to preserve variable names.
     - Assign the origin name if present: "{original_name}"
     - If present but not available, derive a similar name: "{original_name}${n}" (eg. result$3).
     - If not present, make up a name: "$${n}"

     Color variables one scope/block at a time - outer scope first.
  *)

  type t =
    { size : int
    ; mutable scopes : (S.t * Javascript.IdentSet.t) list
    }

  let create size = { size; scopes = [] }

  let record_block t scope (b : Js_traverse.block) =
    let defs =
      match b with
      | Catch (p, _) ->
          bound_idents_of_binding p
          @ Javascript.IdentSet.elements scope.Js_traverse.def_local
      | Normal -> Javascript.IdentSet.elements scope.Js_traverse.def_local
      | Params _ ->
          Javascript.IdentSet.elements
            (IdentSet.union scope.Js_traverse.def_var scope.Js_traverse.def_local)
    in
    let all =
      Javascript.IdentSet.union
        (Javascript.IdentSet.union scope.Js_traverse.def_var scope.Js_traverse.def_local)
        scope.Js_traverse.use
    in
    let defs =
      List.fold_left
        ~init:S.empty
        ~f:(fun acc x ->
          match (x : Javascript.ident) with
          | V i -> S.add i acc
          | S _ -> acc)
        defs
    in
    t.scopes <- (defs, all) :: t.scopes

  let uniq_var = Debug.find "var"

  let allocate_variables t ~count =
    let names = Array.make t.size "" in
    let uniq_var = uniq_var () in
    let unamed = ref 0 in
    let create_unamed =
      if uniq_var
      then
        fun i ->
          "_" ^ Var_printer.Alphabet.to_string Var_printer.Alphabet.javascript i ^ "_"
      else fun i -> Var_printer.Alphabet.to_string Var_printer.Alphabet.javascript i
    in
    List.iter t.scopes ~f:(fun (defs, all) ->
        let reserved =
          IdentSet.fold
            (fun var acc ->
              match var with
              | S { name = Utf8 s; _ } -> StringSet.add s acc
              | V v ->
                  let name = names.(Var.idx v) in
                  if not (String.is_empty name) then StringSet.add name acc else acc)
            all
            StringSet.empty
        in
        (* We first allocate variable that have a name *)
        let reserved, others =
          S.fold
            (fun var (reserved, others) ->
              assert (String.is_empty names.(Var.idx var));
              match Var.get_name var with
              | Some expected_name ->
                  assert (not (String.is_empty expected_name));
                  assert (not (StringSet.mem expected_name Reserved.keyword));
                  let name =
                    if not (StringSet.mem expected_name reserved)
                    then expected_name
                    else
                      let expected_name =
                        if Char.equal expected_name.[String.length expected_name - 1] '$'
                        then expected_name
                        else expected_name ^ "$"
                      in
                      let i = ref 0 in
                      while
                        StringSet.mem (Printf.sprintf "%s%d" expected_name !i) reserved
                      do
                        incr i
                      done;
                      Printf.sprintf "%s%d" expected_name !i
                  in
                  names.(Var.idx var) <- name;
                  StringSet.add name reserved, others
              | None -> reserved, var :: others)
            defs
            (reserved, [])
        in
        (* We then allocate variables without names *)
        if not uniq_var then unamed := 0;
        let _reserved =
          let weight v = count.(Var.idx v) in
          List.stable_sort others ~cmp:(fun i j ->
              match compare (weight j) (weight i) with
              | 0 -> Var.compare i j
              | c -> c)
          |> List.fold_left ~init:reserved ~f:(fun reserved var ->
                 let name =
                   while
                     let name = create_unamed !unamed in
                     StringSet.mem name reserved || StringSet.mem name Reserved.keyword
                   do
                     incr unamed
                   done;
                   create_unamed !unamed
                 in
                 names.(Var.idx var) <- name;
                 StringSet.add name reserved)
        in
        ());
    names
end

class traverse record_block =
  object (m)
    inherit Js_traverse.free as super

    method! record_block b =
      record_block m#state b;
      super#record_block b
  end

class traverse_idents_and_labels ~idents ~labels =
  object
    inherit Js_traverse.iter as super

    val ldepth = 0

    method fun_decl (_k, _params, body, _loc) =
      let m = {<ldepth = 0>} in
      m#function_body body

    method statement =
      function
      | Labelled_statement (L l, (s, _)) ->
          let m = {<ldepth = ldepth + 1>} in
          Var.Hashtbl.add labels l ldepth;
          m#statement s
      | s -> super#statement s

    method ident i =
      match i with
      | S _ -> ()
      | V v ->
          let idx = Code.Var.idx v in
          idents.(idx) <- idents.(idx) + 1
  end

class name ident label =
  object (m)
    inherit Js_traverse.map as super

    method ident x = ident x

    method statement =
      function
      | Labelled_statement (l, (s, loc)) ->
          Labelled_statement (label l, (m#statement s, loc))
      | Break_statement (Some l) -> Break_statement (Some (label l))
      | Continue_statement (Some l) -> Continue_statement (Some (label l))
      | s -> super#statement s
  end

let program' (module Strategy : Strategy) p =
  let nv = Var.count () in
  let state = Strategy.create nv in
  let labels = Var.Hashtbl.create 20 in
  let mapper = new traverse (Strategy.record_block state) in
  let p = mapper#program p in
  let count = Array.make nv 0 in
  let () =
    let o = new traverse_idents_and_labels ~idents:count ~labels in
    o#program p
  in
  mapper#record_block Normal;
  let freevar =
    IdentSet.fold
      (fun ident acc ->
        match ident with
        | V v -> Var.Set.add v acc
        | S _ -> acc)
      mapper#get_free
      Var.Set.empty
  in
  let has_free_var = not (Var.Set.is_empty freevar) in
  let unallocated_names = ref Var.Set.empty in
  let names = Strategy.allocate_variables state ~count in
  (* ignore the choosen name for escaping/free [V _] variables *)
  Var.Set.iter (fun x -> names.(Var.idx x) <- "") freevar;
  let ident =
    if Config.Flag.stable_var ()
    then
      function
      | S _ as x -> x
      | V v ->
          let name = Printf.sprintf "v%d" (Code.Var.idx v) in
          ident ~var:v (Utf8_string.of_string_exn name)
    else
      function
      | S _ as x -> x
      | V v as x -> (
          let name = names.(Var.idx v) in
          match name with
          | "" ->
              unallocated_names := Var.Set.add v !unallocated_names;
              x
          | _ -> ident ~var:v (Utf8_string.of_string_exn name))
  in
  let label_printer = Var_printer.create Var_printer.Alphabet.javascript in
  let max_label_depth = Var.Hashtbl.fold (fun _ d acc -> max d acc) labels 0 in
  let lname_per_depth =
    Array.init (max_label_depth + 1) ~f:(fun i -> Var_printer.to_string label_printer i)
  in
  let label = function
    | Label.S _ as l -> l
    | L v ->
        let i = Var.Hashtbl.find labels v in
        S (Utf8_string.of_string_exn lname_per_depth.(i))
  in
  let p = (new name ident label)#program p in
  (if has_free_var || Var.Set.cardinal !unallocated_names > 0
   then
     let () =
       if not (debug_shortvar () || debug ())
       then
         Format.eprintf
           "Some variables escaped (#%d). Use [--debug js_assign] for more info.@."
           (Var.Set.cardinal freevar)
       else
         let (_ : Source_map.info) =
           Js_output.program
             ~accept_unnamed_var:true
             (Pretty_print.to_out_channel stderr)
             p
         in
         Format.eprintf "Some variables escaped:";
         Var.Set.iter (fun v -> Format.eprintf " <%a>" Var.print v) freevar;
         Format.eprintf "@."
     in
     assert false);
  p

let program p =
  if Config.Flag.shortvar ()
  then program' (module Min) p
  else program' (module Preserve) p
