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

  (* [count] is the number of occurrences of each variable, [first] the
     rank of its first occurrence in the program *)
  val allocate_variables : t -> count:int array -> first:int array -> string array
end

module Min : Strategy = struct
  (*
We are trying to achieve the following goals:
(1) variable names should be as short as possible
(2) one should reuse as much as possible a small subset of variable
    names
(3) function parameters should be: function(a,b,...){...}
(4) for longer variable names, variables which are close to one
    another should share a same prefix
(5) similar pieces of code should use the same names

Point (1) minimizes the size of uncompressed files, while point (2) to
(5) improve compression.

We use the following strategy. We maintain the constraint that
variables occurring in a function should keep different names.
We first assign names a, b, ... (in order) to function parameters,
starting from inner functions, skipping variables which have a
conflict with a previously named variable (goal 3). Then, we order
the remaining variables by classes of number of occurrences (powers
of 4), then by order of first occurrence in the program (goals 4 and
5), and greedily assign names to them. For that, we use for each
variable the smallest possible name still available (goal 1/2).

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
    variables using their index as a secondary key rather than just
    based on their weights (the size of the uncompressed file remains
    unchanged)

(1) to (4) predate the use of classes of weights. In most functions,
all variables get a one-char name whatever the order: the order only
matters when variables compete for short names, and a precise order by
weight makes similar functions name their variables differently.
Compared to sorting by weight then by index, using classes of weights
and the order of first occurrence (measured on ocamlc in 2026) costs
0.06% on the uncompressed file but saves 0.35% with gzip, 0.56% with
brotli and 0.42% with zstd. Coarser classes save more on compressed
files but cost more on uncompressed ones.
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

  (* Weight class: floor (log_4 w).

     Variables are sorted by weight class, then by order of first
     occurrence. This balances two effects:
     - sorting by weight gives the shortest names to the most used
       variables, and concentrates occurrences on the first few names
       (a, b, c, ...), which is cheaper to encode after compression;
     - sorting by first occurrence makes similar pieces of code use the
       same names, which compression also exploits (repeated strings).
     Coarser classes favor the latter: they save more on compressed
     files but cost more on uncompressed ones; powers of 4 are a good
     compromise (see the measurements above). The skew matters even
     where all variables get a one-char name: using the order of first
     occurrence there, and the weight only for variables in scopes with
     more than 54 variables, compressed worse. *)
  let weight_class w =
    let rec loop w n = if w < 4 then n else loop (w lsr 2) (n + 1) in
    loop w 0

  let allocate_variables t ~count ~first =
    let weight v = count.(v) in
    let constr = t.constr in
    let len = Array.length constr in
    let cls = Array.map count ~f:weight_class in
    let order = Array.init len ~f:(fun i -> i) in
    Array.stable_sort order ~cmp:(fun i j ->
        match compare cls.(j) cls.(i) with
        | 0 -> compare first.(i) first.(j)
        | c -> c);
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
      let idx = order.(i) in
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
             (* Only the tables where [name] is already taken must skip
                [n]: it remains available in the other ones. *)
             List.iter l ~f:(fun a ->
                 if StringSet.mem name a.existing_names then allocate a n);
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
    match block with
    | Let_scope | Var_scope -> add_constraints state all ~offset:0 []
    | Params p -> add_constraints state all ~offset:0 (bound_idents_of_params p)
    | Catch (p, _) ->
        let ids = bound_idents_of_binding p in
        let all =
          List.fold_left ids ~init:all ~f:(fun all i -> Javascript.IdentSet.add i all)
        in
        add_constraints state all ~offset:5 ids
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
      | Let_scope -> Javascript.IdentSet.elements scope.Js_traverse.def_local
      | Params _ | Var_scope ->
          (* These scopes anchor their [var]s *)
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

  let allocate_variables t ~count ~first:_ =
    let names = Array.make t.size "" in
    let uniq_var = uniq_var () in
    let unnamed = ref 0 in
    let create_unnamed =
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
        if not uniq_var then unnamed := 0;
        let _reserved =
          let weight v = count.(Var.idx v) in
          List.stable_sort others ~cmp:(fun i j ->
              match compare (weight j) (weight i) with
              | 0 -> Var.compare i j
              | c -> c)
          |> List.fold_left ~init:reserved ~f:(fun reserved var ->
              let name =
                while
                  let name = create_unnamed !unnamed in
                  StringSet.mem name reserved || StringSet.mem name Reserved.keyword
                do
                  incr unnamed
                done;
                create_unnamed !unnamed
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
      (* A [Let_scope] (lexical) block that binds nothing block-scoped only
         constrains naming through identifiers that its enclosing scope also
         records (its [var]s hoist and its uses propagate up via
         [merge_block_info]). Recording it would add a redundant constraint
         table, so skip it. [Var_scope]/[Params]/[Catch] anchor their own
         bindings and are always recorded. *)
      (match b with
      | Js_traverse.Let_scope
        when Javascript.IdentSet.is_empty m#state.Js_traverse.def_local -> ()
      | Let_scope | Var_scope | Params _ | Catch _ -> record_block m#state b);
      super#record_block b
  end

(* Count variable occurrences, record the order of their first
   occurrence, and collect the names of source labels *)
class count_occurrences ~idents ~first ~source_labels =
  let rank = ref 0 in
  object
    inherit Js_traverse.iter as super

    method! statement s =
      (match s with
      | Labelled_statement (S (Utf8 l), _) ->
          source_labels := StringSet.add l !source_labels
      | _ -> ());
      super#statement s

    method! ident i =
      match i with
      | S _ -> ()
      | V v ->
          let idx = Code.Var.idx v in
          if idents.(idx) = 0
          then (
            first.(idx) <- !rank;
            incr rank);
          idents.(idx) <- idents.(idx) + 1
  end

(* A generated label [L _] gets the first name used neither by an
   enclosing label of the same function nor by any source label [S _]
   (which may be nested inside it). Without source labels, this is the
   name of its nesting depth. *)
class name ident ~source_labels =
  let printer = Var_printer.create Var_printer.Alphabet.javascript in
  object (m)
    inherit Js_traverse.map as super

    (* Enclosing labels of the current function, innermost first, with
       their new name *)
    val mutable labels : (Label.t * Label.t) list = []

    method ident x = ident x

    (* Labels are not visible across function and class static block
       boundaries *)
    method private reset_labels : 'a. ('a -> 'a) -> 'a -> 'a =
      fun f x ->
        let enclosing = labels in
        labels <- [];
        let x = f x in
        labels <- enclosing;
        x

    method! fun_decl f = m#reset_labels super#fun_decl f

    method! class_element e =
      match e with
      | CEStaticBLock _ -> m#reset_labels super#class_element e
      | _ -> super#class_element e

    method private label l =
      match l with
      | Label.S _ -> l
      | L _ -> (
          match List.find_opt labels ~f:(fun (l', _) -> Label.equal l l') with
          | Some (_, l') -> l'
          | None -> assert false)

    method! statement s =
      match s with
      | Labelled_statement (l, (s, loc)) ->
          let l' =
            match l with
            | S _ -> l
            | L _ ->
                let rec find i =
                  let name = Var_printer.to_string printer i in
                  let l' = Label.of_string (Utf8_string.of_string_exn name) in
                  if
                    StringSet.mem name source_labels
                    || List.exists labels ~f:(fun (_, l'') -> Label.equal l' l'')
                  then find (i + 1)
                  else l'
                in
                find 0
          in
          let enclosing = labels in
          labels <- (l, l') :: labels;
          let s = m#statement s in
          labels <- enclosing;
          Labelled_statement (l', (s, loc))
      | Break_statement (Some l) -> Break_statement (Some (m#label l))
      | Continue_statement (Some l) -> Continue_statement (Some (m#label l))
      | s -> super#statement s
  end

let program' (module Strategy : Strategy) p =
  let nv = Var.count () in
  let state = Strategy.create nv in
  let mapper = new traverse (Strategy.record_block state) in
  mapper#program p;
  let count = Array.make nv 0 in
  let first = Array.make nv max_int in
  let source_labels = ref StringSet.empty in
  (new count_occurrences ~idents:count ~first ~source_labels)#program p;
  (* The program top level anchors its own [var]s and has no enclosing scope
     to record them, so it is a [Var_scope] (never skipped). *)
  mapper#record_block Var_scope;
  let freevar =
    IdentSet.fold
      (fun ident acc ->
        match ident with
        | V v -> Var.Set.add v acc
        | S _ -> acc)
      mapper#get_free
      Var.Set.empty
  in
  let unallocated_names = ref Var.Set.empty in
  let names = Strategy.allocate_variables state ~count ~first in
  (* ignore the chosen name for escaping/free [V _] variables *)
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
  let p = (new name ident ~source_labels:!source_labels)#program p in
  let unnamed = Var.Set.diff !unallocated_names freevar in
  if not (Var.Set.is_empty freevar && Var.Set.is_empty unnamed)
  then (
    (if not (debug_shortvar () || debug ())
     then
       Format.eprintf
         "Some variables escaped (#%d) or were not named (#%d). Use [--debug js_assign] \
          for more info.@."
         (Var.Set.cardinal freevar)
         (Var.Set.cardinal unnamed)
     else
       let (_ : Source_map.info) =
         Js_output.program ~accept_unnamed_var:true (Pretty_print.to_out_channel stderr) p
       in
       let print_vars msg s =
         if not (Var.Set.is_empty s)
         then (
           Format.eprintf "%s:" msg;
           Var.Set.iter (fun v -> Format.eprintf " <%a>" Var.print v) s;
           Format.eprintf "@.")
       in
       print_vars "Some variables escaped" freevar;
       print_vars "Some variables were not named" unnamed);
    assert false);
  p

let program p =
  if Config.Flag.shortvar ()
  then program' (module Min) p
  else program' (module Preserve) p
