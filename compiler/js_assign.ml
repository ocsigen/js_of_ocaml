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

open Javascript
open Util
let debug = Option.Debug.find "shortvar"

module S = Code.VarSet
module VM = Code.VarMap

module Var = Code.Var

module type Strategy = sig
  type t
  val create : int -> t
  val record_block : t -> Js_traverse.t -> catch:bool -> Javascript.ident list -> unit
  val allocate_variables : t -> count:int Javascript.IdentMap.t -> string array
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
    { mutable first_free : int;
      mutable used : BitSet.t }

  let make_alloc_table () =
    { first_free = 0;
      used = BitSet.create () }

  let next_available a i =
    BitSet.next_free a.used (max i a.first_free)

  let allocate a i =
    BitSet.set a.used i;
    if a.first_free = i then begin
      a.first_free <- BitSet.next_free a.used a.first_free
    end

  let is_available l i =
    List.for_all (fun a -> BitSet.mem a.used i) l



  let first_available l =
    let rec find_rec n l =
      let n' = List.fold_left (fun n a -> next_available a n) n l  in
      if n = n' then n else find_rec n' l
    in
    find_rec 0 l

  let mark_allocated l i = List.iter (fun a -> allocate a i) l

  type t = {
    constr : alloc list array; (* Constraints on variables *)
    mutable parameters : Var.t list array; (* Function parameters *)
    mutable constraints : S.t list }     (* For debugging *)

  let create nv =
    { constr = Array.make nv [];
      parameters = [|[]|];
      constraints = [] }

  (* let output_debug_information t count =
   *
   *
   *   let weight v = (IdentMap.find (V v) count) in
   *
   *   let usage =
   *     List.fold_left
   *       (fun u s ->
   *          S.fold
   *            (fun v u -> VM.add v (try 1 + VM.find v u with Not_found -> 1) u)
   *            s u)
   *       VM.empty t.constraints
   *   in
   *
   *   let l = List.map fst (VM.bindings usage) in
   *
   *   let ch = open_out "/tmp/weights.txt" in
   *   List.iter
   *     (fun v ->
   *        Printf.fprintf ch "%d / %d / %d\n" (weight v)
   *          (VM.find v usage) (Var.idx v))
   *     l;
   *   close_out ch;
   *
   *   let ch = open_out "/tmp/problem.txt" in
   *   Printf.fprintf ch "Maximize\n";
   *   let a = Array.of_list l in
   *   Printf.fprintf ch "  ";
   *   for i = 0 to Array.length a - 1 do
   *     let v = a.(i) in
   *     let w = weight v in
   *     if i > 0 then Printf.fprintf ch " + ";
   *     Printf.fprintf ch "%d x%d" w (Var.idx v)
   *   done;
   *   Printf.fprintf ch "\n";
   *   Printf.fprintf ch "Subject To\n";
   *   List.iter
   *     (fun s ->
   *        if S.cardinal s > 0 then begin
   *          Printf.fprintf ch "  ";
   *          let a = Array.of_list (S.elements s) in
   *          for i = 0 to Array.length a - 1 do
   *            if i > 0 then Printf.fprintf ch " + ";
   *            Printf.fprintf ch "x%d" (Var.idx a.(i))
   *          done;
   *          Printf.fprintf ch "<= 54\n"
   *        end)
   *     t.constraints;
   *   Printf.fprintf ch "Binary\n  ";
   *   List.iter (fun v -> Printf.fprintf ch " x%d" (Var.idx v)) l;
   *   Printf.fprintf ch "\nEnd\n";
   *   close_out ch;
   *
   *   let ch = open_out "/tmp/problem2" in
   *   let var x = string_of_int (Var.idx x) in
   *   let a = List.map (fun v -> (var v, weight v)) l in
   *   let b =
   *     List.map (fun s -> List.map var (S.elements s)) t.constraints in
   *   let c = List.map var l in
   *   output_value ch
   *     ((a, b, c) : (string * int) list * string list list * string list);
   *   close_out ch *)

  let allocate_variables t ~count =
    let weight v = try IdentMap.find (V (Var.of_idx v)) count with Not_found -> 0 in
    let constr = t.constr in
    let len = Array.length constr in
    let idx = Array.make len 0 in
    for i = 0 to len - 1 do
      idx.(i) <- i
    done;
    Array.stable_sort (fun i j -> compare (weight j) (weight i)) idx;
    let name = Array.make len "" in
    let n0 = ref 0 in
    let n1 = ref 0 in
    let n2 = ref 0 in
    let n3 = ref 0 in
    let stats i n =
      incr n0;
      if n < 54 then begin incr n1; n2 := !n2 + (weight i) end;
      n3 := !n3 + (weight i)
    in
    let nm ~origin n =
      name.(origin) <- Var.to_string ~origin:(Var.of_idx origin) (Var.of_idx n) in
    let total = ref 0 in
    let bad = ref 0 in
    for i = 0 to Array.length t.parameters - 1 do
      List.iter
        (fun x ->
           incr total;
           let idx = Var.idx x in
           let l = constr.(idx) in
           if is_available l i then begin
             nm ~origin:idx i;
             mark_allocated l i;
             stats idx i
           end else
             incr bad)
        (List.rev t.parameters.(i))
    done;
    if debug () then
      Format.eprintf
        "Function parameter properly assigned: %d/%d@." (!total - !bad) !total;
    for i = 0 to len - 1 do
      let l = constr.(idx.(i)) in
      if l <> [] && String.length name.(idx.(i)) = 0 then begin
        let n = first_available l in
        let idx = idx.(i) in
        nm ~origin:idx n;
        mark_allocated l n;
        stats idx n
      end;
      if l = [] then assert (weight (idx.(i)) = 0);
    done;
    if debug () then begin
      Format.eprintf "short variable count: %d/%d@." !n1 !n0;
      Format.eprintf "short variable occurrences: %d/%d@." !n2 !n3
    end;
    name

  let add_constraints global u ?(offset=0) params =
    if Option.Optim.shortvar () then begin

      let constr = global.constr in
      let c = make_alloc_table () in

      S.iter (fun v -> let i = Var.idx v in constr.(i) <- c :: constr.(i)) u;
      let params = Array.of_list params in
      let len = Array.length params in
      let len_max = len + offset in
      if Array.length global.parameters < len_max then begin
        let a = Array.make (2 * len_max) [] in
        Array.blit global.parameters 0 a 0 (Array.length global.parameters);
        global.parameters <- a
      end;
      for i = 0 to len - 1 do
        match params.(i) with
        | V x ->
          global.parameters.(i + offset) <- x :: global.parameters.(i + offset)
        | _ -> ()
      done;
      global.constraints <- u :: global.constraints
    end

  let record_block state scope ~catch params =
    let offset = if catch then 5 else 0 in
    let all = S.union scope.Js_traverse.def scope.Js_traverse.use in
    add_constraints state all ~offset params;
end


module Preserve : Strategy = struct
  (* Try to preserve variable names.
     - Assign the origin name if present: "{original_name}"
     - If present but not available, derive a similar name: "{original_name}${n}" (eg. result$3).
     - If not present, make up a name: "$${n}"

     Color variables one scope/block at a time - outer scope first.
  *)

  type t = {
    size   : int;
    mutable scopes : (S.t * Js_traverse.t) list
  }
  let create size = { size ; scopes = [] }

  let record_block t scope ~catch param =
    let defs = match catch, param with
      | true, [V x] -> S.singleton x
      | true, [S _] -> S.empty
      | true, _   -> assert false
      | false, _  -> scope.Js_traverse.def
    in
    t.scopes <- (defs,scope) :: t.scopes
  let allocate_variables t ~count:_ =
    let names = Array.make t.size "" in
    List.iter (fun (defs, state) ->
      let assigned =
        List.fold_left StringSet.union StringSet.empty
          [
            state.Js_traverse.def_name;
            state.Js_traverse.use_name;
            Reserved.keyword
          ]
      in
      let assigned = S.fold (fun var acc  ->
        let name = names.(Var.idx var) in
        if name <> ""
        then StringSet.add name acc
        else acc
      ) (S.union state.Js_traverse.use state.Js_traverse.def) assigned  in
      let _assigned = S.fold (fun var assigned ->
        assert (names.(Var.idx var) = "");
        let name =
          match Var.get_name var with
          | Some expected_name ->
            assert(expected_name <> "");
            if not (StringSet.mem expected_name assigned)
            then expected_name
            else
              let i = ref 0 in
              while StringSet.mem (Printf.sprintf "%s$%d" expected_name !i) assigned do
                incr i
              done;
              Printf.sprintf "%s$%d" expected_name !i
          | None -> Var.to_string var
        in
        names.(Var.idx var) <- name;
        StringSet.add name assigned
      ) defs assigned in
      ()
    ) t.scopes;
    names

end

class traverse record_block = object(m)
  inherit Js_traverse.free as super
  method block ?(catch=false) params =
    record_block m#state ~catch params;
    super#block params
end

let program' (module Strategy : Strategy) p =
  let nv = Var.count () in
  let state = Strategy.create nv in
  let mapper = new traverse (Strategy.record_block state) in
  let p = mapper#program p in
  mapper#block [];
  if S.cardinal (mapper#get_free) <> 0
  then begin
    Util.failwith_ "Some variables escaped (#%d)" (S.cardinal (mapper#get_free))
    (* S.iter(fun s -> (Format.eprintf "%s@." (Var.to_string s))) coloring#get_free *)
  end;
  let names = Strategy.allocate_variables state ~count:mapper#state.Js_traverse.count in
  (* if debug () then output_debug_information state coloring#state.Js_traverse.count; *)
  let color = function
    | V v ->
      let name = names.(Var.idx v) in
      assert (name <> "");
      S {name;var=Some v}
    | x -> x
  in
  (new Js_traverse.subst color)#program p


let program p =
  if Option.Optim.shortvar ()
  then program' (module Min) p
  else program' (module Preserve) p
