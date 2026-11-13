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

let debug = Debug.find "bound-checks"

let times = Debug.find "times"

let stats = Debug.find "stats"

(* An access can be performed without checking its index when the range
   analysis proves that the index is nonnegative and less than the length of
   the array, string or bigarray (see [Int_range.valid_index]). *)
let f ~types ~ranges p =
  let t = Timer.make () in
  let valid ~at ~obj i =
    let res = Int_range.valid_index ranges ~at ~obj i in
    if debug ()
    then
      Format.eprintf
        "access %a to %a with index %a: %s@."
        Var.print
        at
        Var.print
        obj
        (fun f i ->
          match i with
          | Pv x -> Var.print f x
          | Pc c -> Print.constant f c)
        i
        (if res then "removed" else "kept");
    res
  in
  let arrays = ref 0 in
  let strings = ref 0 in
  let bigarrays = ref 0 in
  let subst = ref Var.Map.empty in
  let blocks =
    Addr.Map.map
      (fun block ->
        let body =
          List.filter_map
            ~f:(fun ins ->
              match ins with
              | Let (x, Prim (Extern (name, hint), (Pv a :: i :: _ as args))) -> (
                  match Int_range.access name with
                  | Some Array_access when valid ~at:x ~obj:a i ->
                      (* The check returns the array *)
                      incr arrays;
                      subst := Var.Map.add x a !subst;
                      None
                  | Some (String_access unsafe_name) when valid ~at:x ~obj:a i ->
                      incr strings;
                      Some (Let (x, Prim (Extern (unsafe_name, hint), args)))
                  | Some Bigarray_access -> (
                      (* Only accesses to a C layout bigarray, whose indices
                         are between 0 and the dimension *)
                      let kind =
                        match hint, Typing.var_type types a with
                        | ( Some
                              (Optimization_hint.Hint_bigarray
                                 { unsafe = false; kind; layout = C })
                          , _ )
                        | None, Bigarray { unsafe = false; kind; layout = C } -> Some kind
                        | _ -> None
                      in
                      match kind with
                      | Some kind when valid ~at:x ~obj:a i ->
                          incr bigarrays;
                          Some
                            (Let
                               ( x
                               , Prim
                                   ( Extern
                                       ( name
                                       , Some
                                           (Optimization_hint.Hint_bigarray
                                              { unsafe = true; kind; layout = C }) )
                                   , args ) ))
                      | Some _ | None -> Some ins)
                  | Some (Array_access | String_access _) | None -> Some ins)
              | _ -> Some ins)
            block.body
        in
        { block with body })
      p.blocks
  in
  let p = { p with blocks } in
  let p =
    if Var.Map.is_empty !subst
    then p
    else
      let rec resolve x =
        match Var.Map.find_opt x !subst with
        | Some y -> resolve y
        | None -> x
      in
      Subst.Excluding_Binders.program resolve p
  in
  if times () then Format.eprintf "  bound checks: %a@." Timer.print t;
  if stats ()
  then
    Format.eprintf
      "Stats - bound checks removed: %d arrays, %d strings, %d bigarrays@."
      !arrays
      !strings
      !bigarrays;
  p
