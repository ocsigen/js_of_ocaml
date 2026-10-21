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
              | Let
                  ( y
                  , Prim
                      ( Extern
                          ( ( "caml_check_bound"
                            | "caml_check_bound_gen"
                            | "caml_check_bound_float" )
                          , _ )
                      , [ Pv a; i ] ) )
                when valid ~at:y ~obj:a i ->
                  (* The check returns the array *)
                  incr arrays;
                  subst := Var.Map.add y a !subst;
                  None
              | Let
                  ( x
                  , Prim
                      ( Extern
                          ( (( "caml_string_get"
                             | "caml_bytes_get"
                             | "caml_string_set"
                             | "caml_bytes_set" ) as name)
                          , hint )
                      , (Pv s :: i :: _ as args) ) )
                when valid ~at:x ~obj:s i ->
                  incr strings;
                  let name =
                    match name with
                    | "caml_string_get" -> "caml_string_unsafe_get"
                    | "caml_bytes_get" -> "caml_bytes_unsafe_get"
                    | "caml_string_set" -> "caml_string_unsafe_set"
                    | _ -> "caml_bytes_unsafe_set"
                  in
                  Some (Let (x, Prim (Extern (name, hint), args)))
              | Let
                  ( x
                  , Prim
                      ( Extern ((("caml_ba_get_1" | "caml_ba_set_1") as name), hint)
                      , (Pv ba :: i :: _ as args) ) ) -> (
                  (* Only accesses to a C layout bigarray, whose indices are
                     between 0 and the dimension *)
                  let kind =
                    match hint, Typing.var_type types ba with
                    | ( Some
                          (Optimization_hint.Hint_bigarray
                             { unsafe = false; kind; layout = C })
                      , _ )
                    | None, Bigarray { unsafe = false; kind; layout = C } -> Some kind
                    | _ -> None
                  in
                  match kind with
                  | Some kind when valid ~at:x ~obj:ba i ->
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
