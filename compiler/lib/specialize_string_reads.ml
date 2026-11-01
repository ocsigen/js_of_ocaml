(* Js_of_ocaml compiler
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

(* When strings are JavaScript strings (Wasm, use-js-string),
   [caml_bytes_of_string] (that is, [Bytes.unsafe_of_string]) copies the
   string into a fresh byte array. The standard library uses it to read
   strings through the [Bytes] functions ([String.sub],
   [String.get_int32_le], ...), which then costs time linear in the size of
   the string. When the byte array is only ever read, these reads are
   performed on the string instead, and the copy is removed. *)

open! Stdlib
open Code

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

let string_primitive name =
  match name with
  | "caml_ml_bytes_length" -> Some "caml_ml_string_length"
  | "caml_bytes_unsafe_get" -> Some "caml_string_unsafe_get"
  | "caml_bytes_get" -> Some "caml_string_get"
  | "caml_bytes_geti8" -> Some "caml_string_geti8"
  | "caml_bytes_geti16" -> Some "caml_string_geti16"
  | "caml_bytes_get16" -> Some "caml_string_get16"
  | "caml_bytes_get16u" -> Some "caml_string_get16u"
  | "caml_bytes_get32" -> Some "caml_string_get32"
  | "caml_bytes_get32u" -> Some "caml_string_get32u"
  | "caml_bytes_get64" -> Some "caml_string_get64"
  | "caml_bytes_get64u" -> Some "caml_string_get64u"
  | "caml_bytes_getf32" -> Some "caml_string_getf32"
  | "caml_bytes_getf32u" -> Some "caml_string_getf32u"
  | "caml_blit_bytes" -> Some "caml_blit_string"
  | _ -> None

let enabled () =
  match Config.target () with
  | `Wasm -> Config.wasm_use_js_string ()
  | `JavaScript -> false

let iter_instr_read_only_uses ~read ~other i =
  match i with
  | Let (_, Prim (Extern (name, _), Pv x :: rem))
    when Option.is_some (string_primitive name) ->
      read x;
      List.iter rem ~f:(fun a ->
          match a with
          | Pv y -> other y
          | Pc _ -> ())
  | _ -> Freevars.iter_instr_free_vars other i

(* Removing an instruction can leave two consecutive events: keep just the
   last one *)
let remove_consecutive_events body =
  List.fold_left body ~init:[] ~f:(fun acc i ->
      match i, acc with
      | Event _, Event _ :: prev -> i :: prev
      | _ -> i :: acc)
  |> List.rev

let iter_read_only_uses ~read ~other block =
  List.iter block.body ~f:(fun i -> iter_instr_read_only_uses ~read ~other i);
  Freevars.iter_last_free_var other block.branch

let f p =
  if not (enabled ())
  then p
  else
    let previous_p = p in
    let t = Timer.make () in
    (* The byte arrays [x = caml_bytes_of_string s], mapped to [s] *)
    let candidates = Var.Hashtbl.create 16 in
    Addr.Map.iter
      (fun _ block ->
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (x, Prim (Extern ("caml_bytes_of_string", _), [ Pv s ])) ->
                Var.Hashtbl.replace candidates x s
            | _ -> ()))
      p.blocks;
    if Var.Hashtbl.length candidates = 0
    then p
    else (
      (* Keep the ones which are only read by primitives with a string
         counterpart *)
      Addr.Map.iter
        (fun _ block ->
          iter_read_only_uses
            ~read:(fun _ -> ())
            ~other:(fun x -> Var.Hashtbl.remove candidates x)
            block)
        p.blocks;
      let count = ref 0 in
      let p =
        if Var.Hashtbl.length candidates = 0
        then p
        else
          let blocks =
            Addr.Map.map
              (fun block ->
                let body =
                  List.filter_map block.body ~f:(fun i ->
                      match i with
                      | Let (x, Prim (Extern ("caml_bytes_of_string", _), _))
                        when Var.Hashtbl.mem candidates x ->
                          incr count;
                          None
                      | Let (y, Prim (Extern (name, _), Pv x :: rem))
                        when Var.Hashtbl.mem candidates x ->
                          let name = Option.get (string_primitive name) in
                          Some
                            (Let
                               ( y
                               , Prim
                                   ( Extern (name, None)
                                   , Pv (Var.Hashtbl.find candidates x) :: rem ) ))
                      | _ -> Some i)
                  |> remove_consecutive_events
                in
                { block with body })
              p.blocks
          in
          { p with blocks }
      in
      if times () then Format.eprintf "  specialize string reads: %a@." Timer.print t;
      if stats () then Format.eprintf "Stats - specialize string reads: %d@." !count;
      if debug_stats ()
      then Code.check_updates ~name:"specialize_string_reads" previous_p p ~updates:!count;
      Code.invariant p;
      p)
