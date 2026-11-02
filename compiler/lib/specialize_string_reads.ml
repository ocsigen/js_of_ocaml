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
  | "caml_input_value_from_bytes" -> Some "caml_input_value_from_string"
  | "caml_marshal_data_size" -> Some "caml_marshal_data_size_from_string"
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

(* With separate compilation, [Bytes] functions cannot be inlined into the
   [String] functions which call them on a copy of their argument. In the
   [Stdlib__String] unit, the functions which read a bounded number of bytes
   at a given position are made to read a window of the string instead: the
   result of these functions does not depend on the position, and the window
   has the same bounds behavior. [String.sub] is made to copy only the
   substring. The functions are recognized by name, and checked to have the
   expected shape, so this does nothing when names are not available. *)

(* The functions reading at most 8 bytes from their second argument *)
let window_functions =
  [ "get_utf_8_uchar"
  ; "get_utf_16be_uchar"
  ; "get_utf_16le_uchar"
  ; "get_int8"
  ; "get_uint16_le"
  ; "get_uint16_be"
  ; "get_int16_ne"
  ; "get_int16_le"
  ; "get_int16_be"
  ; "get_int32_le"
  ; "get_int32_be"
  ; "get_int64_le"
  ; "get_int64_be"
  ]

let registers_global p name =
  Addr.Map.fold
    (fun _ block found ->
      found
      || List.exists block.body ~f:(fun i ->
          match i with
          | Let (_, Prim (Extern ("caml_register_global", _), [ _; Pc (String n) ])) ->
              String.equal n name
          | _ -> false))
    p.blocks
    false

let stdlib_string p =
  if not (enabled () && registers_global p "Stdlib__String")
  then p
  else
    let defs = Var.Hashtbl.create 1024 in
    let closures = Var.Hashtbl.create 64 in
    Addr.Map.iter
      (fun _ block ->
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (f, (Closure (params, (pc, _), _) as e)) ->
                Var.Hashtbl.replace defs f e;
                Var.Hashtbl.replace closures f (params, pc)
            | Let (x, e) -> Var.Hashtbl.replace defs x e
            | _ -> ()))
      p.blocks;
    let is_bytes_field g =
      match Var.Hashtbl.find_opt defs g with
      | Some (Field (b, _, _)) -> (
          match Var.Hashtbl.find_opt defs b with
          | Some (Prim (Extern ("caml_get_global", _), [ Pc (String "Stdlib__Bytes") ]))
            -> true
          | _ -> false)
      | _ -> false
    in
    (* [bos] is [Bytes.unsafe_of_string] in [Stdlib__String] *)
    let is_bos f =
      Option.equal String.equal (Var.get_name f) (Some "bos") && is_bytes_field f
    in
    (* The string [s] when [x] is a copy [x = caml_bytes_of_string s] *)
    let copied_string x =
      match Var.Hashtbl.find_opt defs x with
      | Some (Prim (Extern ("caml_bytes_of_string", _), [ Pv s ])) -> Some s
      | Some (Apply { f; args = [ s ]; _ }) when is_bos f -> Some s
      | _ -> None
    in
    (* Rewrite the closure starting at [pc] which calls a [Stdlib__Bytes]
       function [y = g x i rem] on a copy [x] of a string [s], when [x] has
       no other use: the definition of [x] is removed and the call replaced
       by [f ~y ~g ~x ~s ~i ~rem ~exact]. (The name of the closure tells
       which function [g] is.) *)
    let rewrite_closure pc blocks ~f =
      let pcs =
        Code.traverse { fold = fold_children } (fun pc l -> pc :: l) pc p.blocks []
      in
      let call =
        List.find_map pcs ~f:(fun pc ->
            List.find_map (Addr.Map.find pc p.blocks).body ~f:(fun i ->
                match i with
                | Let (y, Apply { f = g; args = x :: i :: rem; exact })
                  when is_bytes_field g -> (
                    match copied_string x with
                    | Some s -> Some (y, g, x, s, i, rem, exact)
                    | None -> None)
                | _ -> None))
      in
      match call with
      | None -> None
      | Some (y, g, x, s, i, rem, exact) ->
          let uses = ref 0 in
          let count v = if Var.equal v x then incr uses in
          List.iter pcs ~f:(fun pc ->
              Freevars.iter_block_free_vars count (Addr.Map.find pc p.blocks));
          if !uses <> 1
          then None
          else
            let replacement = f ~y ~g ~x ~s ~i ~rem ~exact in
            Option.map replacement ~f:(fun replacement ->
                List.fold_left pcs ~init:blocks ~f:(fun blocks pc ->
                    let block = Addr.Map.find pc p.blocks in
                    let body =
                      List.concat_map block.body ~f:(fun i ->
                          match i with
                          | Let (x', _) when Var.equal x x' -> []
                          | Let (y', Apply _) when Var.equal y y' -> replacement
                          | _ -> [ i ])
                      |> remove_consecutive_events
                    in
                    Addr.Map.add pc { block with body } blocks))
    in
    let count = ref 0 in
    let blocks =
      Var.Hashtbl.fold
        (fun f (params, pc) blocks ->
          let rewritten =
            match Var.get_name f, params with
            | Some name, [ _; _ ] when List.mem ~eq:String.equal name window_functions ->
                rewrite_closure pc blocks ~f:(fun ~y ~g ~x ~s ~i ~rem ~exact ->
                    let zero = Var.fresh () in
                    Some
                      [ Let
                          ( x
                          , Prim
                              ( Extern ("caml_string_window", None)
                              , [ Pv s; Pv i; Pc (Int (Targetint.of_int_exn 8)) ] ) )
                      ; Let (zero, Constant (Int Targetint.zero))
                      ; Let (y, Apply { f = g; args = x :: zero :: rem; exact })
                      ])
            | Some "sub", [ _; _; _ ] ->
                rewrite_closure pc blocks ~f:(fun ~y ~g:_ ~x:_ ~s ~i:ofs ~rem ~exact:_ ->
                    match rem with
                    | [ len ] ->
                        Some
                          [ Let
                              ( y
                              , Prim
                                  ( Extern ("caml_string_sub_bytes", None)
                                  , [ Pv s; Pv ofs; Pv len ] ) )
                          ]
                    | _ -> None)
            | _ -> None
          in
          match rewritten with
          | Some blocks ->
              incr count;
              blocks
          | None -> blocks)
        closures
        p.blocks
    in
    if stats () then Format.eprintf "Stats - stdlib string functions: %d@." !count;
    let p = { p with blocks } in
    Code.invariant p;
    p
