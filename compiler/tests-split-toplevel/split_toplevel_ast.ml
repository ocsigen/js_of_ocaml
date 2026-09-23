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

(* Unit tests of [Split_toplevel] on hand-written Wasm code. *)

open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler
module W = Wasm_ast

let () = Config.Param.set "toplevel_split_size" "5"

let g = Code.Var.fresh_n "g"

let f = Code.Var.fresh_n "f"

let filler () = List.init 5 (fun _ -> W.GlobalSet (g, W.Const (I32 1l)))

(* A local must not be used both by the residual function and by an
   outlined function: its value would not be shared. *)
let run title x body =
  let name = Code.Var.fresh_n "toplevel" in
  let locals, _, fields = Split_toplevel.f ~name ~locals:[ x, W.I32 ] body in
  let declares l = List.exists (fun (y, _) -> Code.Var.equal x y) l in
  let users =
    (if declares locals then 1 else 0)
    + List.length
        (List.filter
           (function
             | W.Function { locals; _ } -> declares locals
             | _ -> false)
           fields)
  in
  Printf.printf
    "%s: %d outlined functions, local used in %d function(s)\n"
    title
    (List.length fields)
    users

(* Two sequences nested in the same instruction: the end of the first
   one and the start of the second one must not share a position,
   otherwise the absence of live locals at the start of the second
   sequence makes the end of the first one a cut point. *)
let () =
  let t = Code.Var.fresh_n "t" in
  run
    "local read after the first sequence"
    t
    [ W.Drop
        (Call
           ( f
           , [ Seq (filler () @ [ LocalSet (t, Const (I32 7l)) ], LocalGet t)
             ; Seq (filler (), Const (I32 0l))
             ] ))
    ];
  let y = Code.Var.fresh_n "y" in
  run
    "local read in the second sequence"
    y
    [ W.Drop
        (Call
           ( f
           , [ Seq (filler (), LocalTee (y, Const (I32 7l)))
             ; Seq (filler () @ [ Drop (LocalGet y) ], Const (I32 0l))
             ] ))
    ]

(* A local written in an expression right before a nested sequence
   must be live at the start of this sequence. *)
let () =
  let y = Code.Var.fresh_n "y" in
  run
    "local written before a sequence"
    y
    [ W.Drop
        (Call
           ( f
           , [ LocalTee (y, Const (I32 7l))
             ; Seq (filler () @ [ Drop (LocalGet y) ], Const (I32 0l))
             ] ))
    ]

(* Locations are stateful: an [Event] applies to the code until the
   next one. The outlined function must start with the location in
   effect at the start of the run, and the location in effect at the
   end of the run must be restored after the call site. *)
let () =
  let ev line = W.Event { Parse_info.zero with src = Some "a.ml"; line } in
  let names = Hashtbl.create 16 in
  let name x =
    match Hashtbl.find_opt names x with
    | Some n -> n
    | None ->
        let n = Printf.sprintf "f%d" (Hashtbl.length names) in
        Hashtbl.add names x n;
        n
  in
  let print_body body =
    List.iter
      (fun (i : W.instruction) ->
        match i with
        | Event { line; _ } -> Printf.printf " @%d" line
        | GlobalSet _ -> print_string " set"
        | CallInstr (f, _) -> Printf.printf " call %s" (name f)
        | Nop -> print_string " nop"
        | _ -> print_string " ?")
      body;
    print_newline ()
  in
  let toplevel = Code.Var.fresh_n "toplevel" in
  let body =
    [ ev 1 ]
    @ filler ()
    @ [ ev 2; GlobalSet (g, Const (I32 1l)); Nop; Nop; Nop; ev 3 ]
    @ filler ()
    @ [ Nop ]
  in
  let _, body, fields = Split_toplevel.f ~name:toplevel ~locals:[] body in
  print_string "toplevel:";
  print_body body;
  List.iter
    (function
      | W.Function { name = f; body; _ } ->
          Printf.printf "%s:" (name f);
          print_body body
      | _ -> ())
    fields

(* Deeply nested loops, where each loop reads a local written by the
   enclosing loop right before it: the liveness analysis used to take
   time exponential in the nesting depth. *)
let () =
  let loop body =
    W.Loop ({ params = []; result = [] }, body @ [ W.Br_if (0, GlobalGet g) ])
  in
  let locals = ref [] in
  let rec nest d =
    let u = Code.Var.fresh () in
    locals := (u, (W.I32 : W.value_type)) :: !locals;
    let inner =
      if d = 0
      then []
      else
        let inner, u' = nest (d - 1) in
        W.LocalSet (u', Const (I32 1l)) :: inner
    in
    [ loop (W.Drop (LocalGet u) :: inner) ], u
  in
  let body, u = nest 30 in
  let body = W.LocalSet (u, Const (I32 1l)) :: body in
  let _, _, fields =
    Split_toplevel.f ~name:(Code.Var.fresh_n "toplevel") ~locals:!locals body
  in
  Printf.printf "nested loops: %d outlined functions\n" (List.length fields)
