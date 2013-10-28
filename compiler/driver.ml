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

let debug = Option.Debug.find "main"
let times = Option.Debug.find "times"

open Util

let tailcall p =
  if debug () then Format.eprintf "Tail-call optimization...@.";
  Tailcall.f p

let deadcode' p =
  if debug () then Format.eprintf "Dead-code...@.";
  Deadcode.f p

let deadcode p =
  let r,_,_ = deadcode' p
  in r

let inline p =
  if Option.Optim.inline () && Option.Optim.deadcode ()
  then
    let (p,live_vars,_) = deadcode' p in
    if debug () then Format.eprintf "Inlining...@.";
    Inline.f p live_vars
  else p

let specialize_1 (p,info) =
  if debug () then Format.eprintf "Specialize...@.";
  Specialize.f p info

let specialize_js (p,info) =
  if debug () then Format.eprintf "Specialize js...@.";
  Specialize_js.f p info

let specialize' (p,info) =
  let p = specialize_1 (p,info)in
  let p = specialize_js (p,info) in
  p,info

let specialize p =
  fst (specialize' p)

let eval (p,info) =
  if Option.Optim.staticeval()
  then
    let (p,live_vars,_) = deadcode' p in
    Eval.f info live_vars p
  else p

let flow p =
  if debug () then Format.eprintf "Data flow...@.";
  Flow.f p

let flow_simple p =
  if debug () then Format.eprintf "Data flow...@.";
  Flow.f ~skip_param:true p

let phi p =
  if debug () then Format.eprintf "Variable passing simplification...@.";
  Phisimpl.f p

let print p =
  if debug () then Code.print_program (fun _ _ -> "") p; p

let (>>>) x f = f x

let (>>) f g = fun x -> g (f x)

let rec loop max name round i (p : 'a) : 'a =
  let p' = round p in
  if i >= max || Code.eq p' p
  then p'
  else
    begin
      if times ()
      then Format.eprintf "Start Iteration (%s) %d...@." name i;
      loop max name round (i+1) p'
    end

let identity x = x

(* o1 *)

let o1 : 'a -> 'a=
  print >>
  tailcall >>
  phi >>
  flow >>
  specialize >>
  inline >>
  deadcode >>
  print >>
  flow >>
  specialize >>
  inline >>
  deadcode >>
  phi >>
  flow >>
  specialize >>
  identity

(* o2 *)

let o2 : 'a -> 'a =
  loop 10 "o1" o1 1 >>
  print

(* o3 *)

let round1 : 'a -> 'a =
  print >>
  tailcall >>
  inline >> (* inlining may reveal new tailcall opt *)
  deadcode >>
  (* deadcode required before flow simple -> provided by constant *)
  flow_simple >> (* flow simple to keep information for furture tailcall opt *)
  specialize' >>
  eval >>
  identity

let round2 =
  flow >>
  specialize' >>
  eval >>
  deadcode >>
  o1

let o3 =
  loop 10 "tailcall+inline" round1 1 >>
  loop 10 "flow" round2 1 >>
  print


let profile = ref o1

let generate (p,live_vars,_) =
  if times ()
  then Format.eprintf "Start Generation...@.";
  Generate.f p live_vars


let header formatter ~standalone js =
  if standalone then begin
    Pretty_print.string formatter
      "// This program was compiled from OCaml by js_of_ocaml 1.3";
    Pretty_print.newline formatter;
  end;
  js

let link formatter ~standalone ?linkall js =
  if standalone
  then
    begin
      if times ()
      then Format.eprintf "Start Linking...@.";
      let traverse = new Js_traverse.free in
      let js = traverse#program js in
      let free = traverse#get_free_name in

      let prim = Primitive.get_external () in
      let prov = Linker.get_provided () in

      let all_external = StringSet.union prim prov in

      let used = StringSet.inter free all_external in

      let other =  StringSet.diff free used in

      let res = VarPrinter.get_reserved() in
      let other = StringSet.diff other res in
      let js,missing = Linker.resolve_deps ?linkall js used in
      if not (StringSet.is_empty missing)
      then begin
        Format.eprintf "Missing primitives:@.";
        StringSet.iter (fun nm -> Format.eprintf "  %s@." nm) missing
      end;
      if not (StringSet.is_empty other)
      then begin
        Format.eprintf "Missing var def:@.";
        StringSet.iter (fun nm -> Format.eprintf "  %s@." nm) other
      end;

      js
    end
  else js

let optimize_var js =
  if times ()
  then Format.eprintf "Start Optimizing...@.";
  let js =
    if (Option.Optim.shortvar ())
    then
      let keeps = StringSet.singleton "caml_get_global_data" in
      (new Js_traverse.rename_str keeps)#program js
    else js in
  let js =
    if Option.Optim.compact_vardecl ()
    then (new Js_traverse.compact_vardecl)#program js
    else js in
  js

let coloring js =
  if times ()
  then Format.eprintf "Start Coloring...@.";
  js,Js_assign.program js

let output formatter d (js,to_string) =
  if times ()
  then Format.eprintf "Start Writing file...@.";
  Js_output.program formatter d to_string js

let pack ~standalone js =
  let module J = Javascript in
  if standalone then
    let f = J.EFun ((None, [], js), None) in
    [J.Statement (J.Expression_statement ((J.ECall (f, [])), None))]
  else
    let f = J.EFun ((None, [J.V (Code.Var.fresh ())], js), None) in
    [J.Statement (J.Expression_statement (f, None))]

let configure formatter p =
  let pretty = Option.Optim.pretty () in
  Pretty_print.set_compact formatter (not pretty);
  Code.Var.set_pretty pretty;
  p

let f ?(standalone=true) ?linkall formatter d =
  configure formatter >>
  !profile >>
  deadcode' >>
  generate >>

  link formatter ~standalone ?linkall >>

  pack ~standalone >>
  optimize_var >>

  coloring >>

  header formatter ~standalone >>
  output formatter d

let from_string prims s formatter =
  let (p,d) = Parse_bytecode.from_string prims s in
  f ~standalone:false formatter d p

let set_profile = function
  | 1 -> profile := o1
  | 2 -> profile := o2
  | 3 -> profile := o3
  | _ -> ()
