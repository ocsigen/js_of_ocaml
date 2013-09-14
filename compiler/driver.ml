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

let constant p =
  if Option.Optim.constant ()
  then
    let (p,_,defs) = deadcode' p in
    if debug () then Format.eprintf "Constant...@.";
    Constant.f p defs
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
  inline >>
  deadcode >>
  print >>
  flow >>
  inline >>
  deadcode >>
  phi >>
  flow >>
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
  constant >>
  (* deadcode required before flow simple -> provided by constant *)
  flow_simple >> (* flow simple to keep information for furture tailcall opt *)
  identity

let round2 =
  constant >> o1

let o3 =
  loop 10 "tailcall+inline" round1 1 >>
  loop 10 "flow" round2 1 >>
  print


let profile = ref o1

let generate ~standalone (p,live_vars,_) =
  if times ()
  then Format.eprintf "Start Generation...@.";
  Generate.f ~standalone p live_vars


let header formatter ~standalone js =
  if standalone then begin
    Pretty_print.string formatter
      "// This program was compiled from OCaml by js_of_ocaml 1.3";
    Pretty_print.newline formatter;
  end;
  js

let link formatter ~standalone ?linkall pretty js =
  if standalone
  then
    begin
      if times ()
      then Format.eprintf "Start Linking...@.";
      let missing = Linker.resolve_deps ?linkall formatter (Primitive.get_used ()) in
      match missing with
        | [] -> ()
        | l ->
          Format.eprintf "Missing primitives:@.";
          List.iter (fun nm -> Format.eprintf "  %s@." nm) l

    end;
  js

let coloring js =
  if Option.Optim.shortvar ()
  then
    begin
      if times ()
      then Format.eprintf "Start Coloring...@.";
      js,Js_var.program js
    end
  else js, (fun v -> Code.Var.to_string v)

let output formatter d (js,to_string) =
  if times ()
  then Format.eprintf "Start Writing file...@.";
  Js_output.program formatter d to_string js


let configure formatter p =
  Pretty_print.set_compact formatter (not (Option.Optim.pretty ()));
  p

let f ?(standalone=true) ?linkall formatter d =
  configure formatter >>
  !profile >>
  deadcode' >>
  generate ~standalone >>
  header formatter ~standalone >>
  link formatter ~standalone ?linkall false >>
  coloring >>
  output formatter d

let from_string prims s formatter =
  let (p,d) = Parse_bytecode.from_string prims s in
  f ~standalone:false formatter d p

let set_profile = function
  | 1 -> profile := o1
  | 2 -> profile := o2
  | 3 -> profile := o3
  | _ -> ()
