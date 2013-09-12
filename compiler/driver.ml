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

let debug = Util.debug "main"
let times = Util.debug "times"
let tailcall p =
  if debug () then Format.eprintf "Tail-call optimization...@.";
  Tailcall.f p

let deadcode' p =
  if debug () then Format.eprintf "Dead-code...@.";
  Deadcode.f p

let deadcode p =
  let r,_ = deadcode' p
  in r

let inline p =
  let (p,live_vars) = deadcode' p in
  if debug () then Format.eprintf "Inlining...@.";
  Inline.f p live_vars

let constant p =
p
(*
  let (p,_,defs) = deadcode' p in
  if debug () then Format.eprintf "Constant...@.";
  Constant.f p defs
*)
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

let rec loop max name round i p =
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

let o1 =
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

let o2 =
  loop 10 "o1" o1 1 >>
  print

(* o3 *)

let round1 =
  print >>
  tailcall >>
  inline >> (* inlining may reveal new tailcall opt *)
  constant >>
  flow_simple >> (* flow simple to keep information for furture tailcall opt *)
  identity

let round2 =
  constant >> o1

let o3 =
  loop 10 "tailcall+inline" round1 1 >>
  loop 10 "flow" round2 1 >>
  print


let profile = ref o1


let f_generate formatter  ~standalone ?linkall d (p,live_vars) =
  if times ()
  then Format.eprintf "Start Generation...@.";
  Generate.f formatter ~standalone ?linkall p d live_vars

let f_link formatter ~standalone ?linkall pretty js =
  if times ()
  then Format.eprintf "Start Linking...@.";
  Generate.f_link formatter ~standalone ?linkall pretty;
  js

let f_coloring js =
  if times ()
  then Format.eprintf "Start Coloring...@.";
  js,Js_var.program js

let f_output formatter d (js,subs) =
  if times ()
  then Format.eprintf "Start Writing file...@.";
  Js_output.program formatter js d

let f ?(standalone=true) ?linkall formatter d =
  !profile >>
  deadcode' >>
  f_generate formatter ~standalone ?linkall d >>
  f_link formatter ~standalone ?linkall false >>
  f_coloring >>
  f_output formatter d

let from_string prims s formatter =
  let (p,d) = Parse_bytecode.from_string prims s in
  f ~standalone:false formatter d p

let set_profile = function
  | 1 -> profile := o1
  | 2 -> profile := o2
  | 3 -> profile := o3
  | _ -> ()
