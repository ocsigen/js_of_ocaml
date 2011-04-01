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

let f ?standalone p =
if debug () then Code.print_program (fun _ _ -> "") p;

if debug () then Format.eprintf "Tail-call optimization...@.";
  let p = Tailcall.f p in

if debug () then Format.eprintf "Variable passing simplification...@.";
  let p = Phisimpl.f p in

if debug () then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

if debug () then Format.eprintf "Inlining...@.";
  let p = Inline.f p live_vars in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in


if debug () then Code.print_program (fun _ _ -> "") p;
if debug () then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

if debug () then Format.eprintf "Inlining...@.";
  let p = Inline.f p live_vars in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in


if debug () then Format.eprintf "Variable passing simplification...@.";
  let p = Phisimpl.f p in

if debug () then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

if debug () then Code.print_program (fun _ _ -> "") p;
  fun formatter -> Generate.f formatter ?standalone p live_vars

let from_string prims s =
  let p = Parse.from_string prims s in
  f ~standalone:false p

let set_pretty () = Generate.set_pretty (); Parse.set_pretty ()
