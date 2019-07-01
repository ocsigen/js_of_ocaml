(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Jacques-Pascal Deplaix
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

(** Ocamlbuild plugin to build with js_of_ocaml *)

(**
   {2 Initialize}

   Initialize the js_of_ocaml ocamlbuild plugin with the following
   code in {i myocamlbuild.ml}:
   {[
     let _ = Ocamlbuild_plugin.dispatch Ocamlbuild_js_of_ocaml.dispatcher
   ]}
   {b Side note}: {!Ocamlbuild_plugin.dispatch} should be used only
   once. The last call will override previous ones.

   {3 With Oasis}

   If you use oasis, {b myocamlbuild.ml} should look like:
   {[
     let _ =
       Ocamlbuild_plugin.dispatch
         (fun hook ->
            dispatch_default hook;
            Ocamlbuild_js_of_ocaml.dispatcher
              ~oasis_executables:["src/yourprogram.byte"]
              hook;
         )
   ]}


   {2 Build }

   Build a JavaScript program {b myprog.js} by calling the command:
   {[
     ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" myprog.js
   ]}
   It will first build the bytecode {b myprog.byte} and finally produce {b myprog.js} (in {b _build}).


   {2 Options}

   One can pass option to the Js_of_ocaml compiler using tags.
   See <<a_manual chapter="options" |Options>>.

   Available tags:
   - {b pretty}: Pretty print the generated javascript.
   - {b debuginfo}: Output debug information.
   - {b noinline}: Disable inlining
   - {b sourcemap}: Generate sourcemap
   - {b tailcall(none)}: Set the tailcall optimisation (default "trampoline")
   - {b opt(3)}: Set the compilation profile (default 1)
   - {b debug}: enables {b pretty}, {b debuginfo}, {b sourcemap}


   {3 Exemples}

   In the {b _tags} file:
   {[
     <myprog.js>:pretty, opt(3)
   ]}


   {2 Dispatchers} *)

val dispatcher :
  ?oasis_executables:Ocamlbuild_plugin.Pathname.t list -> Ocamlbuild_plugin.hook -> unit
(** The main dispatcher

    [?oasis_executables] is the paths of the executables
    (having the .byte extension) you want to compile
    as a javascript executable. The former executables are still compiled.

    Side note: {!Ocamlbuild_plugin.dispatch} should be used only once as
    it record only one function for an ocamlbuild module.
*)

(** {2 Low level functions} *)

val oasis_support : executables:Ocamlbuild_plugin.Pathname.t list -> unit
(** Map each targets given as argument to ocamlbuild and replace each element
    that exists in [~executables] by its corresponding .js target.
*)
