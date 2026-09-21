(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

open! Util

[@@@if ocaml_version >= (5, 0, 0)]

(* A function only needs a CPS version if it may run below an effect
   handler, that is, if its stack frame may be captured by a continuation.
   [call_unknown] calls a function unknown to the compiler, which could
   perform an effect, but it is only called from toplevel code, which never
   runs below a handler: it is compiled in direct style only. [in_fiber] is
   called from the fiber body, which escapes since it is passed to
   [try_with]: it may run below the handler and is double-translated.

   The functions are local: the toplevel values of a compilation unit are
   exported, hence escape, and may then be called from anywhere. *)
let%expect_test "functions never below an effect handler are not double-translated" =
  let code =
    {|
    [@@@alert "-unsafe_effects"] (* OxCaml warns about [Effect.perform] *)
    open Effect
    open Effect.Deep
    type _ Effect.t += E : unit Effect.t
    let l = ref []
    let fns = ref [ (fun () -> ()) ]
    let () =
      let in_fiber () =
        l := (fun () -> ()) :: !l; (* prevent inlining *)
        perform E
      in
      let call_unknown () =
        l := (fun () -> ()) :: !l; (* prevent inlining *)
        match !fns with
        | g :: _ -> g ()
        | [] -> ()
      in
      (* Two call sites, so that the functions do not get inlined *)
      call_unknown ();
      call_unknown ();
      let handler =
        { effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | E -> Some (fun (k : (a, _) continuation) -> continue k ())
              | _ -> None) }
      in
      try_with (fun () -> in_fiber ()) () handler;
      try_with (fun () -> in_fiber (); in_fiber ()) () handler
    |}
  in
  let program = compile_and_parse ~effects:`Double_translation code in
  print_fun_decl program (Some "call_unknown");
  print_double_fun_decl program "call_unknown";
  print_double_fun_decl program "in_fiber";
  [%expect
    {|
           function call_unknown(_c_){
            l[1] = [0, function(param){return 0;}, l[1]];
            _c_ = fns[1];
            if(! _c_) return;
            var g = _c_[1];
            return caml_call1(g, 0);
           }
           //end
           not found
           function in_fiber$0(_c_){
            _c_ = l[1];
            l[1] = [0, _b_(), _c_];
            return runtime.caml_raise_unhandled(E);
           }
           //end
           function in_fiber$1(_c_, cont){
            _c_ = l[1];
            l[1] = [0, _b_(), _c_];
            return runtime.caml_perform_effect(E, cont);
           }
           //end
           var in_fiber = caml_cps_closure(in_fiber$0, in_fiber$1);
           //end
           |}]
