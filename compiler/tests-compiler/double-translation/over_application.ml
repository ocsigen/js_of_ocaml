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

(* [make] does not perform any effect, but it is applied to more arguments
   than it takes, and the closure it returns does. The call is not exact,
   since [g] may be one of two functions of different arities. The call point
   must be in CPS, and so must be the function it belongs to. *)
let%expect_test "over-application of a function without CPS version" =
  compile_and_run
    ~effects:`Double_translation
    {|
    [@@@alert "-unsafe_effects"] (* OxCaml warns about [Effect.perform] *)
    open Effect
    open Effect.Deep
    type _ Effect.t += E : int -> int Effect.t
    let[@inline never] make () =
      let r = ref 1 in
      fun x -> perform (E x) + !r
    let g = if Array.length Sys.argv > 100 then fun () x -> x else make
    let () =
      print_int
        (try_with
           (fun () -> g () 1)
           ()
           { effc =
               (fun (type a) (e : a Effect.t) ->
                 match e with
                 | E n -> Some (fun (k : (a, _) continuation) -> continue k (n + 40))
                 | _ -> None)
           })
    |};
  [%expect {| 42 |}]
