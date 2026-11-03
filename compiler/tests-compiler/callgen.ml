(* Js_of_ocaml compiler
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

(* With [--enable callgen], the arity check is inlined at each call site;
   the function and its arguments must still be evaluated only once. *)

let%expect_test "function evaluated once" =
  let prog =
    {|
    let h = ref (fun () -> print_endline "called"; fun a b -> a + b)
    let () =
      if Array.length Sys.argv > 10 then
        h := (fun () -> fun a -> print_string "partial"; fun b -> a - b)
    let test () = let k = !h () in k 1 2
    let () = print_int (test ()); print_newline ()
    |}
  in
  Util.compile_and_run ~pretty:false ~flags:[ "--enable"; "callgen" ] prog;
  [%expect {|
    called
    3
    |}];
  let program = Util.compile_and_parse ~flags:[ "--enable"; "callgen" ] prog in
  Util.print_fun_decl program (Some "test");
  [%expect
    {|
           function test(_b_){
            _b_ = h[1];
            var
             k =
               (_b_.l >= 0 ? _b_.l : _b_.l = _b_.length) === 1
                ? _b_(0)
                : runtime.caml_call_gen(_b_, [0]);
            return (k.l >= 0 ? k.l : k.l = k.length) === 2
                    ? k(1, 2)
                    : runtime.caml_call_gen(k, [1, 2]);
           }
           //end
           |}]

(* With CPS, each argument list contains the continuation, which can
   contain the next call: duplicating the arguments made the generated
   code exponential in the number of consecutive calls. We count the
   calls to [caml_trampoline_return] in the generated code: there is
   one per call site when nothing is duplicated. *)
[@@@if ocaml_version >= (5, 0, 0)]

let continuation_prog =
  {|
    type _ Effect.t += E : int Effect.t
    let h = ref (fun x -> x + 1)
    let () =
      if Array.length Sys.argv > 10 then h := (fun x -> Effect.perform E + x)
      else if Array.length Sys.argv > 5 then h := (fun x y -> x + y) 1
    let f x =
      let x = !h x in let x = !h x in let x = !h x in
      let x = !h x in let x = !h x in let x = !h x in
      x
    let () =
      print_int
        (Effect.Deep.try_with f 0
           { effc =
               (fun (type a) (e : a Effect.t) ->
                 match e with
                 | E -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
                     Effect.Deep.continue k 1)
                 | _ -> None) })
    |}

let count_trampoline_returns s =
  let pattern = "caml_trampoline_return(" in
  let n = ref 0 in
  for i = 0 to String.length s - String.length pattern do
    if String.equal (String.sub s i (String.length pattern)) pattern then incr n
  done;
  !n

let%expect_test "continuations not duplicated (CPS)" =
  Util.print_program
    (Util.compile_and_parse
       ~effects:`Cps
       ~flags:[ "--enable"; "callgen" ]
       continuation_prog);
  Printf.printf "%d\n" (count_trampoline_returns [%expect.output]);
  [%expect {| 7 |}];
  Util.compile_and_run ~effects:`Cps ~flags:[ "--enable"; "callgen" ] continuation_prog;
  [%expect {| 6 |}]

let%expect_test "continuations not duplicated (double translation)" =
  Util.print_program
    (Util.compile_and_parse
       ~effects:`Double_translation
       ~flags:[ "--enable"; "callgen" ]
       continuation_prog);
  Printf.printf "%d\n" (count_trampoline_returns [%expect.output]);
  [%expect {| 7 |}];
  Util.compile_and_run
    ~effects:`Double_translation
    ~flags:[ "--enable"; "callgen" ]
    continuation_prog;
  [%expect {| 6 |}]
