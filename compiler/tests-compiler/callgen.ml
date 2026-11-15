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

open! Util

(* With [--enable callgen], applications are not delegated to [caml_callN]
   but inlined: the function and the arguments then occur several times
   (arity test, call to [caml_call_gen], trampoline). They must be
   evaluated only once, and a function literal must not be duplicated:
   its variables would be bound twice. *)

let compile ?effects code =
  match compile_and_parse ?effects ~flags:[ "--enable=callgen" ] code with
  | program -> Some program
  | exception Failure _ -> None

(* Only keep the error message, without the (machine-dependent) command *)
let print_error output =
  String.split_on_char '\n' output
  |> List.map String.trim
  |> List.filter (String.starts_with ~prefix:"Error:")
  |> List.iter print_endline

let%expect_test "function literal passed to an unknown function" =
  (match
     compile
       {|
         let test iter =
           let r = ref 0 in
           iter (fun a -> let v = a + !r in if v > 0 then r := v);
           !r
       |}
   with
  | Some program -> print_fun_decl program (Some "test")
  | None -> print_error [%expect.output]);
  [%expect
    {|
    function test(iter){
     var r = [0, 0];
     function _a_(a){
      var v = a + r[1] | 0, _a_ = 0 < v ? 1 : 0, _a_ = _a_ && (r[1] = v, 0);
      return _a_;
     }
     (iter.l >= 0 ? iter.l : iter.l = iter.length) === 1
      ? iter(_a_)
      : runtime.caml_call_gen(iter, [_a_]);
     return r[1];
    }
    //end
    |}]

let%expect_test "function computed by a call" =
  (match
     compile
       {|
         let count = ref 0
         let make g x = incr count; g x
         let test g x = (make g x) 1
       |}
   with
  | Some program -> print_fun_decl program (Some "test")
  | None -> print_error [%expect.output]);
  [%expect
    {|
    function test(g, x){
     var _a_ = make(g, x);
     return (_a_.l >= 0 ? _a_.l : _a_.l = _a_.length) === 1
             ? _a_(1)
             : runtime.caml_call_gen(_a_, [1]);
    }
    //end
    |}]

let continuations =
  {|
     let cb = ref (fun () -> ())
     let check = ref (fun x -> x > 0)
     let fail = ref (fun x -> x)

     let test x =
       !cb ();
       let y = if !check x then x + 1 else x in
       if !check y then
         let g z = if z > 2 then y else !fail z in
         g (y * 2)
       else !fail y
|}

let%expect_test "continuations / CPS" =
  (match compile ~effects:`Cps continuations with
  | Some program -> print_fun_decl program (Some "test")
  | None -> print_error [%expect.output]);
  [%expect
    {|
    function test(x, cont){
     var _a_ = cb[1];
     function _b_(_b_){
      _b_ = check[1];
      function _c_(_c_){
       var y = _c_ ? x + 1 | 0 : x, _c_ = check[1];
       function _d_(_d_){
        if(! _d_){
         _d_ = fail[1];
         return runtime.caml_stack_check_depth()
                 ? (_d_.l
                     >= 0
                     ? _d_.l
                     : _d_.l = _d_.length)
                   === 2
                   ? _d_(y, cont)
                   : runtime.caml_call_gen(_d_, [y, cont])
                 : runtime.caml_trampoline_return(_d_, [y, cont], 0);
        }
        var z = y * 2 | 0;
        if(2 < z) return cont(y);
        _d_ = fail[1];
        return runtime.caml_stack_check_depth()
                ? (_d_.l
                    >= 0
                    ? _d_.l
                    : _d_.l = _d_.length)
                  === 2
                  ? _d_(z, cont)
                  : runtime.caml_call_gen(_d_, [z, cont])
                : runtime.caml_trampoline_return(_d_, [z, cont], 0);
       }
       return runtime.caml_stack_check_depth()
               ? (_c_.l
                   >= 0
                   ? _c_.l
                   : _c_.l = _c_.length)
                 === 2
                 ? _c_(y, _d_)
                 : runtime.caml_call_gen(_c_, [y, _d_])
               : runtime.caml_trampoline_return(_c_, [y, _d_], 0);
      }
      return runtime.caml_stack_check_depth()
              ? (_b_.l
                  >= 0
                  ? _b_.l
                  : _b_.l = _b_.length)
                === 2
                ? _b_(x, _c_)
                : runtime.caml_call_gen(_b_, [x, _c_])
              : runtime.caml_trampoline_return(_b_, [x, _c_], 0);
     }
     return runtime.caml_stack_check_depth()
             ? (_a_.l
                 >= 0
                 ? _a_.l
                 : _a_.l = _a_.length)
               === 2
               ? _a_(0, _b_)
               : runtime.caml_call_gen(_a_, [0, _b_])
             : runtime.caml_trampoline_return(_a_, [0, _b_], 0);
    }
    //end
    |}]

let%expect_test "continuations / double translation" =
  (match compile ~effects:`Double_translation continuations with
  | Some _ -> print_endline "ok"
  | None -> print_error [%expect.output]);
  [%expect {| ok |}]

(* Tests from #2592 *)

let run ?effects code =
  match compile_and_run ?effects ~pretty:false ~flags:[ "--enable=callgen" ] code with
  | () -> true
  | exception Failure _ -> false

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
  if not (run prog) then print_error [%expect.output];
  [%expect {|
    called
    3
    |}];
  (match compile prog with
  | Some program -> print_fun_decl program (Some "test")
  | None -> print_error [%expect.output]);
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
  (match compile ~effects:`Cps continuation_prog with
  | Some program ->
      print_program program;
      Printf.printf "%d\n" (count_trampoline_returns [%expect.output])
  | None -> print_error [%expect.output]);
  [%expect {| 7 |}];
  if not (run ~effects:`Cps continuation_prog) then print_error [%expect.output];
  [%expect {| 6 |}]

let%expect_test "continuations not duplicated (double translation)" =
  (match compile ~effects:`Double_translation continuation_prog with
  | Some program ->
      print_program program;
      Printf.printf "%d\n" (count_trampoline_returns [%expect.output])
  | None -> print_error [%expect.output]);
  [%expect {| 7 |}];
  if not (run ~effects:`Double_translation continuation_prog)
  then print_error [%expect.output];
  [%expect {| 6 |}]
