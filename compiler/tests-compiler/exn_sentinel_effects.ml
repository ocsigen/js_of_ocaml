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

(* Functions called under an exception handler signal exceptions by
   returning a sentinel value with effects as well, provided they are
   only called directly, and never through a CPS or trampolined call
   (see [Raising_calls]) *)

open! Util

[@@@if ocaml_version >= (5, 0, 0) && not oxcaml]

(* Use separate compilation, whose output does not depend on the
   standard library, but hide all definitions so that no function
   escapes *)
let compile_and_parse ~effects prog =
  compile_and_parse ~effects ("include (struct\n" ^ prog ^ "\nend : sig end)\n")

let prog =
  {|
open Effect
open Effect.Deep

type _ Effect.t += Get : int Effect.t

let rec find x = function
  | [] -> raise Not_found
  | y :: r -> if x = y then y else find x r

let search x l = try find x l with Not_found -> -1

(* Performs an effect: CPS *)
let search_eff l = try find (perform Get) l with Not_found -> -2

let three l = find 1 l + find 2 l + find 3 l

let () =
  let l = Sys.opaque_identity [ 1; 2; 3 ] in
  Printf.printf "%d %d\n" (search 2 l) (search 5 l);
  Printf.printf "%d\n" (try three l with Not_found -> -3);
  Printf.printf "%d\n" (try three [ 1; 2 ] with Not_found -> -3);
  let run (v : int) =
    match_with
      search_eff
      l
      { retc = (fun x -> x)
      ; exnc = raise
      ; effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Get -> Some (fun (k : (a, _) continuation) -> continue k v)
            | _ -> None)
      }
  in
  Printf.printf "%d %d\n" (run 3) (run 7);
  Printf.printf
    "%d\n"
    (match_with
       (fun () -> search 3 l + try find 9 l with Not_found -> 100)
       ()
       { retc = (fun x -> x); exnc = raise; effc = (fun _ -> None) })
|}

let%expect_test "cps" =
  let program = compile_and_parse ~effects:`Cps prog in
  print_fun_decl program (Some "find");
  print_fun_decl program (Some "search_eff");
  print_fun_decl program (Some "three");
  [%expect
    {|
    function find(x, _i_){
     for(;;){
      if(! _i_)
       return caml_raise_sentinel(caml_maybe_attach_backtrace(Stdlib[8], 1));
      var r = _i_[2], y = _i_[1];
      if(runtime.caml_equal(x, y)) return y;
      _i_ = r;
     }
    }
    //end
    function search_eff(l, cont){
     runtime.caml_push_trap
      (function(exn){
        if(exn === Stdlib[8]) return cont(- 2);
        var raise = caml_pop_trap(), exn$0 = caml_maybe_attach_backtrace(exn, 0);
        return raise(exn$0);
       });
     return runtime.caml_perform_effect
             (Get,
              function(_i_){
               _i_ = find(_i_, l);
               if(_i_ === caml_exn_sentinel) throw caml_take_exn();
               caml_pop_trap();
               return cont(_i_);
              });
    }
    //end
    function three(l){
     var _g_ = find(3, l);
     if(_g_ === caml_exn_sentinel) return _g_;
     var _h_ = find(2, l);
     if(_h_ === caml_exn_sentinel) return _h_;
     var _i_ = find(1, l);
     return _i_ === caml_exn_sentinel ? _i_ : (_i_ + _h_ | 0) + _g_ | 0;
    }
    //end
    |}];
  compile_and_run ~effects:`Cps prog;
  [%expect
    {|
           2 -1
           6
           -3
           3 -2
           103
           |}]

let%expect_test "double translation" =
  let program = compile_and_parse ~effects:`Double_translation prog in
  print_double_fun_decl program "search_eff";
  [%expect
    {|
    function search_eff$0(l){
     a:
     try{
      var _g_ = find(runtime.caml_raise_unhandled(Get), l);
      if(_g_ === caml_exn_sentinel){var exn = caml_take_exn(); break a;}
      return _g_;
     }
     catch(exn$0){exn = caml_wrap_exception(exn$0);}
     if(exn === Stdlib[8]) return - 2;
     throw caml_maybe_attach_backtrace(exn, 0);
    }
    //end
    function search_eff$1(l, cont){
     runtime.caml_push_trap
      (function(exn$0){
        if(exn$0 === Stdlib[8]) return cont(- 2);
        var raise = caml_pop_trap(), exn = caml_maybe_attach_backtrace(exn$0, 0);
        return raise(exn);
       });
     return runtime.caml_perform_effect
             (Get,
              function(_g_){
               _g_ = find(_g_, l);
               if(_g_ === caml_exn_sentinel) throw caml_take_exn();
               caml_pop_trap();
               return cont(_g_);
              });
    }
    //end
    var search_eff = caml_cps_closure(search_eff$0, search_eff$1);
    //end
    |}];
  compile_and_run ~effects:`Double_translation prog;
  [%expect
    {|
           2 -1
           6
           -3
           3 -2
           103
           |}]

(* With double translation, functions called through closures are
   wrapped as well; a wrapper gets the CPS version of the function *)
let prog_wrapped =
  {|
open Effect
open Effect.Deep

type _ Effect.t += Get : int Effect.t

let rec find x = function
  | [] -> raise Not_found
  | y :: r -> if x = y then y else find x r

(* Has a CPS version; its direct-style version is raising *)
let maybe_eff b x l = if b then perform Get else find x l

let handle f =
  match_with f ()
    { retc = (fun x -> x)
    ; exnc = raise
    ; effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Get -> Some (fun (k : (a, _) continuation) -> continue k 42)
          | _ -> None)
    }

let () =
  let l = Sys.opaque_identity [ 1; 2; 3 ] in
  (* [find] and [maybe_eff] escape *)
  let tbl = Hashtbl.create 1 in
  Hashtbl.add tbl 0 find;
  let tbl' = Hashtbl.create 1 in
  Hashtbl.add tbl' 0 maybe_eff;
  let find' = Hashtbl.find tbl 0 and maybe_eff' = Hashtbl.find tbl' 0 in
  let p = Printf.printf "%d\n" in
  p (try find 2 l with Not_found -> -1);
  p (try find 5 l with Not_found -> -1);
  p (try maybe_eff false 3 l with Not_found -> -1);
  p (try maybe_eff false 5 l with Not_found -> -1);
  p (try find' 5 l with Not_found -> -2);
  p (try maybe_eff' false 5 l with Not_found -> -2);
  p (handle (fun () -> try find' 5 l with Not_found -> -3));
  p (handle (fun () -> try maybe_eff' true 5 l with Not_found -> -3));
  p (handle (fun () -> try maybe_eff' false 5 l with Not_found -> -3));
  p (handle (fun () -> try maybe_eff' false 1 l with Not_found -> -3));
  p (handle (fun () -> find' 1 l + maybe_eff' true 0 l));
  p (try handle (fun () -> find' 7 l) with Not_found -> -4);
  p (try handle (fun () -> maybe_eff' false 7 l) with Not_found -> -4)
|}

let%expect_test "double translation, wrapped functions" =
  let program = compile_and_parse ~effects:`Double_translation prog_wrapped in
  print_double_fun_decl program "maybe_eff";
  [%expect
    {|
    function maybe_eff$1(b, x, l){
     var _g_ = maybe_eff$3(b, x, l);
     if(_g_ === caml_exn_sentinel) throw caml_take_exn();
     return _g_;
    }
    //end
    function maybe_eff$2(b, x, l, cont){
     return b ? runtime.caml_perform_effect(Get, cont) : cont(find(x, l));
    }
    //end
    var maybe_eff = caml_cps_closure(maybe_eff$1, maybe_eff$2);
    //end
    |}];
  compile_and_run ~effects:`Double_translation prog_wrapped;
  [%expect
    {|
           2
           -1
           3
           -1
           -2
           -2
           -3
           42
           -3
           1
           43
           -4
           -4
           |}];
  compile_and_run ~effects:`Cps prog_wrapped;
  [%expect
    {|
           2
           -1
           3
           -1
           -2
           -2
           -3
           42
           -3
           1
           43
           -4
           -4
           |}]
