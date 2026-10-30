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
   returning a sentinel value (see [Raising_calls]) *)

open! Util

(* In OxCaml, raise is always reraise, which changes the generated code. *)
[@@@if not oxcaml]

let prog =
  {|
(* Exported, hence also called through closures: wrapped *)
let rec find x = function
  | [] -> raise Not_found
  | y :: r -> if x = y then y else find x r

(* A tail call to a raising function returns the sentinel as is *)
let find_pos x l = if x < 0 then raise Exit else find x l

let search x l = try find_pos x l with Not_found | Exit -> -1

(* Assert_failure is thrown, even from a raising function *)
let checked x l =
  if x > 100 then assert false;
  find x l

let check x l = try checked x l with Not_found -> -1

(* A raising function only called directly, called directly from
   enough places outside handlers to get a throwing wrapper *)
let direct l =
  let rec mem x = function
    | [] -> raise Not_found
    | y :: r -> if x = y then y else mem x r
  in
  let a = try mem 1 l with Not_found -> 0 in
  let b = mem 2 l in
  let c = mem 3 l in
  let d = mem 4 l in
  a + b + c + d

(* The exception is not used *)
let notrace x = try if x then raise_notrace Exit; 1 with _ -> 0

(* Mutually recursive raising functions *)
let rec count n = if n < 0 then raise Not_found else if n = 0 then 0 else 1 + count' (n - 1)
and count' n = 1 + count (n - 1)

let parity n = try count n with Not_found -> -1
|}

let%expect_test "generated code" =
  let program = compile_and_parse prog in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_equal = runtime.caml_equal,
        caml_exn_sentinel = runtime.caml_exn_sentinel,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_raise_sentinel = runtime.caml_raise_sentinel,
        caml_take_exn = runtime.caml_take_exn,
        caml_wrap_exception = runtime.caml_wrap_exception,
        Stdlib = runtime.caml_get_global("Stdlib"),
        Assert_failure = runtime.caml_get_global_predef("Assert_failure"),
        _a_ = [0, runtime.caml_string_of_jsbytes("test.ml"), 14, 18];
       function find$0(x, _a_){
        for(;;){
         if(! _a_)
          return caml_raise_sentinel(caml_maybe_attach_backtrace(Stdlib[8], 1));
         var r = _a_[2], y = _a_[1];
         if(caml_equal(x, y)) return y;
         _a_ = r;
        }
       }
       function find(x, _a_){
        _a_ = find$0(x, _a_);
        if(_a_ === caml_exn_sentinel) throw caml_take_exn();
        return _a_;
       }
       function find_pos$0(x, l){
        return 0 <= x
                ? find$0(x, l)
                : caml_raise_sentinel(caml_maybe_attach_backtrace(Stdlib[3], 1));
       }
       function find_pos(x, l){
        var _a_ = find_pos$0(x, l);
        if(_a_ === caml_exn_sentinel) throw caml_take_exn();
        return _a_;
       }
       function search(x, l){
        a:
        try{
         var _a_ = find_pos$0(x, l);
         if(_a_ === caml_exn_sentinel){var exn = caml_take_exn(); break a;}
         return _a_;
        }
        catch(exn$0){exn = caml_wrap_exception(exn$0);}
        if(exn !== Stdlib[8] && exn !== Stdlib[3])
         throw caml_maybe_attach_backtrace(exn, 0);
        return - 1;
       }
       function checked$0(x, l){
        return 100 < x
                ? caml_raise_sentinel
                  (caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1))
                : find$0(x, l);
       }
       function checked(x, l){
        var _a_ = checked$0(x, l);
        if(_a_ === caml_exn_sentinel) throw caml_take_exn();
        return _a_;
       }
       function check(x, l){
        a:
        try{
         var _a_ = checked$0(x, l);
         if(_a_ === caml_exn_sentinel){var exn = caml_take_exn(); break a;}
         return _a_;
        }
        catch(exn$0){exn = caml_wrap_exception(exn$0);}
        if(exn === Stdlib[8]) return - 1;
        throw caml_maybe_attach_backtrace(exn, 0);
       }
       function direct(l){
        function mem(x, _a_){
         for(;;){
          if(! _a_)
           return caml_raise_sentinel(caml_maybe_attach_backtrace(Stdlib[8], 1));
          var r = _a_[2], y = _a_[1];
          if(caml_equal(x, y)) return y;
          _a_ = r;
         }
        }
        function mem$0(x, _a_){
         _a_ = mem(x, _a_);
         if(_a_ === caml_exn_sentinel) throw caml_take_exn();
         return _a_;
        }
        a:
        {
         b:
         try{
          var _a_ = mem(1, l);
          if(_a_ === caml_exn_sentinel){var exn = caml_take_exn(); break b;}
          var a = _a_;
          break a;
         }
         catch(exn$0){exn = caml_wrap_exception(exn$0);}
         if(exn !== Stdlib[8]) throw caml_maybe_attach_backtrace(exn, 0);
         a = 0;
        }
        var b = mem$0(2, l), c = mem$0(3, l), d = mem$0(4, l);
        return ((a + b | 0) + c | 0) + d | 0;
       }
       function notrace(x){a: try{if(x) break a; return 1;}catch(exn){} return 0;}
       function count$1(n){
        if(0 > n)
         return caml_raise_sentinel(caml_maybe_attach_backtrace(Stdlib[8], 1));
        if(0 === n) return 0;
        var _a_ = count$2(n - 1 | 0);
        return _a_ === caml_exn_sentinel ? _a_ : 1 + _a_ | 0;
       }
       function count(n){
        var _a_ = count$1(n);
        if(_a_ === caml_exn_sentinel) throw caml_take_exn();
        return _a_;
       }
       function count$2(n){
        var _a_ = count$1(n - 1 | 0);
        return _a_ === caml_exn_sentinel ? _a_ : 1 + _a_ | 0;
       }
       function count$0(n){
        var _a_ = count$2(n);
        if(_a_ === caml_exn_sentinel) throw caml_take_exn();
        return _a_;
       }
       function parity(n){
        a:
        try{
         var _a_ = count$1(n);
         if(_a_ === caml_exn_sentinel){var exn = caml_take_exn(); break a;}
         return _a_;
        }
        catch(exn$0){exn = caml_wrap_exception(exn$0);}
        if(exn === Stdlib[8]) return - 1;
        throw caml_maybe_attach_backtrace(exn, 0);
       }
       find.r = find$0;
       find_pos.r = find_pos$0;
       checked.r = checked$0;
       count.r = count$1;
       count$0.r = count$2;
       runtime.caml_register_global
        ([0,
          find,
          find_pos,
          search,
          checked,
          check,
          direct,
          notrace,
          count,
          count$0,
          parity],
         "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "run" =
  compile_and_run
    (prog
    ^ {|
let catch f = try string_of_int (f ()) with
  | Not_found -> "Not_found"
  | Exit -> "Exit"
  | Assert_failure _ -> "Assert_failure"

let l = [ 1; 2; 3; 4 ]

let () =
  let g = Sys.opaque_identity find in
  List.iter print_endline
    [ catch (fun () -> find 3 l)
    ; catch (fun () -> find 5 l)
    ; catch (fun () -> g 5 l)
    ; catch (fun () -> find_pos 5 l)
    ; catch (fun () -> find_pos (-1) l)
    ; catch (fun () -> search 2 l)
    ; catch (fun () -> search 5 l)
    ; catch (fun () -> search (-1) l)
    ; catch (fun () -> check 2 l)
    ; catch (fun () -> check 5 l)
    ; catch (fun () -> check 200 l)
    ; catch (fun () -> direct l)
    ; catch (fun () -> direct [ 2; 3; 4 ])
    ; catch (fun () -> direct [ 1; 2; 3 ])
    ; catch (fun () -> notrace true)
    ; catch (fun () -> notrace false)
    ; catch (fun () -> parity 4)
    ; catch (fun () -> parity 3)
    ; catch (fun () -> count 3)
    ; catch (fun () -> checked 200 l)
    ]
|}
    );
  [%expect
    {|
           3
           Not_found
           Not_found
           Not_found
           Exit
           2
           -1
           -1
           2
           -1
           Assert_failure
           10
           9
           Not_found
           0
           1
           4
           -1
           Not_found
           Assert_failure
           |}]

(* The body of a wrapped function called through a variable other than
   the function itself is retrieved from the wrapper *)
let prog' =
  {|
let rec find x = function
  | [] -> raise Not_found
  | y :: r -> if x = y then y else find x r

let rec apply h n x l =
  if n = 0
  then
    try
      let r = h x l in
      r
    with Not_found -> -1
  else
    let a = apply h (n - 1) (x + 1) l in
    let b = apply h (n - 1) x l in
    a + b

let () =
  (* [find] escapes *)
  let tbl = Hashtbl.create 1 in
  Hashtbl.add tbl 0 find;
  print_int (apply find 2 1 (Sys.opaque_identity [ 1; 2 ]));
  print_newline ();
  print_int (apply find 3 1 (Sys.opaque_identity [ 1 ]));
  print_newline ();
  print_int (try (Hashtbl.find tbl 0) 5 [ 1 ] with Not_found -> 7);
  print_newline ()
|}

let%expect_test "raising body" =
  let program = compile_and_parse_whole_program ~flags:[ "--opt"; "2" ] prog' in
  print_fun_decl program (Some "apply");
  [%expect
    {|
    function apply(h, n, x, l){
     if(0 !== n){
      var a = apply(h, n - 1 | 0, x + 1 | 0, l), b = apply(h, n - 1 | 0, x, l);
      return a + b | 0;
     }
     a:
     try{
      var h$0 = h.r, r = h$0(x, l);
      if(r === caml_exn_sentinel){var exn = caml_take_exn(); break a;}
      return r;
     }
     catch(exn$0){exn = caml_wrap_exception(exn$0);}
     if(exn === Not_found) return - 1;
     throw caml_maybe_attach_backtrace(exn, 0);
    }
    //end
    |}];
  compile_and_run ~flags:[ "--opt"; "2" ] prog';
  [%expect {|
           4
           -6
           7
           |}]
