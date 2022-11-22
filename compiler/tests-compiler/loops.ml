(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

open Util

let%expect_test "rec-fun" =
  let program =
    compile_and_parse
      {|
    let rec fun_with_loop acc = function
  | [] -> List.rev (List.rev (List.rev acc))
  | x :: xs -> fun_with_loop (x :: acc) xs
|}
  in
  print_fun_decl program (Some "fun_with_loop");
  [%expect
    {|
    function fun_with_loop(acc,param)
     {var acc$0=acc,param$0=param;
      for(;;)
       {if(param$0)
         {var
           param$1=param$0[2],
           x=param$0[1],
           acc$1=[0,x,acc$0],
           acc$0=acc$1,
           param$0=param$1;
          continue}
        var
         _a_=caml_call1(Stdlib_List[9],acc$0),
         _b_=caml_call1(Stdlib_List[9],_a_);
        return caml_call1(Stdlib_List[9],_b_)}} |}]

let%expect_test "rec-fun-2" =
  let program =
    compile_and_parse
      {|
let rec fun_with_loop acc = function
  | [] -> List.rev (List.rev (List.rev acc))
  | [ 1 ] ->
      let a = ref acc in
      for i = 0 to 10 do
        a := 1 :: !a
      done;
      !a
  | x :: xs ->
      let a = ref acc in
      for i = 0 to 10 do
        a := 1 :: !a
      done;
      fun_with_loop (x :: !a) xs
  |}
  in
  print_fun_decl program (Some "fun_with_loop");
  [%expect
    {|
    function fun_with_loop(acc,param)
     {var acc$0=acc,param$0=param;
      a:
      for(;;)
       {if(param$0)
         {var _a_=param$0[1];
          if(1 === _a_ && ! param$0[2])
           {var a$0=[0,acc$0],i$0=0;
            for(;;)
             {a$0[1] = [0,1,a$0[1]];
              var _c_=i$0 + 1 | 0;
              if(10 !== i$0){var i$0=_c_;continue}
              return a$0[1]}}
          var xs=param$0[2],a=[0,acc$0],i=0;
          for(;;)
           {a[1] = [0,1,a[1]];
            var _b_=i + 1 | 0;
            if(10 !== i){var i=_b_;continue}
            var acc$1=[0,_a_,a[1]],acc$0=acc$1,param$0=xs;
            continue a}}
        var
         _d_=caml_call1(Stdlib_List[9],acc$0),
         _e_=caml_call1(Stdlib_List[9],_d_);
        return caml_call1(Stdlib_List[9],_e_)}}
 |}]

let%expect_test "for-for-while" =
  let program =
    compile_and_parse
      {|
let id = ref 0
let for_for_while () =
  for k = 1 to 10 do
    for j = 1 to 10 do
      while k * j < 10 do
        incr id
      done
    done
  done
  |}
  in
  print_fun_decl program (Some "for_for_while");
  [%expect
    {|
    function for_for_while(param)
     {var k=1;
      a:
      for(;;)
       {var j=1;
        b:
        for(;;)
         for(;;)
          {if(10 <= runtime.caml_mul(k,j))
            {var _b_=j + 1 | 0;
             if(10 !== j){var j=_b_;continue b}
             var _a_=k + 1 | 0;
             if(10 !== k){var k=_a_;continue a}
             return 0}
           id[1]++;
           continue}}} |}]

let%expect_test "for-for-while-try" =
  let program =
    compile_and_parse
      {|
let id = ref 0
let for_for_while () =
  for k = 1 to 10 do
    for j = 1 to 10 do
      while k / j < 10 do
        ignore (try k / j with _ -> raise Not_found);
        incr id
      done
    done
  done
  |}
  in
  print_fun_decl program (Some "for_for_while");
  [%expect
    {|
    function for_for_while(param)
     {var k=1;
      a:
      for(;;)
       {var j=1;
        b:
        for(;;)
         for(;;)
          {if(10 <= caml_div(k,j))
            {var _b_=j + 1 | 0;
             if(10 !== j){var j=_b_;continue b}
             var _a_=k + 1 | 0;
             if(10 !== k){var k=_a_;continue a}
             return 0}
           try {caml_div(k,j)}catch(_c_){throw Stdlib[8]}
           id[1]++;
           continue}}} |}]

let%expect_test "loop seq.ml" =
  let program =
    compile_and_parse
      {|

    type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let rec equal eq xs ys =
  match xs(), ys() with
  | Nil, Nil ->
      true
  | Cons (x, xs), Cons (y, ys) ->
      equal eq xs ys
  | Nil, Cons (_, _)
  | Cons (_, _), Nil ->
      false
  |}
  in
  print_fun_decl program (Some "equal");
  [%expect
    {|
    function equal(eq,xs,ys)
     {var xs$0=xs,ys$0=ys;
      for(;;)
       {var match=caml_call1(xs$0,0),match$0=caml_call1(ys$0,0);
        if(match)
         {if(match$0)
           {var ys$1=match$0[2],xs$1=match[2],xs$0=xs$1,ys$0=ys$1;continue}}
        else
         if(! match$0)return 1;
        return 0}} |}]

let%expect_test "try-catch inside loop" =
  let program =
    compile_and_parse
      {|
let f t x =
  let rec loop t x =
    match Hashtbl.find t x with
    | [ y ] -> y = x + 1 || loop t y
    | _ -> false
    | exception Exit -> false
  in
  let other t x =
    match Hashtbl.find t x with
    | exception Not_found -> -1
    | [ x ] -> if loop t x then 1 else 2
    | _ -> -2
  in
  other t x
  |}
  in
  print_fun_decl program (Some "f");
  [%expect
    {|
    function f(t,x)
     {function other(t,x$0)
       {try
         {var val$0=caml_call2(Stdlib_Hashtbl[6],t,x$0)}
        catch(_d_)
         {_d_ = caml_wrap_exception(_d_);
          if(_d_ === Stdlib[8])return - 1;
          throw _d_}
        if(val$0 && ! val$0[2])
         {var x$1=val$0[1],x=x$1;
          for(;;)
           {try
             {var switch$0=0,val=caml_call2(Stdlib_Hashtbl[6],t,x);switch$0 = 1}
            catch(_c_)
             {_c_ = caml_wrap_exception(_c_);
              if(_c_ !== Stdlib[3])throw _c_;
              var _b_=0}
            if(switch$0)
             {var switch$1=0;
              if(val && ! val[2])
               {var y=val[1],_a_=y === (x + 1 | 0)?1:0;
                if(! _a_){var x=y;continue}
                var _b_=_a_}
              else
               switch$1 = 1;
              if(switch$1)var _b_=0}
            return _b_?1:2}}
        return - 2}
      return other(t,x)} |}]
