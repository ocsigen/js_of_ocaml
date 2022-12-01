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
        return caml_call1(Stdlib_List[9],_b_)}}
    //end |}]

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
       {if(! param$0)
         {var
           _d_=caml_call1(Stdlib_List[9],acc$0),
           _e_=caml_call1(Stdlib_List[9],_d_);
          return caml_call1(Stdlib_List[9],_e_)}
        var _a_=param$0[1];
        if(1 === _a_ && ! param$0[2])
         {var a$0=[0,acc$0],i$0=0;
          for(;;)
           {a$0[1] = [0,1,a$0[1]];
            var _c_=i$0 + 1 | 0;
            if(10 === i$0)return a$0[1];
            var i$0=_c_}}
        var xs=param$0[2],a=[0,acc$0],i=0;
        for(;;)
         {a[1] = [0,1,a[1]];
          var _b_=i + 1 | 0;
          if(10 !== i){var i=_b_;continue}
          var acc$1=[0,_a_,a[1]],acc$0=acc$1,param$0=xs;
          continue a}}}
    //end
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
          {if(10 > runtime.caml_mul(k,j)){id[1]++;continue}
           var _b_=j + 1 | 0;
           if(10 !== j){var j=_b_;continue b}
           var _a_=k + 1 | 0;
           if(10 === k)return 0;
           var k=_a_;
           continue a}}}
    //end |}]

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
          {if(10 > caml_div(k,j))
            {try {caml_div(k,j)}catch(_c_){throw Stdlib[8]}id[1]++;continue}
           var _b_=j + 1 | 0;
           if(10 !== j){var j=_b_;continue b}
           var _a_=k + 1 | 0;
           if(10 === k)return 0;
           var k=_a_;
           continue a}}}
    //end |}]

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
        return 0}}
    //end |}]

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
                var _b_=_a_;
                switch$1 = 1}
              if(! switch$1)var _b_=0}
            return _b_?1:2}}
        return - 2}
      return other(t,x)}
    //end |}]

let%expect_test "loop-and-switch" =
  let program =
    compile_and_parse
      {|
let inspect x=
let rec loop_rec x = match x with
| 0 -> 1
| 2 -> let n = Random.int 2 in n + n
| 3 -> let n = Random.int 2 in loop_rec n
| n -> 2
in
let rec loop x =
match x with
| 0 -> 1
| 1 -> loop_rec 2 + 2
| n -> loop (n + 1)
in loop x

|}
  in
  print_fun_decl program (Some "inspect");
  [%expect
    {|
    function inspect(x)
     {var x$2=x;
      for(;;)
       {if(0 === x$2)return 1;
        if(1 !== x$2){var x$3=x$2 + 1 | 0,x$2=x$3;continue}
        var x$0=2;
        for(;;)
         {var switch$0=0;
          if(3 < x$0 >>> 0)
           switch$0 = 1;
          else
           switch(x$0)
            {case 0:var _a_=1;break;
             case 2:var n=caml_call1(Stdlib_Random[5],2),_a_=n + n | 0;break;
             case 3:var x$1=caml_call1(Stdlib_Random[5],2),x$0=x$1;continue;
             default:switch$0 = 1}
          if(switch$0)var _a_=2;
          return _a_ + 2 | 0}}}
    //end |}]

let%expect_test "buffer.add_substitute" =
  let program =
    compile_and_parse
      {|
let add_substitute =
  let closing = function
    | '(' -> ')'
    | '{' -> '}'
    | _ -> assert false
  in
  (* opening and closing: open and close characters, typically ( and )
     k: balance of opening and closing chars
     s: the string where we are searching
     start: the index where we start the search. *)
  let advance_to_closing opening closing k s start =
    let rec advance k i lim =
      if i >= lim
      then raise Not_found
      else if s.[i] = opening
      then advance (k + 1) (i + 1) lim
      else if s.[i] = closing
      then if k = 0 then i else advance (k - 1) (i + 1) lim
      else advance k (i + 1) lim
    in
    advance k start (String.length s)
  in
  let advance_to_non_alpha s start =
    let rec advance i lim =
      if i >= lim
      then lim
      else
        match s.[i] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
        | _ -> i
    in
    advance start (String.length s)
  in
  (* We are just at the beginning of an ident in s, starting at start. *)
  let find_ident s start lim =
    if start >= lim
    then raise Not_found
    else
      match s.[start] with
      (* Parenthesized ident ? *)
      | ('(' | '{') as c ->
          let new_start = start + 1 in
          let stop = advance_to_closing c (closing c) 0 s new_start in
          String.sub s new_start (stop - start - 1), stop + 1
      (* Regular ident *)
      | _ ->
          let stop = advance_to_non_alpha s (start + 1) in
          String.sub s start (stop - start), stop
  in
  let add_char = Buffer.add_char in
  let add_string = Buffer.add_string in
  (* Substitute $ident, $(ident), or ${ident} in s,
      according to the function mapping f. *)
  let add_substitute b f s =
    let lim = String.length s in
    let rec subst previous i =
      if i < lim
      then (
        match s.[i] with
        | '$' as current when previous = '\\' ->
            add_char b current;
            subst ' ' (i + 1)
        | '$' ->
            let j = i + 1 in
            let ident, next_i = find_ident s j lim in
            add_string b (f ident);
            subst ' ' next_i
        | current when previous == '\\' ->
            add_char b '\\';
            add_char b current;
            subst ' ' (i + 1)
        | '\\' as current -> subst current (i + 1)
        | current ->
            add_char b current;
            subst current (i + 1))
      else if previous = '\\'
      then add_char b previous
    in
    subst ' ' 0
  in
  add_substitute
|}
  in
  print_fun_decl program (Some "add_substitute");
  [%expect
    {|
    function add_substitute(b,f,s)
     {var lim$1=caml_ml_string_length(s),previous=32,i$7=0;
      for(;;)
       {if(i$7 >= lim$1)
         {var _c_=92 === previous?1:0;
          return _c_?caml_call2(add_char,b,previous):_c_}
        var current=caml_string_get(s,i$7);
        if(36 !== current)
         {if(92 === previous)
           {caml_call2(add_char,b,92);
            caml_call2(add_char,b,current);
            var i$9=i$7 + 1 | 0,previous=32,i$7=i$9;
            continue}
          if(92 === current)
           {var i$10=i$7 + 1 | 0,previous=current,i$7=i$10;continue}
          caml_call2(add_char,b,current);
          var i$11=i$7 + 1 | 0,previous=current,i$7=i$11;
          continue}
        if(92 === previous)
         {caml_call2(add_char,b,current);
          var i$8=i$7 + 1 | 0,previous=32,i$7=i$8;
          continue}
        var start=i$7 + 1 | 0;
        if(lim$1 <= start)throw Stdlib[8];
        var opening=caml_string_get(s,start),switch$0=0;
        if(40 !== opening && 123 !== opening)
         {var i$6=start + 1 | 0,lim$0=caml_ml_string_length(s),i$3=i$6;
          for(;;)
           {if(lim$0 <= i$3)
             var stop=lim$0;
            else
             {var match=caml_string_get(s,i$3),switch$1=0;
              if(91 <= match)
               {if(97 <= match)
                 {if(123 > match)switch$1 = 1}
                else
                 if(95 === match)switch$1 = 1}
              else
               if(58 <= match)
                {if(65 <= match)switch$1 = 1}
               else
                if(48 <= match)switch$1 = 1;
              if(switch$1){var i$4=i$3 + 1 | 0,i$3=i$4;continue}
              var stop=i$3}
            var
             match$0=
              [0,caml_call3(Stdlib_String[15],s,start,stop - start | 0),stop];
            switch$0 = 1;
            break}}
        if(! switch$0)
         {var i$5=start + 1 | 0,k$2=0;
          if(40 === opening)
           var _b_=41;
          else
           {if(123 !== opening)throw [0,Assert_failure,_a_];var _b_=125}
          var lim=caml_ml_string_length(s),k=k$2,i=i$5;
          for(;;)
           {if(lim <= i)throw Stdlib[8];
            if(caml_string_get(s,i) === opening)
             {var i$0=i + 1 | 0,k$0=k + 1 | 0,k=k$0,i=i$0;continue}
            if(caml_string_get(s,i) !== _b_){var i$2=i + 1 | 0,i=i$2;continue}
            if(0 !== k){var i$1=i + 1 | 0,k$1=k - 1 | 0,k=k$1,i=i$1;continue}
            var
             match$0=
              [0,
               caml_call3(Stdlib_String[15],s,i$5,(i - start | 0) - 1 | 0),
               i + 1 | 0];
            break}}
        var next_i=match$0[2],ident=match$0[1];
        caml_call2(add_string,b,caml_call1(f,ident));
        var previous=32,i$7=next_i}}
    //end |}]
