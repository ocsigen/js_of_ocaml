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

let list_rev = List.rev
(* Avoid to expose the offset of stdlib modules *)
let () = ignore (list_rev [])

let rec fun_with_loop acc = function
  | [] -> list_rev (list_rev (list_rev acc))
  | x :: xs -> fun_with_loop (x :: acc) xs
|}
  in
  print_fun_decl program (Some "fun_with_loop");
  [%expect
    {|
    function fun_with_loop(acc$1, param$0){
     var acc = acc$1, param = param$0;
     for(;;){
      if(! param)
       return caml_call1
               (list_rev, caml_call1(list_rev, caml_call1(list_rev, acc)));
      var xs = param[2], x = param[1], acc$0 = [0, x, acc];
      acc = acc$0;
      param = xs;
     }
    }
    //end
    |}]

let%expect_test "rec-fun-2" =
  let program =
    compile_and_parse
      {|
let list_rev = List.rev
(* Avoid to expose the offset of stdlib modules *)
let () = ignore (list_rev [])


let rec fun_with_loop acc = function
  | [] -> list_rev (list_rev (list_rev acc))
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
    function fun_with_loop(acc$1, param$0){
     var acc = acc$1, param = param$0;
     for(;;){
      if(! param)
       return caml_call1
               (list_rev, caml_call1(list_rev, caml_call1(list_rev, acc)));
      var x = param[1];
      if(1 === x && ! param[2]){
       var a$0 = [0, acc], i$0 = 0;
       for(;;){
        a$0[1] = [0, 1, a$0[1]];
        var _b_ = i$0 + 1 | 0;
        if(10 === i$0) return a$0[1];
        i$0 = _b_;
       }
      }
      var xs = param[2], a = [0, acc], i = 0;
      for(;;){
       a[1] = [0, 1, a[1]];
       var _a_ = i + 1 | 0;
       if(10 === i) break;
       i = _a_;
      }
      var acc$0 = [0, x, a[1]];
      acc = acc$0;
      param = xs;
     }
    }
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
    function for_for_while(param){
     var k = 1;
     for(;;){
      var j = 1;
      for(;;)
       if(10 <= runtime.caml_mul(k, j)){
        var _b_ = j + 1 | 0;
        if(10 === j) break;
        j = _b_;
       }
       else
        id[1]++;
      var _a_ = k + 1 | 0;
      if(10 === k) return 0;
      k = _a_;
     }
    }
    //end
    |}]

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
    function for_for_while(param){
     var k = 1;
     for(;;){
      var j = 1;
      for(;;)
       if(10 <= caml_div(k, j)){
        var _b_ = j + 1 | 0;
        if(10 === j) break;
        j = _b_;
       }
       else{
        try{caml_div(k, j);}
        catch(exn){throw caml_maybe_attach_backtrace(Stdlib[8], 1);}
        id[1]++;
       }
      var _a_ = k + 1 | 0;
      if(10 === k) return 0;
      k = _a_;
     }
    }
    //end
    |}]

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
    function equal(eq, xs$1, ys$1){
     var xs = xs$1, ys = ys$1;
     for(;;){
      var match = caml_call1(xs, 0), match$0 = caml_call1(ys, 0);
      if(match){
       if(match$0){
        var ys$0 = match$0[2], xs$0 = match[2];
        xs = xs$0;
        ys = ys$0;
        continue;
       }
      }
      else if(! match$0) return 1;
      return 0;
     }
    }
    //end
    |}]

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
    function f(t, x){
     try{var val$0 = caml_call2(Stdlib_Hashtbl[6], t, x);}
     catch(exn){
      var exn$0 = caml_wrap_exception(exn);
      if(exn$0 === Stdlib[8]) return - 1;
      throw caml_maybe_attach_backtrace(exn$0, 0);
     }
     if(val$0 && ! val$0[2]){
      var x$1 = val$0[1], x$0 = x$1;
      for(;;){
       a:
       {
        try{var val = caml_call2(Stdlib_Hashtbl[6], t, x$0);}
        catch(exn$0){
         var exn = caml_wrap_exception(exn$0);
         if(exn !== Stdlib[3]) throw caml_maybe_attach_backtrace(exn, 0);
         var _a_ = 0;
         break a;
        }
        if(val && ! val[2]){
         var y = val[1], _b_ = y === (x$0 + 1 | 0) ? 1 : 0;
         if(_b_){var _a_ = _b_; break a;}
         x$0 = y;
         continue;
        }
        var _a_ = 0;
       }
       return _a_ ? 1 : 2;
      }
     }
     return - 2;
    }
    //end
    |}]

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
    function inspect(x){
     var x$1 = x;
     for(;;){
      if(0 === x$1) return 1;
      if(1 === x$1) break;
      var x$2 = x$1 + 1 | 0;
      x$1 = x$2;
     }
     var x$0 = 2;
     for(;;){
      a:
      {
       if(3 >= x$0 >>> 0)
        switch(x$0){
          case 0:
           var _a_ = 1; break a;
          case 2:
           var n = caml_call1(Stdlib_Random[5], 2), _a_ = n + n | 0; break a;
          case 3:
           var n$0 = caml_call1(Stdlib_Random[5], 2); x$0 = n$0; continue;
        }
       var _a_ = 2;
      }
      return _a_ + 2 | 0;
     }
    }
    //end
    |}]

let%expect_test "buffer.add_substitute" =
  let program =
    compile_and_parse
      {|
let string_length = String.length
let string_sub = String.sub

(* Avoid to expose the offset of stdlib modules *)
let () = ignore (string_length "asd")
let () = ignore (string_sub "asd" 0 3)

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
    advance k start (string_length s)
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
    advance start (string_length s)
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
          string_sub s new_start (stop - start - 1), stop + 1
      (* Regular ident *)
      | _ ->
          let stop = advance_to_non_alpha s (start + 1) in
          string_sub s start (stop - start), stop
  in
  let add_char = Buffer.add_char in
  let add_string = Buffer.add_string in
  (* Substitute $ident, $(ident), or ${ident} in s,
      according to the function mapping f. *)
  let add_substitute b f s =
    let lim = string_length s in
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
    function add_substitute(b, f, s){
     var lim$1 = caml_ml_string_length(s), previous = 32, i$4 = 0;
     for(;;){
      if(i$4 >= lim$1){
       var _b_ = 92 === previous ? 1 : 0;
       return _b_ ? caml_call2(add_char, b, previous) : _b_;
      }
      var previous$0 = caml_string_get(s, i$4);
      if(36 === previous$0)
       if(92 === previous){
        caml_call2(add_char, b, previous$0);
        var i$5 = i$4 + 1 | 0;
        previous = 32;
        i$4 = i$5;
       }
       else{
        var start$0 = i$4 + 1 | 0;
        if(lim$1 <= start$0) throw caml_maybe_attach_backtrace(Stdlib[8], 1);
        var opening = caml_string_get(s, start$0);
        a:
        {
         if(40 !== opening && 123 !== opening){
          var
           start = start$0 + 1 | 0,
           lim$0 = caml_ml_string_length(s),
           i$2 = start;
          for(;;){
           if(lim$0 <= i$2){var stop$0 = lim$0; break;}
           var match = caml_string_get(s, i$2);
           if(91 <= match){
            if(97 <= match){
             if(123 <= match){var stop$0 = i$2; break;}
            }
            else if(95 !== match){var stop$0 = i$2; break;}
           }
           else if(58 <= match){
            if(65 > match){var stop$0 = i$2; break;}
           }
           else if(48 > match){var stop$0 = i$2; break;}
           var i$3 = i$2 + 1 | 0;
           i$2 = i$3;
          }
          var
           match$0 =
             [0, caml_call3(string_sub, s, start$0, stop$0 - start$0 | 0), stop$0];
          break a;
         }
         var new_start = start$0 + 1 | 0, k$2 = 0;
         if(40 === opening)
          var closing = 41;
         else{
          if(123 !== opening)
           throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
          var closing = 125;
         }
         var lim = caml_ml_string_length(s), k = k$2, stop = new_start;
         for(;;){
          if(lim <= stop) throw caml_maybe_attach_backtrace(Stdlib[8], 1);
          if(caml_string_get(s, stop) === opening){
           var i = stop + 1 | 0, k$0 = k + 1 | 0;
           k = k$0;
           stop = i;
          }
          else if(caml_string_get(s, stop) === closing){
           if(0 === k) break;
           var i$0 = stop + 1 | 0, k$1 = k - 1 | 0;
           k = k$1;
           stop = i$0;
          }
          else{var i$1 = stop + 1 | 0; stop = i$1;}
         }
         var
          match$0 =
            [0,
             caml_call3(string_sub, s, new_start, (stop - start$0 | 0) - 1 | 0),
             stop + 1 | 0];
        }
        var next_i = match$0[2], ident = match$0[1];
        caml_call2(add_string, b, caml_call1(f, ident));
        previous = 32;
        i$4 = next_i;
       }
      else if(92 === previous){
       caml_call2(add_char, b, 92);
       caml_call2(add_char, b, previous$0);
       var i$6 = i$4 + 1 | 0;
       previous = 32;
       i$4 = i$6;
      }
      else if(92 === previous$0){
       var i$7 = i$4 + 1 | 0;
       previous = previous$0;
       i$4 = i$7;
      }
      else{
       caml_call2(add_char, b, previous$0);
       var i$8 = i$4 + 1 | 0;
       previous = previous$0;
       i$4 = i$8;
      }
     }
    }
    //end
    |}]

let%expect_test "Bytes.trim" =
  let program =
    compile_and_parse_whole_program
      {|
let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  let open Bytes in
  let len = length s in
  let i = ref 0 in
  while !i < len && is_space (unsafe_get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (unsafe_get s !j) do
    decr j
  done;
  if !j >= !i then
    sub s !i (!j - !i + 1)
  else
    empty

let trim x = x |> Bytes.of_string |> trim |> Bytes.to_string

let () = print_endline (trim "  ")
let () = print_endline (trim " ")
|}
  in
  print_fun_decl program (Some "trim");
  [%expect
    {|
    function trim(x){
     var
      s$0 = copy(caml_bytes_of_string(x)),
      len = caml_ml_bytes_length(s$0),
      i = [0, 0];
     for(;;){
      if(i[1] >= len) break;
      if(! is_space(caml_bytes_unsafe_get(s$0, i[1]))) break;
      i[1]++;
     }
     var j = [0, len - 1 | 0];
     for(;;){
      if(i[1] > j[1]) break;
      if(! is_space(caml_bytes_unsafe_get(s$0, j[1]))) break;
      j[1]--;
     }
     a:
     {
      if(i[1] <= j[1]){
       var len$0 = (j[1] - i[1] | 0) + 1 | 0, ofs = i[1];
       if
        (0 <= ofs && 0 <= len$0 && (caml_ml_bytes_length(s$0) - len$0 | 0) >= ofs){
        var r = caml_create_bytes(len$0);
        caml_blit_bytes(s$0, ofs, r, 0, len$0);
        var b = r;
        break a;
       }
       throw caml_maybe_attach_backtrace([0, Invalid_argument, s], 1);
      }
      var b = empty;
     }
     return caml_string_of_bytes(copy(b));
    }
    //end |}]
