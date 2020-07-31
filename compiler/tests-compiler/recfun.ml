(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

open Util

module M = struct
  type state = int

  type hash_value = int

  let get_hash_value x = x

  let create () = 42

  let fold_int state i = state + i

  let hash_fold_string state s = state + String.length s

  let hash_fold_list fold state l = List.fold_left fold state l

  type t =
    | Atom of string
    | List of t list

  let rec (hash_fold_t : state -> t -> state) =
    (fun hsv arg ->
       match arg with
       | Atom _a0 ->
           let hsv = fold_int hsv 0 in
           let hsv = hsv in
           hash_fold_string hsv _a0
       | List _a0 ->
           let hsv = fold_int hsv 1 in
           let hsv = hsv in
           hash_fold_list hash_fold_t hsv _a0
      : state -> t -> state)

  and (hash : t -> hash_value) =
    let func arg =
      get_hash_value
        (let hsv = create () in
         hash_fold_t hsv arg)
    in
    fun x -> func x
end

let%expect_test _ =
  let program =
    compile_and_parse
      {|
type state = int
type hash_value = int
let get_hash_value x = x
let create () = 42
let fold_int state i = state + i
let hash_fold_string state s = state + String.length s
let hash_fold_list fold state l = List.fold_left fold state l

let myfun x =
  let module M = struct
    type t =
      | Atom of string
      | List of t list

    let rec (hash_fold_t : state -> t -> state) =
      (fun hsv ->
         fun arg ->
           match arg with
           | Atom _a0 ->
             let hsv = fold_int hsv 0 in
             let hsv = hsv in hash_fold_string hsv _a0
           | List _a0 ->
             let hsv = fold_int hsv 1 in
             let hsv = hsv in hash_fold_list hash_fold_t hsv _a0 : state -> t -> state)
    and (hash : t -> hash_value) =
      let func arg = get_hash_value
          (let hsv = create () in hash_fold_t hsv arg) in
      fun x -> func x
  end
  in
  M.hash_fold_t (create ()) (List [ Atom "asd"]),
  M.hash (List [ Atom "asd"]),
  M.hash (List [ ])

  |}
  in
  print_fun_decl program (Some "myfun");
  [%expect
    {|
    function myfun(x)
     {function hash_fold_t(hsv,arg)
       {if(0 === arg[0])
         {var a0=arg[1],hsv$0=hsv | 0;return hash_fold_string(hsv$0,a0)}
        var a0$0=arg[1],hsv$1=hsv + 1 | 0;
        return hash_fold_list(hash_fold_t,hsv$1,a0$0)}
      function hash(x){return hash_fold_t(42,x)}
      var _d_=hash(_a_),_e_=hash(_b_);
      return [0,hash_fold_t(42,_c_),_e_,_d_]} |}]
