(* Js_of_ocaml tests
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

(* Exercise the paths redirected by jump threading: tests whose outcome is
   known on some incoming edges, in loops and exception handlers *)

let get (a : int array) i = if i >= 0 && i < Array.length a then a.(i) else -1

let is_ident c =
  if
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | '0' .. '9' -> false
    | _ -> c = '_'
  then 1
  else 2

let opt x y =
  let r =
    match x with
    | 0 -> None
    | 1 -> Some y
    | 2 -> Some (y + 1)
    | _ -> None
  in
  match r with
  | None -> 0
  | Some z -> z * 2

let opt_reused x y =
  let r = if x > 0 then Some y else None in
  match r with
  | None -> List.length (Option.to_list r)
  | Some z -> z + List.length (Option.to_list r)

type t =
  | A
  | B
  | C of int
  | D

let variant x =
  let k =
    if x = 0
    then A
    else if x = 1
    then B
    else if x = 2
    then C (Sys.opaque_identity x)
    else D
  in
  match k with
  | A -> "a"
  | B -> "b"
  | C n -> "c" ^ string_of_int n
  | D -> "d"

(* The boolean is computed in a loop *)
let count_matching l =
  let n = ref 0 in
  List.iter (fun (x, y) -> if (x > 0 && y > 0) || (x < 0 && y < 0) then incr n) l;
  !n

(* The loop condition is a boolean computed at the end of the body *)
let find a x =
  let i = ref 0 in
  let found = ref false in
  while (not !found) && !i < Array.length a do
    if a.(!i) = x then found := true else incr i
  done;
  if !found then Some !i else None

(* A loop exited through a test whose outcome is known on the entry edge *)
let rec first_positive l =
  match l with
  | [] -> None
  | x :: r -> if x > 0 || (x = 0 && r = []) then Some x else first_positive r

(* Booleans computed by exception handlers *)
let mem tbl x =
  let b =
    try
      ignore (Hashtbl.find tbl x);
      true
    with Not_found -> false
  in
  if b then "found" else "missing"

let in_try x y =
  try
    let b = x > 0 && y > 0 in
    if b then raise Exit;
    if x < 0 || y < 0 then "negative" else "zero"
  with Exit -> "positive"

let in_handler x y =
  try if x = y then raise Not_found else x - y
  with Not_found ->
    let b = x > 0 || y > 0 in
    if b then 1 else -1

let%expect_test "jump threading" =
  let a = Sys.opaque_identity [| 1; 2; 3 |] in
  List.iter (fun i -> Printf.printf "%d " (get a i)) [ -1; 0; 2; 3; max_int; min_int ];
  print_newline ();
  String.iter (fun c -> Printf.printf "%d" (is_ident c)) "aZ_0-9 ";
  print_newline ();
  List.iter (fun x -> Printf.printf "%d " (opt x 10)) [ 0; 1; 2; 3 ];
  print_newline ();
  List.iter (fun x -> Printf.printf "%d " (opt_reused x 10)) [ 0; 1 ];
  print_newline ();
  List.iter (fun x -> Printf.printf "%s " (variant x)) [ 0; 1; 2; 3 ];
  print_newline ();
  Printf.printf "%d\n" (count_matching [ 1, 1; -1, -1; 1, -1; 0, 0; -2, 3; 3, 3 ]);
  List.iter
    (fun x ->
      match find [| 4; 5; 6 |] x with
      | Some i -> Printf.printf "%d " i
      | None -> print_string "- ")
    [ 4; 6; 7 ];
  print_newline ();
  List.iter
    (fun l ->
      match first_positive l with
      | Some x -> Printf.printf "%d " x
      | None -> print_string "- ")
    [ []; [ -1; 0; 2 ]; [ -1; 0 ]; [ 0; 3 ] ];
  print_newline ();
  let tbl = Hashtbl.create 1 in
  Hashtbl.add tbl 1 ();
  Printf.printf "%s %s\n" (mem tbl 1) (mem tbl 2);
  List.iter (fun (x, y) -> Printf.printf "%s " (in_try x y)) [ 1, 1; 1, 0; -1, 1; 0, 0 ];
  print_newline ();
  List.iter (fun (x, y) -> Printf.printf "%d " (in_handler x y)) [ 1, 1; 0, 0; 3, 1 ];
  print_newline ();
  [%expect
    {|
           -1 1 3 -1 -1 -1
           1112222
           0 20 22 0
           0 11
           a b c2 d
           3
           0 2 -
           - 2 0 3
           found missing
           positive zero negative zero
           1 -1 2
           |}]
