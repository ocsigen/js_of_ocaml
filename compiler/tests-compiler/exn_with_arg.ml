(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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
 * along with this program; if not, should be able to obtain it from
 * https://www.gnu.org/licenses/.
 *)

(* [raise (E arg)] is compiled to a call to a runtime function that builds
   the exception and attaches the backtrace. *)

let prog =
  {|
exception E of int
let f x = if x < 0 then raise (E x) else x
let g s = if s = "" then invalid_arg "g" else s
let h x = match x with 0 -> raise Not_found | 1 -> failwith "one" | _ -> raise (E (x + 1))
|}

let%expect_test "generated code" =
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "f");
  Util.print_fun_decl program (Some "h");
  [%expect
    {|
           function f(x){if(0 <= x) return x; throw caml_exn_with_arg(E, x);}
           //end
           function h(x){
            if(0 === x) throw caml_maybe_attach_backtrace(Stdlib[8], 1);
            if(1 === x) return caml_call1(Stdlib[2], cst_one);
            throw caml_exn_with_arg(E, x + 1 | 0);
           }
           //end
           |}]

let%expect_test "semantics" =
  Util.compile_and_run
    (prog
    ^ {|
let show f =
  match f () with
  | _ -> print_endline "no exception"
  | exception E n -> Printf.printf "E %d\n" n
  | exception Invalid_argument s -> Printf.printf "Invalid_argument %s\n" s
  | exception Failure s -> Printf.printf "Failure %s\n" s
  | exception Not_found -> print_endline "Not_found"
let () =
  show (fun () -> f (-3));
  show (fun () -> f 3);
  show (fun () -> g "");
  show (fun () -> h 0);
  show (fun () -> h 1);
  show (fun () -> h 5);
  (try raise (E 7) with E n as e -> Printf.printf "%d %s\n" n (Printexc.to_string e))
|}
    );
  [%expect
    {|
           E -3
           no exception
           Invalid_argument g
           Not_found
           Failure one
           E 6
           7 Test.E(7)
           |}]
