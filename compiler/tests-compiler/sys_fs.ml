(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2021 Hugo Heuzard
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

let%expect_test _ =
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  let oc = open_out "aaa/bbb" in
  close_out oc;
  (try ignore(Sys.readdir "aaa/bb") with
  | Sys_error _ -> ()
  | e -> print_endline (Printexc.to_string e));
  (try ignore(Sys.readdir "aaa/bbb") with
  | Sys_error _ -> ()
  | e -> print_endline (Printexc.to_string e));
  Sys.remove "aaa/bbb";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {| |}]

let%expect_test _ =
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  let l = Sys.readdir "aaa" |> Array.to_list in
  List.iter print_endline l;
  (match Sys.rmdir "aaa" with
  | exception _ -> ()
  | _ -> print_endline "BUG");
  Sys.rmdir "aaa/bbb";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {|
    bbb
    bbb|}]

let%expect_test _ =
  (* Check we can rename a directory *)
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  Sys.mkdir "aaa/bbb/ccc" 0o777;
  let oc = open_out "aaa/bbb/ccc/ddd" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;
  Sys.rename "aaa/bbb" "aaa/bbb2";
  let ic = open_in "aaa/bbb2/ccc/ddd" in
  let line = input_line ic in
  close_in ic;
  Printf.printf "new file contents: %s\n%!" line;
  Sys.remove "aaa/bbb2/ccc/ddd";
  Sys.rmdir "aaa/bbb2/ccc";
  Sys.rmdir "aaa/bbb2";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {|
    new file contents: Hello world
    new file contents: Hello world
    |}]

let%expect_test _ =
  (* Check we can rename a directory over another directory *)
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  Sys.mkdir "aaa/bbb/ccc" 0o777;
  let oc = open_out "aaa/bbb/ccc/ddd" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;
  Sys.mkdir "aaa/bbb2" 0o777;
  Sys.rename "aaa/bbb" "aaa/bbb2";
  let ic = open_in "aaa/bbb2/ccc/ddd" in
  let line = input_line ic in
  close_in ic;
  Printf.printf "new file contents: %s\n%!" line;
  Sys.remove "aaa/bbb2/ccc/ddd";
  Sys.rmdir "aaa/bbb2/ccc";
  Sys.rmdir "aaa/bbb2";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {|
    new file contents: Hello world
    new file contents: Hello world
    |}]

let%expect_test _ =
  (* Check we can't rename a directory over another non-empty directory *)
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  Sys.mkdir "aaa/bbb/ccc" 0o777;
  let oc = open_out "aaa/bbb/ccc/ddd" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;
  Sys.mkdir "aaa/bbb2" 0o777;
  let oc = open_out "aaa/bbb2/ccc" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;
  (match Sys.rename "aaa/bbb" "aaa/bbb2" with
  | exception Sys_error _ -> ()
  | _ -> failwith "BUG: rename should have failed");
  Sys.remove "aaa/bbb/ccc/ddd";
  Sys.rmdir "aaa/bbb/ccc";
  Sys.rmdir "aaa/bbb";
  Sys.remove "aaa/bbb2/ccc";
  Sys.rmdir "aaa/bbb2";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {| |}]

let%expect_test _ =
  (* Check we can rename a file to a pre-existing file *)
  compile_and_run
    {|
let f () =
  let mk f content =
    let oc = open_out f in
    Printf.fprintf oc "%s\n" content;
    close_out oc
  in
  mk "aaa" "aaa";
  mk "bbb" "bbb";
  Sys.rename "aaa" "bbb";
  let ic = open_in "bbb" in
  let line = input_line ic in
  close_in ic;
  Printf.printf "contents of 'bbb': %s\n%!" line;
  Sys.remove "bbb";
  (match Sys.remove "aaa" with
  | exception _ -> ()
  | _ -> print_endline "BUG")
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {|
    contents of 'bbb': aaa
    contents of 'bbb': aaa
    |}]


let%expect_test _ =
  (* Check we can't rename a directory over a file *)
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  Sys.mkdir "aaa/bbb/ccc" 0o777;
  let oc = open_out "aaa/bbb/ccc/ddd" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;
  let oc = open_out "aaa/bbb2" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;  
  (match Sys.rename "aaa/bbb" "aaa/bbb2"
   with 
   | () -> failwith "BUG: rename should have failed"
   | exception Sys_error _ -> ());
  Sys.remove "aaa/bbb/ccc/ddd";
  Sys.rmdir "aaa/bbb/ccc";
  Sys.rmdir "aaa/bbb";
  Sys.remove "aaa/bbb2";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {| |}]

let%expect_test _ =
  (* Check we can't rename a file over a directory *)
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  Sys.mkdir "aaa/bbb/ccc" 0o777;
  let oc = open_out "aaa/bbb/ccc/ddd" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;
  let oc = open_out "aaa/bbb2" in
  Printf.fprintf oc "Hello world\n";
  close_out oc;  
  (match Sys.rename "aaa/bbb2" "aaa/bbb" with
   | exception Sys_error _ -> ()
   | _ -> failwith "BUG: rename should have failed");
  Sys.remove "aaa/bbb/ccc/ddd";
  Sys.rmdir "aaa/bbb/ccc";
  Sys.rmdir "aaa/bbb";
  Sys.remove "aaa/bbb2";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {| |}]

let%expect_test _ =
  compile_and_run
    {|
  (match Sys.mkdir "/not/exists" 0o777 with
  | exception Sys_error path -> print_endline "EXPECTED ERROR"
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "BUG");
  |};
  [%expect {|EXPECTED ERROR|}]

let%expect_test _ =
  compile_and_run
    {|
  (match Sys.rmdir "/not/exists" with
  | exception Sys_error path -> print_endline "EXPECTED ERROR"
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "BUG");
  |};
  [%expect {|EXPECTED ERROR|}]
