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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Introcaml only: polymorphic printing through [Introspect] works in
   JavaScript, where blocks carry their descriptor in their header word, above
   the tag. Not run on wasm, which has no support yet. *)
[@@@if introspect]

type color =
  | Red
  | Green
  | Blue

type point =
  { x : int
  ; name : string
  }

type shape =
  | Circle of point * int
  | Poly of point list
  | Empty

let p x = Introspect.Print.print_any_endline x

let%expect_test "constants" =
  p (Some 5);
  p [ 1; 2; 3 ];
  p { x = 1; name = "p" };
  p (Circle ({ x = 0; name = "o" }, 1), Poly [ { x = 1; name = "a" } ], Empty);
  p (`Foo 3, `Bar, Red, Blue);
  p ("abc", 'c', 3.14, -7);
  [%expect
    {|
    Some 5
    [1; 2; 3]
    {x = 1; name = "p"}
    (Circle ({x = 0; name = "o"}, 1), Poly [{x = 1; name = "a"}], Empty)
    (`Foo 3, `Bar, Red, Blue)
    ("abc", 'c', 3.14, -7)
    |}]

let%expect_test "allocated values" =
  let mk i = Some (i, string_of_int i) in
  p (List.init 3 mk);
  p (ref 3);
  p (Ok 1, (Error "e" : (int, string) result));
  p (fun x -> x);
  p [| 1; 2 |];
  (* [Obj.dup] allocates afresh: the copy has no descriptor, as in native *)
  p (Obj.obj (Obj.dup (Obj.repr { x = 1; name = "p" })) : point);
  [%expect
    {|
    [Some (0, "0"); Some (1, "1"); Some (2, "2")]
    {contents = 3}
    (Ok 1, Error "e")
    <closure>
    [|1; 2|]
    (1, "p")
    |}]

let%expect_test "reserved bits" =
  Printf.printf
    "%b %b\n"
    (Obj.reserved_bits () > 0)
    (Obj.get_reserved (Obj.repr (Some 5)) <> 0);
  [%expect {| true true |}]

type float_record =
  { fa : float
  ; fb : float
  }

(* Float blocks allocated by MAKEFLOATBLOCK carry a descriptor (a float array
   literal is one, as are records built from non-constant fields), those
   built by the runtime do not. A constant float record is a Lambda
   [Const_float_array], which carries no descriptor in Introcaml (yet): it is
   emitted without reserved bits, so none is recovered. *)
let%expect_test "float blocks" =
  let f x = { fa = x; fb = 2.5 } in
  Printf.printf
    "%b %b %b %b\n"
    (Obj.get_reserved (Obj.repr [| 1.5; 2.5 |]) <> 0)
    (Obj.get_reserved (Obj.repr (f 1.5)) <> 0)
    (Obj.get_reserved (Obj.repr (Array.map (fun x -> x +. 1.) [| 1.5 |])) = 0)
    (Obj.get_reserved (Obj.repr { fa = 1.5; fb = 2.5 }) <> 0);
  [%expect {| true true true false |}]

(* The descriptor lives above the tag in the header word: it must not leak
   into tag reads, structural comparison or hashing. Blocks built by the
   runtime carry no descriptor. *)
let%expect_test "descriptor is invisible to tag, compare and hash" =
  let literal = [| 1; 2 |] in
  let built = Array.map Fun.id literal in
  Printf.printf
    "%d %d %b %b %b %b\n"
    (Obj.tag (Obj.repr (Some 5)))
    (Obj.tag (Obj.repr literal))
    (Obj.get_reserved (Obj.repr literal) <> 0)
    (Obj.get_reserved (Obj.repr built) = 0)
    (literal = built && compare literal built = 0)
    (Hashtbl.hash literal = Hashtbl.hash built);
  [%expect {| 0 0 true true true true |}]

let%expect_test "set_reserved" =
  let v = Obj.repr (Array.map Fun.id [| 1; 2 |]) in
  let desc = Obj.get_reserved (Obj.repr [| 1; 2 |]) in
  let set = Obj.set_reserved v desc in
  Printf.printf
    "%b %b %d %b %b\n"
    set
    (Obj.get_reserved v = desc)
    (Obj.tag v)
    (Obj.set_reserved (Obj.repr 3) desc)
    (Obj.set_reserved (Obj.repr [||]) desc);
  p (Obj.obj v : int array);
  [%expect {|
    true true 0 false false
    [|1; 2|]
    |}]

(* Lazy values change tag in place; the descriptor survives. *)
let%expect_test "lazy" =
  let l = lazy (Sys.opaque_identity 1 + 2) in
  let set = Obj.set_reserved (Obj.repr l) 42 in
  let forced = Lazy.force l in
  Printf.printf
    "%b %d %d %d\n"
    set
    forced
    (Obj.get_reserved (Obj.repr l))
    (Obj.tag (Obj.repr l));
  [%expect {| true 3 42 250 |}]

let%expect_test "marshal" =
  let v = Circle ({ x = 0; name = "o" }, 1), [ 1.5; 2.5 ], [| 1.5; 2.5 |] in
  let roundtrip flags = Marshal.from_string (Marshal.to_string v flags) 0 in
  let dropped, kept = roundtrip [], roundtrip [ Marshal.Reserved_bits ] in
  let d (a, _, _) = Obj.get_reserved (Obj.repr a) in
  let fa (_, _, a) = Obj.get_reserved (Obj.repr a) in
  Printf.printf
    "%b %b %b %b %b\n"
    (d dropped = 0)
    (d kept = d v)
    (fa kept = fa v)
    (dropped = v)
    (kept = v);
  p kept;
  [%expect
    {|
    true true true true true
    (Circle ({x = 0; name = "o"}, 1), [1.5; 2.5], [|1.5; 2.5|])
    |}]
