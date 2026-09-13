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

(* Introcaml only: the DevTools custom formatter installed by
   [Devtools.register_formatters]. The formatter is driven here the way the
   console would: [header], then [body] for every nested "object" node, and
   the resulting JsonML is rendered as an indented tree. *)
[@@@if introspect]

open Js_of_ocaml

class type formatter = object
  method header : Js.Unsafe.any -> Js.Unsafe.any -> Js.Unsafe.any Js.opt Js.meth

  method hasBody : Js.Unsafe.any -> Js.Unsafe.any -> bool Js.t Js.meth

  method body : Js.Unsafe.any -> Js.Unsafe.any -> Js.Unsafe.any Js.meth
end

let formatter () : formatter Js.t =
  Devtools.register_formatters ();
  let formatters : formatter Js.t Js.js_array Js.t =
    Js.Unsafe.global##.devtoolsFormatters
  in
  Js.Optdef.get (Js.array_get formatters 0) (fun () -> assert false)

(* Render JsonML: "object" nodes are expanded through the formatter (or shown
   as [<native>] when it declines them) down to [max_depth] levels, lists
   become one line per item. *)
let rec render ~max_depth f depth (node : Js.Unsafe.any) =
  if String.equal (Js.to_string (Js.typeof node)) "string"
  then Js.to_string (Js.Unsafe.coerce node)
  else
    let node : Js.Unsafe.any Js.js_array Js.t = Js.Unsafe.coerce node in
    let get i = Js.Optdef.get (Js.array_get node i) (fun () -> assert false) in
    let children depth =
      List.init
        (max 0 (node##.length - 2))
        (fun i -> render ~max_depth f depth (get (i + 2)))
    in
    match Js.to_string (Js.Unsafe.coerce (get 0)) with
    | "object" ->
        let attrs = get 1 in
        let obj = Js.Unsafe.get attrs (Js.string "object") in
        let config = Js.Unsafe.get attrs (Js.string "config") in
        expand ~max_depth f depth obj config
    | "ol" -> String.concat "" (List.map (fun s -> "\n" ^ s) (children (depth + 1)))
    | "li" -> String.make (2 * depth) ' ' ^ String.concat "" (children depth)
    | _ -> String.concat "" (children depth)

and expand ~max_depth f depth obj config =
  match Js.Opt.to_option (f##header obj config) with
  | None -> "<native>"
  | Some header ->
      let header = render ~max_depth f depth header in
      if depth < max_depth && Js.to_bool (f##hasBody obj config)
      then header ^ render ~max_depth f depth (f##body obj config)
      else header

let expand ?(max_depth = 100) v =
  expand ~max_depth (formatter ()) 0 (Js.Unsafe.inject v) (Js.Unsafe.inject Js.undefined)

let show ?max_depth v = print_endline (expand ?max_depth v)

type point =
  { x : int
  ; name : string
  }

type shape =
  | Circle of point * int
  | Poly of point list
  | Empty

type color =
  | Red
  | Green

type node =
  { v : int
  ; mutable next : node option
  }

let%expect_test "registration is idempotent" =
  Devtools.register_formatters ();
  Devtools.register_formatters ();
  let formatters : Js.Unsafe.any Js.js_array Js.t =
    Js.Unsafe.global##.devtoolsFormatters
  in
  print_int formatters##.length;
  [%expect {| 1 |}]

let%expect_test "records and constructors" =
  show { x = 1; name = "p" };
  [%expect {|
    {x = 1; name = "p"}
      x: 1
      name: "p"
    |}];
  show (Circle ({ x = 0; name = "o" }, 1));
  [%expect
    {|
    Circle ({x = 0; name = "o"}, 1)
      0: {x = 0; name = "o"}
        x: 0
        name: "o"
      1: 1
    |}];
  show (Poly [ { x = 1; name = "a" }; { x = 2; name = "b" } ]);
  [%expect
    {|
    Poly [{x = 1; name = "a"}; {x = 2; name = "b"}]
      [{x = 1; name = "a"}; {x = 2; name = "b"}]
        0: {x = 1; name = "a"}
          x: 1
          name: "a"
        1: {x = 2; name = "b"}
          x: 2
          name: "b"
    |}]

let%expect_test "immediates keep the approximation of their type" =
  show (Some Green, [ Red ], 'c', 3.14, "s");
  [%expect
    {|
    (Some Green, [Red], 'c', 3.14, "s")
      0: Some Green
        Green
      1: [Red]
        0: Red
      2: 'c'
      3: 3.14
      4: "s"
    |}]

let%expect_test "lists, arrays, options, results, polymorphic variants" =
  show (List.init 3 (fun i -> Some i), [| 1; 2 |], (Ok 1 : (int, string) result), `Foo 3);
  [%expect
    {|
    ([Some 0; Some 1; Some 2], [|1; 2|], Ok 1, `Foo 3)
      0: [Some 0; Some 1; Some 2]
        0: Some 0
          0
        1: Some 1
          1
        2: Some 2
          2
      1: [|1; 2|]
        0: 1
        1: 2
      2: Ok 1
        1
      3: `Foo 3
        3
    |}]

let%expect_test "long lists are truncated" =
  let lines = String.split_on_char '\n' (expand (List.init 105 Fun.id)) in
  let n = List.length lines in
  Printf.printf "%d lines, first and last six:\n" n;
  List.iteri (fun i l -> if i = 0 || i >= n - 6 then print_endline l) lines;
  [%expect
    {|
    107 lines, first and last six:
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42...
      ...: [100; 101; 102; 103; 104]
        0: 100
        1: 101
        2: 102
        3: 103
        4: 104
    |}]

let%expect_test "closures, references, cycles" =
  let r = ref [ 1 ] in
  show (r, fun x -> x);
  [%expect
    {|
    ({contents = [1]}, <closure>)
      0: {contents = [1]}
        contents: [1]
          0: 1
      1: <closure>
    |}];
  let rec cyclic = { x = 1; name = "c" } :: cyclic in
  show ~max_depth:0 cyclic;
  [%expect {| [{x = 1; name = "c"}; <cycle>] |}];
  let n = { v = 1; next = None } in
  n.next <- Some n;
  show ~max_depth:3 n;
  [%expect
    {|
           {v = 1; next = Some <cycle>}
             v: 1
             next: Some {v = 1; next = <cycle>}
               {v = 1; next = Some <cycle>}
                 v: 1
                 next: Some {v = 1; next = <cycle>}
           |}]

exception Custom of string * int

let%expect_test "exceptions and lazy values are recognized without descriptor" =
  show (Custom ("boom", 42));
  [%expect
    {|
    Jsoo_lib_expect_tests_introspect.Test_devtools.Custom/28 ("boom", 42)
      0: "boom"
      1: 42
    |}];
  let l = lazy (1 + 2) in
  show l;
  ignore (Lazy.force l);
  show l;
  [%expect {|
           <lazy>
           lazy 3
             3
           |}]

(* Under js_of_ocaml, int32 and nativeint are unboxed numbers: they print as
   integers and, being JavaScript primitives, are never handed to the
   formatter at top level. *)
let%expect_test "int32 and nativeint" =
  let f = formatter () in
  let claims v =
    Js.Opt.test (f##header (Js.Unsafe.inject v) (Js.Unsafe.inject Js.undefined))
  in
  Printf.printf "top-level claims: int32=%b nativeint=%b\n" (claims 7l) (claims 9n);
  show (Int32.max_int, Int32.min_int, -7l, 0l);
  show (Nativeint.max_int, Nativeint.min_int, -9n, 0n);
  show (Some 7l, [ 1l; -2l ], [| 3n |], { x = 1; name = "n" }, ref 5n);
  [%expect
    {|
           top-level claims: int32=false nativeint=false
           (2147483647, -2147483648, -7, 0)
             0: 2147483647
             1: -2147483648
             2: -7
             3: 0
           (2147483647, -2147483648, -9, 0)
             0: 2147483647
             1: -2147483648
             2: -9
             3: 0
           (Some 7, [1; -2], [|3|], {x = 1; name = "n"}, {contents = 5})
             0: Some 7
               7
             1: [1; -2]
               0: 1
               1: -2
             2: [|3|]
               0: 3
             3: {x = 1; name = "n"}
               x: 1
               name: "n"
             4: {contents = 5}
               contents: 5
           |}]

(* Floats are unboxed numbers: integral ones cannot be told from integers *)
let%expect_test "floats" =
  show (1.0, 3.14, -0.5, 1e100, nan, infinity, neg_infinity, Some 2.5, [ 0.25 ]);
  [%expect
    {|
           (1, 3.14, -0.5, 1e+100, nan, infinity, neg_infinity, Some 2.5, [0.25])
             0: 1
             1: 3.14
             2: -0.5
             3: 1e+100
             4: nan
             5: infinity
             6: neg_infinity
             7: Some 2.5
               2.5
             8: [0.25]
               0: 0.25
           |}]

(* JavaScript values inside OCaml ones are handed back to the console *)
let%expect_test "embedded JavaScript values" =
  show
    ( Js.Unsafe.obj [| "a", Js.Unsafe.inject 1 |]
    , new%js Js.date_now
    , Js.null
    , Js.undefined
    , Js._true
    , Js._false
    , object
        method m = 1
      end );
  [%expect
    {|
           (<Object>, <Date>, null, undefined, true, false, <object>)
             0: <native>
             1: <native>
             2: null
             3: undefined
             4: true
             5: false
             6: <object>
           |}]

let%expect_test "plain JavaScript values are left to the console" =
  let f = formatter () in
  let claims v =
    Js.Opt.test (f##header (Js.Unsafe.inject v) (Js.Unsafe.inject Js.undefined))
  in
  Printf.printf
    "%b %b %b\n"
    (claims (Js.Unsafe.obj [||]))
    (claims (Js.array [| 1; 2 |]))
    (claims (Some 1));
  [%expect {| false false true |}]

let%expect_test "bytes, boxed integers and bigarrays" =
  let f = formatter () in
  let claims v =
    Js.Opt.test (f##header (Js.Unsafe.inject v) (Js.Unsafe.inject Js.undefined))
  in
  let ba = Bigarray.Array1.of_array Bigarray.int Bigarray.c_layout [| 1; 2 |] in
  Printf.printf
    "top-level claims: bytes=%b int64=%b bigarray=%b\n"
    (claims (Bytes.of_string "abc"))
    (claims 42L)
    (claims ba);
  show (Bytes.of_string "a\"b", 42L, 7l, 9n, ba, Some ba);
  show (Bytes.of_string "abc");
  show 42L;
  [%expect
    {|
           top-level claims: bytes=true int64=true bigarray=false
           (Bytes.of_string "a\"b", 42L, 7, 9, <bigarray 2>, Some <bigarray 2>)
             0: Bytes.of_string "a\"b"
             1: 42L
             2: 7
             3: 9
             4: <bigarray 2>
             5: Some <bigarray 2>
               <bigarray 2>
           Bytes.of_string "abc"
           42L
           |}]
