(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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
open Js_of_ocaml

let url_string_url u = Url.url_of_string (Url.string_of_url u)

let url = "http://ocsigen.org/js_of_ocaml/"

let%expect_test _ =
  (match Url.url_of_string url with
  | None -> print_endline "can't parse current url2"
  | Some u -> (
      match url_string_url u with
      | None -> print_endline "can't parse pretty-printed url"
      | Some v -> if u = v then () else print_endline "no fixpoint"));
  [%expect {||}]

let%expect_test _ =
  let t1 = Url.urlencode "/toto+ blah&tutu" in
  let t2 = Url.urlencode ~with_plus:false "/toto+ blah&tutu" in
  if t1 = "/toto%2B%20blah%26tutu" && t2 = "/toto+%20blah%26tutu"
  then ()
  else print_endline "escaping error";
  [%expect {||}]

let%expect_test "[decode_arguments]" =
  let test url =
    ListLabels.iter (Url.decode_arguments url) ~f:(fun (key, value) ->
        Printf.printf "'%s' => '%s'\n" key value)
  in
  (* Incorrect usage: passing in full url. *)
  test "https://foo.com/?foo=bar&baz%20=qux%22quux";
  [%expect {|
    'https://foo.com/?foo' => 'bar'
    'baz ' => 'qux"quux' |}];
  (* correct usage: only the arguments part *)
  test "foo=bar&baz%20=qux%22quux";
  [%expect {|
    'foo' => 'bar'
    'baz ' => 'qux"quux' |}];
  ignore ()
