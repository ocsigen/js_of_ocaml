(* Js_of_ocaml
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
 *)

open Js_of_ocaml

let is_promise any =
  Js.instanceof
    (Js.Unsafe.coerce any : _ Js.t)
    (Js.Unsafe.global##._Promise : _ Js.constr)

let%expect_test "is_supported returns a bool" =
  let (_ : bool) = Crypto.is_supported () in
  print_endline "PASSED";
  [%expect {| PASSED |}]

let%expect_test "algorithm params round-trip through JS objects" =
  let describe (p : Crypto.params) =
    match p with
    | Crypto.SHA_256 -> "SHA-256"
    | Crypto.RSASSA_PKCS1_v1_5 -> "RSASSA-PKCS1-v1_5"
    | Crypto.AES_GCM { tag_length; _ } ->
        Printf.sprintf
          "AES-GCM tag=%s"
          (match tag_length with
          | Some t -> string_of_int t
          | None -> "none")
    | Crypto.PBKDF2 { iterations; _ } -> Printf.sprintf "PBKDF2 iter=%d" iterations
    | Crypto.RSA_keygen { name; modulus_length; _ } ->
        Printf.sprintf "RSA-keygen %s mod=%d" (Js.to_string name) modulus_length
    | Crypto.RSA_import { name; hash } ->
        Printf.sprintf
          "RSA-import %s hash=%s"
          (Js.to_string name)
          (match Crypto.of_algorithm hash with
          | Crypto.SHA_256 -> "SHA-256"
          | _ -> "?")
    | Crypto.AES_keygen { name; length } ->
        Printf.sprintf "AES-keygen %s len=%d" (Js.to_string name) length
    | Crypto.ECDSA { hash } ->
        Printf.sprintf
          "ECDSA hash=%s"
          (match Crypto.of_algorithm hash with
          | Crypto.SHA_384 -> "SHA-384"
          | _ -> "?")
    | Crypto.Other s -> "Other " ^ Js.to_string s
    | _ -> "other"
  in
  let rt p = print_endline (describe (Crypto.of_algorithm (Crypto.to_algorithm p))) in
  let buf () =
    Crypto.buffer_source_of_array_buffer_view (new%js Typed_array.uint8Array 12)
  in
  rt Crypto.SHA_256;
  rt Crypto.RSASSA_PKCS1_v1_5;
  rt (Crypto.AES_GCM { iv = buf (); additional_data = None; tag_length = Some 128 });
  rt
    (Crypto.PBKDF2
       { hash = Crypto.to_algorithm Crypto.SHA_256; salt = buf (); iterations = 100_000 });
  rt
    (Crypto.RSA_keygen
       { name = Js.string "RSA-OAEP"
       ; modulus_length = 2048
       ; public_exponent = new%js Typed_array.uint8Array 3
       ; hash = Crypto.to_algorithm Crypto.SHA_256
       });
  (* Same name "AES-GCM" but a key-generation shape: disambiguated by fields. *)
  rt (Crypto.AES_keygen { name = Js.string "AES-GCM"; length = 256 });
  rt (Crypto.ECDSA { hash = Crypto.to_algorithm Crypto.SHA_384 });
  (* RSA import params {name, hash}: distinct from the keygen and operation
     shapes of the same name, no longer lossy. *)
  rt
    (Crypto.RSA_import
       { name = Js.string "RSASSA-PKCS1-v1_5"; hash = Crypto.to_algorithm Crypto.SHA_256 });
  [%expect
    {|
    SHA-256
    RSASSA-PKCS1-v1_5
    AES-GCM tag=128
    PBKDF2 iter=100000
    RSA-keygen RSA-OAEP mod=2048
    AES-keygen AES-GCM len=256
    ECDSA hash=SHA-384
    RSA-import RSASSA-PKCS1-v1_5 hash=SHA-256 |}]

let%expect_test "key enums round-trip" =
  let usages =
    [ Crypto.Key_usage.Encrypt
    ; Crypto.Key_usage.Derive_bits
    ; Crypto.Key_usage.Unwrap_key
    ]
  in
  let back = Crypto.Key_usage.list_of_js (Crypto.Key_usage.list_to_js usages) in
  List.iter (fun u -> print_string (Js.to_string (Crypto.Key_usage.to_js u) ^ " ")) back;
  print_newline ();
  print_endline (Js.to_string (Crypto.Key_type.to_js Crypto.Key_type.Private));
  print_endline (Js.to_string (Crypto.Key_format.to_js Crypto.Key_format.Pkcs8));
  print_endline
    (match Crypto.Key_usage.of_js (Js.string "deriveKey") with
    | Crypto.Key_usage.Derive_key -> "ok"
    | _ -> "bad");
  [%expect {|
    encrypt deriveBits unwrapKey
    private
    pkcs8
    ok |}]

(* [crypto] is not available under QuickJS, so the functional tests are gated. *)

let%expect_test ("getRandomValues fills the array in place" [@when not quickjs]) =
  let a = new%js Typed_array.uint8Array 16 in
  let b = (Crypto.crypto ())##getRandomValues a in
  (* The same object is returned, and it keeps its length. *)
  print_endline (string_of_bool (a == b));
  print_endline (string_of_int b##.length);
  [%expect {|
    true
    16 |}]

let%expect_test ("randomUUID has the canonical shape" [@when not quickjs]) =
  let uuid = Js.to_string (Crypto.crypto ())##randomUUID in
  print_endline (string_of_int (String.length uuid));
  print_endline
    (string_of_bool
       (Char.equal uuid.[8] '-'
       && Char.equal uuid.[13] '-'
       && Char.equal uuid.[18] '-'
       && Char.equal uuid.[23] '-'));
  [%expect {|
    36
    true |}]

let%expect_test ("digest returns a promise" [@when not quickjs]) =
  let data = new%js Typed_array.uint8Array 3 in
  Typed_array.set data 0 1;
  Typed_array.set data 1 2;
  Typed_array.set data 2 3;
  let p =
    (Crypto.subtle ())##digest
      (Crypto.algorithm (Js.string "SHA-256"))
      (Crypto.buffer_source_of_array_buffer_view data)
  in
  (* The result settles asynchronously, so we can only assert synchronously that
     a promise was returned. *)
  print_endline (string_of_bool (is_promise (Promise.to_any p)));
  [%expect {| true |}]

let%expect_test ("generateKey builds and returns a promise" [@when not quickjs]) =
  (* AES yields a single key; the method picks the result type. *)
  let p : Crypto.cryptoKey Js.t Promise.t =
    (Crypto.subtle ())##generateKey
      (Crypto.to_algorithm
         (Crypto.AES_keygen { name = Js.string "AES-GCM"; length = 256 }))
      Js._true
      (Crypto.Key_usage.list_to_js [ Crypto.Key_usage.Encrypt; Crypto.Key_usage.Decrypt ])
  in
  print_endline (string_of_bool (is_promise (Promise.to_any p)));
  [%expect {| true |}]
