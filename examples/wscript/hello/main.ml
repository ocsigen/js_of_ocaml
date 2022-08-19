open Js_of_ocaml
open Js_of_ocaml_wscript

let wut () =
  Printf.printf "%s\n" Digest.(string "bonjour" |> to_hex);
  let ta = new%js Typed_array.uint8Array 5 in
  Typed_array.set ta 0 42;
  Printf.printf "%d\n" (Typed_array.unsafe_get ta 0)

let () =
  Wscript.(echo (String "direct string"));
  Printf.printf "Hello, world!\n";
  Printf.printf "What's your name? %!";
  Printf.printf "Hi %s, I'm an OCaml program!\n" (Js.to_string (Wscript.stdin##readLine));
