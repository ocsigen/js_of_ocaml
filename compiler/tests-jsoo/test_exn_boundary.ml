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

(* OCaml exceptions crossing the OCaml/JavaScript boundary: an exception
   escaping from a callback is a JavaScript Error, and gets back to OCaml
   unchanged. *)

open Js_of_ocaml

exception Custom of string * int

let log s = print_endline s

(* Calls [f] and describes what JavaScript caught *)
let describe : (unit -> unit) Js.callback -> Js.js_string Js.t =
 fun f ->
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr
       {|(function (f) {
           try { f(); return "no exception"; }
           catch (e) {
             return [typeof e, e instanceof Error, e.name, e.message,
                     typeof e.stack === "string"].join(" | ");
           } })|})
    [| Js.Unsafe.inject f |]

(* Calls [f] from JavaScript, catching and rethrowing what it throws *)
let call_from_js : (unit -> unit) Js.callback -> unit =
 fun f ->
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "(function (f) { try { f(); } catch (e) { throw e; } })")
    [| Js.Unsafe.inject f |]

let throw_type_error () : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "(function () { throw new TypeError('bad type'); })")
    [||]

let () =
  (* JavaScript sees an Error *)
  log
    (Js.to_string
       (describe (Js.wrap_callback (fun () -> raise (Custom ("raw callback", 7))))));
  log (Js.to_string (describe (Js.wrap_callback (fun () -> raise Not_found))));
  log (Js.to_string (describe (Js.wrap_callback (fun () -> failwith "boom"))));
  (* The same exception gets back to OCaml *)
  (try call_from_js (Js.wrap_callback (fun () -> raise (Custom ("round trip", 1))))
   with Custom (s, n) -> log (Printf.sprintf "caught Custom (%S, %d)" s n));
  (try call_from_js (Js.wrap_callback (fun () -> raise Not_found))
   with Not_found -> log "caught Not_found");
  (* Nested callbacks *)
  (try
     call_from_js
       (Js.wrap_callback (fun () ->
            call_from_js (Js.wrap_callback (fun () -> raise (Custom ("nested", 2))))))
   with Custom (s, n) -> log (Printf.sprintf "caught Custom (%S, %d)" s n));
  (* Js_error.Exn escapes as the JavaScript error itself *)
  let reraise () = try throw_type_error () with Js_error.Exn _ as e -> raise e in
  log (Js.to_string (describe (Js.wrap_callback reraise)));
  (try call_from_js (Js.wrap_callback reraise)
   with Js_error.Exn e -> log ("caught Js_error.Exn: " ^ Js_error.message e));
  (* Promise rejection reasons *)
  let err = Promise.error_of_exn (Custom ("rejected", 3)) in
  let e :
      < name : Js.js_string Js.t Js.readonly_prop
      ; message : Js.js_string Js.t Js.readonly_prop >
      Js.t =
    Js.Unsafe.coerce (Promise.error_to_any err)
  in
  log
    (Printf.sprintf
       "error_of_exn: instanceof Error %b, %s: %s"
       (Js.instanceof (Promise.error_to_any err) Js.error_constr)
       (Js.to_string e##.name)
       (Js.to_string e##.message));
  (match Promise.exn_of_error err with
  | Some (Custom (s, n)) -> log (Printf.sprintf "exn_of_error: Custom (%S, %d)" s n)
  | Some _ | None -> log "exn_of_error: unexpected");
  match
    Promise.exn_of_error (Promise.error_of_any (Js.Unsafe.inject (Js.string "x")))
  with
  | Some _ -> log "exn_of_error on a string: unexpected"
  | None -> log "exn_of_error on a string: None"
