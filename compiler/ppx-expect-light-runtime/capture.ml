(* Js_of_ocaml compiler
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

(* Capture of stdout/stderr during a test. Both channels are redirected,
   interleaved, into a temporary file using {!Out_channel_redirect} (which works
   on native, js_of_ocaml and wasm_of_ocaml). Output is read back per-[%expect]
   by tracking how many bytes have already been consumed. *)

module Redirect = Out_channel_redirect.Expert

type t =
  { fname : string
  ; oc : out_channel
  ; r_out : Redirect.redirection
  ; r_err : Redirect.redirection
  ; mutable consumed : int
  }

let flush_all () =
  flush stdout;
  flush stderr;
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ()

let start () =
  let fname, oc = Filename.open_temp_file "expect_light-" ".out" in
  flush_all ();
  let r_out = Redirect.redirect ~into:oc stdout in
  let r_err = Redirect.redirect ~into:oc stderr in
  { fname; oc; r_out; r_err; consumed = 0 }

(* Slice of the captured output written since the previous call. *)
let read t =
  flush_all ();
  flush t.oc;
  let ic = open_in_bin t.fname in
  let len = in_channel_length ic in
  let s =
    if len > t.consumed
    then (
      seek_in ic t.consumed;
      really_input_string ic (len - t.consumed))
    else ""
  in
  close_in ic;
  t.consumed <- len;
  s

let stop t =
  flush_all ();
  Redirect.stop t.r_err;
  Redirect.stop t.r_out;
  close_out_noerr t.oc;
  try Sys.remove t.fname with Sys_error _ -> ()
