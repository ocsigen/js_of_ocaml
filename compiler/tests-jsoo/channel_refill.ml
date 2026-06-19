(* A refill hook that returns more data than fits in the channel buffer
   must not have its data truncated, and reading must keep pos_in
   consistent (the offset has to advance). *)
open Js_of_ocaml

let () =
  let f = Filename.temp_file "fill" ".txt" in
  (let oc = open_out f in
   close_out oc);
  let ic = open_in_bin f in
  let chunk = String.make 200000 'x' in
  let sent = ref false in
  Sys_js.set_channel_filler ic (fun () ->
      if !sent
      then ""
      else (
        sent := true;
        chunk));
  let first = really_input_string ic 100 in
  Printf.printf "first=%d pos=%d\n" (String.length first) (pos_in ic);
  let rest = In_channel.input_all ic in
  Printf.printf "total=%d\n" (100 + String.length rest);
  close_in ic;
  Sys.remove f
