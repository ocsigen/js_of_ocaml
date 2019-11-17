(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

open Js_of_ocaml
open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len
    then []
    else if Char.equal p.[cur] '\000'
    then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
    else split beg (cur + 1)
  in
  Array.of_list (split 0 0)

let setup =
  lazy
    (Topdirs.dir_directory "/static/cmis";
     Hashtbl.add
       Toploop.directive_table
       "enable"
       (Toploop.Directive_string Config.Flag.enable);
     Hashtbl.add
       Toploop.directive_table
       "disable"
       (Toploop.Directive_string Config.Flag.disable);
     Hashtbl.add
       Toploop.directive_table
       "debug_on"
       (Toploop.Directive_string Debug.enable);
     Hashtbl.add
       Toploop.directive_table
       "debug_off"
       (Toploop.Directive_string Debug.disable);
     Hashtbl.add
       Toploop.directive_table
       "tailcall"
       (Toploop.Directive_string (Config.Param.set "tc"));
     let initial_primitive_count =
       Array.length (split_primitives (Symtable.data_primitive_names ()))
     in
     (* this needs to stay synchronized with toplevel.js *)
     let compile (s : bytes array) =
       let s = String.concat ~sep:"" (List.map ~f:Bytes.to_string (Array.to_list s)) in
       let prims = split_primitives (Symtable.data_primitive_names ()) in
       let unbound_primitive p =
         try
           ignore (Js.Unsafe.eval_string p);
           false
         with _ -> true
       in
       let stubs = ref [] in
       Array.iteri prims ~f:(fun i p ->
           if i >= initial_primitive_count && unbound_primitive p
           then
             stubs :=
               Format.sprintf "function %s(){caml_failwith(\"%s not implemented\")}" p p
               :: !stubs);
       let output_program = Driver.from_string prims s in
       let b = Buffer.create 100 in
       output_program (Pretty_print.to_buffer b);
       Format.(pp_print_flush std_formatter ());
       Format.(pp_print_flush err_formatter ());
       flush stdout;
       flush stderr;
       let res = Buffer.contents b in
       let res = String.concat ~sep:"" !stubs ^ res in
       let res : unit -> _ = Js.Unsafe.global##toplevelEval (res : string) in
       res
     in
     Js.Unsafe.global##.toplevelCompile := compile (*XXX HACK!*);
     (Js.Unsafe.global##.toplevelEval
     := fun (x : string) ->
     let f : < .. > Js.t -> < .. > Js.t = Js.Unsafe.eval_string x in
     fun () ->
       let res = f Js.Unsafe.global in
       Format.(pp_print_flush std_formatter ());
       Format.(pp_print_flush err_formatter ());
       flush stdout;
       flush stderr;
       res);
     Js.Unsafe.global##.toplevelReloc
     := Js.Unsafe.callback (fun name ->
            let name = Js.to_string name in
            Js_of_ocaml_compiler.Ocaml_compiler.Symtable.reloc_ident name);
     ())

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in
    String.blit ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    (match ppf with
    | Some ppf ->
        Format.fprintf ppf "%s" (Bytes.sub_string buffer ~pos:0 ~len:len'');
        if nl then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''

let use ffp content =
  let name = "/dev/fake_stdin" in
  if Sys.file_exists name then Sys.remove name;
  Sys_js.create_file ~name ~content;
  Toploop.use_silently ffp name

let execute printval ?pp_code ?highlight_location pp_answer s =
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) pp_code) in
  (try
     while true do
       try
         let phr = !Toploop.parse_toplevel_phrase lb in
         let phr = JsooTopPpx.preprocess_phrase phr in
         ignore (Toploop.execute_phrase printval pp_answer phr : bool)
       with
       | End_of_file -> raise End_of_file
       | x ->
           (match highlight_location with
           | None -> ()
           | Some f -> (
               match JsooTopError.loc x with
               | None -> ()
               | Some loc -> f loc));
           Errors.report_error Format.err_formatter x
     done
   with End_of_file -> ());
  flush_all ()

let initialize () =
  Sys.interactive := false;
  Lazy.force setup;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  Sys.interactive := true
