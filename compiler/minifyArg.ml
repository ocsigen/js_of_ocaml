(* Js_of_ocaml compiler
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

open Cmdliner

type t = {
  common : CommonArg.t;
  (* minify option *)
  stdin : bool;
  output_file : string option;
  files : string list
}

let options =
  let files =
    Arg.(value & pos_all file [] & info [] ~docv:"JS_FILES")
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info ["o"] ~docv:"FILE" ~doc)
  in
  let stdin =
    let doc = "Read from standard input." in
    Arg.(value & flag & info ["stdin"] ~doc)
  in
  let build_t common files output_file stdin =
    `Ok {
      common;
      stdin;
      output_file;
      files
    }
  in
  let t =
    Term.(pure build_t
          $ CommonArg.t
          $ files
          $ output_file
          $ stdin)
  in
  Term.ret t

let info =
  let doc =
    "JavaScript minifier"
  in
  let man = [
    `S "DESCRIPTION";
    `P "jsoo_minify is a JavaScript minifier.";
    `S "BUGS";
    `P "Bugs are tracked on github at \
        $(i,https://github.com/ocsigen/js_of_ocaml/issues).";
    `S "SEE ALSO";
    `P "js_of_ocaml(1)";
    `S "AUTHORS";
    `P "Jerome Vouillon, Hugo Heuzard.";
    `S "LICENSE";
    `P "Copyright (C) 2010-2014.";
    `P "jsoo_minify is free software, you can redistribute it and/or modify \
        it under the terms of the GNU Lesser General Public License as published \
        by the Free Software Foundation, with linking exception; \
        either version 2.1 of the License, or (at your option) any later version."
  ]
  in
  let version = match Compiler_version.git_version with
    | "" -> Compiler_version.s
    | v  -> Printf.sprintf "%s+git-%s"Compiler_version.s v in
  Term.info "jsoo_minify" ~version ~doc ~man
