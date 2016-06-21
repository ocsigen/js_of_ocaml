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

type t = {
  common : CommonArg.t;
  (* compile option *)
  profile : Driver.profile option;
  source_map : (string option * Source_map.t) option;
  runtime_files : string list;
  runtime_only : bool;
  output_file : string option;
  input_file : string option;
  params : (string * string) list;
  static_env : (string * string) list;
  wrap_with_fun : bool;
  (* toplevel *)
  dynlink : bool;
  linkall : bool;
  toplevel : bool;
  nocmis : bool;
  (* filesystem *)
  include_dir : string list;
  fs_files : string list;
  fs_output : string option;
  fs_external : bool;
}

val options : t Cmdliner.Term.t
val info : Cmdliner.Term.info
