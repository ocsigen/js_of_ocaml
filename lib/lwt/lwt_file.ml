(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Pierre Chambart
 * Laboratoire PPS - CNRS Université Paris Diderot
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
open Js
open Dom
open File
open! Import

let read_with_filereader (fileReader : fileReader t constr) kind file =
  let reader = new%js fileReader in
  let res, w = Lwt.task () in
  reader##.onloadend :=
    handler (fun _ ->
        if reader##.readyState == DONE
        then
          Lwt.wakeup
            w
            (match Opt.to_option (CoerceTo.string reader##.result) with
            | None -> assert false (* can't happen: called with good readAs_ *)
            | Some s -> s)
        else ();
        (* CCC TODO: handle errors *)
        Js._false);
  Lwt.on_cancel res (fun () -> reader##abort);
  (match kind with
  | `BinaryString -> reader##readAsBinaryString file
  | `Text -> reader##readAsText file
  | `Text_withEncoding e -> reader##readAsText_withEncoding file e
  | `DataURL -> reader##readAsDataURL file);
  res

let reader kind file = read_with_filereader fileReader kind file

let readAsBinaryString file = reader `BinaryString file

let readAsText file = reader `Text file

let readAsText_withEncoding file e = reader (`Text_withEncoding e) file

let readAsDataURL file = reader `DataURL file
