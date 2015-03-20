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

include Lwt_log_core

let js_val = Lwt.new_key ()

let console = make
    ~close:(fun _ -> Lwt.return_unit)
    ~output:(fun section level logs ->
        let str =
          (Js.string
             (Printf.sprintf "[%s] %s" (Section.name section) (String.concat "\n" logs)))
        in
        (match level,Lwt.get js_val with
         | Debug,None -> Firebug.console##debug(str)
         | Debug,Some v -> Firebug.console##debug_2(str,v)

         | Info,None
         | Notice,None ->  Firebug.console##info(str)
         | Info,Some v
         | Notice,Some v ->  Firebug.console##info_2(str,v)

         | Warning,None -> Firebug.console##warn(str)
         | Warning,Some v -> Firebug.console##warn_2(str,v)

         | Error,None
         | Fatal,None ->   Firebug.console##error(str)
         | Error,Some v
         | Fatal,Some v ->   Firebug.console##error_2(str,v)
        );
        Lwt.return_unit
      )

let log ?inspect ?exn ?section ?location ?logger ~level message =
  let inspect = match inspect with None -> None | Some v -> Some (Obj.repr v) in
  Lwt.with_value js_val inspect (fun () ->
      log ?exn ?section ?location ?logger ~level message
    )
let log_f ?inspect ?exn ?section ?location ?logger ~level format =
  Printf.ksprintf (log ?inspect ?exn ?section ?location ?logger ~level) format

let ign_log ?inspect ?exn ?section ?location ?logger ~level message =
  try
    ignore (log ?inspect ?exn ?section ?location ?logger ~level message)
  with _ ->
    ()

let ign_log_f ?inspect ?exn ?section ?location ?logger ~level format =
  Printf.ksprintf (ign_log ?inspect ?exn ?section ?location ?logger ~level) format

let debug ?inspect ?exn ?section ?location ?logger msg = log ?inspect ?exn ?section ?location ?logger ~level:Debug msg
let debug_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (debug ?inspect ?exn ?section ?location ?logger) fmt
let info ?inspect ?exn ?section ?location ?logger msg = log ?inspect ?exn ?section ?location ?logger ~level:Info msg
let info_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (info ?inspect ?exn ?section ?location ?logger) fmt
let notice ?inspect ?exn ?section ?location ?logger msg = log ?inspect ?exn ?section ?location ?logger ~level:Notice msg
let notice_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (notice ?inspect ?exn ?section ?location ?logger) fmt
let warning ?inspect ?exn ?section ?location ?logger msg = log ?inspect ?exn ?section ?location ?logger ~level:Warning msg
let warning_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (warning ?inspect ?exn ?section ?location ?logger) fmt
let error ?inspect ?exn ?section ?location ?logger msg = log ?inspect ?exn ?section ?location ?logger ~level:Error msg
let error_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (error ?inspect ?exn ?section ?location ?logger) fmt
let fatal ?inspect ?exn ?section ?location ?logger msg = log ?inspect ?exn ?section ?location ?logger ~level:Fatal msg
let fatal_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (fatal ?inspect ?exn ?section ?location ?logger) fmt

let ign_debug ?inspect ?exn ?section ?location ?logger msg = ign_log ?inspect ?exn ?section ?location ?logger ~level:Debug msg
let ign_debug_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_debug ?inspect ?exn ?section ?location ?logger) fmt
let ign_info ?inspect ?exn ?section ?location ?logger msg = ign_log ?inspect ?exn ?section ?location ?logger ~level:Info msg
let ign_info_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_info ?inspect ?exn ?section ?location ?logger) fmt
let ign_notice ?inspect ?exn ?section ?location ?logger msg = ign_log ?inspect ?exn ?section ?location ?logger ~level:Notice msg
let ign_notice_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_notice ?inspect ?exn ?section ?location ?logger) fmt
let ign_warning ?inspect ?exn ?section ?location ?logger msg = ign_log ?inspect ?exn ?section ?location ?logger ~level:Warning msg
let ign_warning_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_warning ?inspect ?exn ?section ?location ?logger) fmt
let ign_error ?inspect ?exn ?section ?location ?logger msg = ign_log ?inspect ?exn ?section ?location ?logger ~level:Error msg
let ign_error_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_error ?inspect ?exn ?section ?location ?logger) fmt
let ign_fatal ?inspect ?exn ?section ?location ?logger msg = ign_log ?inspect ?exn ?section ?location ?logger ~level:Fatal msg
let ign_fatal_f ?inspect ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_fatal ?inspect ?exn ?section ?location ?logger) fmt


(*let raise_error ?inspect ?exn ?section ?location ?logger msg =
  Lwt.ignore_result (log ?inspect ?exn ?section ?location ?logger ~level:Error msg);
  failwith msg *)
(*let raise_error_f ?inspect ?exn ?section ?location ?logger fmt =
  Printf.ksprintf (raise_error ?inspect ?exn ?section ?location ?logger) fmt *)
