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

open Js
open Dom

class type blob = object
  method size : int readonly_prop
  method _type : js_string t readonly_prop
  method slice : int -> int -> blob meth
  method slice_withContentType : int -> int -> js_string t -> blob meth
end

class type file = object
  inherit blob
  method name : js_string t readonly_prop
  method lastModifiedDate : js_string t readonly_prop
end

(* in firefox 3.0-3.5 file.name is not available, we use the nonstandard fileName instead *)
class type file_name_only = object
  method name : js_string t optdef readonly_prop
  method fileName : js_string t optdef readonly_prop
end

let filename file =
  let file = (Js.Unsafe.coerce file:file_name_only t) in
  match Optdef.to_option (file##name) with
    | None ->
      begin
	match Optdef.to_option (file##fileName) with
	  | None -> failwith "can't retrieve file name: not implemented"
	  | Some name -> name
      end
    | Some name -> name

type file_any = < > t

module CoerceTo = struct
  let string (e : file_any) =
    if typeof e = string "string"
    then Js.some (Unsafe.coerce e:js_string t)
    else Js.null
  let arrayBuffer (e : file_any) =
    if instanceof e Typed_array.arrayBuffer
    then Js.some (Unsafe.coerce e:Typed_array.arrayBuffer t)
    else Js.null
end

class type fileList = object
  inherit [file] Dom.nodeList
end

class type fileError = object
  method code : int readonly_prop
end

class type ['a] progressEvent = object
  inherit ['a] event
  method lengthComputable : bool t readonly_prop
  method loaded : int readonly_prop
  method total : int readonly_prop
end

class type progressEventTarget = object ('self)
  method onloadstart : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onprogress : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onload : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onabort : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onerror : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onloadend : ('self t, 'self progressEvent t) event_listener writeonly_prop
end

type readyState = EMPTY | LOADING | DONE

class type fileReader = object ('self)

  method readAsArrayBuffer : #blob t -> unit meth
  method readAsBinaryString : #blob t -> unit meth
  method readAsText : #blob t -> unit meth
  method readAsText_withEncoding : #blob t -> js_string t -> unit meth
  method readAsDataURL : #blob t -> unit meth

  method abort : unit meth

  method readyState : readyState readonly_prop

  method result : file_any readonly_prop
  method error : fileError t readonly_prop

  inherit progressEventTarget
end

module ReaderEvent = struct
  type typ = fileReader progressEvent t Dom.Event.typ
  let loadstart = Event.make "loadstart"
  let progress = Event.make "progress"
  let abort = Event.make "abort"
  let error = Event.make "error"
  let load = Event.make "load"
  let loadend = Event.make "loadend"
end

let fileReader : fileReader t constr =
  Unsafe.variable "window.FileReader"

let read_with_filereader (fileReader : fileReader t constr) kind file =
  let reader = jsnew fileReader () in
  let (res, w) = Lwt.task () in
  reader##onloadend <- handler
    (fun _ ->
      if reader##readyState = DONE then
        Lwt.wakeup w
	  (match Opt.to_option (CoerceTo.string (reader##result)) with
	    | None -> assert false (* can't happen: called with good readAs_ *)
	    | Some s -> s)
      else (); (* CCC TODO: handle errors *)
      Js._false);
  Lwt.on_cancel res (fun () -> reader##abort ());
  (match kind with
    | `BinaryString -> reader##readAsBinaryString(file)
    | `Text -> reader##readAsText(file)
    | `Text_withEncoding e -> reader##readAsText_withEncoding(file,e)
    | `DataURL -> reader##readAsDataURL(file));
  res

(* Old firefox specific part: available in firefox 3.0, deprecated in 3.6.
   Those are nonstandard extensions. *)
class type old_firefox_file =
object
  method getAsBinary : js_string t meth
  method getAsBinary_presence : unit optdef readonly_prop
  method getAsDataURL : js_string t meth
  method getAsDataURL_presence : unit optdef readonly_prop
  method getAsText_presence : unit optdef readonly_prop
  method getAsText : js_string t -> js_string t meth
end

let old_firefox_reader kind file =
  let file = (Js.Unsafe.coerce file:old_firefox_file t) in
  let fail () = failwith "browser can't read file: unimplemented" in
  match kind with
    | `BinaryString ->
      if Js.Optdef.test ( file##getAsBinary_presence )
      then file##getAsBinary()
      else fail ()
    | `Text ->
      if Js.Optdef.test ( file##getAsText_presence )
      then file##getAsText(Js.string "utf8")
      else fail ()
    | `Text_withEncoding e ->
      if Js.Optdef.test ( file##getAsText_presence )
      then file##getAsText(e)
      else fail ()
    | `DataURL ->
      if Js.Optdef.test ( file##getAsDataURL_presence )
      then file##getAsDataURL()
      else fail ()
(* end of old firefox specific part *)

let reader kind file =
  match Js.Optdef.to_option (Js.def fileReader) with
    | None -> Lwt.return (old_firefox_reader kind file)
    | Some fileReader -> read_with_filereader fileReader kind file

let readAsBinaryString file =
  reader `BinaryString file

let readAsText file =
  reader `Text file

let readAsText_withEncoding file e =
  reader (`Text_withEncoding e) file

let readAsDataURL file =
  reader `DataURL file

let addEventListener = Dom.addEventListener
