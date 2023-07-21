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
open! Import

class type blob =
  object
    method size : int readonly_prop

    method _type : js_string t readonly_prop

    method slice : int -> int -> blob t meth

    method slice_withContentType : int -> int -> js_string t -> blob t meth
  end

let blob_constr = Unsafe.global##._Blob

type 'a make_blob =
  ?contentType:string -> ?endings:[ `Transparent | `Native ] -> 'a -> blob t

let rec filter_map f = function
  | [] -> []
  | v :: q -> (
      match f v with
      | None -> filter_map f q
      | Some v' -> v' :: filter_map f q)

let make_blob_options contentType endings =
  let options =
    filter_map
      (fun (name, v) ->
        match v with
        | None -> None
        | Some v -> Some (name, Unsafe.inject (string v)))
      [ "type", contentType
      ; ( "endings"
        , match endings with
          | None -> None
          | Some `Transparent -> Some "transparent"
          | Some `Native -> Some "native" )
      ]
  in
  match options with
  | [] -> undefined
  | l -> Unsafe.obj (Array.of_list l)

let blob_raw ?contentType ?endings a =
  let options = make_blob_options contentType endings in
  new%js blob_constr (array a) options

let blob_from_string ?contentType ?endings s =
  blob_raw ?contentType ?endings [| string s |]

let blob_from_any ?contentType ?endings l =
  let l =
    List.map
      (function
        | `arrayBuffer a -> Unsafe.inject a
        | `arrayBufferView a -> Unsafe.inject a
        | `string s -> Unsafe.inject (string s)
        | `js_string s -> Unsafe.inject s
        | `blob b -> Unsafe.inject b)
      l
  in
  blob_raw ?contentType ?endings (Array.of_list l)

class type file =
  object
    inherit blob

    method name : js_string t readonly_prop

    method lastModifiedDate : js_string t readonly_prop
  end

(* in firefox 3.0-3.5 file.name is not available, we use the nonstandard fileName instead *)
class type file_name_only =
  object
    method name : js_string t optdef readonly_prop

    method fileName : js_string t optdef readonly_prop
  end

let filename file =
  let file : file_name_only t = Js.Unsafe.coerce file in
  match Optdef.to_option file##.name with
  | None -> (
      match Optdef.to_option file##.fileName with
      | None -> failwith "can't retrieve file name: not implemented"
      | Some name -> name)
  | Some name -> name

type file_any = < > t

let doc_constr = Unsafe.global##._Document

module CoerceTo = struct
  external json : file_any -> 'a Opt.t = "%identity"

  let document (e : file_any) =
    if instanceof e doc_constr
    then Js.some (Unsafe.coerce e : element document t)
    else Js.null

  let blob (e : file_any) =
    if instanceof e blob_constr then Js.some (Unsafe.coerce e : #blob t) else Js.null

  let string (e : file_any) =
    if Js.equals (typeof e) (Js.string "string")
    then Js.some (Unsafe.coerce e : js_string t)
    else Js.null

  let arrayBuffer (e : file_any) =
    if instanceof e Typed_array.arrayBuffer
    then Js.some (Unsafe.coerce e : Typed_array.arrayBuffer t)
    else Js.null
end

class type fileList =
  object
    inherit [file] Dom.nodeList
  end

class type fileError =
  object
    method code : int readonly_prop
  end

class type ['a] progressEvent =
  object
    inherit ['a] event

    method lengthComputable : bool t readonly_prop

    method loaded : int readonly_prop

    method total : int readonly_prop
  end

class type progressEventTarget =
  object ('self)
    method onloadstart : ('self t, 'self progressEvent t) event_listener writeonly_prop

    method onprogress : ('self t, 'self progressEvent t) event_listener writeonly_prop

    method onload : ('self t, 'self progressEvent t) event_listener writeonly_prop

    method onabort : ('self t, 'self progressEvent t) event_listener writeonly_prop

    method onerror : ('self t, 'self progressEvent t) event_listener writeonly_prop

    method onloadend : ('self t, 'self progressEvent t) event_listener writeonly_prop
  end

type readyState =
  | EMPTY
  | LOADING
  | DONE

class type fileReader =
  object ('self)
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

let fileReader : fileReader t constr = Js.Unsafe.global##._FileReader

let addEventListener = Dom.addEventListener
