open Js
open Dom_html

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

type file_any

module CoerceTo : sig
  val string : file_any -> js_string t Opt.t
  (* CCC need to be able to coerce to array buffer *)
end

class type fileList = object
  inherit [file] Dom.nodeList
end

class type fileError = object
  method code : int readonly_prop
end

type readyState = EMPTY | LOADING | DONE

class type fileReader = object ('self)

  method readAsArrayBuffer : blob t -> unit meth
  method readAsBinaryString : blob t -> unit meth
  method readAsText : blob t -> unit meth
  method readAsText_withEncoding : blob t -> js_string t -> unit meth
  method readAsDataURL : blob t -> unit meth

  method abort : unit meth

  method readyState : readyState readonly_prop

  method result : file_any readonly_prop
  method error : fileError t readonly_prop

  method onloadstart : ('self t, event t) event_listener writeonly_prop
  method onprogress : ('self t, event t) event_listener writeonly_prop
  method onload : ('self t, event t) event_listener writeonly_prop
  method onabort : ('self t, event t) event_listener writeonly_prop
  method onerror : ('self t, event t) event_listener writeonly_prop
  method onloadend : ('self t, event t) event_listener writeonly_prop

  inherit Dom_html.eventTarget
end

val readAsBinaryString : blob t -> js_string t Lwt.t
val readAsText : blob t -> js_string t Lwt.t
val readAsText_withEncoding  : blob t -> js_string t -> js_string t Lwt.t
val readAsDataURL : blob t -> js_string t Lwt.t
