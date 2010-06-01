
open Js

type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

class type xmlHttpRequest = object
  method onreadystatechange : (unit -> unit) prop
  method readyState : readyState readonly_prop
  method _open :
    js_string t -> js_string t -> bool t ->
    js_string t opt -> js_string t opt -> unit meth
  method setRequestHeader : js_string t -> js_string t -> unit meth
  method send : js_string t opt -> unit meth
  method _send : #Dom.element #Dom.document -> unit meth (* overloading! *)
(*
  void send(Document data);
  void send([AllowAny] DOMJs_String? data);
*)
  method abort : unit meth
  method status : int readonly_prop
  method statusText : js_string t readonly_prop
  method getResponseHeader : js_string t -> js_string t meth
  method getAllResponseHeaders : js_string t meth
  method responseText : js_string t readonly_prop
  method responseXML : Dom.element Dom.document t readonly_prop
end

val create : unit -> xmlHttpRequest t

val send_request :
  js_string t -> (xmlHttpRequest t -> unit) -> js_string t opt -> unit
