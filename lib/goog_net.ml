open Js

type errorCode =
    NO_ERROR
  | ACCESS_DENIED
  | FILE_NOT_FOUND
  | FF_SILENT_ERROR
  | CUSTOM_ERROR
  | EXCEPTION
  | HTTP_ERROR
  | ABORT
  | TIMEOUT
  | OFFLINE

module XhrIO = struct

  class type c = object
    inherit Goog.Events.eventTarget
    method abort : errorCode opt -> unit meth
    method getLastError : string t meth
    method getLastUri : string t meth
(*
    method getReadyState : XmlHttp.readyState meth
FIX:can return undefined!
    method getResponseHeader : string t -> string t meth
FIX:which type?
    method getResponseJson : string t opt -> < > t meth
*)
    method getResponseText : string t meth
    method getResponseXml : Dom.element Dom.document opt meth
    method getStatus : int meth
    method getStatusText : string meth
    method getTimeoutInterval : float meth
    method isActive : bool t meth
    method isComplete : bool t meth
    method isSuccess : bool t meth
(*collections?
    method send :
      string t -> string t opt -> string t -> ... -> unit meth
*)
    method setTimeoutInterval : float -> unit meth
  end

  class type u = object
    method cleanup : unit meth
(*
    method protectEntryPoints : ... -> bool -> unit meth
    method send :
      string t -> (Goog.Events.event -> unit) opt ->
      string t opt -> string t opt -> ... ->
      float opt ->
      unit meth
*)
    method _CONTENT_TYPE_HEADER : string readonly_prop
    method _FORM_CONTENT_TYPE : string readonly_prop
  end
end
