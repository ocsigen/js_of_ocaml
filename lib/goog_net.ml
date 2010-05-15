
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
  open Js.Obj

  class type t = object
    inherit Goog.Events.eventTarget
    method abort : errorCode Nullable.t -> unit meth
    method getLastError : Js.string meth
    method getLastUri : Js.string meth
(*
    method getReadyState : XmlHttp.readyState meth
FIX:can return undefined!
    method getResponseHeader : Js.string -> Js.string meth
FIX:which type?
    method getResponseJson : Js.string Nullable.t -> < > t meth
*)
    method getResponseText : Js.string meth
    method getResponseXml : Dom.Dom.element Dom.Dom.document Nullable.t meth
    method getStatus : int meth
    method getStatusText : string meth
    method getTimeoutInterval : float meth
    method isActive : Js.bool meth
    method isComplete : Js.bool meth
    method isSuccess : Js.bool meth
(*collections?
    method send :
      Js.string -> Js.string Nullable.t -> Js.string -> ... -> unit meth
*)
    method setTimeoutInterval : float -> unit meth
  end

  class type u = object
    method cleanup : unit meth
(*
    method protectEntryPoints : ... -> bool -> unit meth
    method send :
      Js.string -> (Goog.Events.event -> unit) Nullable.t ->
      Js.string Nullable.t -> Js.string Nullable.t -> ... ->
      float Nullable.t ->
      unit meth
*)
    method _CONTENT_TYPE_HEADER : string readonly_prop
    method _FORM_CONTENT_TYPE : string readonly_prop
  end
end
