open Js.Obj
open Dom

class type disposable = object
  method dispose : unit meth
  method getDisposed : bool meth
  method isDisposed : bool meth
end

module Events = struct
  class type event = object
    inherit disposable
    method preventDefault : unit meth
    method stopPropagation : unit meth
  end

  class type browserEvent = object
    inherit event
    method altKey  : Js.bool readonly_prop
    method button : int readonly_prop
    method charCode : int readonly_prop
    method clientX : int readonly_prop
    method clientY : int readonly_prop
    method ctrlKey : Js.bool readonly_prop
    method keyCode : int readonly_prop
    method metaKey : Js.bool readonly_prop
    method offsetX : int readonly_prop
    method offsetY : int readonly_prop
    method platformModifierKey : Js.bool readonly_prop
    method relatedTarget : Dom.node Nullable.t readonly_prop
    method screenX : int readonly_prop
    method screenY : int readonly_prop
    method shiftKey : Js.bool readonly_prop
    method target : Dom.node (*Nullable.t?*) prop
  end

  class type eventTarget = object
    inherit disposable
    method addEventListener : Js.string -> (event -> Js.bool) -> unit meth
    method dispatchEvent : event -> Js.bool meth
    method getParentEventTarget : unit -> eventTarget meth
    method removeEventHandler : Js.string -> (event -> Js.bool) -> unit meth
    method setParentEventTarget : eventTarget -> unit meth
  end

  class type eventWrapper = object
    (*XXXX*)
  end

  class type eventHandler = object
    inherit disposable
    method handleEvent : event -> unit meth
(*FIX: type could/should be more general*)
    method listen :
      eventTarget -> string -> (event -> unit) Nullable.t ->
      Js.bool Nullable.t -> eventHandler meth
(*FIX: type could/should be more general*)
    method listenOnce :
      eventTarget -> string -> (event -> unit) Nullable.t ->
      Js.bool Nullable.t -> eventHandler meth
    method listenWithWrapper :
      eventTarget -> eventWrapper -> (event -> unit) Nullable.t ->
      Js.bool Nullable.t -> eventHandler meth
    method removeAll : unit meth
    method unlisten :
      eventTarget -> string -> (event -> unit) Nullable.t ->
      Js.bool Nullable.t -> eventHandler meth
    method unlistenWithWrapper :
      eventTarget -> eventWrapper -> (event -> unit) Nullable.t ->
      Js.bool Nullable.t -> eventHandler meth
  end
end

module Uri = struct
  class type uri = object
    method clone : uri t meth
    method getDecodedQuery : Js.string meth
    method getDomain : Js.string meth
    method getEncodedQuery : Js.string meth
    method getFragment : Js.string meth
    method getIgnoreCase : Js.bool meth
(*
    method getParameterValue : Js.string -> unit meth (*XXX*)
    method getParameterValues
*)
    method getPath : Js.string meth
    method getPort : int Nullable.t meth
    method getQuery : Js.string meth
(*
    method getQueryData : querydata t meth
*)
    method getScheme : Js.string meth
    method getUserInfo : Js.string meth
    method hasDomain : Js.bool meth
    method hasFragment : Js.bool meth
    method hasPath : Js.bool meth
    method hasPort : Js.bool meth
    method hasQuery : Js.bool meth
    method hasSameDomainAs : uri t -> Js.bool meth
    method hasScheme : Js.bool meth
    method hasUserInfo : Js.bool meth
    method isReadOnly : Js.bool meth
    method makeUnique : uri t meth
    method removeParameter : Js.string -> uri t meth
    method resolve : uri t -> uri t meth
    method setDomain : Js.string -> bool -> uri t meth
    method setFragment : Js.string -> bool -> uri t meth
    method setIgnoreCase : Js.bool -> unit meth
(*
    method setParameterValue : 
    method setParameterValues : 
*)
    method setPath : Js.string -> Js.bool -> uri t meth
    method setPort : int -> uri t meth
    method setQuery : Js.string -> Js.bool -> uri t meth
(*
    method setQueryData : 
*)
    method setReadOnly : Js.bool -> unit meth
    method setScheme : Js.string -> bool -> uri t meth
    method setUserInfo : Js.string -> bool -> uri t meth
    method toString : Js.string meth
  end
end
