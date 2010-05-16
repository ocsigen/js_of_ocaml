open Js
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
    method altKey  : bool t readonly_prop
    method button : int readonly_prop
    method charCode : int readonly_prop
    method clientX : int readonly_prop
    method clientY : int readonly_prop
    method ctrlKey : bool t readonly_prop
    method keyCode : int readonly_prop
    method metaKey : bool t readonly_prop
    method offsetX : int readonly_prop
    method offsetY : int readonly_prop
    method platformModifierKey : bool t readonly_prop
    method relatedTarget : Dom.node opt readonly_prop
    method screenX : int readonly_prop
    method screenY : int readonly_prop
    method shiftKey : bool t readonly_prop
    method target : Dom.node (*opt?*) prop
  end

  class type eventTarget = object
    inherit disposable
    method addEventListener : string t -> (event -> bool t) -> unit meth
    method dispatchEvent : event -> bool t meth
    method getParentEventTarget : unit -> eventTarget meth
    method removeEventHandler : string t -> (event -> bool t) -> unit meth
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
      eventTarget -> string -> (event -> unit) opt ->
      bool t opt -> eventHandler meth
(*FIX: type could/should be more general*)
    method listenOnce :
      eventTarget -> string -> (event -> unit) opt ->
      bool t opt -> eventHandler meth
    method listenWithWrapper :
      eventTarget -> eventWrapper -> (event -> unit) opt ->
      bool t opt -> eventHandler meth
    method removeAll : unit meth
    method unlisten :
      eventTarget -> string -> (event -> unit) opt ->
      bool t opt -> eventHandler meth
    method unlistenWithWrapper :
      eventTarget -> eventWrapper -> (event -> unit) opt ->
      bool t opt -> eventHandler meth
  end
end

module Uri = struct
  class type uri = object
    method clone : uri t meth
    method getDecodedQuery : string t meth
    method getDomain : string t meth
    method getEncodedQuery : string t meth
    method getFragment : string t meth
    method getIgnoreCase : bool t meth
(*
    method getParameterValue : string t -> unit meth (*XXX*)
    method getParameterValues
*)
    method getPath : string t meth
    method getPort : int opt meth
    method getQuery : string t meth
(*
    method getQueryData : querydata t meth
*)
    method getScheme : string t meth
    method getUserInfo : string t meth
    method hasDomain : bool t meth
    method hasFragment : bool t meth
    method hasPath : bool t meth
    method hasPort : bool t meth
    method hasQuery : bool t meth
    method hasSameDomainAs : uri t -> bool t meth
    method hasScheme : bool t meth
    method hasUserInfo : bool t meth
    method isReadOnly : bool t meth
    method makeUnique : uri t meth
    method removeParameter : string t -> uri t meth
    method resolve : uri t -> uri t meth
    method setDomain : string t -> bool -> uri t meth
    method setFragment : string t -> bool -> uri t meth
    method setIgnoreCase : bool t -> unit meth
(*
    method setParameterValue : 
    method setParameterValues : 
*)
    method setPath : string t -> bool t -> uri t meth
    method setPort : int -> uri t meth
    method setQuery : string t -> bool t -> uri t meth
(*
    method setQueryData : 
*)
    method setReadOnly : bool t -> unit meth
    method setScheme : string t -> bool -> uri t meth
    method setUserInfo : string t -> bool -> uri t meth
    method toString : string t meth
  end
end
