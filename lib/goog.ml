open Js.Obj
open Dom

class type disposable = object
  method dispose : unit meth
  method getDisposed : bool meth
  method isDisposed : bool meth
end

module Event = struct
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

module Ui = struct
  
end
