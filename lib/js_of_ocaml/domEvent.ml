open Js

type mouse_button =
  | No_button
  | Left_button
  | Middle_button
  | Right_button

class type ['a] event =
  object
    method _type : js_string t readonly_prop

    method target : 'a t opt readonly_prop

    method currentTarget : 'a t opt readonly_prop

    (* Legacy methods *)
    method srcElement : 'a t opt readonly_prop
  end

and ['a] mouseEvent =
  object
    inherit ['a] event

    method relatedTarget : 'a t opt optdef readonly_prop

    method clientX : int readonly_prop

    (* Relative to viewport *)
    method clientY : int readonly_prop

    method screenX : int readonly_prop

    (* Relative to the edge of the screen *)
    method screenY : int readonly_prop

    method ctrlKey : bool t readonly_prop

    method shiftKey : bool t readonly_prop

    method altKey : bool t readonly_prop

    method metaKey : bool t readonly_prop

    method which : mouse_button optdef readonly_prop

    (* Legacy methods *)
    method button : int readonly_prop

    method fromElement : 'a t opt optdef readonly_prop

    method toElement : 'a t opt optdef readonly_prop

    method pageX : int optdef readonly_prop

    method pageY : int optdef readonly_prop
  end

and ['a] keyboardEvent =
  object
    inherit ['a] event

    method altKey : bool t readonly_prop

    method shiftKey : bool t readonly_prop

    method ctrlKey : bool t readonly_prop

    method metaKey : bool t readonly_prop

    method location : int readonly_prop

    (* Standardized but not fully supported properties *)
    method key : js_string t optdef readonly_prop

    method code : js_string t optdef readonly_prop

    (* Deprecated properties *)
    method which : int optdef readonly_prop

    method charCode : int optdef readonly_prop

    method keyCode : int readonly_prop

    method keyIdentifier : js_string t optdef readonly_prop
  end

and ['a] mousewheelEvent =
  object
    (* All browsers but Firefox *)
    inherit ['a] mouseEvent

    method wheelDelta : int readonly_prop

    method wheelDeltaX : int optdef readonly_prop

    method wheelDeltaY : int optdef readonly_prop
  end

and ['a] mouseScrollEvent =
  object
    (* Firefox *)
    inherit ['a] mouseEvent

    method detail : int readonly_prop

    method axis : int optdef readonly_prop

    method _HORIZONTAL_AXIS : int optdef readonly_prop

    method _VERTICAL_AXIS : int optdef readonly_prop
  end

and ['a] touchEvent =
  object
    inherit ['a] event

    method touches : 'a touchList t readonly_prop

    method targetTouches : 'a touchList t readonly_prop

    method changedTouches : 'a touchList t readonly_prop

    method ctrlKey : bool t readonly_prop

    method shiftKey : bool t readonly_prop

    method altKey : bool t readonly_prop

    method metaKey : bool t readonly_prop

    method relatedTarget : 'a t opt optdef readonly_prop
  end

and ['a] touchList =
  object
    method length : int readonly_prop

    method item : int -> 'a touch t optdef meth
  end

and ['a] touch =
  object
    method identifier : int readonly_prop

    method target : 'a t optdef readonly_prop

    method screenX : int readonly_prop

    method screenY : int readonly_prop

    method clientX : int readonly_prop

    method clientY : int readonly_prop

    method pageX : int readonly_prop

    method pageY : int readonly_prop
  end

and ['a] dragEvent =
  object
    inherit ['a] mouseEvent

    method dataTransfer : 'a dataTransfer t readonly_prop
  end

and ['a] dataTransfer =
  object
    method dropEffect : js_string t prop

    method effectAllowed : js_string t prop

    method files : File.fileList t readonly_prop

    method types : js_string t js_array t readonly_prop

    method addElement : 'a t -> unit meth

    method clearData : js_string t -> unit meth

    method clearData_all : unit meth

    method getData : js_string t -> js_string t meth

    method setData : js_string t -> js_string t -> unit meth

    method setDragImage : 'a t -> int -> int -> unit meth
  end

and ['a] eventTarget =
  object ('self)
    method onclick : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method ondblclick : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method onmousedown : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method onmouseup : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method onmouseover : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method onmousemove : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method onmouseout : ('self t, 'a mouseEvent t) Dom.event_listener Js.writeonly_prop

    method onkeypress :
      ('self t, 'a keyboardEvent t) Dom.event_listener Js.writeonly_prop

    method onkeydown : ('self t, 'a keyboardEvent t) Dom.event_listener Js.writeonly_prop

    method onkeyup : ('self t, 'a keyboardEvent t) Dom.event_listener Js.writeonly_prop

    method onscroll : ('self t, 'a event t) Dom.event_listener Js.writeonly_prop

    method ondragstart : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop

    method ondragend : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop

    method ondragenter : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop

    method ondragover : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop

    method ondragleave : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop

    method ondrag : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop

    method ondrop : ('self t, 'a dragEvent t) Dom.event_listener Js.writeonly_prop
  end
