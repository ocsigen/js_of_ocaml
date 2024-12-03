(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
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
open! Import

external caml_js_on_ie : unit -> bool t = "caml_js_on_ie"

let onIE = Js.to_bool (caml_js_on_ie ())

external html_escape : js_string t -> js_string t = "caml_js_html_escape"

external html_entities : js_string t -> js_string t opt = "caml_js_html_entities"

let decode_html_entities s =
  Js.Opt.get (html_entities s) (fun () -> failwith ("Invalid entity " ^ Js.to_string s))

class type cssStyleDeclaration = object
  method setProperty :
    js_string t -> js_string t -> js_string t optdef -> js_string t meth

  method getPropertyValue : js_string t -> js_string t meth

  method getPropertyPriority : js_string t -> js_string t meth

  method removeProperty : js_string t -> js_string t meth

  method animation : js_string t prop

  method animationDelay : js_string t prop

  method animationDirection : js_string t prop

  method animationDuration : js_string t prop

  method animationFillMode : js_string t prop

  method animationIterationCount : js_string t prop

  method animationName : js_string t prop

  method animationPlayState : js_string t prop

  method animationTimingFunction : js_string t prop

  method background : js_string t prop

  method backgroundAttachment : js_string t prop

  method backgroundColor : js_string t prop

  method backgroundImage : js_string t prop

  method backgroundPosition : js_string t prop

  method backgroundRepeat : js_string t prop

  method border : js_string t prop

  method borderBottom : js_string t prop

  method borderBottomColor : js_string t prop

  method borderBottomStyle : js_string t prop

  method borderBottomWidth : js_string t prop

  method borderCollapse : js_string t prop

  method borderColor : js_string t prop

  method borderLeft : js_string t prop

  method borderLeftColor : js_string t prop

  method borderLeftStyle : js_string t prop

  method borderLeftWidth : js_string t prop

  method borderRadius : js_string t prop

  method borderRight : js_string t prop

  method borderRightColor : js_string t prop

  method borderRightStyle : js_string t prop

  method borderRightWidth : js_string t prop

  method borderSpacing : js_string t prop

  method borderStyle : js_string t prop

  method borderTop : js_string t prop

  method borderTopColor : js_string t prop

  method borderTopStyle : js_string t prop

  method borderTopWidth : js_string t prop

  method borderWidth : js_string t prop

  method bottom : js_string t prop

  method captionSide : js_string t prop

  method clear : js_string t prop

  method clip : js_string t prop

  method color : js_string t prop

  method content : js_string t prop

  method counterIncrement : js_string t prop

  method counterReset : js_string t prop

  method cssFloat : js_string t prop

  method cssText : js_string t prop

  method cursor : js_string t prop

  method direction : js_string t prop

  method display : js_string t prop

  method emptyCells : js_string t prop

  method fill : js_string t prop

  method font : js_string t prop

  method fontFamily : js_string t prop

  method fontSize : js_string t prop

  method fontStyle : js_string t prop

  method fontVariant : js_string t prop

  method fontWeight : js_string t prop

  method height : js_string t prop

  method left : js_string t prop

  method letterSpacing : js_string t prop

  method lineHeight : js_string t prop

  method listStyle : js_string t prop

  method listStyleImage : js_string t prop

  method listStylePosition : js_string t prop

  method listStyleType : js_string t prop

  method margin : js_string t prop

  method marginBottom : js_string t prop

  method marginLeft : js_string t prop

  method marginRight : js_string t prop

  method marginTop : js_string t prop

  method maxHeight : js_string t prop

  method maxWidth : js_string t prop

  method minHeight : js_string t prop

  method minWidth : js_string t prop

  method opacity : js_string t optdef prop

  method outline : js_string t prop

  method outlineColor : js_string t prop

  method outlineOffset : js_string t prop

  method outlineStyle : js_string t prop

  method outlineWidth : js_string t prop

  method overflow : js_string t prop

  method overflowX : js_string t prop

  method overflowY : js_string t prop

  method padding : js_string t prop

  method paddingBottom : js_string t prop

  method paddingLeft : js_string t prop

  method paddingRight : js_string t prop

  method paddingTop : js_string t prop

  method pageBreakAfter : js_string t prop

  method pageBreakBefore : js_string t prop

  method pointerEvents : js_string t prop

  method position : js_string t prop

  method right : js_string t prop

  method stroke : js_string t prop

  method strokeWidth : js_string t prop

  method tableLayout : js_string t prop

  method textAlign : js_string t prop

  method textAnchor : js_string t prop

  method textDecoration : js_string t prop

  method textIndent : js_string t prop

  method textTransform : js_string t prop

  method top : js_string t prop

  method transform : js_string t prop

  method verticalAlign : js_string t prop

  method visibility : js_string t prop

  method whiteSpace : js_string t prop

  method width : js_string t prop

  method wordSpacing : js_string t prop

  method zIndex : js_string t prop
end

type ('a, 'b) event_listener = ('a, 'b) Dom.event_listener

type mouse_button =
  | No_button
  | Left_button
  | Middle_button
  | Right_button

type delta_mode =
  | Delta_pixel
  | Delta_line
  | Delta_page

class type event = object
  inherit [element] Dom.event
end

and ['a] customEvent = object
  inherit [element, 'a] Dom.customEvent
end

and focusEvent = object
  inherit event

  method relatedTarget : element t opt optdef readonly_prop
end

and mouseEvent = object
  inherit event

  method relatedTarget : element t opt optdef readonly_prop

  method clientX : number_t readonly_prop

  method clientY : number_t readonly_prop

  method screenX : number_t readonly_prop

  method screenY : number_t readonly_prop

  method offsetX : number_t readonly_prop

  method offsetY : number_t readonly_prop

  method ctrlKey : bool t readonly_prop

  method shiftKey : bool t readonly_prop

  method altKey : bool t readonly_prop

  method metaKey : bool t readonly_prop

  method button : int readonly_prop

  method which : mouse_button optdef readonly_prop

  method fromElement : element t opt optdef readonly_prop

  method toElement : element t opt optdef readonly_prop

  method pageX : number_t optdef readonly_prop

  method pageY : number_t optdef readonly_prop
end

and keyboardEvent = object
  inherit event

  method altKey : bool t readonly_prop

  method shiftKey : bool t readonly_prop

  method ctrlKey : bool t readonly_prop

  method metaKey : bool t readonly_prop

  method location : int readonly_prop

  method key : js_string t optdef readonly_prop

  method code : js_string t optdef readonly_prop

  method which : int optdef readonly_prop

  method charCode : int optdef readonly_prop

  method keyCode : int readonly_prop

  method getModifierState : js_string t -> bool t meth

  method keyIdentifier : js_string t optdef readonly_prop
end

and mousewheelEvent = object
  (* All modern browsers *)
  inherit mouseEvent

  method wheelDelta : int readonly_prop

  method wheelDeltaX : int optdef readonly_prop

  method wheelDeltaY : int optdef readonly_prop

  method deltaX : number_t readonly_prop

  method deltaY : number_t readonly_prop

  method deltaZ : number_t readonly_prop

  method deltaMode : delta_mode readonly_prop
end

and mouseScrollEvent = object
  (* Firefox *)
  inherit mouseEvent

  method detail : int readonly_prop

  method axis : int optdef readonly_prop

  method _HORIZONTAL_AXIS : int optdef readonly_prop

  method _VERTICAL_AXIS : int optdef readonly_prop
end

and touchEvent = object
  inherit event

  method touches : touchList t readonly_prop

  method targetTouches : touchList t readonly_prop

  method changedTouches : touchList t readonly_prop

  method ctrlKey : bool t readonly_prop

  method shiftKey : bool t readonly_prop

  method altKey : bool t readonly_prop

  method metaKey : bool t readonly_prop

  method relatedTarget : element t opt optdef readonly_prop
end

and touchList = object
  method length : int readonly_prop

  method item : int -> touch t optdef meth
end

and touch = object
  method identifier : int readonly_prop

  method target : element t optdef readonly_prop

  method screenX : number_t readonly_prop

  method screenY : number_t readonly_prop

  method clientX : number_t readonly_prop

  method clientY : number_t readonly_prop

  method pageX : number_t readonly_prop

  method pageY : number_t readonly_prop
end

and submitEvent = object
  inherit event

  method submitter : element t optdef readonly_prop
end

and dragEvent = object
  inherit mouseEvent

  method dataTransfer : dataTransfer t readonly_prop
end

and clipboardEvent = object
  inherit event

  method clipboardData : dataTransfer t readonly_prop
end

and toggleEvent = object
  inherit event

  method newState : js_string t readonly_prop

  method oldState : js_string t readonly_prop
end

and dataTransfer = object
  method dropEffect : js_string t prop

  method effectAllowed : js_string t prop

  method files : File.fileList t readonly_prop

  method types : js_string t js_array t readonly_prop

  method addElement : element t -> unit meth

  method clearData : js_string t -> unit meth

  method clearData_all : unit meth

  method getData : js_string t -> js_string t meth

  method setData : js_string t -> js_string t -> unit meth

  method setDragImage : element t -> int -> int -> unit meth
end

and eventTarget = object ('self)
  method onclick : ('self t, mouseEvent t) event_listener writeonly_prop

  method ondblclick : ('self t, mouseEvent t) event_listener writeonly_prop

  method onmousedown : ('self t, mouseEvent t) event_listener writeonly_prop

  method onmouseup : ('self t, mouseEvent t) event_listener writeonly_prop

  method onmouseover : ('self t, mouseEvent t) event_listener writeonly_prop

  method onmousemove : ('self t, mouseEvent t) event_listener writeonly_prop

  method onmouseout : ('self t, mouseEvent t) event_listener writeonly_prop

  method onkeypress : ('self t, keyboardEvent t) event_listener writeonly_prop

  method onkeydown : ('self t, keyboardEvent t) event_listener writeonly_prop

  method onkeyup : ('self t, keyboardEvent t) event_listener writeonly_prop

  method onscroll : ('self t, event t) event_listener writeonly_prop

  method onwheel : ('self t, mousewheelEvent t) event_listener writeonly_prop

  method ondragstart : ('self t, dragEvent t) event_listener writeonly_prop

  method ondragend : ('self t, dragEvent t) event_listener writeonly_prop

  method ondragenter : ('self t, dragEvent t) event_listener writeonly_prop

  method ondragover : ('self t, dragEvent t) event_listener writeonly_prop

  method ondragleave : ('self t, dragEvent t) event_listener writeonly_prop

  method ondrag : ('self t, dragEvent t) event_listener writeonly_prop

  method ondrop : ('self t, dragEvent t) event_listener writeonly_prop

  method onanimationstart : ('self t, animationEvent t) event_listener writeonly_prop

  method onanimationend : ('self t, animationEvent t) event_listener writeonly_prop

  method onanimationiteration : ('self t, animationEvent t) event_listener writeonly_prop

  method onanimationcancel : ('self t, animationEvent t) event_listener writeonly_prop

  method ontransitionrun : ('self t, transitionEvent t) event_listener writeonly_prop

  method ontransitionstart : ('self t, transitionEvent t) event_listener writeonly_prop

  method ontransitionend : ('self t, transitionEvent t) event_listener writeonly_prop

  method ontransitioncancel : ('self t, transitionEvent t) event_listener writeonly_prop

  method ongotpointercapture : ('self t, pointerEvent t) event_listener writeonly_prop

  method onlostpointercapture : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointerenter : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointercancel : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointerdown : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointerleave : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointermove : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointerout : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointerover : ('self t, pointerEvent t) event_listener writeonly_prop

  method onpointerup : ('self t, pointerEvent t) event_listener writeonly_prop

  method dispatchEvent : event t -> bool t meth
end

and popStateEvent = object
  inherit event

  method state : Js.Unsafe.any readonly_prop
end

and pointerEvent = object
  inherit mouseEvent

  method pointerId : int Js.readonly_prop

  method width : number_t Js.readonly_prop

  method height : number_t Js.readonly_prop

  method pressure : number_t Js.readonly_prop

  method tangentialPressure : number_t Js.readonly_prop

  method tiltX : int Js.readonly_prop

  method tiltY : int Js.readonly_prop

  method twist : int Js.readonly_prop

  method pointerType : Js.js_string Js.t Js.readonly_prop

  method isPrimary : bool Js.t Js.readonly_prop
end

and storageEvent = object
  inherit event

  method key : js_string t opt readonly_prop

  method oldValue : js_string t opt readonly_prop

  method newValue : js_string t opt readonly_prop

  method url : js_string t readonly_prop

  method storageArea : storage t opt readonly_prop
end

and storage = object
  method length : int readonly_prop

  method key : int -> js_string t opt meth

  method getItem : js_string t -> js_string t opt meth

  method setItem : js_string t -> js_string t -> unit meth

  method removeItem : js_string t -> unit meth

  method clear : unit meth
end

and hashChangeEvent = object
  inherit event

  method oldURL : js_string t readonly_prop

  method newURL : js_string t readonly_prop
end

and animationEvent = object
  inherit event

  method animationName : js_string t readonly_prop

  method elapsedTime : number_t readonly_prop

  method pseudoElement : js_string t readonly_prop
end

and transitionEvent = object
  inherit event

  method propertyName : js_string t readonly_prop

  method elapsedTime : number_t readonly_prop

  method pseudoElement : js_string t readonly_prop
end

and mediaEvent = object
  inherit event
end

and messageEvent = object
  inherit event

  method data : Unsafe.any opt readonly_prop

  method source : Unsafe.any opt readonly_prop
end

and nodeSelector = object
  method querySelector : js_string t -> element t opt meth

  method querySelectorAll : js_string t -> element Dom.nodeList t meth
end

and tokenList = object
  method length : int readonly_prop

  method item : int -> js_string t optdef meth

  method contains : js_string t -> bool t meth

  method add : js_string t -> unit meth

  method remove : js_string t -> unit meth

  method toggle : js_string t -> bool t meth

  method stringifier : js_string t prop
end

and element = object
  inherit Dom.element

  inherit nodeSelector

  method id : js_string t prop

  method title : js_string t prop

  method lang : js_string t prop

  method dir : js_string t prop

  method className : js_string t prop

  method classList : tokenList t readonly_prop

  method closest : js_string t -> element t opt meth

  method style : cssStyleDeclaration t prop

  method innerHTML : js_string t prop

  method outerHTML : js_string t prop

  method textContent : js_string t opt prop

  method innerText : js_string t prop

  method clientLeft : int readonly_prop

  method clientTop : int readonly_prop

  method clientWidth : int readonly_prop

  method clientHeight : int readonly_prop

  method offsetLeft : int readonly_prop

  method offsetTop : int readonly_prop

  method offsetParent : element t opt readonly_prop

  method offsetWidth : int readonly_prop

  method offsetHeight : int readonly_prop

  method scrollLeft : number_t prop

  method scrollTop : number_t prop

  method scrollWidth : int prop

  method scrollHeight : int prop

  method getClientRects : clientRectList t meth

  method getBoundingClientRect : clientRect t meth

  method scrollIntoView : bool t -> unit meth

  method click : unit meth

  method focus : unit meth

  method blur : unit meth

  inherit eventTarget
end

and clientRect = object
  method top : number_t readonly_prop

  method right : number_t readonly_prop

  method bottom : number_t readonly_prop

  method left : number_t readonly_prop

  method width : number_t optdef readonly_prop

  method height : number_t optdef readonly_prop
end

and clientRectList = object
  method length : int readonly_prop

  method item : int -> clientRect t opt meth
end

let no_handler : ('a, 'b) event_listener = Dom.no_handler

let handler = Dom.handler

let full_handler = Dom.full_handler

let invoke_handler = Dom.invoke_handler

module Event = struct
  type 'a typ = 'a Dom.Event.typ

  let cancel = Dom.Event.make "cancel"

  let click = Dom.Event.make "click"

  let close = Dom.Event.make "close"

  let copy = Dom.Event.make "copy"

  let cut = Dom.Event.make "cut"

  let paste = Dom.Event.make "paste"

  let dblclick = Dom.Event.make "dblclick"

  let mousedown = Dom.Event.make "mousedown"

  let mouseup = Dom.Event.make "mouseup"

  let mouseover = Dom.Event.make "mouseover"

  let mousemove = Dom.Event.make "mousemove"

  let mouseout = Dom.Event.make "mouseout"

  let keypress = Dom.Event.make "keypress"

  let keydown = Dom.Event.make "keydown"

  let keyup = Dom.Event.make "keyup"

  let mousewheel = Dom.Event.make "mousewheel"

  let wheel = Dom.Event.make "wheel"

  let _DOMMouseScroll = Dom.Event.make "DOMMouseScroll"

  let touchstart = Dom.Event.make "touchstart"

  let touchmove = Dom.Event.make "touchmove"

  let touchend = Dom.Event.make "touchend"

  let touchcancel = Dom.Event.make "touchcancel"

  let dragstart = Dom.Event.make "dragstart"

  let dragend = Dom.Event.make "dragend"

  let dragenter = Dom.Event.make "dragenter"

  let dragover = Dom.Event.make "dragover"

  let dragleave = Dom.Event.make "dragleave"

  let drag = Dom.Event.make "drag"

  let drop = Dom.Event.make "drop"

  let hashchange = Dom.Event.make "hashchange"

  let change = Dom.Event.make "change"

  let input = Dom.Event.make "input"

  let timeupdate = Dom.Event.make "timeupdate"

  let submit = Dom.Event.make "submit"

  let scroll = Dom.Event.make "scroll"

  let focus = Dom.Event.make "focus"

  let blur = Dom.Event.make "blur"

  let load = Dom.Event.make "load"

  let unload = Dom.Event.make "unload"

  let beforeunload = Dom.Event.make "beforeunload"

  let resize = Dom.Event.make "resize"

  let orientationchange = Dom.Event.make "orientationchange"

  let popstate = Dom.Event.make "popstate"

  let error = Dom.Event.make "error"

  let abort = Dom.Event.make "abort"

  let select = Dom.Event.make "select"

  let online = Dom.Event.make "online"

  let offline = Dom.Event.make "offline"

  let checking = Dom.Event.make "checking"

  let noupdate = Dom.Event.make "noupdate"

  let downloading = Dom.Event.make "downloading"

  let progress = Dom.Event.make "progress"

  let updateready = Dom.Event.make "updateready"

  let cached = Dom.Event.make "cached"

  let obsolete = Dom.Event.make "obsolete"

  let domContentLoaded = Dom.Event.make "DOMContentLoaded"

  let animationstart = Dom.Event.make "animationstart"

  let animationend = Dom.Event.make "animationend"

  let animationiteration = Dom.Event.make "animationiteration"

  let animationcancel = Dom.Event.make "animationcancel"

  let transitionrun = Dom.Event.make "transitionrun"

  let transitionstart = Dom.Event.make "transitionstart"

  let transitionend = Dom.Event.make "transitionend"

  let transitioncancel = Dom.Event.make "transitioncancel"

  let canplay = Dom.Event.make "canplay"

  let canplaythrough = Dom.Event.make "canplaythrough"

  let durationchange = Dom.Event.make "durationchange"

  let emptied = Dom.Event.make "emptied"

  let ended = Dom.Event.make "ended"

  let gotpointercapture = Dom.Event.make "gotpointercapture"

  let loadeddata = Dom.Event.make "loadeddata"

  let loadedmetadata = Dom.Event.make "loadedmetadata"

  let loadstart = Dom.Event.make "loadstart"

  let lostpointercapture = Dom.Event.make "lostpointercapture"

  let message = Dom.Event.make "message"

  let pause = Dom.Event.make "pause"

  let play = Dom.Event.make "play"

  let playing = Dom.Event.make "playing"

  let pointerenter = Dom.Event.make "pointerenter"

  let pointercancel = Dom.Event.make "pointercancel"

  let pointerdown = Dom.Event.make "pointerdown"

  let pointerleave = Dom.Event.make "pointerleave"

  let pointermove = Dom.Event.make "pointermove"

  let pointerout = Dom.Event.make "pointerout"

  let pointerover = Dom.Event.make "pointerover"

  let pointerup = Dom.Event.make "pointerup"

  let ratechange = Dom.Event.make "ratechange"

  let seeked = Dom.Event.make "seeked"

  let seeking = Dom.Event.make "seeking"

  let stalled = Dom.Event.make "stalled"

  let suspend = Dom.Event.make "suspend"

  let volumechange = Dom.Event.make "volumechange"

  let waiting = Dom.Event.make "waiting"

  let toggle = Dom.Event.make "toggle"

  let make = Dom.Event.make
end

type event_listener_id = Dom.event_listener_id

let addEventListener = Dom.addEventListener

let addEventListenerWithOptions = Dom.addEventListenerWithOptions

let removeEventListener = Dom.removeEventListener

let createCustomEvent = Dom.createCustomEvent

class type ['node] collection = object
  method length : int readonly_prop

  method item : int -> 'node t opt meth

  method namedItem : js_string t -> 'node t opt meth
end

class type htmlElement = element

class type headElement = object
  inherit element

  method profile : js_string t prop
end

class type linkElement = object
  inherit element

  method disabled : bool t prop

  method charset : js_string t prop

  method crossorigin : js_string t prop

  method href : js_string t prop

  method hreflang : js_string t prop

  method media : js_string t prop

  method rel : js_string t prop

  method rev : js_string t prop

  method target : js_string t prop

  method _type : js_string t prop
end

class type titleElement = object
  inherit element

  method text : js_string t prop
end

class type metaElement = object
  inherit element

  method content : js_string t prop

  method httpEquiv : js_string t prop

  method name : js_string t prop

  method scheme : js_string t prop
end

class type baseElement = object
  inherit element

  method href : js_string t prop

  method target : js_string t prop
end

class type styleElement = object
  inherit element

  method disabled : bool t prop

  method media : js_string t prop

  method _type : js_string t prop
end

class type bodyElement = element

class type formElement = object
  inherit element

  method elements : element collection t readonly_prop

  method length : int readonly_prop

  method acceptCharset : js_string t prop

  method action : js_string t prop

  method enctype : js_string t prop

  method _method : js_string t prop

  method target : js_string t prop

  method submit : unit meth

  method reset : unit meth

  method onsubmit : ('self t, submitEvent t) event_listener writeonly_prop
end

class type optGroupElement = object
  inherit element

  method disabled : bool t prop

  method label : js_string t prop
end

class type optionElement = object
  inherit optGroupElement

  method form : formElement t opt readonly_prop

  method defaultSelected : bool t prop

  method text : js_string t readonly_prop

  method index : int readonly_prop

  method selected : bool t prop

  method value : js_string t prop
end

class type selectElement = object ('self)
  inherit element

  method _type : js_string t readonly_prop

  method selectedIndex : int prop

  method value : js_string t prop

  method length : int prop

  method form : formElement t opt readonly_prop

  method options : optionElement collection t readonly_prop

  method disabled : bool t prop

  method multiple : bool t prop

  method name : js_string t readonly_prop

  method size : int prop

  method tabIndex : int prop

  method add : #optGroupElement t -> #optGroupElement t opt -> unit meth

  method remove : int -> unit meth

  method required : bool t writeonly_prop

  method onchange : ('self t, event t) event_listener prop

  method oninput : ('self t, event t) event_listener prop
end

class type inputElement = object ('self)
  inherit element

  method defaultValue : js_string t prop

  method defaultChecked : js_string t prop

  method form : formElement t opt readonly_prop

  method accept : js_string t prop

  method accessKey : js_string t prop

  method align : js_string t prop

  method alt : js_string t prop

  method checked : bool t prop

  method disabled : bool t prop

  method maxLength : int prop

  method name : js_string t readonly_prop

  method readOnly : bool t prop

  method required : bool t writeonly_prop

  method size : int prop

  method src : js_string t prop

  method tabIndex : int prop

  method _type : js_string t readonly_prop

  method useMap : js_string t prop

  method value : js_string t prop

  method select : unit meth

  method files : File.fileList t optdef readonly_prop

  method placeholder : js_string t writeonly_prop

  method selectionDirection : js_string t prop

  method selectionStart : int prop

  method selectionEnd : int prop

  method onselect : ('self t, event t) event_listener prop

  method onchange : ('self t, event t) event_listener prop

  method oninput : ('self t, event t) event_listener prop

  method onblur : ('self t, focusEvent t) event_listener prop

  method onfocus : ('self t, focusEvent t) event_listener prop
end

class type textAreaElement = object ('self)
  inherit element

  method defaultValue : js_string t prop

  method form : formElement t opt readonly_prop

  method accessKey : js_string t prop

  method cols : int prop

  method disabled : bool t prop

  method name : js_string t readonly_prop

  method readOnly : bool t prop

  method rows : int prop

  method selectionDirection : js_string t prop

  method selectionEnd : int prop

  method selectionStart : int prop

  method tabIndex : int prop

  method _type : js_string t readonly_prop

  method value : js_string t prop

  method select : unit meth

  method required : bool t writeonly_prop

  method placeholder : js_string t writeonly_prop

  method onselect : ('self t, event t) event_listener prop

  method onchange : ('self t, event t) event_listener prop

  method oninput : ('self t, event t) event_listener prop

  method onblur : ('self t, focusEvent t) event_listener prop

  method onfocus : ('self t, focusEvent t) event_listener prop
end

class type buttonElement = object
  inherit element

  method form : formElement t opt readonly_prop

  method accessKey : js_string t prop

  method disabled : bool t prop

  method name : js_string t readonly_prop

  method tabIndex : int prop

  method _type : js_string t readonly_prop

  method value : js_string t prop
end

class type labelElement = object
  inherit element

  method form : formElement t opt readonly_prop

  method accessKey : js_string t prop

  method htmlFor : js_string t prop
end

class type fieldSetElement = object
  inherit element

  method form : formElement t opt readonly_prop
end

class type legendElement = object
  inherit element

  method form : formElement t opt readonly_prop

  method accessKey : js_string t prop
end

class type uListElement = element

class type oListElement = element

class type dListElement = element

class type liElement = element

class type dialogElement = object
  inherit element

  method close : unit meth

  method close_returnValue : js_string t -> unit meth

  method open_ : bool t prop

  method returnValue : js_string t prop

  method show : unit meth

  method showModal : unit meth

  method oncancel : ('self t, event t) event_listener prop

  method onclose : ('self t, event t) event_listener prop
end

class type divElement = element

class type paragraphElement = element

class type headingElement = element

class type quoteElement = object
  inherit element

  method cite : js_string t prop
end

class type preElement = element

class type brElement = element

class type hrElement = element

class type modElement = object
  inherit element

  method cite : js_string t prop

  method dateTime : js_string t prop
end

class type anchorElement = object
  inherit element

  method accessKey : js_string t prop

  method charset : js_string t prop

  method coords : js_string t prop

  method download : js_string t prop

  method href : js_string t prop

  method hreflang : js_string t prop

  method name : js_string t prop

  method rel : js_string t prop

  method rev : js_string t prop

  method shape : js_string t prop

  method tabIndex : int prop

  method target : js_string t prop

  method _type : js_string t prop
end

class type detailsElement = object ('self)
  inherit element

  method open_ : bool t prop

  method name : js_string t prop

  method ontoggle : ('self t, toggleEvent t) event_listener prop
end

class type imageElement = object ('self)
  inherit element

  method alt : js_string t prop

  method src : js_string t prop

  method useMap : js_string t prop

  method isMap : bool t prop

  method width : int prop

  method height : int prop

  method naturalWidth : int optdef readonly_prop

  method naturalHeight : int optdef readonly_prop

  method complete : bool t prop

  method onload : ('self t, event t) event_listener prop

  method onerror : ('self t, event t) event_listener prop

  method onabort : ('self t, event t) event_listener prop
end

class type objectElement = object
  inherit element

  method form : formElement t opt readonly_prop

  method code : js_string t prop

  method archive : js_string t prop

  method codeBase : js_string t prop

  method codeType : js_string t prop

  method data : js_string t prop

  method declare : bool t prop

  method height : js_string t prop

  method name : js_string t prop

  method standby : js_string t prop

  method tabIndex : int prop

  method _type : js_string t prop

  method useMap : js_string t prop

  method width : js_string t prop

  method document : Dom.element Dom.document t opt readonly_prop
end

class type paramElement = object
  inherit element

  method name : js_string t prop

  method _type : js_string t prop

  method value : js_string t prop

  method valueType : js_string t prop
end

class type areaElement = object
  inherit element

  method accessKey : js_string t prop

  method alt : js_string t prop

  method coords : js_string t prop

  method href : js_string t prop

  method noHref : bool t prop

  method shape : js_string t prop

  method tabIndex : int prop

  method target : js_string t prop
end

class type mapElement = object
  inherit element

  method areas : areaElement collection t readonly_prop

  method name : js_string t prop
end

class type scriptElement = object
  inherit element

  method text : js_string t prop

  method charset : js_string t prop

  method defer : bool t prop

  method src : js_string t prop

  method _type : js_string t prop

  method async : bool t prop
end

class type embedElement = object
  inherit element

  method src : js_string t prop

  method height : js_string t prop

  method width : js_string t prop

  method _type : js_string t prop
end

class type tableCellElement = object
  inherit element

  method cellIndex : int readonly_prop

  method abbr : js_string t prop

  method align : js_string t prop

  method axis : js_string t prop

  method ch : js_string t prop

  method chOff : js_string t prop

  method colSpan : int prop

  method headers : js_string t prop

  method rowSpan : int prop

  method scope : js_string t prop

  method vAlign : js_string t prop
end

class type tableRowElement = object
  inherit element

  method rowIndex : int readonly_prop

  method sectionRowIndex : int readonly_prop

  method cells : tableCellElement collection t readonly_prop

  method align : js_string t prop

  method ch : js_string t prop

  method chOff : js_string t prop

  method vAlign : js_string t prop

  method insertCell : int -> tableCellElement t meth

  method deleteCell : int -> unit meth
end

class type tableColElement = object
  inherit element

  method align : js_string t prop

  method ch : js_string t prop

  method chOff : js_string t prop

  method span : int prop

  method vAlign : js_string t prop

  method width : js_string t prop
end

class type tableSectionElement = object
  inherit element

  method align : js_string t prop

  method ch : js_string t prop

  method chOff : js_string t prop

  method vAlign : js_string t prop

  method rows : tableRowElement collection t readonly_prop

  method insertRow : int -> tableRowElement t meth

  method deleteRow : int -> unit meth
end

class type tableCaptionElement = element

class type tableElement = object
  inherit element

  method caption : tableCaptionElement t prop

  method tHead : tableSectionElement t prop

  method tFoot : tableSectionElement t prop

  method rows : tableRowElement collection t readonly_prop

  method tBodies : tableSectionElement collection t readonly_prop

  method align : js_string t prop

  method border : js_string t prop

  method cellPadding : js_string t prop

  method cellSpacing : js_string t prop

  method frame : js_string t prop

  method rules : js_string t prop

  method summary : js_string t prop

  method width : js_string t prop

  method createTHead : tableSectionElement t meth

  method deleteTHead : unit meth

  method createTFoot : tableSectionElement t meth

  method deleteTFoot : unit meth

  method createCaption : tableCaptionElement t meth

  method deleteCaption : unit meth

  method insertRow : int -> tableRowElement t meth

  method deleteRow : int -> unit meth
end

class type timeRanges = object
  method length : int readonly_prop

  method start : int -> number_t meth

  method end_ : int -> number_t meth
end

type networkState =
  | NETWORK_EMPTY
  | NETWORK_IDLE
  | NETWORK_LOADING
  | NETWORK_NO_SOURCE

type readyState =
  | HAVE_NOTHING
  | HAVE_METADATA
  | HAVE_CURRENT_DATA
  | HAVE_FUTURE_DATA
  | HAVE_ENOUGH_DATA

(* http://www.w3schools.com/tags/ref_av_dom.asp *)
(* only features supported by all browser. (IE9+) *)
class type mediaElement = object
  inherit element

  method canPlayType : js_string t -> js_string t meth

  method load : unit meth

  method play : unit meth

  method pause : unit meth

  method autoplay : bool t prop

  method buffered : timeRanges t readonly_prop

  method controls : bool t prop

  method currentSrc : js_string t readonly_prop

  method currentTime : number_t prop

  method duration : number_t readonly_prop

  method ended : bool t readonly_prop

  method loop : bool t prop

  method mediagroup : js_string t prop

  method muted : bool t prop

  method networkState_int : int readonly_prop

  method networkState : networkState readonly_prop

  method paused : bool t readonly_prop

  method playbackRate : number_t prop

  method played : timeRanges t readonly_prop

  method preload : js_string t prop

  method readyState_int : int readonly_prop

  method readyState : readyState readonly_prop

  method seekable : timeRanges t readonly_prop

  method seeking : bool t readonly_prop

  method src : js_string t prop

  method volume : number_t prop

  method oncanplay : ('self t, mediaEvent t) event_listener writeonly_prop

  method oncanplaythrough : ('self t, mediaEvent t) event_listener writeonly_prop

  method ondurationchange : ('self t, mediaEvent t) event_listener writeonly_prop

  method onemptied : ('self t, mediaEvent t) event_listener writeonly_prop

  method onended : ('self t, mediaEvent t) event_listener writeonly_prop

  method onloadeddata : ('self t, mediaEvent t) event_listener writeonly_prop

  method onloadedmetadata : ('self t, mediaEvent t) event_listener writeonly_prop

  method onloadstart : ('self t, mediaEvent t) event_listener writeonly_prop

  method onpause : ('self t, mediaEvent t) event_listener writeonly_prop

  method onplay : ('self t, mediaEvent t) event_listener writeonly_prop

  method onplaying : ('self t, mediaEvent t) event_listener writeonly_prop

  method onratechange : ('self t, mediaEvent t) event_listener writeonly_prop

  method onseeked : ('self t, mediaEvent t) event_listener writeonly_prop

  method onseeking : ('self t, mediaEvent t) event_listener writeonly_prop

  method onstalled : ('self t, mediaEvent t) event_listener writeonly_prop

  method onsuspend : ('self t, mediaEvent t) event_listener writeonly_prop

  method onvolumechange : ('self t, mediaEvent t) event_listener writeonly_prop

  method onwaiting : ('self t, mediaEvent t) event_listener writeonly_prop
end

class type audioElement = object
  inherit mediaElement
end

class type videoElement = object
  inherit mediaElement
end

type context = js_string t

let _2d_ = Js.string "2d"

type canvasPattern

class type canvasElement = object
  inherit element

  method width : int prop

  method height : int prop

  method toDataURL : js_string t meth

  method toDataURL_type : js_string t -> js_string t meth

  method toDataURL_type_compression : js_string t -> number_t -> js_string t meth

  method getContext : js_string t -> canvasRenderingContext2D t meth
end

and canvasRenderingContext2D = object
  method canvas : canvasElement t readonly_prop

  method save : unit meth

  method restore : unit meth

  method scale : number_t -> number_t -> unit meth

  method rotate : number_t -> unit meth

  method translate : number_t -> number_t -> unit meth

  method transform :
    number_t -> number_t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method setTransform :
    number_t -> number_t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method globalAlpha : number_t prop

  method globalCompositeOperation : js_string t prop

  method strokeStyle : js_string t writeonly_prop

  method strokeStyle_gradient : canvasGradient t writeonly_prop

  method strokeStyle_pattern : canvasPattern t writeonly_prop

  method fillStyle : js_string t writeonly_prop

  method fillStyle_gradient : canvasGradient t writeonly_prop

  method fillStyle_pattern : canvasPattern t writeonly_prop

  method createLinearGradient :
    number_t -> number_t -> number_t -> number_t -> canvasGradient t meth

  method createRadialGradient :
       number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> canvasGradient t meth

  method createPattern : imageElement t -> js_string t -> canvasPattern t meth

  method createPattern_fromCanvas : canvasElement t -> js_string t -> canvasPattern t meth

  method createPattern_fromVideo : videoElement t -> js_string t -> canvasPattern t meth

  method lineWidth : number_t prop

  method lineCap : js_string t prop

  method lineJoin : js_string t prop

  method miterLimit : number_t prop

  method shadowOffsetX : number_t prop

  method shadowOffsetY : number_t prop

  method shadowBlur : number_t prop

  method shadowColor : js_string t prop

  method clearRect : number_t -> number_t -> number_t -> number_t -> unit meth

  method fillRect : number_t -> number_t -> number_t -> number_t -> unit meth

  method strokeRect : number_t -> number_t -> number_t -> number_t -> unit meth

  method beginPath : unit meth

  method closePath : unit meth

  method moveTo : number_t -> number_t -> unit meth

  method lineTo : number_t -> number_t -> unit meth

  method quadraticCurveTo : number_t -> number_t -> number_t -> number_t -> unit meth

  method bezierCurveTo :
    number_t -> number_t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method arcTo : number_t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method rect : number_t -> number_t -> number_t -> number_t -> unit meth

  method arc :
    number_t -> number_t -> number_t -> number_t -> number_t -> bool t -> unit meth

  method ellipse :
       number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> bool t
    -> unit meth

  method fill : unit meth

  method stroke : unit meth

  method clip : unit meth

  method isPointInPath : number_t -> number_t -> bool t meth

  method drawFocusRing : #element t -> number_t -> number_t -> bool t -> bool t meth

  method font : js_string t prop

  method textAlign : js_string t prop

  method textBaseline : js_string t prop

  method fillText : js_string t -> number_t -> number_t -> unit meth

  method fillText_withWidth : js_string t -> number_t -> number_t -> number_t -> unit meth

  method strokeText : js_string t -> number_t -> number_t -> unit meth

  method strokeText_withWidth :
    js_string t -> number_t -> number_t -> number_t -> unit meth

  method measureText : js_string t -> textMetrics t meth

  method drawImage : imageElement t -> number_t -> number_t -> unit meth

  method drawImage_withSize :
    imageElement t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method drawImage_full :
       imageElement t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> unit meth

  method drawImage_fromCanvas : canvasElement t -> number_t -> number_t -> unit meth

  method drawImage_fromCanvasWithSize :
    canvasElement t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method drawImage_fullFromCanvas :
       canvasElement t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> unit meth

  method drawImage_fromVideoWithVideo :
    videoElement t -> number_t -> number_t -> unit meth

  method drawImage_fromVideoWithSize :
    videoElement t -> number_t -> number_t -> number_t -> number_t -> unit meth

  method drawImage_fullFromVideo :
       videoElement t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> unit meth

  method createImageData : int -> int -> imageData t meth

  method getImageData : number_t -> number_t -> number_t -> number_t -> imageData t meth

  method putImageData : imageData t -> number_t -> number_t -> unit meth
end

and canvasGradient = object
  method addColorStop : number_t -> js_string t -> unit meth
end

and textMetrics = object
  method width : number_t readonly_prop
end

and imageData = object
  method width : int readonly_prop

  method height : int readonly_prop

  method data : canvasPixelArray t readonly_prop
end

and canvasPixelArray = object
  method length : int readonly_prop
end

external pixel_get : canvasPixelArray t -> int -> int = "caml_js_get"

external pixel_set : canvasPixelArray t -> int -> int -> unit = "caml_js_set"

class type range = object
  method collapsed : bool t readonly_prop

  method startOffset : int readonly_prop

  method endOffset : int readonly_prop

  method startContainer : Dom.node t readonly_prop

  method endContainer : Dom.node t readonly_prop

  method setStart : Dom.node t -> int -> unit meth

  method setEnd : Dom.node t -> int -> unit meth

  method setStartBefore : Dom.node t -> unit meth

  method setEndBefore : Dom.node t -> unit meth

  method setStartAfter : Dom.node t -> unit meth

  method setEndAfter : Dom.node t -> unit meth

  method selectNode : Dom.node t -> unit meth

  method selectNodeContents : Dom.node t -> unit meth

  method collapse : bool t -> unit meth

  method cloneContents : Dom.documentFragment t meth

  method extractContents : Dom.documentFragment t meth

  method deleteContents : unit meth

  method insertNode : Dom.node t -> unit meth

  method surroundContents : Dom.node t -> unit meth

  method cloneRange : range t meth

  method toString : js_string t meth
end

(** Information on current selection *)
class type selection = object
  method anchorNode : Dom.node t readonly_prop

  method anchorOffset : int readonly_prop

  method focusNode : Dom.node t readonly_prop

  method focusOffset : int readonly_prop

  method isCollapsed : bool t readonly_prop

  method rangeCount : int readonly_prop

  method getRangeAt : int -> range t meth

  method collapse : bool t -> unit meth

  method extend : Dom.node t -> int -> unit meth

  method modify : js_string t -> js_string t -> js_string t -> unit meth

  method collapseToStart : unit meth

  method collapseToEnd : unit meth

  method selectAllChildren : Dom.node t -> unit meth

  method addRange : range t -> unit meth

  method removeRange : range t -> unit meth

  method removeAllRanges : unit meth

  method deleteFromDocument : unit meth

  method containsNode : Dom.node t -> bool t -> bool t meth

  method toString : js_string t meth
end

class type document = object
  inherit [element] Dom.document

  inherit nodeSelector

  inherit eventTarget

  method title : js_string t prop

  method referrer : js_string t readonly_prop

  method domain : js_string t prop

  method _URL : js_string t readonly_prop

  method head : headElement t prop

  method body : bodyElement t prop

  method documentElement : htmlElement t readonly_prop

  method images : imageElement collection t readonly_prop

  method applets : element collection t readonly_prop

  method links : element collection t readonly_prop

  method forms : formElement collection t readonly_prop

  method anchors : element collection t readonly_prop

  method cookie : js_string t prop

  method designMode : js_string t prop

  method open_ : unit meth

  method close : unit meth

  method write : js_string t -> unit meth

  method execCommand : js_string t -> bool t -> js_string t opt -> unit meth

  method createRange : range t meth

  method readyState : js_string t readonly_prop

  method getElementsByClassName : js_string t -> element Dom.nodeList t meth

  method getElementsByName : js_string t -> element Dom.nodeList t meth

  method activeElement : element t opt readonly_prop

  method hidden : bool t readonly_prop

  method onfullscreenchange : (document t, event t) event_listener writeonly_prop

  method onwebkitfullscreenchange : (document t, event t) event_listener writeonly_prop

  inherit eventTarget
end

type interval_id

type timeout_id

type animation_frame_request_id

class type location = object
  method href : js_string t prop

  method protocol : js_string t prop

  method host : js_string t prop

  method hostname : js_string t prop

  method origin : js_string t optdef readonly_prop

  method port : js_string t prop

  method pathname : js_string t prop

  method search : js_string t prop

  method hash : js_string t prop

  method assign : js_string t -> unit meth

  method replace : js_string t -> unit meth

  method reload : unit meth
end

let location_origin (loc : location t) =
  Optdef.case
    loc##.origin
    (fun () ->
      let protocol = loc##.protocol in
      let hostname = loc##.hostname in
      let port = loc##.port in
      if protocol##.length = 0 && hostname##.length = 0
      then Js.string ""
      else
        let origin = protocol##concat_2 (Js.string "//") hostname in
        if port##.length > 0 then origin##concat_2 (Js.string ":") loc##.port else origin)
    (fun o -> o)

class type history = object
  method length : int readonly_prop

  method state : Js.Unsafe.any readonly_prop

  method go : int opt -> unit meth

  method back : unit meth

  method forward : unit meth

  method pushState : 'a. 'a -> js_string t -> js_string t opt -> unit meth

  method replaceState : 'a. 'a -> js_string t -> js_string t opt -> unit meth
end

class type undoManager = object end

class type navigator = object
  method appCodeName : js_string t readonly_prop

  method appName : js_string t readonly_prop

  method appVersion : js_string t readonly_prop

  method cookieEnabled : bool t readonly_prop

  method onLine : bool t readonly_prop

  method platform : js_string t readonly_prop

  method vendor : js_string t readonly_prop

  method userAgent : js_string t readonly_prop

  method language : js_string t optdef readonly_prop

  method userLanguage : js_string t optdef readonly_prop

  method maxTouchPoints : int readonly_prop
end

class type screen = object
  method width : int readonly_prop

  method height : int readonly_prop

  method availWidth : int readonly_prop

  method availHeight : int readonly_prop
end

class type applicationCache = object
  method status : int readonly_prop

  method update : unit meth

  method abort : unit meth

  method swapCache : unit meth

  method onchecking : (applicationCache t, event t) event_listener prop

  method onerror : (applicationCache t, event t) event_listener prop

  method onnoupdate : (applicationCache t, event t) event_listener prop

  method ondownloading : (applicationCache t, event t) event_listener prop

  method onprogress : (applicationCache t, event t) event_listener prop

  method onupdateready : (applicationCache t, event t) event_listener prop

  method oncached : (applicationCache t, event t) event_listener prop

  method onobsolete : (applicationCache t, event t) event_listener prop

  inherit eventTarget
end

class type _URL = object
  method createObjectURL : #File.blob t -> js_string t meth

  method revokeObjectURL : js_string t -> unit meth
end

class type window = object
  inherit eventTarget

  method document : document t readonly_prop

  method applicationCache : applicationCache t readonly_prop

  method name : js_string t prop

  method location : location t readonly_prop

  method history : history t readonly_prop

  method undoManager : undoManager t readonly_prop

  method navigator : navigator t readonly_prop

  method getSelection : selection t meth

  method close : unit meth

  method closed : bool t readonly_prop

  method stop : unit meth

  method focus : unit meth

  method blur : unit meth

  method scrollX : number_t readonly_prop

  method scrollY : number_t readonly_prop

  method scroll : number_t -> number_t -> unit meth

  method scrollTo : number_t -> number_t -> unit meth

  method scrollBy : number_t -> number_t -> unit meth

  method sessionStorage : storage t optdef readonly_prop

  method localStorage : storage t optdef readonly_prop

  method top : window t readonly_prop

  method parent : window t readonly_prop

  method frameElement : element t opt readonly_prop

  method open_ : js_string t -> js_string t -> js_string t opt -> window t opt meth

  method alert : js_string t -> unit meth

  method confirm : js_string t -> bool t meth

  method prompt : js_string t -> js_string t -> js_string t opt meth

  method print : unit meth

  method setInterval : (unit -> unit) Js.callback -> number_t -> interval_id meth

  method clearInterval : interval_id -> unit meth

  method setTimeout : (unit -> unit) Js.callback -> number_t -> timeout_id meth

  method clearTimeout : timeout_id -> unit meth

  method requestAnimationFrame :
    (number_t -> unit) Js.callback -> animation_frame_request_id meth

  method cancelAnimationFrame : animation_frame_request_id -> unit meth

  method screen : screen t readonly_prop

  method innerWidth : int readonly_prop

  method innerHeight : int readonly_prop

  method outerWidth : int readonly_prop

  method outerHeight : int readonly_prop

  method getComputedStyle : #element t -> cssStyleDeclaration t meth

  method getComputedStyle_pseudoElt :
    #element t -> js_string t -> cssStyleDeclaration t meth

  method atob : js_string t -> js_string t meth

  method btoa : js_string t -> js_string t meth

  method onload : (window t, event t) event_listener prop

  method onunload : (window t, event t) event_listener prop

  method onbeforeunload : (window t, event t) event_listener prop

  method onblur : (window t, focusEvent t) event_listener prop

  method onfocus : (window t, focusEvent t) event_listener prop

  method onresize : (window t, event t) event_listener prop

  method onorientationchange : (window t, event t) event_listener prop

  method onpopstate : (window t, popStateEvent t) event_listener prop

  method onhashchange : (window t, hashChangeEvent t) event_listener prop

  method ononline : (window t, event t) event_listener writeonly_prop

  method onoffline : (window t, event t) event_listener writeonly_prop

  method _URL : _URL t readonly_prop

  method devicePixelRatio : number_t readonly_prop
end

let window : window t = Js.Unsafe.global

(* The toplevel object *)

let document = window##.document

let getElementById id =
  Js.Opt.case
    (document##getElementById (Js.string id))
    (fun () -> raise Not_found)
    (fun pnode -> pnode)

let getElementById_exn id =
  Js.Opt.case
    (document##getElementById (Js.string id))
    (fun () -> failwith (Printf.sprintf "getElementById_exn: %S not found" id))
    (fun pnode -> pnode)

let getElementById_opt id = Js.Opt.to_option (document##getElementById (Js.string id))

let getElementById_coerce id coerce =
  Js.Opt.case
    (document##getElementById (Js.string id))
    (fun () -> None)
    (fun e -> Js.Opt.to_option (coerce e))

(****)

class type frameSetElement = object
  inherit element

  method cols : js_string t prop

  method rows : js_string t prop
end

class type frameElement = object
  inherit element

  method frameBorder : js_string t prop

  method longDesc : js_string t prop

  method marginHeight : js_string t prop

  method marginWidth : js_string t prop

  method name : js_string t prop

  method noResize : bool t prop

  method scrolling : js_string t prop

  method src : js_string t prop

  method contentDocument : document t opt readonly_prop
end

class type iFrameElement = object
  inherit element

  method frameBorder : js_string t prop

  method height : js_string t prop

  method width : js_string t prop

  method longDesc : js_string t prop

  method marginHeight : js_string t prop

  method marginWidth : js_string t prop

  method name : js_string t prop

  method scrolling : js_string t prop

  method src : js_string t prop

  method contentDocument : document t opt readonly_prop

  method contentWindow : window t readonly_prop
end

(****)

(*XXX Should provide creation functions a la lablgtk... *)

let opt_iter x f =
  match x with
  | None -> ()
  | Some v -> f v

let createElement (doc : document t) name = doc##createElement (Js.string name)

let unsafeCreateElement doc name = Js.Unsafe.coerce (createElement doc name)

let createElementSyntax = ref `Unknown

let rec unsafeCreateElementEx ?_type ?name doc elt =
  if Poly.(_type = None) && Poly.(name = None)
  then Js.Unsafe.coerce (createElement doc elt)
  else
    match !createElementSyntax with
    | `Standard ->
        let res = Js.Unsafe.coerce (createElement doc elt) in
        opt_iter _type (fun t -> res##._type := t);
        opt_iter name (fun n -> res##.name := n);
        res
    | `Extended ->
        let a = new%js Js.array_empty in
        ignore (a##push_2 (Js.string "<") (Js.string elt));
        opt_iter _type (fun t ->
            ignore (a##push_3 (Js.string " type=\"") (html_escape t) (Js.string "\"")));
        opt_iter name (fun n ->
            ignore (a##push_3 (Js.string " name=\"") (html_escape n) (Js.string "\"")));
        ignore (a##push (Js.string ">"));
        Js.Unsafe.coerce (doc##createElement (a##join (Js.string "")))
    | `Unknown ->
        createElementSyntax :=
          if
            try
              let el : inputElement Js.t =
                Js.Unsafe.coerce
                  (document##createElement (Js.string "<input name=\"x\">"))
              in
              Js.equals el##.tagName##toLowerCase (Js.string "input")
              && Js.equals el##.name (Js.string "x")
            with _ -> false
          then `Extended
          else `Standard;
        unsafeCreateElementEx ?_type ?name doc elt

let createHtml doc : htmlElement t = unsafeCreateElement doc "html"

let createHead doc : headElement t = unsafeCreateElement doc "head"

let createLink doc : linkElement t = unsafeCreateElement doc "link"

let createTitle doc : titleElement t = unsafeCreateElement doc "title"

let createMeta doc : metaElement t = unsafeCreateElement doc "meta"

let createBase doc : baseElement t = unsafeCreateElement doc "base"

let createStyle doc : styleElement t = unsafeCreateElement doc "style"

let createBody doc : bodyElement t = unsafeCreateElement doc "body"

let createForm doc : formElement t = unsafeCreateElement doc "form"

let createOptgroup doc : optGroupElement t = unsafeCreateElement doc "optgroup"

let createOption doc : optionElement t = unsafeCreateElement doc "option"

let createSelect ?_type ?name doc : selectElement t =
  unsafeCreateElementEx ?_type ?name doc "select"

let createInput ?_type ?name doc : inputElement t =
  unsafeCreateElementEx ?_type ?name doc "input"

let createTextarea ?_type ?name doc : textAreaElement t =
  unsafeCreateElementEx ?_type ?name doc "textarea"

let createButton ?_type ?name doc : buttonElement t =
  unsafeCreateElementEx ?_type ?name doc "button"

let createLabel doc : labelElement t = unsafeCreateElement doc "label"

let createFieldset doc : fieldSetElement t = unsafeCreateElement doc "fieldset"

let createLegend doc : legendElement t = unsafeCreateElement doc "legend"

let createUl doc : uListElement t = unsafeCreateElement doc "ul"

let createOl doc : oListElement t = unsafeCreateElement doc "ol"

let createDl doc : dListElement t = unsafeCreateElement doc "dl"

let createLi doc : liElement t = unsafeCreateElement doc "li"

let createDialog doc : dialogElement t = unsafeCreateElement doc "dialog"

let createDiv doc : divElement t = unsafeCreateElement doc "div"

let createEmbed doc : embedElement t = unsafeCreateElement doc "embed"

let createP doc : paragraphElement t = unsafeCreateElement doc "p"

let createH1 doc : headingElement t = unsafeCreateElement doc "h1"

let createH2 doc : headingElement t = unsafeCreateElement doc "h2"

let createH3 doc : headingElement t = unsafeCreateElement doc "h3"

let createH4 doc : headingElement t = unsafeCreateElement doc "h4"

let createH5 doc : headingElement t = unsafeCreateElement doc "h5"

let createH6 doc : headingElement t = unsafeCreateElement doc "h6"

let createQ doc : quoteElement t = unsafeCreateElement doc "q"

let createBlockquote doc : quoteElement t = unsafeCreateElement doc "blockquote"

let createPre doc : preElement t = unsafeCreateElement doc "pre"

let createBr doc : brElement t = unsafeCreateElement doc "br"

let createHr doc : hrElement t = unsafeCreateElement doc "hr"

let createIns doc : modElement t = unsafeCreateElement doc "ins"

let createDel doc : modElement t = unsafeCreateElement doc "del"

let createA doc : anchorElement t = unsafeCreateElement doc "a"

let createImg doc : imageElement t = unsafeCreateElement doc "img"

let createObject doc : objectElement t = unsafeCreateElement doc "object"

let createParam doc : paramElement t = unsafeCreateElement doc "param"

let createMap doc : mapElement t = unsafeCreateElement doc "map"

let createArea doc : areaElement t = unsafeCreateElement doc "area"

let createScript doc : scriptElement t = unsafeCreateElement doc "script"

let createTable doc : tableElement t = unsafeCreateElement doc "table"

let createCaption doc : tableCaptionElement t = unsafeCreateElement doc "caption"

let createCol doc : tableColElement t = unsafeCreateElement doc "col"

let createColgroup doc : tableColElement t = unsafeCreateElement doc "colgroup"

let createThead doc : tableSectionElement t = unsafeCreateElement doc "thead"

let createTfoot doc : tableSectionElement t = unsafeCreateElement doc "tfoot"

let createTbody doc : tableSectionElement t = unsafeCreateElement doc "tbody"

let createTr doc : tableRowElement t = unsafeCreateElement doc "tr"

let createTh doc : tableCellElement t = unsafeCreateElement doc "th"

let createTd doc : tableCellElement t = unsafeCreateElement doc "td"

let createSub doc = createElement doc "sub"

let createSup doc = createElement doc "sup"

let createSpan doc = createElement doc "span"

let createTt doc = createElement doc "tt"

let createI doc = createElement doc "i"

let createB doc = createElement doc "b"

let createBig doc = createElement doc "big"

let createSmall doc = createElement doc "small"

let createEm doc = createElement doc "em"

let createStrong doc = createElement doc "strong"

let createCite doc = createElement doc "cite"

let createDfn doc = createElement doc "dfn"

let createCode doc = createElement doc "code"

let createSamp doc = createElement doc "samp"

let createKbd doc = createElement doc "kbd"

let createVar doc = createElement doc "var"

let createAbbr doc = createElement doc "abbr"

let createDd doc = createElement doc "dd"

let createDt doc = createElement doc "dt"

let createNoscript doc = createElement doc "noscript"

let createAddress doc = createElement doc "address"

let createFrameset doc : frameSetElement t = unsafeCreateElement doc "frameset"

let createFrame doc : frameElement t = unsafeCreateElement doc "frame"

let createIframe doc : iFrameElement t = unsafeCreateElement doc "iframe"

let createAudio doc : audioElement t = unsafeCreateElement doc "audio"

let createVideo doc : audioElement t = unsafeCreateElement doc "video"

exception Canvas_not_available

let createCanvas doc : canvasElement t =
  let c = unsafeCreateElement doc "canvas" in
  if not (Opt.test c##.getContext) then raise Canvas_not_available;
  c

let html_element : htmlElement t constr = Js.Unsafe.global##._HTMLElement

module CoerceTo = struct
  let element : #Dom.node Js.t -> element Js.t Js.opt =
    if not (Js.Optdef.test (def html_element))
    then
      (* ie < 9 does not have HTMLElement: we have to cheat to check
         that something is an html element *)
      fun e ->
        if not (Js.Optdef.test (def (Js.Unsafe.coerce e)##.innerHTML))
        then Js.null
        else Js.some (Js.Unsafe.coerce e)
    else
      fun e ->
        if Js.instanceof e html_element then Js.some (Js.Unsafe.coerce e) else Js.null

  let unsafeCoerce tag (e : #element t) =
    if Js.equals e##.tagName##toLowerCase (Js.string tag)
    then Js.some (Js.Unsafe.coerce e)
    else Js.null

  let a e = unsafeCoerce "a" e

  let area e = unsafeCoerce "area" e

  let base e = unsafeCoerce "base" e

  let blockquote e = unsafeCoerce "blockquote" e

  let body e = unsafeCoerce "body" e

  let br e = unsafeCoerce "br" e

  let button e = unsafeCoerce "button" e

  let canvas e = unsafeCoerce "canvas" e

  let caption e = unsafeCoerce "caption" e

  let col e = unsafeCoerce "col" e

  let colgroup e = unsafeCoerce "colgroup" e

  let del e = unsafeCoerce "del" e

  let details e = unsafeCoerce "details" e

  let div e = unsafeCoerce "div" e

  let dl e = unsafeCoerce "dl" e

  let fieldset e = unsafeCoerce "fieldset" e

  let embed e = unsafeCoerce "embed" e

  let form e = unsafeCoerce "form" e

  let frameset e = unsafeCoerce "frameset" e

  let frame e = unsafeCoerce "frame" e

  let h1 e = unsafeCoerce "h1" e

  let h2 e = unsafeCoerce "h2" e

  let h3 e = unsafeCoerce "h3" e

  let h4 e = unsafeCoerce "h4" e

  let h5 e = unsafeCoerce "h5" e

  let h6 e = unsafeCoerce "h6" e

  let head e = unsafeCoerce "head" e

  let hr e = unsafeCoerce "hr" e

  let html e = unsafeCoerce "html" e

  let iframe e = unsafeCoerce "iframe" e

  let img e = unsafeCoerce "img" e

  let input e = unsafeCoerce "input" e

  let ins e = unsafeCoerce "ins" e

  let label e = unsafeCoerce "label" e

  let legend e = unsafeCoerce "legend" e

  let li e = unsafeCoerce "li" e

  let link e = unsafeCoerce "link" e

  let map e = unsafeCoerce "map" e

  let meta e = unsafeCoerce "meta" e

  let _object e = unsafeCoerce "object" e

  let ol e = unsafeCoerce "ol" e

  let optgroup e = unsafeCoerce "optgroup" e

  let option e = unsafeCoerce "option" e

  let p e = unsafeCoerce "p" e

  let param e = unsafeCoerce "param" e

  let pre e = unsafeCoerce "pre" e

  let q e = unsafeCoerce "q" e

  let script e = unsafeCoerce "script" e

  let select e = unsafeCoerce "select" e

  let style e = unsafeCoerce "style" e

  let table e = unsafeCoerce "table" e

  let tbody e = unsafeCoerce "tbody" e

  let td e = unsafeCoerce "td" e

  let textarea e = unsafeCoerce "textarea" e

  let tfoot e = unsafeCoerce "tfoot" e

  let th e = unsafeCoerce "th" e

  let thead e = unsafeCoerce "thead" e

  let title e = unsafeCoerce "title" e

  let tr e = unsafeCoerce "tr" e

  let ul e = unsafeCoerce "ul" e

  let audio e = unsafeCoerce "audio" e

  let video e = unsafeCoerce "video" e

  let unsafeCoerceEvent constr (ev : #event t) =
    if Js.Optdef.test (def constr) && Js.instanceof ev constr
    then Js.some (Js.Unsafe.coerce ev)
    else Js.null

  let mouseEvent ev = unsafeCoerceEvent Js.Unsafe.global##._MouseEvent ev

  let keyboardEvent ev = unsafeCoerceEvent Js.Unsafe.global##._KeyboardEvent ev

  let wheelEvent ev = unsafeCoerceEvent Js.Unsafe.global##._WheelEvent ev

  let mouseScrollEvent ev = unsafeCoerceEvent Js.Unsafe.global##._MouseScrollEvent ev

  let popStateEvent ev = unsafeCoerceEvent Js.Unsafe.global##._PopStateEvent ev

  let messageEvent ev = unsafeCoerceEvent Js.Unsafe.global##._MessageEvent ev
end

(****)

let eventTarget = Dom.eventTarget

let eventRelatedTarget (e : #mouseEvent t) =
  Optdef.get e##.relatedTarget (fun () ->
      match Js.to_string e##._type with
      | "mouseover" -> Optdef.get e##.fromElement (fun () -> assert false)
      | "mouseout" -> Optdef.get e##.toElement (fun () -> assert false)
      | _ -> Js.null)

let eventAbsolutePosition' (e : #mouseEvent t) =
  let body = document##.body in
  let html = document##.documentElement in
  ( Js.to_float e##.clientX
    +. Js.to_float body##.scrollLeft
    +. Js.to_float html##.scrollLeft
  , Js.to_float e##.clientY
    +. Js.to_float body##.scrollTop
    +. Js.to_float html##.scrollTop )

let eventAbsolutePosition (e : #mouseEvent t) =
  Optdef.case
    e##.pageX
    (fun () -> eventAbsolutePosition' e)
    (fun x ->
      Optdef.case
        e##.pageY
        (fun () -> eventAbsolutePosition' e)
        (fun y -> Js.to_float x, Js.to_float y))

let elementClientPosition (e : #element t) =
  let r = e##getBoundingClientRect in
  let body = document##.body in
  let html = document##.documentElement in
  ( truncate (Js.to_float r##.left) - body##.clientLeft - html##.clientLeft
  , truncate (Js.to_float r##.top) - body##.clientTop - html##.clientTop )

let getDocumentScroll () =
  let body = document##.body in
  let html = document##.documentElement in
  ( Js.to_float body##.scrollLeft +. Js.to_float html##.scrollLeft
  , Js.to_float body##.scrollTop +. Js.to_float html##.scrollTop )

let buttonPressed (ev : #mouseEvent Js.t) =
  Js.Optdef.case
    ev##.which
    (fun () ->
      match ev##.button with
      | 1 -> Left_button
      | 2 -> Right_button
      | 4 -> Middle_button
      | _ -> No_button)
    (fun x -> x)

let addMousewheelEventListenerWithOptions e ?capture ?once ?passive h =
  addEventListenerWithOptions
    ?capture
    ?once
    ?passive
    e
    Event.wheel
    (handler (fun (e : mousewheelEvent t) ->
         let dx = -Optdef.get e##.wheelDeltaX (fun () -> 0) / 40 in
         let dy = -Optdef.get e##.wheelDeltaY (fun () -> e##.wheelDelta) / 40 in
         h (e :> mouseEvent t) ~dx ~dy))

let addMousewheelEventListener e h capt =
  addMousewheelEventListenerWithOptions ~capture:capt e h

(*****)

module Keyboard_code = struct
  type t =
    | Unidentified
    (* Alphabetic Characters *)
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    (* Digits *)
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | Minus
    | Equal
    (* Whitespace *)
    | Tab
    | Enter
    | Space
    (* Editing *)
    | Escape
    | Backspace
    | Insert
    | Delete
    | CapsLock
    (* Misc Printable *)
    | BracketLeft
    | BracketRight
    | Semicolon
    | Quote
    | Backquote
    | Backslash
    | Comma
    | Period
    | Slash
    (* Function keys *)
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    (* Numpad keys *)
    | Numpad0
    | Numpad1
    | Numpad2
    | Numpad3
    | Numpad4
    | Numpad5
    | Numpad6
    | Numpad7
    | Numpad8
    | Numpad9
    | NumpadMultiply
    | NumpadSubtract
    | NumpadAdd
    | NumpadDecimal
    | NumpadEqual
    | NumpadEnter
    | NumpadDivide
    | NumLock
    (* Modifier keys *)
    | ControlLeft
    | ControlRight
    | MetaLeft
    | MetaRight
    | ShiftLeft
    | ShiftRight
    | AltLeft
    | AltRight
    (* Arrow keys *)
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    (* Navigation *)
    | PageUp
    | PageDown
    | Home
    | End
    (* Sound *)
    | VolumeMute
    | VolumeDown
    | VolumeUp
    (* Media *)
    | MediaTrackPrevious
    | MediaTrackNext
    | MediaPlayPause
    | MediaStop
    (* Browser special *)
    | ContextMenu
    | BrowserSearch
    | BrowserHome
    | BrowserFavorites
    | BrowserRefresh
    | BrowserStop
    | BrowserForward
    | BrowserBack
    (* Misc *)
    | OSLeft
    | OSRight
    | ScrollLock
    | PrintScreen
    | IntlBackslash
    | IntlYen
    | Pause

  let try_code v =
    match Js.to_string v with
    (* Alphabetic Characters *)
    | "KeyA" -> KeyA
    | "KeyB" -> KeyB
    | "KeyC" -> KeyC
    | "KeyD" -> KeyD
    | "KeyE" -> KeyE
    | "KeyF" -> KeyF
    | "KeyG" -> KeyG
    | "KeyH" -> KeyH
    | "KeyI" -> KeyI
    | "KeyJ" -> KeyJ
    | "KeyK" -> KeyK
    | "KeyL" -> KeyL
    | "KeyM" -> KeyM
    | "KeyN" -> KeyN
    | "KeyO" -> KeyO
    | "KeyP" -> KeyP
    | "KeyQ" -> KeyQ
    | "KeyR" -> KeyR
    | "KeyS" -> KeyS
    | "KeyT" -> KeyT
    | "KeyU" -> KeyU
    | "KeyV" -> KeyV
    | "KeyW" -> KeyW
    | "KeyX" -> KeyX
    | "KeyY" -> KeyY
    | "KeyZ" -> KeyZ
    (* Digits *)
    | "Digit0" -> Digit0
    | "Digit1" -> Digit1
    | "Digit2" -> Digit2
    | "Digit3" -> Digit3
    | "Digit4" -> Digit4
    | "Digit5" -> Digit5
    | "Digit6" -> Digit6
    | "Digit7" -> Digit7
    | "Digit8" -> Digit8
    | "Digit9" -> Digit9
    | "Minus" -> Minus
    | "Equal" -> Equal
    (* Whitespace *)
    | "Tab" -> Tab
    | "Enter" -> Enter
    | "Space" -> Space
    (* Editing *)
    | "Escape" -> Escape
    | "Backspace" -> Backspace
    | "Insert" -> Insert
    | "Delete" -> Delete
    | "CapsLock" -> CapsLock
    (* Misc Printable *)
    | "BracketLeft" -> BracketLeft
    | "BracketRight" -> BracketRight
    | "Semicolon" -> Semicolon
    | "Quote" -> Quote
    | "Backquote" -> Backquote
    | "Backslash" -> Backslash
    | "Comma" -> Comma
    | "Period" -> Period
    | "Slash" -> Slash
    (* Function keys *)
    | "F1" -> F1
    | "F2" -> F2
    | "F3" -> F3
    | "F4" -> F4
    | "F5" -> F5
    | "F6" -> F6
    | "F7" -> F7
    | "F8" -> F8
    | "F9" -> F9
    | "F10" -> F10
    | "F11" -> F11
    | "F12" -> F12
    (* Numpad keys *)
    | "Numpad0" -> Numpad0
    | "Numpad1" -> Numpad1
    | "Numpad2" -> Numpad2
    | "Numpad3" -> Numpad3
    | "Numpad4" -> Numpad4
    | "Numpad5" -> Numpad5
    | "Numpad6" -> Numpad6
    | "Numpad7" -> Numpad7
    | "Numpad8" -> Numpad8
    | "Numpad9" -> Numpad9
    | "NumpadMultiply" -> NumpadMultiply
    | "NumpadSubtract" -> NumpadSubtract
    | "NumpadAdd" -> NumpadAdd
    | "NumpadDecimal" -> NumpadDecimal
    | "NumpadEqual" -> NumpadEqual
    | "NumpadEnter" -> NumpadEnter
    | "NumpadDivide" -> NumpadDivide
    | "NumLock" -> NumLock
    (* Modifier keys *)
    | "ControlLeft" -> ControlLeft
    | "ControlRight" -> ControlRight
    | "MetaLeft" -> MetaLeft
    | "MetaRight" -> MetaRight
    | "ShiftLeft" -> ShiftLeft
    | "ShiftRight" -> ShiftRight
    | "AltLeft" -> AltLeft
    | "AltRight" -> AltRight
    (* Arrow keys *)
    | "ArrowLeft" -> ArrowLeft
    | "ArrowRight" -> ArrowRight
    | "ArrowUp" -> ArrowUp
    | "ArrowDown" -> ArrowDown
    (* Navigation *)
    | "PageUp" -> PageUp
    | "PageDown" -> PageDown
    | "Home" -> Home
    | "End" -> End
    (* Sound *)
    | "VolumeMute" -> VolumeMute
    | "VolumeDown" -> VolumeDown
    | "VolumeUp" -> VolumeUp
    (* Media *)
    | "MediaTrackPrevious" -> MediaTrackPrevious
    | "MediaTrackNext" -> MediaTrackNext
    | "MediaPlayPause" -> MediaPlayPause
    | "MediaStop" -> MediaStop
    (* Browser special *)
    | "ContextMenu" -> ContextMenu
    | "BrowserSearch" -> BrowserSearch
    | "BrowserHome" -> BrowserHome
    | "BrowserFavorites" -> BrowserFavorites
    | "BrowserRefresh" -> BrowserRefresh
    | "BrowserStop" -> BrowserStop
    | "BrowserForward" -> BrowserForward
    | "BrowserBack" -> BrowserBack
    (* Misc *)
    | "OSLeft" -> OSLeft
    | "OSRight" -> OSRight
    | "ScrollLock" -> ScrollLock
    | "PrintScreen" -> PrintScreen
    | "IntlBackslash" -> IntlBackslash
    | "IntlYen" -> IntlYen
    | "Pause" -> Pause
    | _ -> Unidentified

  let try_key_code_left = function
    | 16 -> ShiftLeft
    | 17 -> ControlLeft
    | 18 -> AltLeft
    | 91 -> MetaLeft
    | _ -> Unidentified

  let try_key_code_right = function
    | 16 -> ShiftRight
    | 17 -> ControlRight
    | 18 -> AltRight
    | 91 -> MetaRight
    | _ -> Unidentified

  let try_key_code_numpad = function
    | 46 -> NumpadDecimal
    | 45 -> Numpad0
    | 35 -> Numpad1
    | 40 -> Numpad2
    | 34 -> Numpad3
    | 37 -> Numpad4
    | 12 -> Numpad5
    | 39 -> Numpad6
    | 36 -> Numpad7
    | 38 -> Numpad8
    | 33 -> Numpad9
    | 13 -> NumpadEnter
    | 111 -> NumpadDivide
    | 107 -> NumpadAdd
    | 109 -> NumpadSubtract
    | 106 -> NumpadMultiply
    | 110 -> NumpadDecimal
    | 96 -> Numpad0
    | 97 -> Numpad1
    | 98 -> Numpad2
    | 99 -> Numpad3
    | 100 -> Numpad4
    | 101 -> Numpad5
    | 102 -> Numpad6
    | 103 -> Numpad7
    | 104 -> Numpad8
    | 105 -> Numpad9
    | _ -> Unidentified

  let try_key_code_normal = function
    | 27 -> Escape
    | 112 -> F1
    | 113 -> F2
    | 114 -> F3
    | 115 -> F4
    | 116 -> F5
    | 117 -> F6
    | 118 -> F7
    | 119 -> F8
    | 120 -> F9
    | 121 -> F10
    | 122 -> F11
    | 123 -> F12
    | 42 -> PrintScreen
    | 145 -> ScrollLock
    | 19 -> Pause
    | 192 -> Backquote
    | 49 -> Digit1
    | 50 -> Digit2
    | 51 -> Digit3
    | 52 -> Digit4
    | 53 -> Digit5
    | 54 -> Digit6
    | 55 -> Digit7
    | 56 -> Digit8
    | 57 -> Digit9
    | 48 -> Digit0
    | 189 -> Minus
    | 187 -> Equal
    | 8 -> Backspace
    | 9 -> Tab
    | 81 -> KeyQ
    | 87 -> KeyW
    | 69 -> KeyE
    | 82 -> KeyR
    | 84 -> KeyT
    | 89 -> KeyY
    | 85 -> KeyU
    | 73 -> KeyI
    | 79 -> KeyO
    | 80 -> KeyP
    | 219 -> BracketLeft
    | 221 -> BracketRight
    | 220 -> Backslash
    | 20 -> CapsLock
    | 65 -> KeyA
    | 83 -> KeyS
    | 68 -> KeyD
    | 70 -> KeyF
    | 71 -> KeyG
    | 72 -> KeyH
    | 74 -> KeyJ
    | 75 -> KeyK
    | 76 -> KeyL
    | 186 -> Semicolon
    | 222 -> Quote
    | 13 -> Enter
    | 90 -> KeyZ
    | 88 -> KeyX
    | 67 -> KeyC
    | 86 -> KeyV
    | 66 -> KeyB
    | 78 -> KeyN
    | 77 -> KeyM
    | 188 -> Comma
    | 190 -> Period
    | 191 -> Slash
    | 32 -> Space
    | 93 -> ContextMenu
    | 45 -> Insert
    | 36 -> Home
    | 33 -> PageUp
    | 46 -> Delete
    | 35 -> End
    | 34 -> PageDown
    | 37 -> ArrowLeft
    | 40 -> ArrowDown
    | 39 -> ArrowRight
    | 38 -> ArrowUp
    | _ -> Unidentified

  let make_unidentified _ = Unidentified

  let try_next value f = function
    | Unidentified -> Optdef.case value make_unidentified f
    | v -> v

  let run_next value f = function
    | Unidentified -> f value
    | v -> v

  let get_key_code evt = evt##.keyCode

  let try_key_location evt =
    match evt##.location with
    | 1 -> run_next (get_key_code evt) try_key_code_left
    | 2 -> run_next (get_key_code evt) try_key_code_right
    | 3 -> run_next (get_key_code evt) try_key_code_numpad
    | _ -> make_unidentified

  let ( |> ) x f = f x

  let of_event evt =
    Unidentified
    |> try_next evt##.code try_code
    |> try_key_location evt
    |> run_next (get_key_code evt) try_key_code_normal

  let of_key_code = try_key_code_normal
end

module Keyboard_key = struct
  type t = Uchar.t option

  let char_of_int value =
    if 0 < value then try Some (Uchar.of_int value) with _ -> None else None

  let empty_string _ = Js.string ""

  let none _ = None

  let of_event evt =
    let key = Optdef.get evt##.key empty_string in
    match key##.length with
    | 0 -> Optdef.case evt##.charCode none char_of_int
    | 1 -> char_of_int (int_of_float (Js.to_float (key##charCodeAt 0)))
    | _ -> None
end

(*****)

let element : #Dom.element t -> element t = Js.Unsafe.coerce

type taggedElement =
  | A of anchorElement t
  | Area of areaElement t
  | Audio of audioElement t
  | Base of baseElement t
  | Blockquote of quoteElement t
  | Body of bodyElement t
  | Br of brElement t
  | Button of buttonElement t
  | Canvas of canvasElement t
  | Caption of tableCaptionElement t
  | Col of tableColElement t
  | Colgroup of tableColElement t
  | Del of modElement t
  | Dialog of dialogElement t
  | Div of divElement t
  | Dl of dListElement t
  | Embed of embedElement t
  | Fieldset of fieldSetElement t
  | Form of formElement t
  | Frameset of frameSetElement t
  | Frame of frameElement t
  | H1 of headingElement t
  | H2 of headingElement t
  | H3 of headingElement t
  | H4 of headingElement t
  | H5 of headingElement t
  | H6 of headingElement t
  | Head of headElement t
  | Hr of hrElement t
  | Html of htmlElement t
  | Iframe of iFrameElement t
  | Img of imageElement t
  | Input of inputElement t
  | Ins of modElement t
  | Label of labelElement t
  | Legend of legendElement t
  | Li of liElement t
  | Link of linkElement t
  | Map of mapElement t
  | Meta of metaElement t
  | Object of objectElement t
  | Ol of oListElement t
  | Optgroup of optGroupElement t
  | Option of optionElement t
  | P of paragraphElement t
  | Param of paramElement t
  | Pre of preElement t
  | Q of quoteElement t
  | Script of scriptElement t
  | Select of selectElement t
  | Style of styleElement t
  | Table of tableElement t
  | Tbody of tableSectionElement t
  | Td of tableCellElement t
  | Textarea of textAreaElement t
  | Tfoot of tableSectionElement t
  | Th of tableCellElement t
  | Thead of tableSectionElement t
  | Title of titleElement t
  | Tr of tableRowElement t
  | Ul of uListElement t
  | Video of videoElement t
  | Other of element t

let other e = Other (e : #element t :> element t)

let tagged (e : #element t) =
  let tag = Js.to_bytestring e##.tagName##toLowerCase in
  if String.length tag = 0
  then other e
  else
    match String.unsafe_get tag 0 with
    | 'a' -> (
        match tag with
        | "a" -> A (Js.Unsafe.coerce e)
        | "area" -> Area (Js.Unsafe.coerce e)
        | "audio" -> Audio (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'b' -> (
        match tag with
        | "base" -> Base (Js.Unsafe.coerce e)
        | "blockquote" -> Blockquote (Js.Unsafe.coerce e)
        | "body" -> Body (Js.Unsafe.coerce e)
        | "br" -> Br (Js.Unsafe.coerce e)
        | "button" -> Button (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'c' -> (
        match tag with
        | "canvas" -> Canvas (Js.Unsafe.coerce e)
        | "caption" -> Caption (Js.Unsafe.coerce e)
        | "col" -> Col (Js.Unsafe.coerce e)
        | "colgroup" -> Colgroup (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'd' -> (
        match tag with
        | "del" -> Del (Js.Unsafe.coerce e)
        | "div" -> Div (Js.Unsafe.coerce e)
        | "dl" -> Dl (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'e' -> (
        match tag with
        | "embed" -> Embed (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'f' -> (
        match tag with
        | "fieldset" -> Fieldset (Js.Unsafe.coerce e)
        | "form" -> Form (Js.Unsafe.coerce e)
        | "frameset" -> Frameset (Js.Unsafe.coerce e)
        | "frame" -> Frame (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'h' -> (
        match tag with
        | "h1" -> H1 (Js.Unsafe.coerce e)
        | "h2" -> H2 (Js.Unsafe.coerce e)
        | "h3" -> H3 (Js.Unsafe.coerce e)
        | "h4" -> H4 (Js.Unsafe.coerce e)
        | "h5" -> H5 (Js.Unsafe.coerce e)
        | "h6" -> H6 (Js.Unsafe.coerce e)
        | "head" -> Head (Js.Unsafe.coerce e)
        | "hr" -> Hr (Js.Unsafe.coerce e)
        | "html" -> Html (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'i' -> (
        match tag with
        | "iframe" -> Iframe (Js.Unsafe.coerce e)
        | "img" -> Img (Js.Unsafe.coerce e)
        | "input" -> Input (Js.Unsafe.coerce e)
        | "ins" -> Ins (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'l' -> (
        match tag with
        | "label" -> Label (Js.Unsafe.coerce e)
        | "legend" -> Legend (Js.Unsafe.coerce e)
        | "li" -> Li (Js.Unsafe.coerce e)
        | "link" -> Link (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'm' -> (
        match tag with
        | "map" -> Map (Js.Unsafe.coerce e)
        | "meta" -> Meta (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'o' -> (
        match tag with
        | "object" -> Object (Js.Unsafe.coerce e)
        | "ol" -> Ol (Js.Unsafe.coerce e)
        | "optgroup" -> Optgroup (Js.Unsafe.coerce e)
        | "option" -> Option (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'p' -> (
        match tag with
        | "p" -> P (Js.Unsafe.coerce e)
        | "param" -> Param (Js.Unsafe.coerce e)
        | "pre" -> Pre (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'q' -> (
        match tag with
        | "q" -> Q (Js.Unsafe.coerce e)
        | _ -> other e)
    | 's' -> (
        match tag with
        | "script" -> Script (Js.Unsafe.coerce e)
        | "select" -> Select (Js.Unsafe.coerce e)
        | "style" -> Style (Js.Unsafe.coerce e)
        | _ -> other e)
    | 't' -> (
        match tag with
        | "table" -> Table (Js.Unsafe.coerce e)
        | "tbody" -> Tbody (Js.Unsafe.coerce e)
        | "td" -> Td (Js.Unsafe.coerce e)
        | "textarea" -> Textarea (Js.Unsafe.coerce e)
        | "tfoot" -> Tfoot (Js.Unsafe.coerce e)
        | "th" -> Th (Js.Unsafe.coerce e)
        | "thead" -> Thead (Js.Unsafe.coerce e)
        | "title" -> Title (Js.Unsafe.coerce e)
        | "tr" -> Tr (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'u' -> (
        match tag with
        | "ul" -> Ul (Js.Unsafe.coerce e)
        | _ -> other e)
    | 'v' -> (
        match tag with
        | "video" -> Video (Js.Unsafe.coerce e)
        | _ -> other e)
    | _ -> other e

let opt_tagged e = Opt.case e (fun () -> None) (fun e -> Some (tagged e))

type taggedEvent =
  | MouseEvent of mouseEvent t
  | KeyboardEvent of keyboardEvent t
  | MessageEvent of messageEvent t
  | MousewheelEvent of mousewheelEvent t
  | MouseScrollEvent of mouseScrollEvent t
  | PopStateEvent of popStateEvent t
  | OtherEvent of event t

let taggedEvent (ev : #event Js.t) =
  Js.Opt.case
    (CoerceTo.mouseEvent ev)
    (fun () ->
      Js.Opt.case
        (CoerceTo.keyboardEvent ev)
        (fun () ->
          Js.Opt.case
            (CoerceTo.wheelEvent ev)
            (fun () ->
              Js.Opt.case
                (CoerceTo.mouseScrollEvent ev)
                (fun () ->
                  Js.Opt.case
                    (CoerceTo.popStateEvent ev)
                    (fun () ->
                      Js.Opt.case
                        (CoerceTo.messageEvent ev)
                        (fun () -> OtherEvent (ev :> event t))
                        (fun ev -> MessageEvent ev))
                    (fun ev -> PopStateEvent ev))
                (fun ev -> MouseScrollEvent ev))
            (fun ev -> MousewheelEvent ev))
        (fun ev -> KeyboardEvent ev))
    (fun ev -> MouseEvent ev)

let opt_taggedEvent ev = Opt.case ev (fun () -> None) (fun ev -> Some (taggedEvent ev))

let stopPropagation ev =
  let e = Js.Unsafe.coerce ev in
  Optdef.case
    e##.stopPropagation
    (fun () -> e##.cancelBubble := Js._true)
    (fun _ -> e##_stopPropagation)

let _requestAnimationFrame : (unit -> unit) Js.callback -> unit =
  Js.Unsafe.pure_expr (fun _ ->
      let w = Js.Unsafe.coerce window in
      let l =
        [ w##.requestAnimationFrame
        ; w##.mozRequestAnimationFrame
        ; w##.webkitRequestAnimationFrame
        ; w##.oRequestAnimationFrame
        ; w##.msRequestAnimationFrame
        ]
      in
      try
        let req = List.find (fun c -> Js.Optdef.test c) l in
        fun callback -> Js.Unsafe.fun_call req [| Js.Unsafe.inject callback |]
      with Not_found ->
        let now () = Js.to_float (new%js Js.date_now)##getTime in
        let last = ref (now ()) in
        fun callback ->
          let t = now () in
          let dt = !last +. (1000. /. 60.) -. t in
          let dt = if Poly.(dt < 0.) then 0. else dt in
          last := t;
          ignore (window##setTimeout callback (Js.float dt)))

(****)

let hasPushState () = Js.Optdef.test (Js.Unsafe.coerce window##.history)##.pushState

let hasPlaceholder () =
  let i = createInput document in
  Js.Optdef.test (Js.Unsafe.coerce i)##.placeholder

let hasRequired () =
  let i = createInput document in
  Js.Optdef.test (Js.Unsafe.coerce i)##.required

let overflow_limit = 2147483_000.

(* ms *)

type timeout_id_safe = timeout_id option ref

let setTimeout callback d : timeout_id_safe =
  let id = ref None in
  let rec loop d () =
    let step, remain =
      if Poly.(d > overflow_limit) then overflow_limit, d -. overflow_limit else d, 0.
    in
    let cb = if Poly.(remain = 0.) then callback else loop remain in
    id := Some (window##setTimeout (Js.wrap_callback cb) (Js.float step))
  in
  loop d ();
  id

let clearTimeout (id : timeout_id_safe) =
  match !id with
  | None -> ()
  | Some x ->
      id := None;
      window##clearTimeout x

let js_array_of_collection (c : #element collection Js.t) : #element Js.t Js.js_array Js.t
    =
  Js.Unsafe.(meth_call (js_expr "[].slice") "call" [| inject c |])
