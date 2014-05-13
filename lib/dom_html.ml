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

external caml_js_on_ie : unit -> bool t = "caml_js_on_ie"

let onIE  = Js.to_bool (caml_js_on_ie ())

external html_escape : js_string t -> js_string t = "caml_js_html_escape"

external decode_html_entities : js_string t -> js_string t = "caml_js_html_entities"

class type cssStyleDeclaration = object
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
  method position : js_string t prop
  method right : js_string t prop
  method tableLayout : js_string t prop
  method textAlign : js_string t prop
  method textDecoration : js_string t prop
  method textIndent : js_string t prop
  method textTransform : js_string t prop
  method top : js_string t prop
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

class type event = object
  inherit [element] Dom.event
end

and mouseEvent = object
  inherit event
  method relatedTarget : element t opt optdef readonly_prop
  method clientX : int readonly_prop
  method clientY : int readonly_prop
  method screenX : int readonly_prop
  method screenY : int readonly_prop
  method ctrlKey : bool t readonly_prop
  method shiftKey : bool t readonly_prop
  method altKey : bool t readonly_prop
  method metaKey : bool t readonly_prop
  method button : int readonly_prop
  method which : mouse_button optdef readonly_prop

  method fromElement : element t opt optdef readonly_prop
  method toElement : element t opt optdef readonly_prop
  method pageX : int optdef readonly_prop
  method pageY : int optdef readonly_prop
end

and keyboardEvent = object
  inherit event
  method charCode : int optdef readonly_prop
  method keyCode : int readonly_prop
  method keyIdentifier : js_string t optdef readonly_prop
  method altKey : bool t readonly_prop
  method shiftKey : bool t readonly_prop
  method ctrlKey : bool t readonly_prop
  method metaKey : bool t readonly_prop
end

and mousewheelEvent = object (* All browsers but Firefox *)
  inherit mouseEvent
  method wheelDelta : int readonly_prop
  method wheelDeltaX : int optdef readonly_prop
  method wheelDeltaY : int optdef readonly_prop
end

and mouseScrollEvent = object (* Firefox *)
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
  method screenX : int readonly_prop
  method screenY : int readonly_prop
  method clientX : int readonly_prop
  method clientY : int readonly_prop
  method pageX : int readonly_prop
  method pageY : int readonly_prop
end

and dragEvent = object
  inherit mouseEvent
  method dataTransfer : dataTransfer t readonly_prop
end

and dataTransfer = object
  method dropEffect : js_string t prop
  method effectAllowed : js_string t prop
  method files : File.fileList t readonly_prop
  method types : Dom.stringList t readonly_prop
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
  method ondragstart : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragend : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragenter : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragover : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragleave : ('self t, dragEvent t) event_listener writeonly_prop
  method ondrag : ('self t, dragEvent t) event_listener writeonly_prop
  method ondrop : ('self t, dragEvent t) event_listener writeonly_prop
end

and popStateEvent = object
  inherit event
  method state : Js.Unsafe.any readonly_prop
end

and storageEvent = object
  inherit event
  method key : js_string t readonly_prop
  method oldValue : js_string t opt readonly_prop
  method keynewValue : js_string t opt readonly_prop
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
  method style : cssStyleDeclaration t prop

  method innerHTML : js_string t prop

  method clientLeft : int readonly_prop
  method clientTop : int readonly_prop
  method clientWidth : int readonly_prop
  method clientHeight : int readonly_prop
  method offsetLeft : int readonly_prop
  method offsetTop : int readonly_prop
  method offsetParent : element t opt readonly_prop
  method offsetWidth : int readonly_prop
  method offsetHeight : int readonly_prop
  method scrollLeft : int prop
  method scrollTop : int prop
  method scrollWidth : int prop
  method scrollHeight : int prop

  method getClientRects : clientRectList t meth
  method getBoundingClientRect : clientRect t meth

  method scrollIntoView: bool t -> unit meth

  inherit eventTarget
end

and clientRect = object
  method top : float readonly_prop
  method right : float readonly_prop
  method bottom : float readonly_prop
  method left : float readonly_prop
  method width : float optdef readonly_prop
  method height : float optdef readonly_prop
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
  let click = Dom.Event.make "click"
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
  let beforeunload = Dom.Event.make "beforeunload"
  let resize = Dom.Event.make "resize"
  let orientationchange = Dom.Event.make "orientationchange"
  let popstate = Dom.Event.make "popstate"
  let hashchange = Dom.Event.make "hashchange"
  let error = Dom.Event.make "error"
  let abort = Dom.Event.make "abort"
  let select = Dom.Event.make "select"

  let online = Dom.Event.make "online"
  let offline = Dom.Event.make "offline"

  let checking = Dom.Event.make "checking"
  let error = Dom.Event.make "error"
  let noupdate = Dom.Event.make "noupdate"
  let downloading = Dom.Event.make "downloading"
  let progress = Dom.Event.make "progress"
  let updateready = Dom.Event.make "updateready"
  let cached = Dom.Event.make "cached"
  let obsolete = Dom.Event.make "obsolete"

  let make = Dom.Event.make
end

type event_listener_id = Dom.event_listener_id

let addEventListener = Dom.addEventListener

let removeEventListener = Dom.removeEventListener

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

  method onsubmit : ('self t, event t) event_listener writeonly_prop
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
  method selected : bool prop
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
  method blur : unit meth
  method focus : unit meth
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
  method blur : unit meth
  method focus : unit meth
  method select : unit meth
  method click : unit meth
  method files : File.fileList t optdef readonly_prop
  method placeholder : js_string t writeonly_prop
  method onselect : ('self t, event t) event_listener prop
  method onchange : ('self t, event t) event_listener prop
  method oninput : ('self t, event t) event_listener prop
  method onblur : ('self t, event t) event_listener prop
  method onfocus : ('self t, event t) event_listener prop
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
  method tabIndex : int prop
  method _type : js_string t readonly_prop
  method value : js_string t prop
  method blur : unit meth
  method focus : unit meth
  method select : unit meth
  method required : bool t writeonly_prop
  method placeholder : js_string t writeonly_prop
  method onselect : ('self t, event t) event_listener prop
  method onchange : ('self t, event t) event_listener prop
  method oninput : ('self t, event t) event_listener prop
  method onblur : ('self t, event t) event_listener prop
  method onfocus : ('self t, event t) event_listener prop
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
  method href : js_string t prop
  method hreflang : js_string t prop
  method name : js_string t prop
  method rel : js_string t prop
  method rev : js_string t prop
  method shape : js_string t prop
  method tabIndex : int prop
  method target : js_string t prop
  method _type : js_string t prop
  method blur : unit meth
  method focus : unit meth
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
  method width  : js_string t prop
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
  method tFood : tableSectionElement t prop
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
  method start : int -> float meth
  method end_ : int -> float meth
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
  method currentTime : float prop
  method duration : float readonly_prop
  method ended : bool t readonly_prop
  method loop : bool t prop
  method mediagroup : js_string t prop
  method muted : bool t prop
  method networkState_int : int readonly_prop
  method networkState : networkState readonly_prop
  method paused : bool t readonly_prop
  method playbackRate : float prop
  method played : timeRanges t readonly_prop
  method preload : js_string t prop
  method readyState_int : int readonly_prop
  method readyState : readyState readonly_prop
  method seekable : timeRanges t readonly_prop
  method seeking : bool t readonly_prop
  method src : js_string t prop
  method volume : float prop
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
  method getContext : js_string t -> canvasRenderingContext2D t meth
end

and canvasRenderingContext2D = object
  method canvas : canvasElement t readonly_prop
  method save : unit meth
  method restore : unit meth
  method scale : float -> float -> unit meth
  method rotate : float -> unit meth
  method translate : float -> float -> unit meth
  method transform :
    float -> float -> float -> float -> float -> float -> unit meth
  method setTransform :
    float -> float -> float -> float -> float -> float -> unit meth
  method globalAlpha : float prop
  method globalCompositeOperation : js_string t prop
  method strokeStyle : js_string t writeonly_prop
  method strokeStyle_gradient : canvasGradient t writeonly_prop
  method strokeStyle_pattern : canvasPattern t writeonly_prop
  method fillStyle : js_string t writeonly_prop
  method fillStyle_gradient : canvasGradient t writeonly_prop
  method fillStyle_pattern : canvasPattern t writeonly_prop
  method createLinearGradient :
    float -> float -> float -> float -> canvasGradient t meth
  method createRadialGradient :
    float -> float -> float -> float -> float -> float ->
    canvasGradient t meth
  method createPattern : imageElement t -> js_string t -> canvasPattern t meth
  method createPattern_fromCanvas :
    canvasElement t -> js_string t -> canvasPattern t meth
  method createPattern_fromVideo :
    videoElement t -> js_string t -> canvasPattern t meth
  method lineWidth : float prop
  method lineCap : js_string t prop
  method lineJoin : js_string t prop
  method miterLimit : float prop

  method shadowOffsetX : float prop
  method shadowOffsetY : float prop
  method shadowBlur : float prop
  method shadowColor : js_string t prop

  method clearRect : float -> float -> float -> float -> unit meth
  method fillRect : float -> float -> float -> float -> unit meth
  method strokeRect : float -> float -> float -> float -> unit meth

  method beginPath : unit meth
  method closePath : unit meth
  method moveTo : float -> float -> unit meth
  method lineTo : float -> float -> unit meth
  method quadraticCurveTo : float -> float -> float -> float -> unit meth
  method bezierCurveTo :
    float -> float -> float -> float -> float -> float -> unit meth
  method arcTo : float -> float -> float -> float -> float -> unit meth
  method rect : float -> float -> float -> float -> unit meth
  method arc :
    float -> float -> float -> float -> float -> bool t -> unit meth
  method fill : unit meth
  method stroke : unit meth
  method clip : unit meth
  method isPointInPath : float -> float -> bool t meth

  method drawFocusRing : #element t -> float -> float -> bool t -> bool t meth

  method font : js_string t prop
  method textAlign : js_string t prop
  method textBaseline : js_string t prop
  method fillText : js_string t -> float -> float -> unit meth
  method fillText_withWidth :
    js_string t -> float -> float -> float -> unit meth
  method strokeText : js_string t -> float -> float -> unit meth
  method strokeText_withWidth :
    js_string t -> float -> float -> float -> unit meth
  method measureText : js_string t -> textMetrics t meth

  method drawImage :
    imageElement t -> float -> float -> unit meth
  method drawImage_withSize :
    imageElement t -> float -> float -> float -> float -> unit meth
  method drawImage_full :
    imageElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit meth
  method drawImage_fromCanvas :
    canvasElement t -> float -> float -> unit meth
  method drawImage_fromCanvasWithSize :
    canvasElement t -> float -> float -> float -> float -> unit meth
  method drawImage_fullFromCanvas :
    canvasElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit meth
  method drawImage_fromVideoWithVideo :
    videoElement t -> float -> float -> unit meth
  method drawImage_fromVideoWithSize :
    videoElement t -> float -> float -> float -> float -> unit meth
  method drawImage_fullFromVideo :
    videoElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit meth

  method createImageData : int -> int -> imageData t meth
  method getImageData : float -> float -> float -> float -> imageData t meth
  method putImageData : imageData t -> float -> float -> unit meth
end

and canvasGradient = object
  method addColorStop : float -> js_string t -> unit meth
end

and textMetrics = object
  method width : float readonly_prop
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

  inherit eventTarget
end

type interval_id
type timeout_id

class type location = object
  method href : js_string t prop
  method protocol : js_string t prop
  method host : js_string t prop
  method hostname : js_string t prop
  method port : js_string t prop
  method pathname : js_string t prop
  method search : js_string t prop
  method hash : js_string t prop

  method assign : js_string t -> unit meth
  method replace : js_string t -> unit meth
  method reload : unit meth
end

class type history = object
  method length : int readonly_prop
  method state : Js.Unsafe.any readonly_prop
  method go : int opt -> unit meth
  method back : unit meth
  method forward : unit meth
  method pushState : 'a. 'a -> js_string t -> js_string t opt -> unit meth
  method replaceState : 'a. 'a -> js_string t -> js_string t opt -> unit meth
end

class type undoManager = object
end

class type selection = object
end

class type navigator = object
  method appCodeName : js_string t readonly_prop
  method appName : js_string t readonly_prop
  method appVersion : js_string t readonly_prop
  method cookieEnabled : bool t readonly_prop
  method onLine : bool t readonly_prop
  method platform : js_string t readonly_prop
  method userAgent : js_string t readonly_prop
  method language : js_string t optdef readonly_prop
  method userLanguage : js_string t optdef readonly_prop
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
  method scroll : int -> int -> unit meth

  method sessionStorage : storage t optdef readonly_prop
  method localStorage : storage t optdef readonly_prop

  method top : window t readonly_prop
  method parent : window t readonly_prop
  method frameElement : element t opt readonly_prop

  method open_ : js_string t -> js_string t -> js_string t opt -> window t meth
  method alert : js_string t -> unit meth
  method confirm : js_string t -> bool t meth
  method prompt : js_string t -> js_string t -> js_string t opt meth
  method print : unit meth

  method setInterval : (unit -> unit) Js.callback -> float -> interval_id meth
  method clearInterval : interval_id -> unit meth

  method setTimeout : (unit -> unit) Js.callback -> float -> timeout_id meth
  method clearTimeout : timeout_id -> unit meth

  method screen : screen t readonly_prop
  method innerWidth : int optdef readonly_prop
  method innerHeight : int optdef readonly_prop
  method outerWidth : int optdef readonly_prop
  method outerHeight : int optdef readonly_prop

  method onload : (window t, event t) event_listener prop
  method onbeforeunload : (window t, event t) event_listener prop
  method onblur : (window t, event t) event_listener prop
  method onfocus : (window t, event t) event_listener prop
  method onresize : (window t, event t) event_listener prop
  method onorientationchange : (window t, event t) event_listener prop
  method onpopstate : (window t, popStateEvent t) event_listener prop
  method onhashchange : (window t, hashChangeEvent t) event_listener prop

  method ononline : (window t, event t) event_listener writeonly_prop
  method onoffline : (window t, event t) event_listener writeonly_prop
end

let window : window t = Js.Unsafe.global (* The toplevel object *)

let document = window##document

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
  method contentWindow  : window t readonly_prop
end

(****)

(*XXX Should provide creation functions a la lablgtk... *)

let opt_iter x f = match x with None -> () | Some v -> f v

let createElement (doc : document t) name = doc##createElement(Js.string name)
let unsafeCreateElement doc name = Js.Unsafe.coerce (createElement doc name)

let createElementSyntax = ref `Unknown

let rec unsafeCreateElementEx ?_type ?name doc elt =
  if _type = None && name = None then
    Js.Unsafe.coerce (createElement doc elt)
  else
    match !createElementSyntax with
      `Standard ->
        let res = Js.Unsafe.coerce (createElement doc elt) in
        opt_iter _type (fun t -> res##_type <- t);
        opt_iter name (fun n -> res##name <- n);
        res
    | `Extended ->
        let a = jsnew Js.array_empty () in
        ignore (a##push_2(Js.string "<", Js.string elt));
        opt_iter _type (fun t ->
          ignore
            (a##push_3(Js.string " type=\"", html_escape t, Js.string "\"")));
        opt_iter name (fun n ->
          ignore
            (a##push_3(Js.string " name=\"", html_escape n, Js.string "\"")));
        ignore (a##push(Js.string ">"));
        Js.Unsafe.coerce (doc##createElement (a##join (Js.string "")))
    | `Unknown ->
        createElementSyntax :=
          if
            try
              let el : inputElement Js.t =
                Js.Unsafe.coerce
                  (document##createElement(Js.string "<input name=\"x\">")) in
              el##tagName##toLowerCase() == Js.string "input" &&
              el##name == Js.string "x"
            with _ ->
              false
          then
            `Extended
          else
            `Standard;
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
let createBlockquote doc : quoteElement t =
  unsafeCreateElement doc "blockquote"
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
let createCaption doc : tableCaptionElement t =
  unsafeCreateElement doc "caption"
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
  if not (Opt.test c##getContext) then raise Canvas_not_available;
  c

let html_element : htmlElement t constr = Js.Unsafe.global ## _HTMLElement

module CoerceTo = struct
  let element : #Dom.node Js.t -> element Js.t Js.opt =
    if def html_element == undefined then
      (* ie < 9 does not have HTMLElement: we have to cheat to check
	 that something is an html element *)
      (fun e ->
	if def ((Js.Unsafe.coerce e)##innerHTML) == undefined then
	  Js.null
	else Js.some (Js.Unsafe.coerce e))
    else
      (fun e ->
	if Js.instanceof e html_element then
	  Js.some (Js.Unsafe.coerce e)
	else Js.null)

  let unsafeCoerce tag (e : #element t) =
    if e##tagName##toLowerCase() == Js.string tag then
      Js.some (Js.Unsafe.coerce e)
    else
      Js.null
  let a e =  unsafeCoerce "a" e
  let area e =  unsafeCoerce "area" e
  let base e =  unsafeCoerce "base" e
  let blockquote e =  unsafeCoerce "blockquote" e
  let body e =  unsafeCoerce "body" e
  let br e =  unsafeCoerce "br" e
  let button e =  unsafeCoerce "button" e
  let canvas e =  unsafeCoerce "canvas" e
  let caption e =  unsafeCoerce "caption" e
  let col e =  unsafeCoerce "col" e
  let colgroup e = unsafeCoerce "colgroup" e
  let del e = unsafeCoerce "del" e
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
    if def constr != undefined && Js.instanceof ev constr then
      Js.some (Js.Unsafe.coerce ev)
    else Js.null

  let mouseEvent ev = unsafeCoerceEvent (Js.Unsafe.global##_MouseEvent) ev
  let keyboardEvent ev =
    unsafeCoerceEvent (Js.Unsafe.global##_KeyboardEvent) ev
  let wheelEvent ev = unsafeCoerceEvent (Js.Unsafe.global##_WheelEvent) ev
  let mouseScrollEvent ev =
    unsafeCoerceEvent (Js.Unsafe.global##_MouseScrollEvent) ev
  let popStateEvent ev =
    unsafeCoerceEvent (Js.Unsafe.global##_PopStateEvent) ev

end

(****)

let eventTarget = Dom.eventTarget

let eventRelatedTarget (e : #mouseEvent t) =
  Optdef.get (e##relatedTarget) (fun () ->
  match Js.to_string (e##_type) with
    "mouseover" -> Optdef.get (e##fromElement) (fun () -> assert false)
  | "mouseout"  -> Optdef.get (e##toElement) (fun () -> assert false)
  | _           -> Js.null)

let eventAbsolutePosition' (e : #mouseEvent t) =
  let body = document##body in
  let html = document##documentElement in
  (e##clientX + body##scrollLeft + html##scrollLeft,
   e##clientY + body##scrollTop + html##scrollTop)

let eventAbsolutePosition (e : #mouseEvent t) =
  Optdef.case (e##pageX) (fun () -> eventAbsolutePosition' e) (fun x ->
  Optdef.case (e##pageY) (fun () -> eventAbsolutePosition' e) (fun y ->
  (x, y)))

let elementClientPosition (e : #element t) =
  let r = e##getBoundingClientRect () in
  let body = document##body in
  let html = document##documentElement in
  (truncate r##left - body##clientLeft - html##clientLeft,
   truncate r##top - body##clientTop - html##clientTop)

let getDocumentScroll () =
  let body = document##body in
  let html = document##documentElement in
  (body##scrollLeft + html##scrollLeft, body##scrollTop + html##scrollTop)

let buttonPressed (ev : #mouseEvent Js.t) =
  Js.Optdef.case (ev##which)
    (fun () ->
      match ev##button with
	| 1 -> Left_button
	| 2 -> Right_button
	| 4 -> Middle_button
	| _ -> No_button)
    (fun x -> x)

let hasMousewheelEvents () =
  let d = createDiv document in
  d##setAttribute(Js.string "onmousewheel", Js.string "return;");
  Js.typeof (Js.Unsafe.get d (Js.string "onmousewheel")) ==
  Js.string "function"

let addMousewheelEventListener e h capt =
  if hasMousewheelEvents () then
    addEventListener e Event.mousewheel
      (handler
         (fun (e : mousewheelEvent t) ->
            let dx = - Optdef.get (e##wheelDeltaX) (fun () -> 0) / 40 in
            let dy =
              - Optdef.get (e##wheelDeltaY) (fun () -> e##wheelDelta) / 40 in
            h (e :> mouseEvent t) ~dx ~dy))
      capt
  else
    addEventListener e Event._DOMMouseScroll
      (handler
         (fun (e : mouseScrollEvent t) ->
            let d = e##detail in
            if e##axis == e##_HORIZONTAL_AXIS then
              h (e :> mouseEvent t) ~dx:d ~dy:0
            else
              h (e :> mouseEvent t) ~dx:0 ~dy:d))
      capt

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
  | P of paramElement t
  | Param of paramElement t
  | Pre of preElement t
  | Q of quoteElement t
  | Script of scriptElement t
  | Select of selectElement t
  | Style of styleElement t
  | Table of tableElement t
  | Tbody of tableSectionElement t
  | Td of tableColElement t
  | Textarea of textAreaElement t
  | Tfoot of tableSectionElement t
  | Th of tableColElement t
  | Thead of tableSectionElement t
  | Title of titleElement t
  | Tr of tableRowElement t
  | Ul of uListElement t
  | Video of videoElement t
  | Other of element t

let other e = Other (e : #element t :> element t)

let tagged (e : #element t) =
  let tag = Js.to_bytestring (e##tagName##toLowerCase()) in
  if String.length tag = 0 then
    other e
  else
    match String.unsafe_get tag 0 with
      'a' ->
        begin match tag with
        | "a" -> A (Js.Unsafe.coerce e)
        | "area" -> Area (Js.Unsafe.coerce e)
        | "audio" -> Audio (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'b' ->
        begin match tag with
        | "base" -> Base (Js.Unsafe.coerce e)
        | "blockquote" -> Blockquote (Js.Unsafe.coerce e)
        | "body" -> Body (Js.Unsafe.coerce e)
        | "br" -> Br (Js.Unsafe.coerce e)
        | "button" -> Button (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'c' ->
        begin match tag with
        | "canvas" -> Canvas (Js.Unsafe.coerce e)
        | "caption" -> Caption (Js.Unsafe.coerce e)
        | "col" -> Col (Js.Unsafe.coerce e)
        | "colgroup" -> Colgroup (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'd' ->
        begin match tag with
        | "del" -> Del (Js.Unsafe.coerce e)
        | "div" -> Div (Js.Unsafe.coerce e)
        | "dl" -> Dl (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'e' ->
        begin match tag with
        | "embed" -> Embed (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'f' ->
        begin match tag with
        | "fieldset" -> Fieldset (Js.Unsafe.coerce e)
        | "form" -> Form (Js.Unsafe.coerce e)
        | "frameset" -> Frameset (Js.Unsafe.coerce e)
        | "frame" -> Frame (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'h' ->
        begin match tag with
        | "h1" -> H1 (Js.Unsafe.coerce e)
        | "h2" -> H2 (Js.Unsafe.coerce e)
        | "h3" -> H3 (Js.Unsafe.coerce e)
        | "h4" -> H4 (Js.Unsafe.coerce e)
        | "h5" -> H5 (Js.Unsafe.coerce e)
        | "h6" -> H6 (Js.Unsafe.coerce e)
        | "head" -> Head (Js.Unsafe.coerce e)
        | "hr" -> Hr (Js.Unsafe.coerce e)
        | "html" -> Html (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'i' ->
        begin match tag with
        | "iframe" -> Iframe (Js.Unsafe.coerce e)
        | "img" -> Img (Js.Unsafe.coerce e)
        | "input" -> Input (Js.Unsafe.coerce e)
        | "ins" -> Ins (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'l' ->
        begin match tag with
        | "label" -> Label (Js.Unsafe.coerce e)
        | "legend" -> Legend (Js.Unsafe.coerce e)
        | "li" -> Li (Js.Unsafe.coerce e)
        | "link" -> Link (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'm' ->
        begin match tag with
        | "map" -> Map (Js.Unsafe.coerce e)
        | "meta" -> Meta (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'o' ->
        begin match tag with
        | "object" -> Object (Js.Unsafe.coerce e)
        | "ol" -> Ol (Js.Unsafe.coerce e)
        | "optgroup" -> Optgroup (Js.Unsafe.coerce e)
        | "option" -> Option (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'p' ->
        begin match tag with
        | "p" -> P (Js.Unsafe.coerce e)
        | "param" -> Param (Js.Unsafe.coerce e)
        | "pre" -> Pre (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'q' ->
        begin match tag with
        | "q" -> Q (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 's' ->
        begin match tag with
        | "script" -> Script (Js.Unsafe.coerce e)
        | "select" -> Select (Js.Unsafe.coerce e)
        | "style" -> Style (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 't' ->
        begin match tag with
        | "table" -> Table (Js.Unsafe.coerce e)
        | "tbody" -> Tbody (Js.Unsafe.coerce e)
        | "td" -> Td (Js.Unsafe.coerce e)
        | "textarea" -> Textarea (Js.Unsafe.coerce e)
        | "tfoot" -> Tfoot (Js.Unsafe.coerce e)
        | "th" -> Th (Js.Unsafe.coerce e)
        | "thead" -> Thead (Js.Unsafe.coerce e)
        | "title" -> Title (Js.Unsafe.coerce e)
        | "tr" -> Tr (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'u' ->
        begin match tag with
        | "ul" -> Ul (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | 'v' ->
        begin match tag with
        | "video" -> Video (Js.Unsafe.coerce e)
        | _ -> other e
        end
    | _ ->
        other e

let opt_tagged e = Opt.case e (fun () -> None) (fun e -> Some (tagged e))

type taggedEvent =
  | MouseEvent of mouseEvent t
  | KeyboardEvent of keyboardEvent t
  | MousewheelEvent of mousewheelEvent t
  | MouseScrollEvent of mouseScrollEvent t
  | PopStateEvent of popStateEvent t
  | OtherEvent of event t

let taggedEvent (ev : #event Js.t) =
  Js.Opt.case (CoerceTo.mouseEvent ev)
    (fun () -> Js.Opt.case (CoerceTo.keyboardEvent ev)
      (fun () -> Js.Opt.case (CoerceTo.wheelEvent ev)
	(fun () -> Js.Opt.case (CoerceTo.mouseScrollEvent ev)
	  (fun () -> Js.Opt.case (CoerceTo.popStateEvent ev)
	    (fun () -> OtherEvent (ev :> event t))
	    (fun ev -> PopStateEvent ev))
	  (fun ev -> MouseScrollEvent ev))
	(fun ev -> MousewheelEvent ev))
      (fun ev -> KeyboardEvent ev))
    (fun ev -> MouseEvent ev)

let opt_taggedEvent ev = Opt.case ev (fun () -> None) (fun ev -> Some (taggedEvent ev))

let stopPropagation ev =
  let e = Js.Unsafe.coerce ev in
  Optdef.case
    (e##stopPropagation)
    (fun () -> e##cancelBubble <- Js._true)
    (fun _ -> e##_stopPropagation())

let _requestAnimationFrame : (unit -> unit) Js.callback -> unit =
  Js.Unsafe.pure_expr
    (fun _ ->
       let w = Js.Unsafe.coerce window in
       let l =
         [w##requestAnimationFrame;
          w##mozRequestAnimationFrame;
          w##webkitRequestAnimationFrame;
          w##oRequestAnimationFrame;
          w##msRequestAnimationFrame]
       in
       try
         let req = List.find (fun c -> Js.Optdef.test c) l in
         fun callback -> Js.Unsafe.fun_call req [|Js.Unsafe.inject callback|]
       with Not_found ->
         let now () = jsnew Js.date_now ()##getTime() in
         let last = ref (now ()) in
         fun callback ->
           let t = now () in
           let dt = !last +. 1000. /. 60. -. t in
           let dt = if dt < 0. then 0. else dt in
           last := t;
           ignore (window##setTimeout (callback, dt)))

(****)

let hasPushState () =
  Js.Optdef.test ((Js.Unsafe.coerce (window##history))##pushState)

let hasPlaceholder () =
 let i = createInput document in
  Js.Optdef.test ((Js.Unsafe.coerce i)##placeholder)

let hasRequired () =
 let i = createInput document in
  Js.Optdef.test ((Js.Unsafe.coerce i)##required)
