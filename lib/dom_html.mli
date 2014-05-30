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

(** DOM HTML binding

This is a partial binding to the DOM HTML API.
*)

open Js

(** {2 CSS style declaration} *)

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

(** {2 Events} *)

type (-'a, -'b) event_listener = ('a, 'b) Dom.event_listener
  (** The type of event listener functions.  The first type parameter
      ['a] is the type of the target object; the second parameter
      ['b] is the type of the event object. *)

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
  method clientX : int readonly_prop (* Relative to viewport *)
  method clientY : int readonly_prop
  method screenX : int readonly_prop (* Relative to the edge of the screen *)
  method screenY : int readonly_prop
  method ctrlKey : bool t readonly_prop
  method shiftKey : bool t readonly_prop
  method altKey : bool t readonly_prop
  method metaKey : bool t readonly_prop
  method which : mouse_button optdef readonly_prop

  (* Legacy methods *)
  method button : int readonly_prop
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

(** Common properties of event target objects: [onclick],
    [onkeypress], ... *)
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

(** Storage *)
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


(** {2 HTML elements} *)

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

(** Properties common to all HTML elements *)
and element = object
  inherit Dom.element
  inherit nodeSelector
  method id : js_string t prop
  method title : js_string t prop
  method lang : js_string t prop
  method dir : js_string t prop
  method className : js_string t prop
  method classList : tokenList t readonly_prop
    (* Not supported by IE9 by default. Add +classList.js to the
       Js_of_ocaml command line for compatibility *)
  method style : cssStyleDeclaration t prop

  method innerHTML : js_string t prop

  method clientLeft : int readonly_prop
  method clientTop : int readonly_prop
  method clientWidth : int readonly_prop
  method clientHeight : int readonly_prop
  method offsetLeft : int readonly_prop
  method offsetTop : int readonly_prop (* Incorrect in IE until IE7 included *)
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

(** Rectangular box (used for element bounding boxes) *)
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

(** Collection of HTML elements *)
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
  method selected : bool t prop
  method value : js_string t prop
end

class type selectElement = object ('self)
  inherit element
  method _type : js_string t readonly_prop (* Cannot be changed under IE *)
  method selectedIndex : int prop
  method value : js_string t prop
  method length : int prop
  method form : formElement t opt readonly_prop
  method options : optionElement collection t readonly_prop
  method disabled : bool t prop
  method multiple : bool t prop
  method name : js_string t readonly_prop (* Cannot be changed under IE *)
  method size : int prop
  method tabIndex : int prop
  method add : #optGroupElement t -> #optGroupElement t opt -> unit meth
  method remove : int -> unit meth
  method blur : unit meth
  method focus : unit meth
  method required : bool t writeonly_prop (* Not supported by IE 9/Safari *)

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
  method name : js_string t readonly_prop (* Cannot be changed under IE *)
  method readOnly : bool t prop
  method required : bool t writeonly_prop (* Not supported by IE 9/Safari *)
  method size : int prop
  method src : js_string t prop
  method tabIndex : int prop
  method _type : js_string t readonly_prop (* Cannot be changed under IE *)
  method useMap : js_string t prop
  method value : js_string t prop
  method blur : unit meth
  method focus : unit meth
  method select : unit meth
  method click : unit meth
  method files : File.fileList t optdef readonly_prop
  method placeholder : js_string t writeonly_prop (* Not supported by IE 9 *)
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
  method name : js_string t readonly_prop (* Cannot be changed under IE *)
  method readOnly : bool t prop
  method rows : int prop
  method tabIndex : int prop
  method _type : js_string t readonly_prop (* Cannot be changed under IE *)
  method value : js_string t prop
  method blur : unit meth
  method focus : unit meth
  method select : unit meth
  method required : bool t writeonly_prop (* Not supported by IE 9/Safari *)
  method placeholder : js_string t writeonly_prop (* Not supported by IE 9 *)

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
  method name : js_string t readonly_prop (* Cannot be changed under IE *)
  method tabIndex : int prop
  method _type : js_string t readonly_prop (* Cannot be changed under IE *)
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
  (* Properties naturalWidth/Height not available in all browsers. *)
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



(** {2 Canvas object} *)

type context
val _2d_ : context

type canvasPattern

class type canvasElement = object
  inherit element
  method width : int prop
  method height : int prop
  method toDataURL : js_string t meth
  method getContext : context -> canvasRenderingContext2D t meth
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
  (* Method createImageData not available in Opera *)
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

(** {2 Document objects} *)

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

val document : document t
  (** The current document *)

(** {2 Window objects} *)

(** Location information *)
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

(** Browser history information *)
class type history = object
  method length : int readonly_prop
  method state : Js.Unsafe.any readonly_prop
  method go : int opt -> unit meth
  method back : unit meth
  method forward : unit meth
  method pushState : 'a. 'a -> js_string t -> js_string t opt -> unit meth
  method replaceState : 'a. 'a -> js_string t -> js_string t opt -> unit meth
end

(** Undo manager *)
class type undoManager = object
(*...*)
end

(** Information on current selection *)
class type selection = object
(*...*)
end

(** Navigator information *)
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

type interval_id
type timeout_id

(** Specification of window objects *)
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

val window : window t
  (** The current window *)

val _requestAnimationFrame : (unit -> unit) Js.callback -> unit
  (** Call the appropriate [requestAnimationFrame] method variant
      (depending on the navigator), or sleep for a short amount
      of time when there no such method is provided. We currently
      prefix the function name with as underscore as the interface of
      this function is not completely standardized yet. Thus, we leave
      the room to a function with a possibly refined type. *)

(* {2 Frames } *)

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

(** {2 Event handlers} *)

val no_handler : ('a, 'b) event_listener
  (** see [Dom.no_handler] *)
val handler : ((#event t as 'b) -> bool t) -> ('a, 'b) event_listener
  (** see [Dom.handler] *)
val full_handler : ('a -> (#event t as 'b) -> bool t) -> ('a, 'b) event_listener
  (** see [Dom.full_handler] *)
val invoke_handler : ('a, 'b) event_listener -> 'a -> 'b -> bool t
  (** see [Dom.invoke_handler] *)
val eventTarget : #event t -> element t
  (** see [Dom.eventTarget] *)

val eventRelatedTarget : #mouseEvent t -> element t opt
  (** Returns this event related target. *)

(** Event types: [mousedown], [keypress], ... *)
module Event : sig
  type 'a typ = 'a Dom.Event.typ
  val click : mouseEvent t typ
  val dblclick : mouseEvent t typ
  val mousedown : mouseEvent t typ
  val mouseup : mouseEvent t typ
  val mouseover : mouseEvent t typ
  val mousemove : mouseEvent t typ
  val mouseout : mouseEvent t typ
  val keypress : keyboardEvent t typ
  val keydown : keyboardEvent t typ
  val keyup : keyboardEvent t typ
  val mousewheel : mousewheelEvent t typ
  val _DOMMouseScroll : mouseScrollEvent t typ
  val touchstart : touchEvent t typ
  val touchmove : touchEvent t typ
  val touchend : touchEvent t typ
  val touchcancel : touchEvent t typ
  val dragstart : dragEvent t typ
  val dragend : dragEvent t typ
  val dragenter : dragEvent t typ
  val dragover : dragEvent t typ
  val dragleave : dragEvent t typ
  val drag : dragEvent t typ
  val drop : dragEvent t typ
  val hashchange : hashChangeEvent t typ
  val change : event t typ
  val input : event t typ
  val timeupdate : event t typ
  val submit : event t typ
  val scroll : event t typ
  val focus : event t typ
  val blur : event t typ
  val load : event t typ
  val beforeunload : event t typ
  val resize : event t typ
  val orientationchange : event t typ
  val popstate : event t typ
  val hashchange : event t typ
  val error : event t typ
  val abort : event t typ
  val select : event t typ

  val online : event t typ
  val offline : event t typ

  val checking : event t typ
  val error : event t typ
  val noupdate : event t typ
  val downloading : event t typ
  val progress : event t typ
  val updateready : event t typ
  val cached : event t typ
  val obsolete : event t typ

  val make : string -> 'a typ
end

type event_listener_id = Dom.event_listener_id

val addEventListener :
  (#eventTarget t as 'a) -> 'b Event.typ ->
  ('a, 'b) event_listener -> bool t -> event_listener_id
  (** Add an event listener.  This function matches the
      [addEventListener] DOM method, except that it returns
      an id for removing the listener. *)

val removeEventListener : event_listener_id -> unit
  (** Remove the given event listener. *)

val addMousewheelEventListener :
  (#eventTarget t as 'a) ->
  (mouseEvent t -> dx:int -> dy:int -> bool t) ->
  bool t -> event_listener_id
  (** Add a mousewheel event listener.  The callback is provided the
      event and the numbers of ticks the mouse wheel moved.  Positive
      means down / right. *)

(****)

(** {2 Mouse event helper functions} *)

val buttonPressed : #mouseEvent Js.t -> mouse_button

(** {2 Position helper functions} *)

val eventAbsolutePosition : #mouseEvent t -> int * int
  (** Returns the absolute position of the mouse pointer. *)
val elementClientPosition : #element t -> int * int
  (** Position of an element relative to the viewport *)
val getDocumentScroll : unit -> int * int
  (** Viewport top/left position *)

(****)

(** {2 Helper functions for creating HTML elements} *)

val createHtml : document t -> htmlElement t
val createHead : document t -> headElement t
val createLink : document t -> linkElement t
val createTitle : document t -> titleElement t
val createMeta : document t -> metaElement t
val createBase : document t -> baseElement t
val createStyle : document t -> styleElement t
val createBody : document t -> bodyElement t
val createForm : document t -> formElement t
val createOptgroup : document t -> optGroupElement t
val createOption : document t -> optionElement t
val createSelect :
  ?_type:js_string t -> ?name:js_string t -> document t -> selectElement t
val createInput :
  ?_type:js_string t -> ?name:js_string t -> document t -> inputElement t
val createTextarea :
  ?_type:js_string t -> ?name:js_string t -> document t -> textAreaElement t
val createButton :
  ?_type:js_string t -> ?name:js_string t -> document t -> buttonElement t
val createLabel : document t -> labelElement t
val createFieldset : document t -> fieldSetElement t
val createLegend : document t -> legendElement t
val createUl : document t -> uListElement t
val createOl : document t -> oListElement t
val createDl : document t -> dListElement t
val createLi : document t -> liElement t
val createDiv : document t -> divElement t
val createEmbed : document t -> embedElement t
val createP : document t -> paragraphElement t
val createH1 : document t -> headingElement t
val createH2 : document t -> headingElement t
val createH3 : document t -> headingElement t
val createH4 : document t -> headingElement t
val createH5 : document t -> headingElement t
val createH6 : document t -> headingElement t
val createQ : document t -> quoteElement t
val createBlockquote : document t -> quoteElement t
val createPre : document t -> preElement t
val createBr : document t -> brElement t
val createHr : document t -> hrElement t
val createIns : document t -> modElement t
val createDel : document t -> modElement t
val createA : document t -> anchorElement t
val createImg : document t -> imageElement t
val createObject : document t -> objectElement t
val createParam : document t -> paramElement t
val createMap : document t -> mapElement t
val createArea : document t -> areaElement t
val createScript : document t -> scriptElement t
val createTable : document t -> tableElement t
val createCaption : document t -> tableCaptionElement t
val createCol : document t -> tableColElement t
val createColgroup : document t -> tableColElement t
val createThead : document t -> tableSectionElement t
val createTfoot : document t -> tableSectionElement t
val createTbody : document t -> tableSectionElement t
val createTr : document t -> tableRowElement t
val createTh : document t -> tableCellElement t
val createTd : document t -> tableCellElement t
val createSub : document t -> element t
val createSup : document t -> element t
val createSpan : document t -> element t
val createTt : document t -> element t
val createI : document t -> element t
val createB : document t -> element t
val createBig : document t -> element t
val createSmall : document t -> element t
val createEm : document t -> element t
val createStrong : document t -> element t
val createCite : document t -> element t
val createDfn : document t -> element t
val createCode : document t -> element t
val createSamp : document t -> element t
val createKbd : document t -> element t
val createVar : document t -> element t
val createAbbr : document t -> element t
val createDd : document t -> element t
val createDt : document t -> element t
val createNoscript : document t -> element t
val createAddress : document t -> element t
val createFrameset : document t -> frameSetElement t
val createFrame : document t -> frameElement t
val createIframe : document t -> iFrameElement t
val createAudio : document t -> audioElement t
val createVideo : document t -> videoElement t

exception Canvas_not_available
val createCanvas : document t -> canvasElement t
  (** @raise Canvas_not_available when canvas elements are not
      supported by the browser. *)

(****)

(** {2 Coercion functions} *)

val element : #Dom.element t -> element t
  (** Coercion from a general DOM element to an HTML element.
      (Unsafe in general.) *)

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

val tagged : #element t -> taggedElement
val opt_tagged : #element t opt -> taggedElement option

type taggedEvent =
  | MouseEvent of mouseEvent t
  | KeyboardEvent of keyboardEvent t
  | MousewheelEvent of mousewheelEvent t
  | MouseScrollEvent of mouseScrollEvent t
  | PopStateEvent of popStateEvent t
  | OtherEvent of event t

val taggedEvent : #event t -> taggedEvent
val opt_taggedEvent : #event t opt -> taggedEvent option

val stopPropagation : #event t -> unit


module CoerceTo : sig

  (** HTMLElement *)

  val element : #Dom.node t -> element t opt
  (* null if it is not an element *)

  val a : #element t -> anchorElement t opt
  val area : #element t -> areaElement t opt
  val audio : #element t -> audioElement t opt
  val base : #element t -> baseElement t opt
  val blockquote : #element t -> quoteElement t opt
  val body : #element t -> bodyElement t opt
  val br : #element t -> brElement t opt
  val button : #element t -> buttonElement t opt
  val canvas : #element t -> canvasElement t opt
  val caption : #element t -> tableCaptionElement t opt
  val col : #element t -> tableColElement t opt
  val colgroup : #element t -> tableColElement t opt
  val del : #element t -> modElement t opt
  val div : #element t -> divElement t opt
  val embed : #element t -> embedElement t opt
  val dl : #element t -> dListElement t opt
  val fieldset : #element t -> fieldSetElement t opt
  val form : #element t -> formElement t opt
  val frameset : #element t -> frameSetElement t opt
  val frame : #element t -> frameElement t opt
  val h1 : #element t -> headingElement t opt
  val h2 : #element t -> headingElement t opt
  val h3 : #element t -> headingElement t opt
  val h4 : #element t -> headingElement t opt
  val h5 : #element t -> headingElement t opt
  val h6 : #element t -> headingElement t opt
  val head : #element t -> headElement t opt
  val hr : #element t -> hrElement t opt
  val html : #element t -> htmlElement t opt
  val iframe : #element t -> iFrameElement t opt
  val img : #element t -> imageElement t opt
  val input : #element t -> inputElement t opt
  val ins : #element t -> modElement t opt
  val label : #element t -> labelElement t opt
  val legend : #element t -> legendElement t opt
  val li : #element t -> liElement t opt
  val link : #element t -> linkElement t opt
  val map : #element t -> mapElement t opt
  val meta : #element t -> metaElement t opt
  val _object : #element t -> objectElement t opt
  val ol : #element t -> oListElement t opt
  val optgroup : #element t -> optGroupElement t opt
  val option : #element t -> optionElement t opt
  val p : #element t -> paramElement t opt
  val param : #element t -> paramElement t opt
  val pre : #element t -> preElement t opt
  val q : #element t -> quoteElement t opt
  val script : #element t -> scriptElement t opt
  val select : #element t -> selectElement t opt
  val style : #element t -> styleElement t opt
  val table : #element t -> tableElement t opt
  val tbody : #element t -> tableSectionElement t opt
  val td : #element t -> tableColElement t opt
  val textarea : #element t -> textAreaElement t opt
  val tfoot : #element t -> tableSectionElement t opt
  val th : #element t -> tableColElement t opt
  val thead : #element t -> tableSectionElement t opt
  val title : #element t -> titleElement t opt
  val tr : #element t -> tableRowElement t opt
  val ul : #element t -> uListElement t opt
  val video : #element t -> videoElement t opt

  (** Event *)

  val mouseEvent : #event t -> mouseEvent t opt
  val keyboardEvent : #event t -> keyboardEvent t opt
  val wheelEvent : #event t -> mousewheelEvent t opt
  val mouseScrollEvent : #event t -> mouseScrollEvent t opt
  val popStateEvent : #event t -> popStateEvent t opt

end

(**/**)

val decode_html_entities : js_string t -> js_string t

val onIE : bool
val hasPushState : unit -> bool
val hasPlaceholder : unit -> bool
val hasRequired : unit -> bool
