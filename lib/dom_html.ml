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

type ('a, 'b) event_listener = ('a, 'b optdef -> bool t) meth_callback opt

class type event = object
  method _type : js_string t readonly_prop
  method target : element t optdef readonly_prop
  method srcElement : element t optdef readonly_prop
end

and mouseEvent = object
  inherit event
  method relatedTarget : element t opt optdef readonly_prop
  method clientX : int readonly_prop
  method clientY : int readonly_prop
  method screenX : int readonly_prop
  method screenY : int readonly_prop

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
end

and element = object
  inherit Dom.element
  method id : js_string t prop
  method title : js_string t prop
  method lang : js_string t prop
  method dir : js_string t prop
  method className : js_string t prop
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

  inherit eventTarget
end

and clientRect = object
  method top : float t readonly_prop
  method right : float t readonly_prop
  method bottom : float t readonly_prop
  method left : float t readonly_prop
  method width : float t optdef readonly_prop
  method height : float t optdef readonly_prop
end

and clientRectList = object
  method length : int readonly_prop
  method item : int -> clientRect t optdef meth
end

let no_handler : ('a, 'b) event_listener = Js.null
let window_event () : #event t = Js.Unsafe.variable "event"
(* The function preventDefault must be called explicitely when
   using addEventListener... *)
let handler f =
  Js.some (Js.wrap_callback
    (fun e ->
       Optdef.case e
         (fun () ->
            let e = window_event () in
            let res = f e in
            e##returnValue <- res; res)
         (fun e ->
            let res = f e in
            if not (Js.to_bool res) then
              (Js.Unsafe.coerce e)##preventDefault ();
            res)))
let full_handler f =
  Js.some (Js.wrap_meth_callback
    (fun this e ->
       Optdef.case e
         (fun () ->
            let e = window_event () in
            let res = f this e in
            e##returnValue <- res; res)
         (fun e ->
            let res = f this e in
            if not (Js.to_bool res) then
              (Js.Unsafe.coerce e)##preventDefault ();
            res)))
let invoke_handler
  (f : ('a, 'b) event_listener) (this : 'a) (event : 'b) : bool t =
  Js.Unsafe.call f this [|Js.Unsafe.inject event|]

module Event = struct
  type 'a typ = js_string t
  let click = Js.string "click"
  let dblclick = Js.string "dblclick"
  let mousedown = Js.string "mousedown"
  let mouseup = Js.string "mouseup"
  let mouseover = Js.string "mouseover"
  let mousemove = Js.string "mousemove"
  let mouseout = Js.string "mouseout"
  let keypress = Js.string "keypress"
  let keydown = Js.string "keydown"
  let keyup = Js.string "keyup"
  let mousewheel = Js.string "mousewheel"
  let _DOMMouseScroll = Js.string "DOMMouseScroll"
end

type event_listener_id = unit -> unit

let addEventListener (e : #eventTarget t) typ h capt =
  if (Js.Unsafe.coerce e)##addEventListener == Js.undefined then begin
    let ev = (Js.string "on")##concat(typ) in
    let callback = fun e -> Js.Unsafe.call (h, e, [||]) in
    (Js.Unsafe.coerce e)##attachEvent(ev, callback);
    fun () -> (Js.Unsafe.coerce e)##detachEvent(ev, callback)
  end else begin
    (Js.Unsafe.coerce e)##addEventListener(typ, h, capt);
    fun () -> (Js.Unsafe.coerce e)##removeEventListener (typ, h, capt)
  end

let removeEventListener id = id ()

class type ['node] collection = object
  method length : int readonly_prop
  method item : int -> 'node t optdef meth
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

  method onchange : ('self t, event t) event_listener prop
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

  method onselect : ('self t, event t) event_listener prop
  method onchange : ('self t, event t) event_listener prop
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

  method onselect : ('self t, event t) event_listener prop
  method onchange : ('self t, event t) event_listener prop
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
  method tbodies : tableSectionElement collection t readonly_prop
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
  method globalAlpha : float_prop
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
(*
  CanvasPattern createPattern(in HTMLVideoElement image, in DOMJs_String repetition);
*)
  method lineWidth : float_prop
  method lineCap : js_string t prop
  method lineJoin : js_string t prop
  method miterLimit : float_prop

  method shadowOffsetX : float_prop
  method shadowOffsetY : float_prop
  method shadowBlur : float_prop
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
(*
  void drawImage(in HTMLVideoElement image, in float dx, in float dy, in optional float dw, in float dh);
  void drawImage(in HTMLVideoElement image, in float sx, in float sy, in float sw, in float sh, in float dx, in float dy, in float dw, in float dh);
*)

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
  method title : js_string t prop
  method referrer : js_string t readonly_prop
  method domain : js_string t readonly_prop
  method _URL : js_string t readonly_prop
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
end

class type undoManager = object
end

class type selection = object
end

class type window = object
  method document : document t readonly_prop
  method name : js_string t prop
  method location : location t readonly_prop
  method history : history t readonly_prop
  method undoManager : undoManager t readonly_prop
  method getSelection : selection t meth
  method close : unit meth
  method stop : unit meth
  method focus : unit meth
  method blur : unit meth

  method top : window t readonly_prop
  method parent : window t readonly_prop
  method frameElement : element t opt readonly_prop

  method alert : js_string t -> unit meth
  method confirm : js_string t -> bool t meth
  method prompt : js_string t -> js_string t -> js_string t meth
  method print : unit meth

  method setInterval : (unit -> unit) Js.callback -> float -> interval_id meth
  method clearInterval : interval_id -> unit meth

  method setTimeout : (unit -> unit) Js.callback -> float -> timeout_id meth
  method clearTimeout : timeout_id -> unit meth

  method onload : (window t, event t) event_listener prop
  method onbeforeunload : (window t, event t) event_listener prop
  method onblur : (window t, event t) event_listener prop
  method onfocus : (window t, event t) event_listener prop
  method onresize : (window t, event t) event_listener prop
end

let window : window t = Js.Unsafe.variable "window"

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
let unsafeCreateElementEx ?_type ?name doc elt =
  if _type = None && name = None then
    Js.Unsafe.coerce (createElement doc elt)
  else if not onIE then begin
    let res = Js.Unsafe.coerce (createElement doc elt) in
    opt_iter _type (fun t -> res##_type <- t);
    opt_iter name (fun n -> res##name <- n);
    res
  end else begin
    let a = jsnew Js.array_empty () in
    ignore (a##push_2(Js.string "<", Js.string elt));
    opt_iter _type (fun t ->
      ignore (a##push_3(Js.string " type=\"", html_escape t, Js.string "\"")));
    opt_iter name (fun n ->
      ignore (a##push_3(Js.string " name=\"", html_escape n, Js.string "\"")));
    ignore (a##push(Js.string ">"));
    Js.Unsafe.coerce (doc##createElement (a##join (Js.string "")))
  end

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

exception Canvas_not_available

let createCanvas doc : canvasElement t =
  let c = unsafeCreateElement doc "canvas" in
  if not (Opt.test c##getContext) then raise Canvas_not_available;
  c

module CoerceTo = struct
  let element e : element Js.t Js.opt =
    if Js.instanceof e (Js.Unsafe.variable "HTMLElement") then
      Js.some (Js.Unsafe.coerce e)
    else
      Js.null

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
end

(****)

let eventTarget (e : #event t) =
  let target =
    Optdef.get (e##target) (fun () ->
    Optdef.get (e##srcElement) (fun () -> assert false))
  in
  (* Workaround for Safari bug *)
  if target##nodeType == Dom.TEXT then
    Js.Unsafe.coerce (Opt.get (target##parentNode) (fun () -> assert false))
  else
    target

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
  (truncate (Js.to_float r##left) - body##clientLeft - html##clientLeft,
   truncate (Js.to_float r##top) - body##clientTop - html##clientTop)

let getDocumentScroll () =
  let body = document##body in
  let html = document##documentElement in
  (body##scrollLeft + html##scrollLeft, body##scrollTop + html##scrollTop)

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

type taggedElement =
  | A of anchorElement t
  | Area of areaElement t
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
  | Other of element t

let tagged (e : #element t) =
  match Js.to_string (e##tagName##toLowerCase()) with
  | "a" -> A (Js.Unsafe.coerce e)
  | "area" -> Area (Js.Unsafe.coerce e)
  | "base" -> Base (Js.Unsafe.coerce e)
  | "blockquote" -> Blockquote (Js.Unsafe.coerce e)
  | "body" -> Body (Js.Unsafe.coerce e)
  | "br" -> Br (Js.Unsafe.coerce e)
  | "button" -> Button (Js.Unsafe.coerce e)
  | "canvas" -> Canvas (Js.Unsafe.coerce e)
  | "caption" -> Caption (Js.Unsafe.coerce e)
  | "col" -> Col (Js.Unsafe.coerce e)
  | "colgroup" -> Colgroup (Js.Unsafe.coerce e)
  | "del" -> Del (Js.Unsafe.coerce e)
  | "div" -> Div (Js.Unsafe.coerce e)
  | "dl" -> Dl (Js.Unsafe.coerce e)
  | "fieldset" -> Fieldset (Js.Unsafe.coerce e)
  | "form" -> Form (Js.Unsafe.coerce e)
  | "frameset" -> Frameset (Js.Unsafe.coerce e)
  | "frame" -> Frame (Js.Unsafe.coerce e)
  | "h1" -> H1 (Js.Unsafe.coerce e)
  | "h2" -> H2 (Js.Unsafe.coerce e)
  | "h3" -> H3 (Js.Unsafe.coerce e)
  | "h4" -> H4 (Js.Unsafe.coerce e)
  | "h5" -> H5 (Js.Unsafe.coerce e)
  | "h6" -> H6 (Js.Unsafe.coerce e)
  | "head" -> Head (Js.Unsafe.coerce e)
  | "hr" -> Hr (Js.Unsafe.coerce e)
  | "html" -> Html (Js.Unsafe.coerce e)
  | "iframe" -> Iframe (Js.Unsafe.coerce e)
  | "img" -> Img (Js.Unsafe.coerce e)
  | "input" -> Input (Js.Unsafe.coerce e)
  | "ins" -> Ins (Js.Unsafe.coerce e)
  | "label" -> Label (Js.Unsafe.coerce e)
  | "legend" -> Legend (Js.Unsafe.coerce e)
  | "li" -> Li (Js.Unsafe.coerce e)
  | "link" -> Link (Js.Unsafe.coerce e)
  | "map" -> Map (Js.Unsafe.coerce e)
  | "meta" -> Meta (Js.Unsafe.coerce e)
  | "object" -> Object (Js.Unsafe.coerce e)
  | "ol" -> Ol (Js.Unsafe.coerce e)
  | "optgroup" -> Optgroup (Js.Unsafe.coerce e)
  | "option" -> Option (Js.Unsafe.coerce e)
  | "p" -> P (Js.Unsafe.coerce e)
  | "param" -> Param (Js.Unsafe.coerce e)
  | "pre" -> Pre (Js.Unsafe.coerce e)
  | "q" -> Q (Js.Unsafe.coerce e)
  | "script" -> Script (Js.Unsafe.coerce e)
  | "select" -> Select (Js.Unsafe.coerce e)
  | "style" -> Style (Js.Unsafe.coerce e)
  | "table" -> Table (Js.Unsafe.coerce e)
  | "tbody" -> Tbody (Js.Unsafe.coerce e)
  | "td" -> Td (Js.Unsafe.coerce e)
  | "textarea" -> Textarea (Js.Unsafe.coerce e)
  | "tfoot" -> Tfoot (Js.Unsafe.coerce e)
  | "th" -> Th (Js.Unsafe.coerce e)
  | "thead" -> Thead (Js.Unsafe.coerce e)
  | "title" -> Title (Js.Unsafe.coerce e)
  | "tr" -> Tr (Js.Unsafe.coerce e)
  | "ul" -> Ul (Js.Unsafe.coerce e)
  | _   -> Other (e : #element t :> element t)

let opt_tagged e = Opt.case e (fun () -> None) (fun e -> Some (tagged e))
