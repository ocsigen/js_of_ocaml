(*
XXXX events
XXXX creation (input)
*)

open Js

module Dom = struct

  class type ['node] nodeList = object
    method item : int -> 'node t meth
    method length : int readonly_prop
  end

  class type node = object
    method nodeName : string t readonly_prop
    method nodeValue : string t opt readonly_prop
    method nodeType : int readonly_prop
    method parentNode : node t opt prop
    method childNodes : node nodeList t prop
    method firstChild : node t opt prop
    method lastChild : node t opt prop
    method previousSibling : node t opt prop
    method nextSibling : node t opt prop

    method insertBefore : node t -> node t opt -> node t meth
    method replaceChild : node t -> node t -> node t meth
    method removeChild : node t -> node t meth
    method appendChild : node t -> node t meth
    method hasChildNodes : bool t meth
    method cloneNode : bool t -> node t meth
  end

  let appendChild (p : #node t) (n : #node t) =
    ignore (p##appendChild ((n :> node t)))

  let removeChild (p : #node t) (n : #node t) =
    ignore (p##removeChild ((n :> node t)))

  let replaceChild (p : #node t) (n : #node t) (o : #node t) =
    ignore (p##replaceChild ((n :> node t), (o :> node t)))

  class type element = object
    inherit node
    method tagName : string t readonly_prop
    method getAttribute : string t -> string t meth
    method setAttribute : string t -> string t -> unit meth
    method removeAttribute : string t -> unit meth
    method hasAttribyte : string t -> bool t meth
  end

  class type characterData =
  object
    inherit node
    method data : string t prop
    method length : int readonly_prop
    method substringData : int -> int -> string t meth
    method appendData : string t -> unit meth
    method insertData : int -> string t -> unit meth
    method deleteData : int -> int -> unit meth
    method replaceData : int -> int -> string t meth
  end

  class type text = characterData

  class type documentFragment = node

  class type ['element] document = object
    inherit element
    method documentElement : 'element t readonly_prop
    method createDocumentFragment : documentFragment t meth
    method createElement : string t -> 'element t meth
    method createTextNode : string t -> text t meth
    method getElementById : string t -> 'element t opt meth
  end
end

module HTML = struct

  class type cssStyleDeclaration = object
    method background : string t prop
    method backgroundAttachment : string t prop
    method backgroundColor : string t prop
    method backgroundImage : string t prop
    method backgroundPosition : string t prop
    method backgroundRepeat : string t prop
    method border : string t prop
    method borderBottom : string t prop
    method borderBottomColor : string t prop
    method borderBottomStyle : string t prop
    method borderBottomWidth : string t prop
    method borderCollapse : string t prop
    method borderColor : string t prop
    method borderLeft : string t prop
    method borderLeftColor : string t prop
    method borderLeftStyle : string t prop
    method borderLeftWidth : string t prop
    method borderRight : string t prop
    method borderRightColor : string t prop
    method borderRightStyle : string t prop
    method borderRightWidth : string t prop
    method borderSpacing : string t prop
    method borderStyle : string t prop
    method borderTop : string t prop
    method borderTopColor : string t prop
    method borderTopStyle : string t prop
    method borderTopWidth : string t prop
    method borderWidth : string t prop
    method bottom : string t prop
    method captionSide : string t prop
    method clear : string t prop
    method clip : string t prop
    method color : string t prop
    method content : string t prop
    method counterIncrement : string t prop
    method counterReset : string t prop
    method cssText : string t prop
    method cursor : string t prop
    method direction : string t prop
    method display : string t prop
    method emptyCells : string t prop
    method font : string t prop
    method fontFamily : string t prop
    method fontSize : string t prop
    method fontStyle : string t prop
    method fontVariant : string t prop
    method fontWeight : string t prop
    method height : string t prop
    method left : string t prop
    method letterSpacing : string t prop
    method lineHeight : string t prop
    method listStyle : string t prop
    method listStyleImage : string t prop
    method listStylePosition : string t prop
    method listStyleType : string t prop
    method margin : string t prop
    method marginBottom : string t prop
    method marginLeft : string t prop
    method marginRight : string t prop
    method marginTop : string t prop
    method maxHeight : string t prop
    method maxWidth : string t prop
    method minHeight : string t prop
    method minWidth : string t prop
    method opacity : string t optdef prop
    method outline : string t prop
    method outlineColor : string t prop
    method outlineOffset : string t prop
    method outlineStyle : string t prop
    method outlineWidth : string t prop
    method overflow : string t prop
    method overflowX : string t prop
    method overflowY : string t prop
    method padding : string t prop
    method paddingBottom : string t prop
    method paddingLeft : string t prop
    method paddingRight : string t prop
    method paddingTop : string t prop
    method pageBreakAfter : string t prop
    method pageBreakBefore : string t prop
    method position : string t prop
    method right : string t prop
    method tableLayout : string t prop
    method textAlign : string t prop
    method textDecoration : string t prop
    method textIndent : string t prop
    method textTransform : string t prop
    method top : string t prop
    method verticalAlign : string t prop
    method visibility : string t prop
    method whiteSpace : string t prop
    method width : string t prop
    method wordSpacing : string t prop
    method zIndex : string t prop
  end

  class type event = object
    method _type : string t readonly_prop
    method target : element t opt readonly_prop
    method srcElement : element t opt readonly_prop
  end

  and mouseEvent = object
  end

  and element = object
    inherit Dom.element
    method id : string t prop
    method title : string t prop
    method lang : string t prop
    method dir : string t prop
    method className : string t prop
    method style : cssStyleDeclaration t prop

    method innerHTML : string t prop

    (* FIX: event? / might be undefined! *)
    method onclick : (unit -> bool t) opt prop
    method onmouseover : (unit -> bool t) opt prop
    method onmouseout : (unit -> bool t) opt prop
  end

(*XXX
  let event_target (e : #event t) =
    let targ =
      match Nullable.maybe e##target with
        Some t ->
          t
      | None ->
          match Nullable.maybe e##srcElement with
            Some t -> t
           | None  -> assert false
    in
    if targ##nodeType = 3 then targ##parentNode else targ
*)

  class type ['node] collection = object
    method length : int readonly_prop
    method item : int -> 'node t opt meth
    method namedItem : string t -> 'node t opt meth
  end

  class type htmlElement = element

  class type headElement = object
    inherit element
    method profile : string t prop
  end

  class type linkElement = object
    inherit element
    method disabled : bool t prop
    method charset : string t prop
    method href : string t prop
    method hreflang : string t prop
    method media : string t prop
    method rel : string t prop
    method rev : string t prop
    method target : string t prop
    method _type : string t prop
  end

  class type titleElement = object
    inherit element
    method text : string t prop
  end

  class type metaElement = object
    inherit element
    method content : string t prop
    method httpEquiv : string t prop
    method name : string t prop
    method scheme : string t prop
  end

  class type baseElement = object
    inherit element
    method href : string t prop
    method target : string t prop
  end

  class type styleElement = object
    inherit element
    method disabled : bool t prop
    method media : string t prop
    method _type : string t prop
  end

  class type bodyElement = element

  class type formElement = object
    inherit element
    method elements : element collection t readonly_prop
    method length : int readonly_prop
    method name : string t prop
    method acceptCharset : string t prop
    method action : string t prop
    method enctype : string t prop
    method _method : string t prop
    method target : string t prop
    method submit : unit meth
    method reset : unit meth
  end

  class type optGroupElement = object
    inherit element
    method disabled : bool t prop
    method label : string t prop
  end

  class type optionElement = object
    inherit element
    method form : formElement t opt readonly_prop
    method defaultSelected : bool t prop
    method text : string t readonly_prop
    method index : int readonly_prop
    method disabled : bool prop
    method label : string t prop
    method selected : bool prop
    method value : string t prop
  end

  class type selectElement = object
    inherit element
    method _type : string t readonly_prop
    method selectedIndex : int prop
    method value : string t prop
    method length : int prop
    method form : formElement t opt readonly_prop
    method options : optionElement collection t readonly_prop
    method disabled : bool t prop
    method multiple : bool t prop
    method name : string t prop
    method size : int prop
    method tabIndex : int prop
    method add : #element -> #element opt -> unit meth
    method remove : int -> unit meth
    method blur : unit meth
    method focus : unit meth

    method onchange : (unit -> bool t) opt prop
  end

  class type inputElement = object
    inherit element
    method defaultValue : string t prop
    method defaultChecked : string t prop
    method form : formElement opt readonly_prop
    method accept : string t prop
    method accessKey : string t prop
    method align : string t prop
    method alt : string t prop
    method checked : bool t prop
    method disabled : bool t prop
    method maxLength : int prop
    method name : string t readonly_prop (* Cannot be changed under IE *)
    method readOnly : bool t prop
    method size : int prop
    method src : string t prop
    method tabIndex : int prop
    method _type : string t prop (* FIX: Cannot be changed under IE *)
    method useMap : string t prop
    method value : string t prop
    method blur : unit meth
    method focus : unit meth
    method select : unit meth
    method click : unit meth

    method onchange : (unit -> bool t) opt prop
  end

  class type textAreaElement = object
    inherit element
    method defaultValue : string t prop
    method form : formElement t opt readonly_prop
    method accessKey : string t prop
    method cols : int prop
    method disabled : bool t prop
    method name : string t prop
    method readOnly : bool t prop
    method rows : int prop
    method tabIndex : int prop
    method _type : string t readonly_prop
    method value : string t prop
    method blur : unit meth
    method focus : unit meth
    method select : unit meth
  end

  class type buttonElement = object
    inherit element
    method form : formElement opt readonly_prop
    method accessKey : string t prop
    method disabled : bool t prop
    method name : string t prop
    method tabIndex : int prop
    method _type : string t readonly_prop
    method value : string t prop
  end

  class type labelElement = object
    inherit element
    method form : formElement t opt readonly_prop
    method accessKey : string t prop
    method htmlFor : string t prop
  end

  class type fieldSetElement = object
    inherit element
    method form : formElement t opt readonly_prop
  end

  class type legendElement = object
    inherit element
    method form : formElement t opt readonly_prop
    method accessKey : string t prop
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
    method cite : string t prop
  end

  class type preElement = element

  class type brElement = element

  class type hrElement = element

  class type modElement = object
    inherit element
    method cite : string t prop
    method dateTime : string t prop
  end

  class type anchorElement = object
    inherit element
    method accessKey : string t prop
    method charset : string t prop
    method coords : string t prop
    method href : string t prop
    method hreflang : string t prop
    method name : string t prop
    method rel : string t prop
    method rev : string t prop
    method shape : string t prop
    method tabIndex : int prop
    method target : string t prop
    method _type : string t prop
    method blur : unit meth
    method focus : unit meth
  end

  class type imageElement = object
    inherit element
    method alt : string t prop
    method height : int prop
    method isMap : bool t prop
    method longDesc : string t prop
    method src : string t prop
    method useMap : string t prop
    method width : int prop
  end

  class type objectElement = object
    inherit element
    method form : formElement t opt readonly_prop
    method code : string t prop
    method archive : string t prop
    method codeBase : string t prop
    method codeType : string t prop
    method data : string t prop
    method declare : bool t prop
    method height : string t prop
    method name : string t prop
    method standby : string t prop
    method tabIndex : int prop
    method _type : string t prop
    method useMap : string t prop
    method width : string t prop
    method document : Dom.element Dom.document t opt readonly_prop
  end

  class type paramElement = object
    inherit element
    method name : string t prop
    method _type : string t prop
    method value : string t prop
    method valueType : string t prop
  end

  class type areaElement = object
    inherit element
    method accessKey : string t prop
    method alt : string t prop
    method coords : string t prop
    method href : string t prop
    method noHref : bool t prop
    method shape : string t prop
    method tabIndex : int prop
    method target : string t prop
  end

  class type mapElement = object
    inherit element
    method areas : areaElement collection t readonly_prop
    method name : string t prop
  end

  class type scriptElement = object
    inherit element
    method text : string t prop
    method charset : string t prop
    method defer : bool t prop
    method src : string t prop
    method _type : string t prop
  end

  class type tableCellElement = object
    inherit element
    method cellIndex : int readonly_prop
    method abbr : string t prop
    method align : string t prop
    method axis : string t prop
    method ch : string t prop
    method chOff : string t prop
    method colSpan : int prop
    method headers : string t prop
    method rowSpan : int prop
    method scope : string t prop
    method vAlign : string t prop
  end

  class type tableRowElement = object
    inherit element
    method rowIndex : int readonly_prop
    method sectionRowIndex : int readonly_prop
    method cells : tableCellElement collection t readonly_prop
    method align : string t prop
    method ch : string t prop
    method chOff : string t prop
    method vAlign : string t prop
    method insertCell : int -> tableCellElement t meth
    method deleteCell : int -> unit meth
  end

  class type tableColElement = object
    inherit element
    method align : string t prop
    method ch : string t prop
    method chOff : string t prop
    method span : int prop
    method vAlign : string t prop
    method width : string t prop
  end

  class type tableSectionElement = object
    inherit element
    method align : string t prop
    method ch : string t prop
    method chOff : string t prop
    method vAlign : string t prop
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
    method align : string t prop
    method border : string t prop
    method cellPadding : string t prop
    method cellSpacing : string t prop
    method frame : string t prop
    method rules : string t prop
    method summary : string t prop
    method width : string t prop
    method createTHead : tableSectionElement t meth
    method deleteTHead : unit meth
    method createTFoot : tableSectionElement t meth
    method deleteTFoot : unit meth
    method createCaption : tableCaptionElement t meth
    method deleteCaption : unit meth
    method insertRow : int -> tableRowElement t meth
    method deleteRow : int -> unit meth
  end

  let _2d_ = Js.string "2d"  (*FIX: use singleton type*)

  type canvasPattern

  class type canvasElement = object
    inherit element
    method width : int prop
    method height : int prop
    method toDataURL : string t meth
    method getContext : string t -> canvasRenderingContext2D t meth
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
    method globalCompositeOperation : string t prop
    method strokeStyle : string t writeonly_prop
    method strokeStyle_gradient : canvasGradient t writeonly_prop
    method strokeStyle_pattern : canvasPattern t writeonly_prop
    method fillStyle : string t writeonly_prop
    method fillStyle_gradient : canvasGradient t writeonly_prop
    method fillStyle_pattern : canvasPattern t writeonly_prop
    method createLinearGradient :
      float -> float -> float -> float -> canvasGradient t meth
    method createRadialGradient :
      float -> float -> float -> float -> float -> float ->
      canvasGradient t meth
    method createPattern : imageElement t -> string t -> canvasPattern t meth
    method createPattern_fromCanvas :
      canvasElement t -> string t -> canvasPattern t meth
(*
  CanvasPattern createPattern(in HTMLVideoElement image, in DOMString repetition);
*)
    method lineWidth : float prop
    method lineCap : string t prop
    method lineJoin : string t prop
    method miterLimit : float prop

    method shadowOffsetX : float prop
    method shadowOffsetY : float prop
    method shadowBlur : float prop
    method shadowColor : string t prop

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

    method drawFocusRing : element t -> float -> float -> bool t -> bool t meth

    method font : string t prop
    method textAlign : string t prop
    method textBaseline : string t prop
    method fillText : string t -> float -> float -> float opt -> unit meth
    method strokeText : string t -> float -> float -> float opt -> unit meth
    method measureText : string t -> textMetrics t meth

    method drawImage :
      imageElement t -> float -> float -> unit meth
    method drawImage_scale :
      imageElement t -> float -> float -> float -> float -> unit meth
    method drawImage_full :
      imageElement t -> float -> float -> float -> float -> unit meth
    method drawImage_fromCanvas :
      canvasElement t -> float -> float -> unit meth
    method drawImage_scaleFromCanvas :
      canvasElement t -> float -> float -> float -> float -> unit meth
    method drawImage_fullFromCanvas :
      canvasElement t -> float -> float -> float -> float -> unit meth
(*
  void drawImage(in HTMLVideoElement image, in float dx, in float dy, in optional float dw, in float dh);
  void drawImage(in HTMLVideoElement image, in float sx, in float sy, in float sw, in float sh, in float dx, in float dy, in float dw, in float dh);
*)

    method createImageData : int -> int -> imageData t meth
    method getImageData : float -> float -> float -> float -> imageData t meth
    method putImageData : imageData t -> float -> float -> unit meth
  end

  and canvasGradient = object
    method addColorStop : float -> string t -> unit meth
  end

  and textMetrics = object
    method width : float readonly_prop
  end

  and imageData = object
    method width : int readonly_prop
    method height : int readonly_prop
    method data : canvasPixelArray t prop
  end

  and canvasPixelArray = object
    method length : int readonly_prop
    (*XXX Fix: getter/setter *)
  end

  class type document = object
    inherit [element] Dom.document
    method title : string t prop
    method referrer : string t readonly_prop
    method domain : string t readonly_prop
    method _URL : string t readonly_prop
    method body : element t prop
    method images : imageElement collection t readonly_prop
    method applets : element collection t readonly_prop
    method links : element collection t readonly_prop
    method forms : formElement collection t readonly_prop
    method anchors : element collection t readonly_prop
    method cookie : string t prop
  end

(*XXX Creation functions a la lablgtk... *)
  let unsafeCreateElement (doc : document t) name =
    Js.Unsafe.coerce (doc##createElement(JsString.of_string name))

  let createHtmlElement doc : htmlElement t = unsafeCreateElement doc "html"
  let createHeadElement doc : headElement t = unsafeCreateElement doc "head"
  let createLinkElement doc : linkElement t = unsafeCreateElement doc "link"
  let createTitleElement doc : titleElement t = unsafeCreateElement doc "title"
  let createMetaElement doc : metaElement t = unsafeCreateElement doc "meta"
  let createBaseElement doc : baseElement t = unsafeCreateElement doc "base"
  let createStyleElement doc : styleElement t = unsafeCreateElement doc "style"
  let createBodyElement doc : bodyElement t = unsafeCreateElement doc "body"
  let createFormElement doc : formElement t = unsafeCreateElement doc "form"
  let createOptGroupElement doc : optGroupElement t =
    unsafeCreateElement doc "optgroup"
  let createOptionElement doc : optionElement t =
    unsafeCreateElement doc "option"
  let createSelectElement doc : selectElement t =
    unsafeCreateElement doc "select"
(*XXX should set name and type here... *)
  let createInputElement doc : inputElement t = unsafeCreateElement doc "input"
  let createTextAreaElement doc : textAreaElement t =
    unsafeCreateElement doc "textArea"
  let createButtonElement doc : buttonElement t =
    unsafeCreateElement doc "button"
  let createLabelElement doc : labelElement t = unsafeCreateElement doc "label"
  let createFieldSetElement doc : fieldSetElement t =
    unsafeCreateElement doc "fieldSet"
  let createLegendElement doc : legendElement t =
    unsafeCreateElement doc "legend"
  let createUlElement doc : uListElement t = unsafeCreateElement doc "ul"
  let createOlElement doc : oListElement t = unsafeCreateElement doc "ol"
  let createDlElement doc : dListElement t = unsafeCreateElement doc "dl"
  let createLiElement doc : liElement t = unsafeCreateElement doc "li"
  let createDivElement doc : divElement t = unsafeCreateElement doc "div"
  let createParagraphElement doc : paragraphElement t =
    unsafeCreateElement doc "p"
  let createH1Element doc : headingElement t = unsafeCreateElement doc "h1"
  let createH2Element doc : headingElement t = unsafeCreateElement doc "h2"
  let createH3Element doc : headingElement t = unsafeCreateElement doc "h3"
  let createH4Element doc : headingElement t = unsafeCreateElement doc "h4"
  let createH5Element doc : headingElement t = unsafeCreateElement doc "h5"
  let createH6Element doc : headingElement t = unsafeCreateElement doc "h6"
  let createQElement doc : quoteElement t = unsafeCreateElement doc "q"
  let createBlockquoteElement doc : quoteElement t =
    unsafeCreateElement doc "blockquote"
  let createPreElement doc : preElement t = unsafeCreateElement doc "pre"
  let createBrElement doc : brElement t = unsafeCreateElement doc "br"
  let createHrElement doc : hrElement t = unsafeCreateElement doc "hr"
  let createInsElement doc : modElement t = unsafeCreateElement doc "ins"
  let createDelElement doc : modElement t = unsafeCreateElement doc "del"
  let createAnchorElement doc : anchorElement t = unsafeCreateElement doc "a"
  let createImageElement doc : imageElement t = unsafeCreateElement doc "img"
  let createObjectElement doc : objectElement t =
    unsafeCreateElement doc "object"
  let createParamElement doc : paramElement t = unsafeCreateElement doc "param"
  let createMapElement doc : mapElement t = unsafeCreateElement doc "map"
  let createAreaElement doc : areaElement t = unsafeCreateElement doc "area"
  let createScriptElement doc : scriptElement t =
    unsafeCreateElement doc "script"
  let createTableElement doc : tableElement t = unsafeCreateElement doc "table"
  let createTableCaptionElement doc : tableCaptionElement t =
    unsafeCreateElement doc "caption"
  let createTableColElement doc : tableColElement t =
    unsafeCreateElement doc "col"
  let createTableColgroupElement doc : tableColElement t =
    unsafeCreateElement doc "colgroup"
  let createTHeadElement doc : tableSectionElement t =
    unsafeCreateElement doc "thead"
  let createTFootElement doc : tableSectionElement t =
    unsafeCreateElement doc "tfoot"
  let createTBodyElement doc : tableSectionElement t =
    unsafeCreateElement doc "tbody"
  let createTableRowElement doc : tableRowElement t =
    unsafeCreateElement doc "tr"
  let createThElement doc : tableCellElement t =
    unsafeCreateElement doc "th"
  let createTdElement doc : tableCellElement t =
    unsafeCreateElement doc "td"

  let createCanvasElement doc : canvasElement t =
    unsafeCreateElement doc "canvas"

  type interval_id
  type timeout_id

  class type location = object
    method hash : string t prop
    method host : string t prop
    method hostname : string t prop
    method href : string t prop
    method pathname : string t prop
    method protocol : string t prop
    method search : string t prop

    method reload : unit meth
    method assign : string t -> unit meth
    method replace : string t -> unit meth
    method toString : string t meth
  end

  class type history = object
  end

  class type undoManager = object
  end

  class type selection = object
  end

  class type window = object
    method document : document t readonly_prop
    method name : string t prop
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

    method onload : (unit -> unit) prop
    method onbeforeunload : (unit -> string t) prop

    method alert : string t -> unit meth
    method confirm : string t -> bool t meth
    method prompt : string t -> string t -> string t meth
    method print : unit meth

    method setInterval : (unit -> unit) -> float -> interval_id meth
    method clearInterval : interval_id -> unit meth

    method setTimeout : (unit -> unit) -> float -> timeout_id meth
    method clearTimeout : timeout_id -> unit meth
  end

  let window : window t = Js.Unsafe.variable "window"
  let document : document t = Js.Unsafe.variable "document"

end

module XMLHttpRequest = struct

  type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

  class type xmlHttpRequest = object
    method onreadystatechange : (unit -> unit) prop
    method readyState : readyState readonly_prop
    method _open :
      string t -> string t -> bool t ->
      string t opt -> string t opt -> unit meth
    method setRequestHeader : string t -> string t -> unit meth
    method send : string t opt -> unit meth
    method _send : #Dom.element #Dom.document -> unit meth (* overloading! *)
(*
  void send(Document data);
  void send([AllowAny] DOMString? data);
*)
    method abort : unit meth
    method status : int readonly_prop
    method statusText : string t readonly_prop
    method getResponseHeader : string t -> string t meth
    method getAllResponseHeaders : string t meth
    method responseText : string t readonly_prop
    method responseXML : Dom.element Dom.document t readonly_prop
  end

  external create : unit -> xmlHttpRequest t = "createXMLHTTPObject"

  let js = JsString.of_string

  let send_request url callback postData =
    let req = create () in
    let meth = js (if postData = null then "GET" else "POST") in
    req##_open (meth, url, Js._true, null, null);
    req##setRequestHeader (js"User-Agent", js"XMLHTTP/1.0");
    Opt.iter postData
      (fun d -> req##setRequestHeader
                  (js"Content-type",js"application/x-www-form-urlencoded"));
    req##onreadystatechange <-
      (fun () ->
         if
           req##readyState = DONE && (req##status = 200 || req##status = 304)
         then
           callback req);
    if req##readyState <> DONE then req##send (postData)

end
