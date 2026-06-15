
# Module `Js_of_ocaml.Dom_html`

DOM HTML binding

This is a partial binding to the DOM HTML API.


### CSS style declaration

```ocaml
class type  cssStyleDeclaration = object ... end
```

### Events

```ocaml
type (-'a, -'b) event_listener = ('a, 'b) Dom.event_listener
```
The type of event listener functions. The first type parameter `'a` is the type of the target object; the second parameter `'b` is the type of the event object.

```ocaml
type mouse_button = 
  | No_button
  | Left_button
  | Middle_button
  | Right_button
```
```ocaml
type delta_mode = 
  | Delta_pixel
  | Delta_line
  | Delta_page
```
```ocaml
type domStringMap
```
`DOMStringMap` used for the `dataset` property. Use the [`DomStringMap`](./Js_of_ocaml-Dom_html-DomStringMap.md) module for typed access.

```ocaml
class type  event = object ... end
```
```ocaml
class type 'a customEvent = object ... end
```
```ocaml
class type  focusEvent = object ... end
```
```ocaml
class type  mouseEvent = object ... end
```
```ocaml
class type  keyboardEvent = object ... end
```
```ocaml
class type  wheelEvent = object ... end
```
```ocaml
class type  mousewheelEvent = wheelEvent
```
```ocaml
class type  mouseScrollEvent = object ... end
```
```ocaml
class type  touchEvent = object ... end
```
```ocaml
class type  touchList = object ... end
```
```ocaml
class type  touch = object ... end
```
```ocaml
class type  submitEvent = object ... end
```
```ocaml
class type  dragEvent = object ... end
```
```ocaml
class type  clipboardEvent = object ... end
```
```ocaml
class type  toggleEvent = object ... end
```
```ocaml
class type  mediaQueryListEvent = object ... end
```
```ocaml
class type  dataTransfer = object ... end
```
```ocaml
class type  eventTarget = object ... end
```
Common properties of event target objects: `onclick`, `onkeypress`, ...

```ocaml
class type  popStateEvent = object ... end
```
```ocaml
class type  pointerEvent = object ... end
```
```ocaml
class type  storageEvent = object ... end
```
```ocaml
class type  storage = object ... end
```
Storage

```ocaml
class type  hashChangeEvent = object ... end
```
```ocaml
class type  animationEvent = object ... end
```
```ocaml
class type  transitionEvent = object ... end
```
```ocaml
class type  mediaEvent = object ... end
```
```ocaml
class type  messageEvent = object ... end
```
```ocaml
class type  staticRange = object ... end
```
```ocaml
class type  compositionEvent = object ... end
```
```ocaml
class type  inputEvent = object ... end
```
```ocaml
class type  errorEvent = object ... end
```
```ocaml
class type  progressEvent = object ... end
```
```ocaml
class type  beforeUnloadEvent = object ... end
```
```ocaml
class type  pageTransitionEvent = object ... end
```

### HTML elements

```ocaml
class type  nodeSelector = object ... end
```
```ocaml
class type  tokenList = object ... end
```
```ocaml
class type  shadowRootInit = object ... end
```
```ocaml
class type  shadowRoot = object ... end
```
```ocaml
class type  animation = object ... end
```
```ocaml
class type  animationTimeline = object ... end
```
```ocaml
class type  documentTimeline = object ... end
```
```ocaml
class type  animationEffect = object ... end
```
```ocaml
class type  keyframeEffect = object ... end
```
```ocaml
class type  computedKeyframe = object ... end
```
```ocaml
class type  optionalEffectTiming = object ... end
```
```ocaml
class type  computedEffectTiming = object ... end
```
```ocaml
class type  keyframeAnimationOptions = object ... end
```
```ocaml
class type  animationPlaybackEvent = object ... end
```
```ocaml
class type  scrollToOptions = object ... end
```
```ocaml
class type  scrollIntoViewOptions = object ... end
```
```ocaml
class type  focusOptions = object ... end
```
```ocaml
class type  showPopover_options = object ... end
```
```ocaml
class type  togglePopover_options = object ... end
```
```ocaml
class type  element = object ... end
```
Properties common to all HTML elements

```ocaml
class type  clientRect = object ... end
```
Rectangular box (used for element bounding boxes)

```ocaml
class type  clientRectList = object ... end
```
```ocaml
class type 'node collection = 'node Js_of_ocaml__.Dom.collection
```
Collection of HTML elements. Alias for [`Dom.collection`](./Js_of_ocaml-Dom-class-type-collection.md).

```ocaml
class type  htmlElement = element
```
```ocaml
class type  headElement = object ... end
```
```ocaml
class type  linkElement = object ... end
```
```ocaml
class type  titleElement = object ... end
```
```ocaml
class type  metaElement = object ... end
```
```ocaml
class type  baseElement = object ... end
```
```ocaml
class type  styleElement = object ... end
```
```ocaml
class type  bodyElement = element
```
```ocaml
class type  validityState = object ... end
```
Result of the constraint validation API on form-associated elements.

```ocaml
class type  submitterElement = object ... end
```
```ocaml
class type  formElement = object ... end
```
```ocaml
class type  labelElement = object ... end
```
```ocaml
class type  optGroupElement = object ... end
```
```ocaml
class type  optionElement = object ... end
```
```ocaml
class type  dataListElement = object ... end
```
```ocaml
class type  selectElement = object ... end
```
```ocaml
class type  inputElement = object ... end
```
```ocaml
class type  textAreaElement = object ... end
```
```ocaml
class type  buttonElement = object ... end
```
```ocaml
class type  fieldSetElement = object ... end
```
```ocaml
class type  legendElement = object ... end
```
```ocaml
class type  outputElement = object ... end
```
```ocaml
class type  progressElement = object ... end
```
```ocaml
class type  meterElement = object ... end
```
```ocaml
class type  templateElement = object ... end
```
```ocaml
class type  slotElement = object ... end
```
```ocaml
class type  pictureElement = element
```
```ocaml
class type  sourceElement = object ... end
```
```ocaml
type trackReadyState = 
  | TRACK_NONE
  | TRACK_LOADING
  | TRACK_LOADED
  | TRACK_ERROR
```
```ocaml
class type  trackElement = object ... end
```
```ocaml
class type  timeElement = object ... end
```
```ocaml
class type  dataElement = object ... end
```
```ocaml
class type  uListElement = element
```
```ocaml
class type  oListElement = element
```
```ocaml
class type  dListElement = element
```
```ocaml
class type  liElement = element
```
```ocaml
class type  dialogElement = object ... end
```
```ocaml
class type  divElement = element
```
```ocaml
class type  paragraphElement = element
```
```ocaml
class type  headingElement = element
```
```ocaml
class type  quoteElement = object ... end
```
```ocaml
class type  preElement = element
```
```ocaml
class type  brElement = element
```
```ocaml
class type  hrElement = element
```
```ocaml
class type  modElement = object ... end
```
```ocaml
class type  anchorElement = object ... end
```
```ocaml
class type  detailsElement = object ... end
```
```ocaml
class type  imageElement = object ... end
```
```ocaml
class type  objectElement = object ... end
```
```ocaml
class type  paramElement = object ... end
```
```ocaml
class type  areaElement = object ... end
```
```ocaml
class type  mapElement = object ... end
```
```ocaml
class type  scriptElement = object ... end
```
```ocaml
class type  embedElement = object ... end
```
```ocaml
class type  tableCellElement = object ... end
```
```ocaml
class type  tableRowElement = object ... end
```
```ocaml
class type  tableColElement = object ... end
```
```ocaml
class type  tableSectionElement = object ... end
```
```ocaml
class type  tableCaptionElement = element
```
```ocaml
class type  tableElement = object ... end
```
```ocaml
class type  timeRanges = object ... end
```
```ocaml
type networkState = 
  | NETWORK_EMPTY
  | NETWORK_IDLE
  | NETWORK_LOADING
  | NETWORK_NO_SOURCE
```
```ocaml
type readyState = 
  | HAVE_NOTHING
  | HAVE_METADATA
  | HAVE_CURRENT_DATA
  | HAVE_FUTURE_DATA
  | HAVE_ENOUGH_DATA
```
```ocaml
class type  mediaElement = object ... end
```
```ocaml
class type  audioElement = object ... end
```
```ocaml
class type  videoElement = object ... end
```

### Canvas object

```ocaml
type context
```
```ocaml
val _2d_ : context
```
```ocaml
type canvasPattern
```
```ocaml
class type  canvasElement = object ... end
```
```ocaml
class type  canvasRenderingContext2D = object ... end
```
```ocaml
class type  canvasGradient = object ... end
```
```ocaml
class type  textMetrics = object ... end
```
```ocaml
class type  imageData = object ... end
```
```ocaml
class type  canvasPixelArray = object ... end
```
```ocaml
val pixel_get : canvasPixelArray Js.t -> int -> int
```
```ocaml
val pixel_set : canvasPixelArray Js.t -> int -> int -> unit
```
```ocaml
type boundary_points_comparison = 
  | Start_to_start
  | Start_to_end
  | End_to_end
  | End_to_start
```
```ocaml
class type  range = object ... end
```
Object representing a range \*

```ocaml
class type  selection = object ... end
```
Information on current selection


### Document objects

```ocaml
class type  document = object ... end
```
```ocaml
val document : document Js.t
```
The current document

```ocaml
val getElementById_opt : string -> element Js.t option
```
`getElementById_opt id` returns the element with the id `id` in the current document. It returns `None` if there are no such element

```ocaml
val getElementById_exn : string -> element Js.t
```
`getElementById_exn id` returns the element with the id `id` in the current document. It raises if there are no such element

```ocaml
val getElementById_coerce : string -> (element Js.t -> 'a Js.opt) -> 'a option
```
`getElementById_coerce id coerce` returns the element with the id `id` in the current document and attempt to coerce it using the provided `coerce` function. It returns `None` if there are no such element or if the `coerce` function returns `Js.none`. Typical usage is the following:

```ocaml
  match Dom_html.getElementById_coerce "myinput" Dom_html.CoerceTo.input with
  | None -> ..
  | Some input -> ..
```
```ocaml
val getElementById : string -> element Js.t
```
`getElementById id` returns the element with the id `id` in the current document. It raises `Not_found` if there are no such element


### Window objects

```ocaml
class type  location = object ... end
```
Location information

```ocaml
val location_origin : location Js.t -> Js.js_string Js.t
```
```ocaml
class type  history = object ... end
```
Browser history information

```ocaml
class type  undoManager = object ... end
```
Undo manager

```ocaml
class type  navigator = object ... end
```
Navigator information

```ocaml
class type  screen = object ... end
```
```ocaml
class type  applicationCache = object ... end
```
```ocaml
type interval_id
```
```ocaml
type timeout_id
```
```ocaml
type animation_frame_request_id
```
```ocaml
class type  _URL = object ... end
```
```ocaml
class type  mediaQueryList = object ... end
```
```ocaml
class type  windowPostMessageOptions = object ... end
```
Options for `Window.postMessage`.

```ocaml
class type  window = object ... end
```
Specification of window objects

```ocaml
val window : window Js.t
```
The current window

```ocaml
class type  frameSetElement = object ... end
```
```ocaml
class type  frameElement = object ... end
```
```ocaml
class type  iFrameElement = object ... end
```

### Event handlers

```ocaml
val no_handler : ('a, 'b) event_listener
```
see `Dom.no_handler`

```ocaml
val handler : ((event Js.t as 'b) -> bool Js.t) -> ('a, 'b) event_listener
```
see `Dom.handler`

```ocaml
val full_handler : 
  ('a -> (event Js.t as 'b) -> bool Js.t) ->
  ('a, 'b) event_listener
```
see `Dom.full_handler`

```ocaml
val listener : ((event Js.t as 'b) -> unit) -> ('a, 'b) event_listener
```
see `Dom.listener`

```ocaml
val full_listener : 
  ('a -> (event Js.t as 'b) -> unit) ->
  ('a, 'b) event_listener
```
see `Dom.full_listener`

```ocaml
val invoke_handler : ('a, 'b) event_listener -> 'a -> 'b -> bool Js.t
```
see `Dom.invoke_handler`

```ocaml
val eventTarget : event Js.t -> element Js.t
```
see `Dom.eventTarget`

```ocaml
val eventRelatedTarget : mouseEvent Js.t -> element Js.t Js.opt
```
Returns this event related target.

```ocaml
module Event : sig ... end
```
Event types: `mousedown`, `keypress`, ...

```ocaml
type event_listener_id = Dom.event_listener_id
```
```ocaml
val addEventListenerWithOptions : 
  (eventTarget Js.t as 'a) ->
  'b Event.typ ->
  ?capture:bool Js.t ->
  ?once:bool Js.t ->
  ?passive:bool Js.t ->
  ('a, 'b) event_listener ->
  event_listener_id
```
Add an event listener. This function matches the option-object variant of the `addEventListener` DOM method, except that it returns an id for removing the listener.

```ocaml
val addEventListener : 
  (eventTarget Js.t as 'a) ->
  'b Event.typ ->
  ('a, 'b) event_listener ->
  bool Js.t ->
  event_listener_id
```
Add an event listener. This function matches the useCapture boolean variant of the `addEventListener` DOM method, except that it returns an id for removing the listener.

```ocaml
val removeEventListener : event_listener_id -> unit
```
Remove the given event listener.

```ocaml
val addMousewheelEventListenerWithOptions : 
  eventTarget Js.t ->
  ?capture:bool Js.t ->
  ?once:bool Js.t ->
  ?passive:bool Js.t ->
  (mouseEvent Js.t -> dx:int -> dy:int -> bool Js.t) ->
  event_listener_id
```
Add a wheel event listener with option-object variant of the `addEventListener` DOM method. The callback is provided the event and the numbers of ticks the mouse wheel moved. Positive means down / right.

```ocaml
val addMousewheelEventListener : 
  eventTarget Js.t ->
  (mouseEvent Js.t -> dx:int -> dy:int -> bool Js.t) ->
  bool Js.t ->
  event_listener_id
```
Add a wheel event listener with the useCapture boolean variant of the `addEventListener` DOM method. The callback is provided the event and the numbers of ticks the mouse wheel moved. Positive means down / right.

```ocaml
val createCustomEvent : 
  ?bubbles:bool ->
  ?cancelable:bool ->
  ?detail:'a ->
  'a customEvent Js.t Event.typ ->
  'a customEvent Js.t
```
See `Dom.createCustomEvent`


### Mouse event helper functions

```ocaml
val buttonPressed : mouseEvent Js.t -> mouse_button
```

### Position helper functions

```ocaml
val eventAbsolutePosition : mouseEvent Js.t -> float * float
```
Returns the absolute position of the mouse pointer.

```ocaml
val elementClientPosition : element Js.t -> int * int
```
Position of an element relative to the viewport

```ocaml
val getDocumentScroll : unit -> float * float
```
Viewport top/left position


### Key event helper functions

```ocaml
module Keyboard_code : sig ... end
```
Use `Keyboard_code` when you want to identify a key that the user pressed. This should be invoked for keydown and keyup events, not keypress events.

```ocaml
module Keyboard_key : sig ... end
```
Use `Keyboard_key` when you want to identify the character that the user typed. This should only be invoked on keypress events, not keydown or keyup events.


### Helper functions for creating HTML elements

```ocaml
val createHtml : document Js.t -> htmlElement Js.t
```
```ocaml
val createHead : document Js.t -> headElement Js.t
```
```ocaml
val createLink : document Js.t -> linkElement Js.t
```
```ocaml
val createTitle : document Js.t -> titleElement Js.t
```
```ocaml
val createMeta : document Js.t -> metaElement Js.t
```
```ocaml
val createBase : document Js.t -> baseElement Js.t
```
```ocaml
val createStyle : document Js.t -> styleElement Js.t
```
```ocaml
val createBody : document Js.t -> bodyElement Js.t
```
```ocaml
val createForm : document Js.t -> formElement Js.t
```
```ocaml
val createOptgroup : document Js.t -> optGroupElement Js.t
```
```ocaml
val createOption : document Js.t -> optionElement Js.t
```
```ocaml
val createSelect : 
  ?_type:Js.js_string Js.t ->
  ?name:Js.js_string Js.t ->
  document Js.t ->
  selectElement Js.t
```
```ocaml
val createInput : 
  ?_type:Js.js_string Js.t ->
  ?name:Js.js_string Js.t ->
  document Js.t ->
  inputElement Js.t
```
```ocaml
val createTextarea : 
  ?_type:Js.js_string Js.t ->
  ?name:Js.js_string Js.t ->
  document Js.t ->
  textAreaElement Js.t
```
```ocaml
val createButton : 
  ?_type:Js.js_string Js.t ->
  ?name:Js.js_string Js.t ->
  document Js.t ->
  buttonElement Js.t
```
```ocaml
val createLabel : document Js.t -> labelElement Js.t
```
```ocaml
val createFieldset : document Js.t -> fieldSetElement Js.t
```
```ocaml
val createLegend : document Js.t -> legendElement Js.t
```
```ocaml
val createUl : document Js.t -> uListElement Js.t
```
```ocaml
val createOl : document Js.t -> oListElement Js.t
```
```ocaml
val createDl : document Js.t -> dListElement Js.t
```
```ocaml
val createLi : document Js.t -> liElement Js.t
```
```ocaml
val createDialog : document Js.t -> dialogElement Js.t
```
```ocaml
val createDiv : document Js.t -> divElement Js.t
```
```ocaml
val createEmbed : document Js.t -> embedElement Js.t
```
```ocaml
val createP : document Js.t -> paragraphElement Js.t
```
```ocaml
val createH1 : document Js.t -> headingElement Js.t
```
```ocaml
val createH2 : document Js.t -> headingElement Js.t
```
```ocaml
val createH3 : document Js.t -> headingElement Js.t
```
```ocaml
val createH4 : document Js.t -> headingElement Js.t
```
```ocaml
val createH5 : document Js.t -> headingElement Js.t
```
```ocaml
val createH6 : document Js.t -> headingElement Js.t
```
```ocaml
val createQ : document Js.t -> quoteElement Js.t
```
```ocaml
val createBlockquote : document Js.t -> quoteElement Js.t
```
```ocaml
val createPre : document Js.t -> preElement Js.t
```
```ocaml
val createBr : document Js.t -> brElement Js.t
```
```ocaml
val createHr : document Js.t -> hrElement Js.t
```
```ocaml
val createIns : document Js.t -> modElement Js.t
```
```ocaml
val createDel : document Js.t -> modElement Js.t
```
```ocaml
val createA : document Js.t -> anchorElement Js.t
```
```ocaml
val createImg : document Js.t -> imageElement Js.t
```
```ocaml
val createObject : document Js.t -> objectElement Js.t
```
```ocaml
val createParam : document Js.t -> paramElement Js.t
```
```ocaml
val createMap : document Js.t -> mapElement Js.t
```
```ocaml
val createArea : document Js.t -> areaElement Js.t
```
```ocaml
val createScript : document Js.t -> scriptElement Js.t
```
```ocaml
val createTable : document Js.t -> tableElement Js.t
```
```ocaml
val createCaption : document Js.t -> tableCaptionElement Js.t
```
```ocaml
val createCol : document Js.t -> tableColElement Js.t
```
```ocaml
val createColgroup : document Js.t -> tableColElement Js.t
```
```ocaml
val createThead : document Js.t -> tableSectionElement Js.t
```
```ocaml
val createTfoot : document Js.t -> tableSectionElement Js.t
```
```ocaml
val createTbody : document Js.t -> tableSectionElement Js.t
```
```ocaml
val createTr : document Js.t -> tableRowElement Js.t
```
```ocaml
val createTh : document Js.t -> tableCellElement Js.t
```
```ocaml
val createTd : document Js.t -> tableCellElement Js.t
```
```ocaml
val createSub : document Js.t -> element Js.t
```
```ocaml
val createSup : document Js.t -> element Js.t
```
```ocaml
val createSpan : document Js.t -> element Js.t
```
```ocaml
val createTt : document Js.t -> element Js.t
```
```ocaml
val createI : document Js.t -> element Js.t
```
```ocaml
val createB : document Js.t -> element Js.t
```
```ocaml
val createBig : document Js.t -> element Js.t
```
```ocaml
val createSmall : document Js.t -> element Js.t
```
```ocaml
val createEm : document Js.t -> element Js.t
```
```ocaml
val createStrong : document Js.t -> element Js.t
```
```ocaml
val createCite : document Js.t -> element Js.t
```
```ocaml
val createDfn : document Js.t -> element Js.t
```
```ocaml
val createCode : document Js.t -> element Js.t
```
```ocaml
val createSamp : document Js.t -> element Js.t
```
```ocaml
val createKbd : document Js.t -> element Js.t
```
```ocaml
val createVar : document Js.t -> element Js.t
```
```ocaml
val createAbbr : document Js.t -> element Js.t
```
```ocaml
val createDd : document Js.t -> element Js.t
```
```ocaml
val createDt : document Js.t -> element Js.t
```
```ocaml
val createNoscript : document Js.t -> element Js.t
```
```ocaml
val createAddress : document Js.t -> element Js.t
```
```ocaml
val createFrameset : document Js.t -> frameSetElement Js.t
```
```ocaml
val createFrame : document Js.t -> frameElement Js.t
```
```ocaml
val createIframe : document Js.t -> iFrameElement Js.t
```
```ocaml
val createAudio : document Js.t -> audioElement Js.t
```
```ocaml
val createVideo : document Js.t -> videoElement Js.t
```
```ocaml
val createOutput : document Js.t -> outputElement Js.t
```
```ocaml
val createProgress : document Js.t -> progressElement Js.t
```
```ocaml
val createMeter : document Js.t -> meterElement Js.t
```
```ocaml
val createDatalist : document Js.t -> dataListElement Js.t
```
```ocaml
val createTemplate : document Js.t -> templateElement Js.t
```
```ocaml
val createSlot : document Js.t -> slotElement Js.t
```
```ocaml
val createPicture : document Js.t -> pictureElement Js.t
```
```ocaml
val createSource : document Js.t -> sourceElement Js.t
```
```ocaml
val createTrack : document Js.t -> trackElement Js.t
```
```ocaml
val createTime : document Js.t -> timeElement Js.t
```
```ocaml
val createData : document Js.t -> dataElement Js.t
```
```ocaml
exception Canvas_not_available
```
```ocaml
val createCanvas : document Js.t -> canvasElement Js.t
```
raises [`Canvas_not_available`](./#exception-Canvas_not_available) when canvas elements are not supported by the browser.

### Coercion functions

```ocaml
val element : Dom.element Js.t -> element Js.t
```
Coercion from a general DOM element to an HTML element. (Unsafe in general.)

```ocaml
type taggedElement = 
  | A of anchorElement Js.t
  | Area of areaElement Js.t
  | Audio of audioElement Js.t
  | Base of baseElement Js.t
  | Blockquote of quoteElement Js.t
  | Body of bodyElement Js.t
  | Br of brElement Js.t
  | Button of buttonElement Js.t
  | Canvas of canvasElement Js.t
  | Caption of tableCaptionElement Js.t
  | Col of tableColElement Js.t
  | Colgroup of tableColElement Js.t
  | Del of modElement Js.t
  | Dialog of dialogElement Js.t
  | Div of divElement Js.t
  | Dl of dListElement Js.t
  | Embed of embedElement Js.t
  | Fieldset of fieldSetElement Js.t
  | Form of formElement Js.t
  | Frameset of frameSetElement Js.t
  | Frame of frameElement Js.t
  | H1 of headingElement Js.t
  | H2 of headingElement Js.t
  | H3 of headingElement Js.t
  | H4 of headingElement Js.t
  | H5 of headingElement Js.t
  | H6 of headingElement Js.t
  | Head of headElement Js.t
  | Hr of hrElement Js.t
  | Html of htmlElement Js.t
  | Iframe of iFrameElement Js.t
  | Img of imageElement Js.t
  | Input of inputElement Js.t
  | Ins of modElement Js.t
  | Label of labelElement Js.t
  | Legend of legendElement Js.t
  | Li of liElement Js.t
  | Link of linkElement Js.t
  | Map of mapElement Js.t
  | Meta of metaElement Js.t
  | Object of objectElement Js.t
  | Ol of oListElement Js.t
  | Optgroup of optGroupElement Js.t
  | Option of optionElement Js.t
  | P of paragraphElement Js.t
  | Param of paramElement Js.t
  | Pre of preElement Js.t
  | Q of quoteElement Js.t
  | Script of scriptElement Js.t
  | Select of selectElement Js.t
  | Style of styleElement Js.t
  | Table of tableElement Js.t
  | Tbody of tableSectionElement Js.t
  | Td of tableCellElement Js.t
  | Textarea of textAreaElement Js.t
  | Tfoot of tableSectionElement Js.t
  | Th of tableCellElement Js.t
  | Thead of tableSectionElement Js.t
  | Title of titleElement Js.t
  | Tr of tableRowElement Js.t
  | Ul of uListElement Js.t
  | Video of videoElement Js.t
  | Other of element Js.t
```
```ocaml
val tagged : element Js.t -> taggedElement
```
```ocaml
val opt_tagged : element Js.t Js.opt -> taggedElement option
```
```ocaml
type taggedEvent = 
  | MouseEvent of mouseEvent Js.t
  | KeyboardEvent of keyboardEvent Js.t
  | MessageEvent of messageEvent Js.t
  | MousewheelEvent of mousewheelEvent Js.t
  | MouseScrollEvent of mouseScrollEvent Js.t
  | PopStateEvent of popStateEvent Js.t
  | OtherEvent of event Js.t
```
```ocaml
val taggedEvent : event Js.t -> taggedEvent
```
```ocaml
val opt_taggedEvent : event Js.t Js.opt -> taggedEvent option
```
```ocaml
val stopPropagation : event Js.t -> unit
```
```ocaml
module DomStringMap : sig ... end
```
Typed access to [`domStringMap`](./#type-domStringMap) (the type of `Element.dataset`).

```ocaml
val attachShadow : 
  ?delegatesFocus:bool Js.t ->
  mode:Js.js_string Js.t ->
  element Js.t ->
  shadowRoot Js.t
```
Wrapper for `Element.attachShadow(init)` taking labeled arguments.

```ocaml
val scrollIntoView : 
  ?behavior:Js.js_string Js.t ->
  ?block:Js.js_string Js.t ->
  ?inline:Js.js_string Js.t ->
  element Js.t ->
  unit
```
Wrapper for `Element.scrollIntoView(options)` taking labeled arguments.

```ocaml
val focus : ?preventScroll:bool Js.t -> element Js.t -> unit
```
Wrapper for `HTMLElement.focus(options)` taking labeled arguments.

```ocaml
val showPopover : ?source:element Js.t -> element Js.t -> unit
```
Wrapper for `Element.showPopover(options)` taking labeled arguments.

```ocaml
val hidePopover : element Js.t -> unit
```
Wrapper for `Element.hidePopover()`.

```ocaml
val togglePopover : 
  ?force:bool Js.t ->
  ?source:element Js.t ->
  element Js.t ->
  bool Js.t
```
Wrapper for `Element.togglePopover(options)` taking labeled arguments. Returns the new open state.

```ocaml
val postMessage : 
  ?transfer:Js.Unsafe.any Js.js_array Js.t ->
  ?targetOrigin:Js.js_string Js.t ->
  window Js.t ->
  'a ->
  unit
```
Wrapper for `window.postMessage(message, options)` taking labeled arguments. When `~targetOrigin` is omitted, the JavaScript default of `"/"` (same origin) applies.

```ocaml
val makeScrollToOptions : 
  ?top:Js.number_t ->
  ?left:Js.number_t ->
  ?behavior:Js.js_string Js.t ->
  unit ->
  scrollToOptions Js.t
```
Smart constructor for [`scrollToOptions`](./Js_of_ocaml-Dom_html-class-type-scrollToOptions.md).

```ocaml
val makeKeyframeAnimationOptions : 
  ?delay:Js.number_t ->
  ?endDelay:Js.number_t ->
  ?fill:Js.js_string Js.t ->
  ?iterationStart:Js.number_t ->
  ?iterations:Js.number_t ->
  ?duration:Js.number_t ->
  ?direction:Js.js_string Js.t ->
  ?easing:Js.js_string Js.t ->
  ?id:Js.js_string Js.t ->
  ?composite:Js.js_string Js.t ->
  ?pseudoElement:Js.js_string Js.t Js.opt ->
  ?timeline:animationTimeline Js.t Js.opt ->
  unit ->
  keyframeAnimationOptions Js.t
```
Smart constructor for [`keyframeAnimationOptions`](./Js_of_ocaml-Dom_html-class-type-keyframeAnimationOptions.md).

```ocaml
val makeOptionalEffectTiming : 
  ?delay:Js.number_t ->
  ?endDelay:Js.number_t ->
  ?fill:Js.js_string Js.t ->
  ?iterationStart:Js.number_t ->
  ?iterations:Js.number_t ->
  ?duration:Js.number_t ->
  ?direction:Js.js_string Js.t ->
  ?easing:Js.js_string Js.t ->
  unit ->
  optionalEffectTiming Js.t
```
Smart constructor for [`optionalEffectTiming`](./Js_of_ocaml-Dom_html-class-type-optionalEffectTiming.md).

```ocaml
module CoerceTo : sig ... end
```
HTMLElement

```ocaml
type timeout_id_safe
```
```ocaml
val setTimeout : (unit -> unit) -> float -> timeout_id_safe
```
Same as `Dom_html.window##setTimeout cb ms` but prevents overflow with delay greater than 24 days.

```ocaml
val clearTimeout : timeout_id_safe -> unit
```
```ocaml
val onload : (unit -> unit) -> unit
```
Register a callback to run when the page finishes loading. If the page has already loaded (e.g. when running as WebAssembly), the callback is invoked immediately.

```ocaml
val js_array_of_collection : 
  element collection Js.t ->
  element Js.t Js.js_array Js.t
```
Convert a `Dom_html.collection` to a Js array


### Deprecated function.

```ocaml
val _requestAnimationFrame : (unit -> unit) Js.callback -> unit
```
Call the appropriate `requestAnimationFrame` method variant (depending on the navigator), or sleep for a short amount of time when there no such method is provided. We currently prefix the function name with as underscore as the interface of this function is not completely standardized yet. Thus, we leave the room to a function with a possibly refined type.

This function is deprecated. Use the requestAnimationFrame of the window object instead.

deprecated \[since 2.6\] Use \[Dom\_html.window\##requestAnimationFrame\] instead.