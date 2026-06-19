
# Module `Js_of_ocaml.Dom`

DOM binding

This is a partial binding to the DOM Core API.


### DOM objects

```ocaml
class type 'node nodeList = object ... end
```
Specification of `NodeList` objects.

```ocaml
class type 'node collection = object ... end
```
Specification of `HTMLCollection` objects. Returned by `Element.children`, `getElementsByTagName`, `getElementsByClassName`, etc. Always live, contains only elements, and adds a `namedItem` lookup.

```ocaml
type nodeType = 
  | OTHER
  | ELEMENT
  | ATTRIBUTE
  | TEXT
  | CDATA_SECTION
  | ENTITY_REFERENCE
  | ENTITY
  | PROCESSING_INSTRUCTION
  | COMMENT
  | DOCUMENT
  | DOCUMENT_TYPE
  | DOCUMENT_FRAGMENT
  | NOTATION
```
```ocaml
module DocumentPosition : sig ... end
```
```ocaml
class type  node = object ... end
```
Specification of `Node` objects.

```ocaml
class type  attr = object ... end
```
Specification of `Attr` objects.

```ocaml
class type 'node namedNodeMap = object ... end
```
Specification of `NamedNodeMap` objects.

```ocaml
class type  element = object ... end
```
Specification of `Element` objects.

```ocaml
class type  getRootNodeOptions = object ... end
```
```ocaml
class type  characterData = object ... end
```
Specification of `CharacterData` objects.

```ocaml
class type  comment = characterData
```
Specification of `Comment` objects

```ocaml
class type  text = characterData
```
Specification of `Text` objects.

```ocaml
class type  documentFragment = node
```
Specification of `DocumentFragment` objects.

```ocaml
class type  documentType = object ... end
```
Specification of `DocumentType` objects.

```ocaml
class type 'element document = object ... end
```
Specification of `Document` objects.


### Helper functions

```ocaml
val insertBefore : node Js.t -> node Js.t -> node Js.t Js.opt -> unit
```
`insertBefore p n c` inserts node `n` as child of node `p`, just before node `c`, or as last child if `c` is null. The expression `insertBefore p n c` behave the same as `p##insertBefore n c` but avoid the need of coercing the different objects to `node t`.

```ocaml
val replaceChild : node Js.t -> node Js.t -> node Js.t -> unit
```
The expression `replaceChild p n c` behave the same as `p##replaceChild n c` (replace `c` by `n` in `p`) but avoid the need of coercing the different objects to `node t`.

```ocaml
val removeChild : node Js.t -> node Js.t -> unit
```
The expression `removeChild n c` behave the same as `n##removeChild c` (remove `c` from `n`) but avoid the need of coercing the different objects to `node t`.

```ocaml
val appendChild : node Js.t -> node Js.t -> unit
```
The expression `appendChild n c` behave the same as `n##appendChild c` (appends `c` to `n`) but avoid the need of coercing the different objects to `node t`.

```ocaml
type child_node
```
The type of values accepted by DOM APIs that take either a node or a string.

```ocaml
val node : node Js.t -> child_node
```
Coerce a DOM node to a [`child_node`](./#type-child_node).

```ocaml
val text : Js.js_string Js.t -> child_node
```
Coerce a string to a [`child_node`](./#type-child_node).

```ocaml
val before : node Js.t -> child_node list -> unit
```
Call the JavaScript `before(...)` method on a DOM node.

```ocaml
val after : node Js.t -> child_node list -> unit
```
Call the JavaScript `after(...)` method on a DOM node.

```ocaml
val replaceWith : node Js.t -> child_node list -> unit
```
Call the JavaScript `replaceWith(...)` method on a DOM node.

```ocaml
val prepend : element Js.t -> child_node list -> unit
```
Call the JavaScript `prepend(...)` method on an element.

```ocaml
val append : element Js.t -> child_node list -> unit
```
Call the JavaScript `append(...)` method on an element.

```ocaml
val replaceChildren : element Js.t -> child_node list -> unit
```
Call the JavaScript `replaceChildren(...)` method on an element.

```ocaml
val remove : node Js.t -> unit
```
Call the JavaScript `remove()` method, removing the node from its parent.

```ocaml
val getRootNode : ?composed:bool Js.t -> node Js.t -> node Js.t
```
Wrapper for `Node.getRootNode(options)` taking labeled arguments. Pass `~composed:Js._true` to cross shadow-DOM boundaries.

```ocaml
val list_of_nodeList : 'a nodeList Js.t -> 'a Js.t list
```
```ocaml
type node_type = 
  | Element of element Js.t
  | Attr of attr Js.t
  | Text of text Js.t
  | Other of node Js.t
```
```ocaml
val nodeType : node Js.t -> node_type
```
```ocaml
module CoerceTo : sig ... end
```

### Events

```ocaml
type (-'a, -'b) event_listener
```
The type of event listener functions. The first type parameter `'a` is the type of the target object; the second parameter `'b` is the type of the event object.

```ocaml
type event_phase = 
  | Phase_none
  | Phase_capturing
  | Phase_at_target
  | Phase_bubbling
```
```ocaml
class type 'a event = object ... end
```
```ocaml
class type ['a, 'b] customEvent = object ... end
```

### Event handlers

```ocaml
val no_handler : ('a, 'b) event_listener
```
Void event handler (Javascript `null` value).

```ocaml
val handler : (('e event Js.t as 'b) -> bool Js.t) -> ('a, 'b) event_listener
```
Create an event handler that invokes the provided function. If the handler returns false, the default action is prevented.

```ocaml
val full_handler : 
  ('a -> ('e event Js.t as 'b) -> bool Js.t) ->
  ('a, 'b) event_listener
```
Create an event handler that invokes the provided function. The event target (implicit parameter `this`) is also passed as argument to the function.

```ocaml
val listener : (('e event Js.t as 'b) -> unit) -> ('a, 'b) event_listener
```
Create an event listener from a `unit`\-returning function. Unlike `handler`, which signals "prevent default" via a `false` return, the callback here must call `preventDefault` explicitly on the event. Convenient for events like `beforeunload` where there is no natural boolean to return.

```ocaml
val full_listener : 
  ('a -> ('e event Js.t as 'b) -> unit) ->
  ('a, 'b) event_listener
```
Same as `listener` but also passes the event target (implicit parameter `this`) to the function.

```ocaml
val invoke_handler : ('a, 'b) event_listener -> 'a -> 'b -> bool Js.t
```
Invoke an existing handler. Useful to chain event handlers. Listeners that return no opinion are normalized to `Js._true`.

```ocaml
val eventTarget : (< .. > as 'a) event Js.t -> 'a Js.t
```
Returns which object is the target of this event. It raises `Not_found` in case there is no target (if the event has not been triggered yet)

```ocaml
type event_listener_id
```
```ocaml
module Event : sig ... end
```
```ocaml
val addEventListenerWithOptions : 
  (< .. > Js.t as 'a) ->
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
  (< .. > Js.t as 'a) ->
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
val preventDefault : 'a event Js.t -> unit
```
Call this to prevent the default handler for the event. To stop propagation of the event, call [`Dom_html.stopPropagation`](./Js_of_ocaml-Dom_html.md#val-stopPropagation).

```ocaml
val createCustomEvent : 
  ?bubbles:bool ->
  ?cancelable:bool ->
  ?detail:'b ->
  ['a, 'b] customEvent Js.t Event.typ ->
  ('a, 'b) customEvent Js.t
```
Create a custom event of given type.


### Other DOM objects

```ocaml
class type  stringList = object ... end
```