
# Module `Js_of_ocaml.WebGL2`

WebGL2 binding.

WebGL2 (`WebGL2RenderingContext`) is a strict superset of WebGL1, so the context [`renderingContext`](./Js_of_ocaml-WebGL2-class-type-renderingContext.md) inherits every method and constant of [`WebGL.renderingContext`](./Js_of_ocaml-WebGL-class-type-renderingContext.md) and adds the WebGL2 ones on top.

Following the pragmatic style of [`WebGL`](./Js_of_ocaml-WebGL.md), the enumerations are kept as abstract types shared between related uses. In particular the many WebGL2 sized internal formats and the plain formats share the single type [`pixelFormat`](./#type-pixelFormat); the binding therefore does not enforce, at compile time, that the (internalformat, format, type) triples passed to e.g. [`renderingContext.texImage3D`](./Js_of_ocaml-WebGL2-class-type-renderingContext.md#method-texImage3D) are mutually coherent (WebGL itself is not type safe there).


### Types reused from WebGL1

```ocaml
type sizei = WebGL.sizei
```
```ocaml
type sizeiptr = WebGL.sizeiptr
```
```ocaml
type intptr = WebGL.intptr
```
```ocaml
type uint = WebGL.uint
```
```ocaml
type clampf = WebGL.clampf
```
```ocaml
type void = WebGL.void
```
```ocaml
type clearBufferMask = WebGL.clearBufferMask
```
```ocaml
type beginMode = WebGL.beginMode
```
```ocaml
type bufferTarget = WebGL.bufferTarget
```
```ocaml
type bufferUsage = WebGL.bufferUsage
```
```ocaml
type enableCap = WebGL.enableCap
```
```ocaml
type dataType = WebGL.dataType
```
```ocaml
type pixelType = WebGL.pixelType
```
```ocaml
type pixelFormat = WebGL.pixelFormat
```
```ocaml
type format = WebGL.format
```
```ocaml
type texTarget = WebGL.texTarget
```
```ocaml
type texFilter = WebGL.texFilter
```
```ocaml
type rbTarget = WebGL.rbTarget
```
```ocaml
type fbTarget = WebGL.fbTarget
```
```ocaml
type attachmentPoint = WebGL.attachmentPoint
```
```ocaml
type 'a parameter = 'a WebGL.parameter
```
```ocaml
type 'a pixelStoreParam = 'a WebGL.pixelStoreParam
```
```ocaml
type buffer = WebGL.buffer
```
```ocaml
type framebuffer = WebGL.framebuffer
```
```ocaml
type program = WebGL.program
```
```ocaml
type renderbuffer = WebGL.renderbuffer
```
```ocaml
type shader = WebGL.shader
```
```ocaml
type texture = WebGL.texture
```
```ocaml
type 'a uniformLocation = 'a WebGL.uniformLocation
```
```ocaml
type 'a texParam = 'a WebGL.texParam
```
```ocaml
type wrapMode = WebGL.wrapMode
```
```ocaml
type depthFunction = WebGL.depthFunction
```
```ocaml
type 'a programParam = 'a WebGL.programParam
```
```ocaml
type 'a vertexAttribParam = 'a WebGL.vertexAttribParam
```
```ocaml
type 'a attachParam = 'a WebGL.attachParam
```
```ocaml
type framebufferStatus = WebGL.framebufferStatus
```
```ocaml
type uniformType = WebGL.uniformType
```
```ocaml
type hintTarget = WebGL.hintTarget
```
```ocaml
type objectType = WebGL.objectType
```

### New WebGL2 objects

```ocaml
type query
```
```ocaml
type sampler
```
```ocaml
type sync
```
```ocaml
type transformFeedback
```
```ocaml
type vertexArrayObject
```

### New WebGL2 enumerations

```ocaml
type queryTarget
```
```ocaml
type currentQueryParam
```
```ocaml
type 'a queryParam
```
```ocaml
type syncCondition
```
```ocaml
type 'a syncParam
```
```ocaml
type syncStatus
```
```ocaml
type clientWaitSyncStatus
```
```ocaml
type clearBuffer
```
```ocaml
type transformFeedbackMode
```
```ocaml
type 'a samplerParam
```
```ocaml
type 'a activeUniformParam
```
```ocaml
type 'a uniformBlockParam
```
```ocaml
type transformFeedbackTarget
```
```ocaml
type textureCompareMode
```
```ocaml
type 'a indexedParameter
```
```ocaml
type 'a internalformatParam
```
```ocaml
type componentType
```
```ocaml
type colorEncoding
```
```ocaml
type syncObjectType
```
```ocaml
class type  renderingContext = object ... end
```

### Getting a WebGL2 context

```ocaml
val getContext : Dom_html.canvasElement Js.t -> renderingContext Js.t Js.opt
```
```ocaml
val getContextWithAttributes : 
  Dom_html.canvasElement Js.t ->
  WebGL.contextAttributes Js.t ->
  renderingContext Js.t Js.opt
```