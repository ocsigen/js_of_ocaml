
# Module `Js_of_ocaml.WebGL`

WebGL binding

5\.1 Types

```ocaml
type sizei = int
```
```ocaml
type sizeiptr = int
```
```ocaml
type intptr = int
```
```ocaml
type uint = int
```
```ocaml
type clampf = Js.number_t
```
```ocaml
type void
```
```ocaml
type clearBufferMask = int
```
```ocaml
type beginMode
```
```ocaml
type blendingFactor
```
```ocaml
type blendMode
```
```ocaml
type bufferTarget
```
```ocaml
type bufferUsage
```
```ocaml
type cullFaceMode
```
```ocaml
type depthFunction
```
```ocaml
type enableCap
```
```ocaml
type errorCode
```
```ocaml
type frontFaceDir
```
```ocaml
type hintTarget
```
```ocaml
type hintMode
```
```ocaml
type textureUnit = int
```
```ocaml
type 'a pixelStoreParam
```
```ocaml
type stencilOp
```
```ocaml
type fbTarget
```
```ocaml
type attachmentPoint
```
```ocaml
type rbTarget
```
```ocaml
type texTarget
```
```ocaml
type 'a parameter
```
```ocaml
type 'a bufferParameter
```
```ocaml
type 'a vertexAttribParam
```
```ocaml
type vertexAttribPointerParam
```
```ocaml
type 'a attachParam
```
```ocaml
type framebufferStatus
```
```ocaml
type 'a renderbufferParam
```
```ocaml
type format
```
```ocaml
type pixelFormat
```
```ocaml
type pixelType
```
```ocaml
type 'a texParam
```
```ocaml
type dataType
```
```ocaml
type shaderType
```
```ocaml
type 'a programParam
```
```ocaml
type 'a shaderParam
```
```ocaml
type textureFilter
```
```ocaml
type wrapMode
```
```ocaml
type texFilter
```
```ocaml
type uniformType
```
```ocaml
type colorspaceConversion
```
```ocaml
type shaderPrecisionType
```
```ocaml
type objectType
```
```ocaml
class type  contextAttributes = object ... end
```
5\.2 WebGLContextAttributes

```ocaml
val defaultContextAttributes : contextAttributes Js.t
```
```ocaml
type buffer
```
```ocaml
type framebuffer
```
```ocaml
type program
```
```ocaml
type renderbuffer
```
```ocaml
type shader
```
```ocaml
type texture
```
```ocaml
type 'a uniformLocation
```
```ocaml
class type  activeInfo = object ... end
```
```ocaml
class type  shaderPrecisionFormat = object ... end
```
```ocaml
class type  renderingContext = object ... end
```
5\.13.1 Attributes

5\.14 WebGLContextEvent

```ocaml
class type  contextEvent = object ... end
```
```ocaml
module Event : sig ... end
```
Get a context

```ocaml
val getContext : Dom_html.canvasElement Js.t -> renderingContext Js.t Js.opt
```
```ocaml
val getContextWithAttributes : 
  Dom_html.canvasElement Js.t ->
  contextAttributes Js.t ->
  renderingContext Js.t Js.opt
```