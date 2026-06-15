
# Class type `WebGL.renderingContext`

5\.13.1 Attributes

```ocaml
method canvas : Js_of_ocaml__.Dom_html.canvasElement Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method drawingBufferWidth : sizei Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method drawingBufferHeight : sizei Js_of_ocaml__.Js.readonly_prop
```
5\.13.2 Getting information about the context

```ocaml
method getContextAttributes : contextAttributes Js_of_ocaml__.Js.t
                                Js_of_ocaml__.Js.meth
```
5\.13.3 Setting and getting state

```ocaml
method activeTexture : textureUnit -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method blendColor : clampf ->
  clampf ->
  clampf ->
  clampf ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method blendEquation : blendMode -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method blendEquationSeparate : blendMode ->
  blendMode ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method blendFunc : blendingFactor ->
  blendingFactor ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method blendFuncSeparate : blendingFactor ->
  blendingFactor ->
  blendingFactor ->
  blendingFactor ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearColor : clampf ->
  clampf ->
  clampf ->
  clampf ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearDepth : clampf -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearStencil : int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method colorMask : bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method cullFace : cullFaceMode -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method depthFunc : depthFunction -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method depthMask : bool Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method depthRange : clampf -> clampf -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method disable : enableCap -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method enable : enableCap -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method frontFace : frontFaceDir -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method getParameter : 'a. 'a parameter -> 'a Js_of_ocaml__.Js.meth
```
```ocaml
method getError : errorCode Js_of_ocaml__.Js.meth
```
```ocaml
method hint : hintTarget -> hintMode -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method isEnabled : enableCap -> bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method lineWidth : Js_of_ocaml__.Js.number_t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method pixelStorei : 'a. 'a pixelStoreParam -> 'a -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method polygonOffset : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method sampleCoverage : clampf ->
  bool Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method stencilFunc : depthFunction -> int -> uint -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method stencilFuncSeparate : cullFaceMode ->
  depthFunction ->
  int ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method stencilMask : uint -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method stencilMaskSeparate : cullFaceMode -> uint -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method stencilOp : stencilOp ->
  stencilOp ->
  stencilOp ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method stencilOpSeparate : cullFaceMode ->
  stencilOp ->
  stencilOp ->
  stencilOp ->
  unit Js_of_ocaml__.Js.meth
```
5\.13.4 Viewing and clipping

```ocaml
method scissor : int -> int -> sizei -> sizei -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method viewport : int -> int -> sizei -> sizei -> unit Js_of_ocaml__.Js.meth
```
5\.13.5 Buffer objects

```ocaml
method bindBuffer : bufferTarget ->
  buffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindBuffer_ : bufferTarget ->
  buffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bufferData_create : bufferTarget ->
  sizeiptr ->
  bufferUsage ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bufferData : bufferTarget ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  bufferUsage ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bufferData_raw : bufferTarget ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t ->
  bufferUsage ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bufferSubData : bufferTarget ->
  intptr ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bufferSubData_raw : bufferTarget ->
  intptr ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method createBuffer : buffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteBuffer : buffer Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method getBufferParameter : 'a. bufferTarget ->
  'a bufferParameter ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method isBuffer : buffer Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
5\.13.6 Framebuffer objects

```ocaml
method bindFramebuffer : fbTarget ->
  framebuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindFramebuffer_ : fbTarget ->
  framebuffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method checkFramebufferStatus : fbTarget ->
  framebufferStatus Js_of_ocaml__.Js.meth
```
```ocaml
method createFramebuffer : framebuffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteFramebuffer : framebuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method framebufferRenderbuffer : fbTarget ->
  attachmentPoint ->
  rbTarget ->
  renderbuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method framebufferTexture2D : fbTarget ->
  attachmentPoint ->
  texTarget ->
  texture Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getFramebufferAttachmentParameter : 'a. fbTarget ->
  attachmentPoint ->
  'a attachParam ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method isFramebuffer : framebuffer Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
5\.13.7 Renderbuffer objects

```ocaml
method bindRenderbuffer : rbTarget ->
  renderbuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindRenderbuffer_ : rbTarget ->
  renderbuffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method createRenderbuffer : renderbuffer Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.meth
```
```ocaml
method deleteRenderbuffer : renderbuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getRenderbufferParameter : 'a. rbTarget ->
  'a renderbufferParam ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method isRenderbuffer : renderbuffer Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method renderbufferStorage : rbTarget ->
  format ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
5\.13.8 Texture objects

```ocaml
method bindTexture : texTarget ->
  texture Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindTexture_ : texTarget ->
  texture Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexImage2D : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexSubImage2D : texTarget ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method copyTexImage2D : texTarget ->
  int ->
  pixelFormat ->
  int ->
  int ->
  sizei ->
  sizei ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method copyTexSubImage2D : texTarget ->
  int ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method createTexture : texture Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteTexture : texture Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method generateMipmap : texTarget -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method getTexParameter : texTarget -> 'a texParam -> 'a Js_of_ocaml__.Js.meth
```
```ocaml
method isTexture : texture Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method texImage2D_new : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  void Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage2D_fromView : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage2D_fromImageData : texTarget ->
  int ->
  pixelFormat ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageData Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage2D_fromImage : texTarget ->
  int ->
  pixelFormat ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage2D_fromCanvas : texTarget ->
  int ->
  pixelFormat ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.canvasElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage2D_fromVideo : texTarget ->
  int ->
  pixelFormat ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.videoElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texParameteri : texTarget ->
  'a texParam ->
  'a ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage2D_fromView : texTarget ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage2D_fromImageData : texTarget ->
  int ->
  int ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageData Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage2D_fromImage : texTarget ->
  int ->
  int ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage2D_fromCanvas : texTarget ->
  int ->
  int ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.canvasElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage2D_fromVideo : texTarget ->
  int ->
  int ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.videoElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
5\.13.9 Programs and Shaders

```ocaml
method attachShader : program Js_of_ocaml__.Js.t ->
  shader Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindAttribLocation : program Js_of_ocaml__.Js.t ->
  uint ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compileShader : shader Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method createProgram : program Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createShader : shaderType ->
  shader Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteProgram : program Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method deleteShader : shader Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method detachShader : program Js_of_ocaml__.Js.t ->
  shader Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getAttachedShaders : program Js_of_ocaml__.Js.t ->
  shader Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method getProgramParameter : 'a. program Js_of_ocaml__.Js.t ->
  'a programParam ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method getProgramInfoLog : program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getShaderParameter : 'a. shader Js_of_ocaml__.Js.t ->
  'a shaderParam ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method getShaderPrecisionFormat : shaderType ->
  shaderPrecisionType ->
  shaderPrecisionFormat Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getShaderInfoLog : shader Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getShaderSource : shader Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method isProgram : program Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method isShader : shader Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method linkProgram : program Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method shaderSource : shader Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method useProgram : program Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method validateProgram : program Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
5\.13.10 Uniforms and attributes

```ocaml
method disableVertexAttribArray : uint -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method enableVertexAttribArray : uint -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method getActiveAttrib : program Js_of_ocaml__.Js.t ->
  uint ->
  activeInfo Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getActiveUniform : program Js_of_ocaml__.Js.t ->
  uint ->
  activeInfo Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getAttribLocation : program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.meth
```
```ocaml
method getUniform : 'a 'b. program Js_of_ocaml__.Js.t ->
  'a uniformLocation Js_of_ocaml__.Js.t ->
  'b Js_of_ocaml__.Js.meth
```
```ocaml
method getUniformLocation : 'a. program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'a uniformLocation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getVertexAttrib : 'a. uint ->
  'a vertexAttribParam ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method getVertexAttribOffset : uint ->
  vertexAttribPointerParam ->
  sizeiptr Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1f : Js_of_ocaml__.Js.number_t uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1fv_typed : Js_of_ocaml__.Js.number_t uniformLocation
                            Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1fv : Js_of_ocaml__.Js.number_t uniformLocation
                      Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1i : int uniformLocation Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1iv_typed : int uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1iv : int uniformLocation Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2f : [ `vec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2fv_typed : [ `vec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2fv : [ `vec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2i : [ `ivec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2iv : [ `ivec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2iv_typed : [ `ivec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3f : [ `vec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3fv_typed : [ `vec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3fv : [ `vec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3i : [ `ivec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  int ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3iv : [ `ivec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3iv_typed : [ `ivec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4f : [ `vec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4fv_typed : [ `vec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4fv : [ `vec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4i : [ `ivec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  int ->
  int ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4iv : [ `ivec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4iv_typed : [ `ivec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix2fv : [ `mat2 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix2fv_typed : [ `mat2 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix3fv : [ `mat3 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix3fv_typed : [ `mat3 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix4fv : [ `mat4 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix4fv_typed : [ `mat4 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib1f : uint ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib1fv : uint ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib1fv_typed : uint ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib2f : uint ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib2fv : uint ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib2fv_typed : uint ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib3f : uint ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib3fv : uint ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib3fv_typed : uint ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib4f : uint ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib4fv : uint ->
  Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttrib4fv_typed : uint ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttribPointer : uint ->
  int ->
  dataType ->
  bool Js_of_ocaml__.Js.t ->
  sizei ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
5\.13.11 Writing to the drawing buffer

```ocaml
method clear : clearBufferMask -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawArrays : beginMode -> int -> sizei -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawElements : beginMode ->
  sizei ->
  dataType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method finish : unit Js_of_ocaml__.Js.meth
```
```ocaml
method flush : unit Js_of_ocaml__.Js.meth
```
5\.13.12 Reading back pixels

```ocaml
method readPixels : int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
5\.13.13 Detecting context lost events

```ocaml
method isContextLost : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
5\.13.14 Detecting and enabling extensions

```ocaml
method getSupportedExtensions : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                                  Js_of_ocaml__.Js.js_array
                                  Js_of_ocaml__.Js.t
                                  Js_of_ocaml__.Js.meth
```
```ocaml
method getExtension : 'a. Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'a Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
Constants

```ocaml
method _DEPTH_BUFFER_BIT_ : clearBufferMask Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BUFFER_BIT_ : clearBufferMask Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_BUFFER_BIT_ : clearBufferMask Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _POINTS : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINES : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINE_LOOP_ : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINE_STRIP_ : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRIANGLES : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRIANGLE_STRIP_ : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRIANGLE_FAN_ : beginMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ZERO : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SRC_COLOR_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE_MINUS_SRC_COLOR_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SRC_ALPHA_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE_MINUS_SRC_ALPHA_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DST_ALPHA_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE_MINUS_DST_ALPHA_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DST_COLOR_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE_MINUS_DST_COLOR_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SRC_ALPHA_SATURATE_ : blendingFactor Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FUNC_ADD_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FUNC_SUBTRACT_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FUNC_REVERSE_SUBTRACT_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CONSTANT_COLOR_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE_MINUS_CONSTANT_COLOR_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CONSTANT_ALPHA_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ONE_MINUS_CONSTANT_ALPHA_ : blendMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ARRAY_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ELEMENT_ARRAY_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STREAM_DRAW_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STATIC_DRAW_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DYNAMIC_DRAW_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRONT : cullFaceMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BACK : cullFaceMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRONT_AND_BACK_ : cullFaceMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CULL_FACE_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DITHER : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_TEST_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_TEST_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SCISSOR_TEST_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _POLYGON_OFFSET_FILL_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLE_ALPHA_TO_COVERAGE_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLE_COVERAGE_ : enableCap Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NO_ERROR_ : errorCode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INVALID_ENUM_ : errorCode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INVALID_VALUE_ : errorCode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INVALID_OPERATION_ : errorCode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _OUT_OF_MEMORY_ : errorCode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CONTEXT_LOST_WEBGL_ : errorCode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INVALID_FRAMEBUFFER_OPERATION_ : errorCode
                                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CW : frontFaceDir Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CCW : frontFaceDir Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DONT_CARE_ : hintMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FASTEST : hintMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NICEST : hintMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _GENERATE_MIPMAP_HINT_ : hintTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_EQUATION_ : blendMode parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_EQUATION_RGB_ : blendMode parameter
                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_EQUATION_ALPHA_ : blendMode parameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_DST_RGB_ : blendingFactor parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_SRC_RGB_ : blendingFactor parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_DST_ALPHA_ : blendingFactor parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_SRC_ALPHA_ : blendingFactor parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_COLOR_ : Js_of_ocaml__.Typed_array.float32Array
                         Js_of_ocaml__.Js.t
                         parameter
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ARRAY_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                                  parameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ELEMENT_ARRAY_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                          Js_of_ocaml__.Js.opt
                                          parameter
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CULL_FACE_PARAM : bool Js_of_ocaml__.Js.t parameter
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLEND_PARAM : bool Js_of_ocaml__.Js.t parameter
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DITHER_PARAM : bool Js_of_ocaml__.Js.t parameter
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_TEST_PARAM : bool Js_of_ocaml__.Js.t parameter
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_TEST_PARAM : bool Js_of_ocaml__.Js.t parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SCISSOR_TEST_PARAM : bool Js_of_ocaml__.Js.t parameter
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _POLYGON_OFFSET_FILL_PARAM : bool Js_of_ocaml__.Js.t parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINE_WIDTH_ : Js_of_ocaml__.Js.number_t parameter
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ALIASED_POINT_SIZE_RANGE_ : Js_of_ocaml__.Typed_array.float32Array
                                      Js_of_ocaml__.Js.t
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ALIASED_LINE_WIDTH_RANGE_ : Js_of_ocaml__.Typed_array.float32Array
                                      Js_of_ocaml__.Js.t
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CULL_FACE_MODE_ : cullFaceMode parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRONT_FACE_ : frontFaceDir parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_RANGE_ : Js_of_ocaml__.Typed_array.float32Array
                         Js_of_ocaml__.Js.t
                         parameter
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_WRITEMASK_ : bool Js_of_ocaml__.Js.t parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_CLEAR_VALUE_ : Js_of_ocaml__.Js.number_t parameter
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_FUNC_ : depthFunction parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_CLEAR_VALUE_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_FUNC_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_FAIL_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_PASS_DEPTH_FAIL_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_PASS_DEPTH_PASS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_REF_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_VALUE_MASK_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_WRITEMASK_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_FUNC_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_FAIL_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_PASS_DEPTH_FAIL_ : int parameter
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_PASS_DEPTH_PASS_ : int parameter
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_REF_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_VALUE_MASK_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BACK_WRITEMASK_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VIEWPORT : Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t
                     parameter
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SCISSOR_BOX_ : Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t
                         parameter
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_CLEAR_VALUE_ : Js_of_ocaml__.Typed_array.float32Array
                               Js_of_ocaml__.Js.t
                               parameter
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_WRITEMASK_ : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
                             Js_of_ocaml__.Js.t
                             parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_ALIGNMENT_PARAM : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PACK_ALIGNMENT_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_TEXTURE_SIZE_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VIEWPORT_DIMS_ : Js_of_ocaml__.Typed_array.int32Array
                               Js_of_ocaml__.Js.t
                               parameter
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SUBPIXEL_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RED_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _GREEN_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BLUE_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ALPHA_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_BITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _POLYGON_OFFSET_UNITS_ : Js_of_ocaml__.Js.number_t parameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _POLYGON_OFFSET_FACTOR_ : Js_of_ocaml__.Js.number_t parameter
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_BINDING_2D_ : texture Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                                parameter
                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_BINDING_CUBE_MAP_ : texture Js_of_ocaml__.Js.t
                                      Js_of_ocaml__.Js.opt
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLE_BUFFERS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLES_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLE_COVERAGE_VALUE_ : Js_of_ocaml__.Js.number_t parameter
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLE_COVERAGE_INVERT_ : bool Js_of_ocaml__.Js.t parameter
                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NUM_COMPRESSED_TEXTURE_FORMATS_ : int parameter
                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COMPRESSED_TEXTURE_FORMATS_ : Js_of_ocaml__.Typed_array.uint32Array
                                        Js_of_ocaml__.Js.t
                                        parameter
                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _GENERATE_MIPMAP_HINT_PARAM_ : hintMode parameter
                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BUFFER_SIZE_ : int bufferParameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BUFFER_USAGE_ : bufferUsage bufferParameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BYTE : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_BYTE_DT : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SHORT : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_SHORT_ : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_ : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT : dataType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_BYTE_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_SHORT_4_4_4_4_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_SHORT_5_5_5_1_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_SHORT_5_6_5_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ALPHA : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LUMINANCE : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LUMINANCE_ALPHA_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_INDEX_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_STENCIL_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_COMPONENT_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAGMENT_SHADER_ : shaderType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_SHADER_ : shaderType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VERTEX_ATTRIBS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VERTEX_UNIFORM_VECTORS_ : int parameter
                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VARYING_VECTORS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_COMBINED_TEXTURE_IMAGE_UNITS_ : int parameter
                                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VERTEX_TEXTURE_IMAGE_UNITS_ : int parameter
                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_TEXTURE_IMAGE_UNITS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_FRAGMENT_UNIFORM_VECTORS_ : int parameter
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SHADER_TYPE_ : shaderType shaderParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DELETE_STATUS_ : bool Js_of_ocaml__.Js.t shaderParam
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COMPILE_STATUS_ : bool Js_of_ocaml__.Js.t shaderParam
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DELETE_STATUS_PROG : bool Js_of_ocaml__.Js.t programParam
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINK_STATUS_ : bool Js_of_ocaml__.Js.t programParam
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VALIDATE_STATUS_ : bool Js_of_ocaml__.Js.t programParam
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ATTACHED_SHADERS_ : int programParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ACTIVE_UNIFORMS_ : int programParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ACTIVE_ATTRIBUTES_ : int programParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SHADING_LANGUAGE_VERSION_ : Js_of_ocaml__.Js.js_string
                                      Js_of_ocaml__.Js.t
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CURRENT_PROGRAM_ : program Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                             parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VENDOR : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t parameter
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERER : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t parameter
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERSION : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t parameter
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_CUBE_MAP_TEXTURE_SIZE_ : int parameter
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ACTIVE_TEXTURE_ : textureUnit parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_BINDING_ : framebuffer Js_of_ocaml__.Js.t
                                 Js_of_ocaml__.Js.opt
                                 parameter
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_BINDING_ : renderbuffer Js_of_ocaml__.Js.t
                                  Js_of_ocaml__.Js.opt
                                  parameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_RENDERBUFFER_SIZE_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NEVER : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LESS : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _EQUAL : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LEQUAL : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _GREATER : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NOTEQUAL : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _GEQUAL : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ALWAYS : depthFunction Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _KEEP : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _REPLACE : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INCR : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DECR : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INVERT : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INCR_WRAP_ : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DECR_WRAP_ : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ZERO_ : stencilOp Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NEAREST : texFilter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINEAR : texFilter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NEAREST_MIPMAP_NEAREST_ : texFilter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINEAR_MIPMAP_NEAREST_ : texFilter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NEAREST_MIPMAP_LINEAR_ : texFilter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINEAR_MIPMAP_LINEAR_ : texFilter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MAG_FILTER_ : texFilter texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MIN_FILTER_ : texFilter texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_WRAP_S_ : wrapMode texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_WRAP_T_ : wrapMode texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NONE_OT : objectType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_OT : objectType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_OT : objectType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_2D_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_POSITIVE_X_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_NEGATIVE_X_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_POSITIVE_Y_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_NEGATIVE_Y_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_POSITIVE_Z_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_CUBE_MAP_NEGATIVE_Z_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE0 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE1 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE2 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE3 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE4 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE5 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE6 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE7 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE8 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE9 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE10 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE11 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE12 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE13 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE14 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE15 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE16 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE17 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE18 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE19 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE20 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE21 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE22 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE23 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE24 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE25 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE26 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE27 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE28 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE29 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE30 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE31 : textureUnit Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _REPEAT : wrapMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CLAMP_TO_EDGE_ : wrapMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MIRRORED_REPEAT_ : wrapMode Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_VEC2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_VEC3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_VEC4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_VEC2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_VEC3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_VEC4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BOOL_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BOOL_VEC2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BOOL_VEC3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BOOL_VEC4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_2D_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_CUBE_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_ENABLED_ : bool Js_of_ocaml__.Js.t
                                         vertexAttribParam
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_SIZE_ : int vertexAttribParam
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_STRIDE_ : int vertexAttribParam
                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_TYPE_ : int vertexAttribParam
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_NORMALIZED_ : bool Js_of_ocaml__.Js.t
                                            vertexAttribParam
                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_POINTER_ : vertexAttribPointerParam
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                                Js_of_ocaml__.Js.opt
                                                vertexAttribParam
                                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CURRENT_VERTEX_ATTRIB_ : Js_of_ocaml__.Typed_array.float32Array
                                   Js_of_ocaml__.Js.t
                                   vertexAttribParam
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LOW_FLOAT_ : shaderPrecisionType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MEDIUM_FLOAT_ : shaderPrecisionType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _HIGH_FLOAT_ : shaderPrecisionType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LOW_INT_ : shaderPrecisionType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MEDIUM_INT_ : shaderPrecisionType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _HIGH_INT_ : shaderPrecisionType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER : fbTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER : rbTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA4 : format Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB5_A1_ : format Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB565 : format Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_COMPONENT16_ : format Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_INDEX8_ : format Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_WIDTH_ : int renderbufferParam
                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_HEIGHT_ : int renderbufferParam
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_INTERNAL_FORMAT_ : format renderbufferParam
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_RED_SIZE_ : int renderbufferParam
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_GREEN_SIZE_ : int renderbufferParam
                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_BLUE_SIZE_ : int renderbufferParam
                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_ALPHA_SIZE_ : int renderbufferParam
                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_DEPTH_SIZE_ : int renderbufferParam
                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RENDERBUFFER_STENCIL_SIZE_ : int renderbufferParam
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_ : objectType attachParam
                                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_RENDERBUFFER : renderbuffer
                                                            Js_of_ocaml__.Js.t
                                                            attachParam
                                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_TEXTURE : texture Js_of_ocaml__.Js.t
                                                       attachParam
                                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_ : int attachParam
                                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_ : int attachParam
                                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT0_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_ATTACHMENT_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_ATTACHMENT_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_STENCIL_ATTACHMENT_ : attachmentPoint
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_COMPLETE_ : framebufferStatus
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_INCOMPLETE_ATTACHMENT_ : framebufferStatus
                                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_ : framebufferStatus
                                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_INCOMPLETE_DIMENSIONS_ : framebufferStatus
                                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_UNSUPPORTED_ : framebufferStatus
                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_FLIP_Y_WEBGL_PARAM : bool Js_of_ocaml__.Js.t parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_PREMULTIPLY_ALPHA_WEBGL_PARAM : bool Js_of_ocaml__.Js.t
                                                 parameter
                                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_COLORSPACE_CONVERSION_WEBGL_PARAM : colorspaceConversion
                                                     parameter
                                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NONE : colorspaceConversion Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _BROWSER_DEFAULT_WEBGL_ : colorspaceConversion
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_ALIGNMENT_ : int pixelStoreParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_FLIP_Y_WEBGL_ : bool Js_of_ocaml__.Js.t pixelStoreParam
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_PREMULTIPLY_ALPHA_WEBGL_ : bool Js_of_ocaml__.Js.t
                                            pixelStoreParam
                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_COLORSPACE_CONVERSION_WEBGL_ : int pixelStoreParam
                                                Js_of_ocaml__.Js.readonly_prop
```