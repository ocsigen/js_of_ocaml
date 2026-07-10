
# Class type `WebGL2.renderingContext`

```ocaml
inherit Js_of_ocaml__.WebGL.renderingContext
```

### 5\.14.2 Setting and getting state (buffers)

```ocaml
method copyBufferSubData : bufferTarget ->
  bufferTarget ->
  intptr ->
  intptr ->
  sizeiptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getBufferSubData : bufferTarget ->
  intptr ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindBufferBase : bufferTarget ->
  uint ->
  buffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindBufferRange : bufferTarget ->
  uint ->
  buffer Js_of_ocaml__.Js.t ->
  intptr ->
  sizeiptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindBufferBase_ : bufferTarget ->
  uint ->
  buffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
`bindBufferBase` accepting `null`, to release an indexed binding.

```ocaml
method bindBufferRange_ : bufferTarget ->
  uint ->
  buffer Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  intptr ->
  sizeiptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bufferData_withOffset : bufferTarget ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  bufferUsage ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`bufferData target srcData usage srcOffset length`; a `length` of `0` means up to the end of `srcData`.

```ocaml
method bufferSubData_withOffset : bufferTarget ->
  intptr ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`bufferSubData target dstByteOffset srcData srcOffset length`.

```ocaml
method getBufferSubData_withOffset : bufferTarget ->
  intptr ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`getBufferSubData target srcByteOffset dstBuffer dstOffset length`.

```ocaml
method getIndexedParameter : 'a. 'a indexedParameter ->
  uint ->
  'a Js_of_ocaml__.Js.meth
```

### 5\.14.3 Programs and shaders

```ocaml
method getFragDataLocation : program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.meth
```

### 5\.14.4 3D textures and texture storage

```ocaml
method texImage3D : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  void Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage3D_fromView : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage3D_fromImageData : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageData Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage3D_fromImage : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage3D_fromCanvas : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.canvasElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage3D_fromVideo : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.videoElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage3D_fromView : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texImage3D_pbo : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
`texImage3D` sourcing its data from the buffer bound to `PIXEL_UNPACK_BUFFER`, at the given byte offset.

```ocaml
method texImage3D_withOffset : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`texImage3D` reading from the view starting at element `srcOffset`.

```ocaml
method texSubImage3D_fromImageData : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageData Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage3D_fromImage : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.imageElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage3D_fromCanvas : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.canvasElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage3D_fromVideo : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Dom_html.videoElement Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage3D_pbo : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage3D_withOffset : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method copyTexSubImage3D : texTarget ->
  int ->
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
method compressedTexImage3D : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexSubImage3D : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexImage2D_pbo : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  sizei ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
`compressedTexImage2D target level internalformat width height border imageSize offset`, reading from the buffer bound to `PIXEL_UNPACK_BUFFER`.

```ocaml
method compressedTexImage2D_withOffset : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`compressedTexImage2D ... srcData srcOffset srcLengthOverride`.

```ocaml
method compressedTexSubImage2D_pbo : texTarget ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  sizei ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexSubImage2D_withOffset : texTarget ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexImage3D_pbo : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  sizei ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexImage3D_withOffset : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  int ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexSubImage3D_pbo : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  sizei ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method compressedTexSubImage3D_withOffset : texTarget ->
  int ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  sizei ->
  pixelFormat ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texStorage2D : texTarget ->
  sizei ->
  pixelFormat ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texStorage3D : texTarget ->
  sizei ->
  pixelFormat ->
  sizei ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```

### 5\.14.5 Framebuffer objects

```ocaml
method blitFramebuffer : int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  clearBufferMask ->
  texFilter ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method framebufferTextureLayer : fbTarget ->
  attachmentPoint ->
  texture Js_of_ocaml__.Js.t ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method framebufferTextureLayer_ : fbTarget ->
  attachmentPoint ->
  texture Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`framebufferTextureLayer` accepting `null`, to detach the attachment.

```ocaml
method invalidateFramebuffer : fbTarget ->
  attachmentPoint Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method invalidateSubFramebuffer : fbTarget ->
  attachmentPoint Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  int ->
  int ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method invalidateFramebuffer_default : fbTarget ->
  clearBuffer Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
`invalidateFramebuffer` for when the default framebuffer is bound, whose attachments are named by `_COLOR_CB`, `_DEPTH_CB` and `_STENCIL_CB` rather than by `attachmentPoint`.

```ocaml
method invalidateSubFramebuffer_default : fbTarget ->
  clearBuffer Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  int ->
  int ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method readBuffer : attachmentPoint -> unit Js_of_ocaml__.Js.meth
```

### 5\.14.6 Renderbuffer objects

```ocaml
method renderbufferStorageMultisample : rbTarget ->
  sizei ->
  pixelFormat ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method renderbufferStorage_ : rbTarget ->
  pixelFormat ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
`renderbufferStorage` accepting the WebGL2 sized internal formats (`pixelFormat`); the WebGL1 `renderbufferStorage` taking a `format` is still available through inheritance.

```ocaml
method getInternalformatParameter : 'a. rbTarget ->
  pixelFormat ->
  'a internalformatParam ->
  'a Js_of_ocaml__.Js.meth
```

### 5\.14.8 Texture objects

```ocaml
method texParameterf : texTarget ->
  Js_of_ocaml__.Js.number_t texParam ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
`texParameterf` for the float texture parameters `_TEXTURE_MIN_LOD_` and `_TEXTURE_MAX_LOD_`; the integer/enum parameters go through the inherited `texParameteri`.

```ocaml
method texImage2D_pbo : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
`texImage2D` sourcing its data from the buffer bound to `PIXEL_UNPACK_BUFFER`, at the given byte offset.

```ocaml
method texImage2D_withOffset : texTarget ->
  int ->
  pixelFormat ->
  sizei ->
  sizei ->
  int ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`texImage2D` reading from the view starting at element `srcOffset`.

```ocaml
method texSubImage2D_pbo : texTarget ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method texSubImage2D_withOffset : texTarget ->
  int ->
  int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```

### 5\.14.12 Reading back pixels

```ocaml
method readPixels_pbo : int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
`readPixels` writing into the buffer bound to `PIXEL_PACK_BUFFER`, at the given byte offset.

```ocaml
method readPixels_withOffset : int ->
  int ->
  sizei ->
  sizei ->
  pixelFormat ->
  pixelType ->
  Js_of_ocaml__.Typed_array.arrayBufferView Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
`readPixels` writing into the view starting at element `dstOffset`.


### 5\.14.9 Multiple render targets

```ocaml
method drawBuffers : attachmentPoint Js_of_ocaml__.Js.js_array
                       Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferiv : clearBuffer ->
  int ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferuiv : clearBuffer ->
  int ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferfv : clearBuffer ->
  int ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferiv_withOffset : clearBuffer ->
  int ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferuiv_withOffset : clearBuffer ->
  int ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferfv_withOffset : clearBuffer ->
  int ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearBufferfi : clearBuffer ->
  int ->
  Js_of_ocaml__.Js.number_t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```

### 5\.14.10 Query objects

```ocaml
method createQuery : query Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteQuery : query Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method isQuery : query Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method beginQuery : queryTarget ->
  query Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method endQuery : queryTarget -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method getQuery : queryTarget ->
  currentQueryParam ->
  query Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method getQueryParameter : 'a. query Js_of_ocaml__.Js.t ->
  'a queryParam ->
  'a Js_of_ocaml__.Js.meth
```

### 5\.14.11 Sampler objects

```ocaml
method createSampler : sampler Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteSampler : sampler Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method isSampler : sampler Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method bindSampler : uint ->
  sampler Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindSampler_ : uint ->
  sampler Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method samplerParameteri : 'a. sampler Js_of_ocaml__.Js.t ->
  'a samplerParam ->
  'a ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method samplerParameterf : sampler Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t samplerParam ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getSamplerParameter : 'a. sampler Js_of_ocaml__.Js.t ->
  'a samplerParam ->
  'a Js_of_ocaml__.Js.meth
```

### 5\.14.12 Sync objects

```ocaml
method fenceSync : syncCondition ->
  int ->
  sync Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method isSync : sync Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteSync : sync Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method clientWaitSync : sync Js_of_ocaml__.Js.t ->
  int ->
  Js_of_ocaml__.Js.number_t ->
  clientWaitSyncStatus Js_of_ocaml__.Js.meth
```
```ocaml
method waitSync : sync Js_of_ocaml__.Js.t ->
  int ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getSyncParameter : 'a. sync Js_of_ocaml__.Js.t ->
  'a syncParam ->
  'a Js_of_ocaml__.Js.meth
```

### 5\.14.13 Transform feedback

```ocaml
method createTransformFeedback : transformFeedback Js_of_ocaml__.Js.t
                                   Js_of_ocaml__.Js.meth
```
```ocaml
method deleteTransformFeedback : transformFeedback Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method isTransformFeedback : transformFeedback Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method bindTransformFeedback : transformFeedbackTarget ->
  transformFeedback Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindTransformFeedback_ : transformFeedbackTarget ->
  transformFeedback Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method beginTransformFeedback : beginMode -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method endTransformFeedback : unit Js_of_ocaml__.Js.meth
```
```ocaml
method transformFeedbackVaryings : program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  transformFeedbackMode ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method getTransformFeedbackVarying : program Js_of_ocaml__.Js.t ->
  uint ->
  Js_of_ocaml__.WebGL.activeInfo Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method pauseTransformFeedback : unit Js_of_ocaml__.Js.meth
```
```ocaml
method resumeTransformFeedback : unit Js_of_ocaml__.Js.meth
```

### 5\.14.14 Uniform buffer objects

```ocaml
method getUniformIndices : program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  uint Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method getActiveUniforms : 'a. program Js_of_ocaml__.Js.t ->
  uint Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  'a activeUniformParam ->
  'a Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getUniformBlockIndex : program Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  uint Js_of_ocaml__.Js.meth
```
```ocaml
method getActiveUniformBlockParameter : 'a. program Js_of_ocaml__.Js.t ->
  uint ->
  'a uniformBlockParam ->
  'a Js_of_ocaml__.Js.meth
```
```ocaml
method getActiveUniformBlockName : program Js_of_ocaml__.Js.t ->
  uint ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method uniformBlockBinding : program Js_of_ocaml__.Js.t ->
  uint ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```

### 5\.14.15 Vertex array objects

```ocaml
method createVertexArray : vertexArrayObject Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.meth
```
```ocaml
method deleteVertexArray : vertexArrayObject Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method isVertexArray : vertexArrayObject Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method bindVertexArray : vertexArrayObject Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bindVertexArray_ : vertexArrayObject Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```

### 5\.14.16 Instanced and range drawing

```ocaml
method drawArraysInstanced : beginMode ->
  int ->
  sizei ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawElementsInstanced : beginMode ->
  sizei ->
  dataType ->
  intptr ->
  sizei ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawRangeElements : beginMode ->
  uint ->
  uint ->
  sizei ->
  dataType ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttribDivisor : uint -> uint -> unit Js_of_ocaml__.Js.meth
```

### 5\.14.17 Integer vertex attributes

```ocaml
method vertexAttribI4i : uint ->
  int ->
  int ->
  int ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttribI4ui : uint ->
  uint ->
  uint ->
  uint ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttribI4iv : uint ->
  Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttribI4uiv : uint ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method vertexAttribIPointer : uint ->
  int ->
  dataType ->
  sizei ->
  intptr ->
  unit Js_of_ocaml__.Js.meth
```

### 5\.14.18 Unsigned integer uniforms

```ocaml
method uniform1ui : int uniformLocation Js_of_ocaml__.Js.t ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2ui : [ `uvec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  uint ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3ui : [ `uvec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  uint ->
  uint ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4ui : [ `uvec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  uint ->
  uint ->
  uint ->
  uint ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform1uiv : int uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform2uiv : [ `uvec2 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform3uiv : [ `uvec3 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniform4uiv : [ `uvec4 ] uniformLocation Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.uint32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix2x3fv : [ `mat2x3 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix3x2fv : [ `mat3x2 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix2x4fv : [ `mat2x4 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix4x2fv : [ `mat4x2 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix3x4fv : [ `mat3x4 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method uniformMatrix4x3fv : [ `mat4x3 ] uniformLocation Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.float32Array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```

### New constants


#### Buffer targets and usages

```ocaml
method _COPY_READ_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COPY_WRITE_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PIXEL_PACK_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PIXEL_UNPACK_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_BUFFER_ : bufferTarget
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BUFFER_ : bufferTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STREAM_READ_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STREAM_COPY_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STATIC_READ_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STATIC_COPY_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DYNAMIC_READ_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DYNAMIC_COPY_ : bufferUsage Js_of_ocaml__.Js.readonly_prop
```

#### Framebuffer targets

```ocaml
method _READ_FRAMEBUFFER_ : fbTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_FRAMEBUFFER_ : fbTarget Js_of_ocaml__.Js.readonly_prop
```

#### New capabilities

```ocaml
method _RASTERIZER_DISCARD_ : enableCap Js_of_ocaml__.Js.readonly_prop
```

#### Texture targets

```ocaml
method _TEXTURE_3D_ : texTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_2D_ARRAY_ : texTarget Js_of_ocaml__.Js.readonly_prop
```

#### New texture parameters

```ocaml
method _TEXTURE_WRAP_R_ : wrapMode texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_BASE_LEVEL_ : int texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MAX_LEVEL_ : int texParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MIN_LOD_ : Js_of_ocaml__.Js.number_t texParam
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MAX_LOD_ : Js_of_ocaml__.Js.number_t texParam
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_COMPARE_MODE_ : textureCompareMode texParam
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_COMPARE_FUNC_ : depthFunction texParam
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COMPARE_REF_TO_TEXTURE_ : textureCompareMode
                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NONE_TCM : textureCompareMode Js_of_ocaml__.Js.readonly_prop
```
The `NONE` value for `_TEXTURE_COMPARE_MODE_`.

```ocaml
method _TEXTURE_IMMUTABLE_FORMAT_ : bool Js_of_ocaml__.Js.t texParam
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_IMMUTABLE_LEVELS_ : int texParam Js_of_ocaml__.Js.readonly_prop
```

#### New external formats

```ocaml
method _RED_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RED_INTEGER_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG_INTEGER_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB_INTEGER_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA_INTEGER_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```

#### New sized internal formats

```ocaml
method _R8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R8_SNORM_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R16F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R32F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R8UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R8I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R16UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R16I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R32UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R32I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG8_SNORM_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG16F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG32F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG8UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG8I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG16UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG16I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG32UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RG32I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SRGB8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB8_SNORM_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _R11F_G11F_B10F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB9_E5_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB16F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB32F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB8UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB8I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB16UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB16I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB32UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB32I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SRGB8_ALPHA8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA8_SNORM_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB10_A2_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA16F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA32F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA8UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA8I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGB10_A2UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA16UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA16I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA32UI_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _RGBA32I_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_COMPONENT24_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_COMPONENT32F_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH24_STENCIL8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH32F_STENCIL8_ : pixelFormat Js_of_ocaml__.Js.readonly_prop
```

#### New pixel types

```ocaml
method _HALF_FLOAT_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_2_10_10_10_REV_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_10F_11F_11F_REV_ : pixelType
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_5_9_9_9_REV_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_24_8_ : pixelType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_32_UNSIGNED_INT_24_8_REV_ : pixelType
                                            Js_of_ocaml__.Js.readonly_prop
```

#### New data types (vertex attributes)

```ocaml
method _HALF_FLOAT_DT : dataType Js_of_ocaml__.Js.readonly_prop
```

#### Draw / read buffers

```ocaml
method _BACK_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _NONE_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT1_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT2_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT3_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT4_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT5_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT6_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT7_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT8_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT9_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT10_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT11_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT12_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT13_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT14_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COLOR_ATTACHMENT15_ : attachmentPoint Js_of_ocaml__.Js.readonly_prop
```

#### clearBuffer targets

```ocaml
method _COLOR_CB : clearBuffer Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_CB : clearBuffer Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _STENCIL_CB : clearBuffer Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DEPTH_STENCIL_CB : clearBuffer Js_of_ocaml__.Js.readonly_prop
```

#### Query targets and parameters

```ocaml
method _ANY_SAMPLES_PASSED_ : queryTarget Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ANY_SAMPLES_PASSED_CONSERVATIVE_ : queryTarget
                                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_ : queryTarget
                                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CURRENT_QUERY_ : currentQueryParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _QUERY_RESULT_ : int queryParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _QUERY_RESULT_AVAILABLE_ : bool Js_of_ocaml__.Js.t queryParam
                                    Js_of_ocaml__.Js.readonly_prop
```

#### Sampler parameters

```ocaml
method _TEXTURE_MAG_FILTER_SP : texFilter samplerParam
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MIN_FILTER_SP : texFilter samplerParam
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_WRAP_S_SP : wrapMode samplerParam
                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_WRAP_T_SP : wrapMode samplerParam
                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_WRAP_R_SP : wrapMode samplerParam
                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MIN_LOD_SP : Js_of_ocaml__.Js.number_t samplerParam
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_MAX_LOD_SP : Js_of_ocaml__.Js.number_t samplerParam
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_COMPARE_MODE_SP : textureCompareMode samplerParam
                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TEXTURE_COMPARE_FUNC_SP : depthFunction samplerParam
                                    Js_of_ocaml__.Js.readonly_prop
```

#### Sync objects

```ocaml
method _SYNC_GPU_COMMANDS_COMPLETE_ : syncCondition
                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SYNC_FLUSH_COMMANDS_BIT_ : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SYNC_STATUS_ : syncStatus syncParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SYNC_CONDITION_ : syncCondition syncParam
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SYNC_FLAGS_ : int syncParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SIGNALED_ : syncStatus Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNALED_ : syncStatus Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ALREADY_SIGNALED_ : clientWaitSyncStatus Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TIMEOUT_EXPIRED_ : clientWaitSyncStatus Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _CONDITION_SATISFIED_ : clientWaitSyncStatus
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _WAIT_FAILED_ : clientWaitSyncStatus Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _OBJECT_TYPE_ : syncObjectType syncParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SYNC_FENCE_ : syncObjectType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TIMEOUT_IGNORED_ : Js_of_ocaml__.Js.number_t
                             Js_of_ocaml__.Js.readonly_prop
```
The special timeout value (`-1`), accepted only by `waitSync`; `clientWaitSync` timeouts are instead capped by `_MAX_CLIENT_WAIT_TIMEOUT_WEBGL_`.


#### Transform feedback

```ocaml
method _TRANSFORM_FEEDBACK_ : transformFeedbackTarget
                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INTERLEAVED_ATTRIBS_ : transformFeedbackMode
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SEPARATE_ATTRIBS_ : transformFeedbackMode
                              Js_of_ocaml__.Js.readonly_prop
```

#### Uniform buffer objects

```ocaml
method _UNIFORM_TYPE_ : int activeUniformParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_SIZE_ : int activeUniformParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_INDEX_ : int activeUniformParam
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_OFFSET_ : int activeUniformParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_ARRAY_STRIDE_ : int activeUniformParam
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_MATRIX_STRIDE_ : int activeUniformParam
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_IS_ROW_MAJOR_ : bool Js_of_ocaml__.Js.t activeUniformParam
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_BINDING_ : int uniformBlockParam
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_DATA_SIZE_ : int uniformBlockParam
                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_ACTIVE_UNIFORMS_ : int uniformBlockParam
                                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES_ : Js_of_ocaml__.Typed_array.uint32Array
                                                  Js_of_ocaml__.Js.t
                                                  uniformBlockParam
                                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER_ : bool Js_of_ocaml__.Js.t
                                                       uniformBlockParam
                                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER_ : bool Js_of_ocaml__.Js.t
                                                         uniformBlockParam
                                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INVALID_INDEX_ : uint Js_of_ocaml__.Js.readonly_prop
```
Returned by `getUniformIndices`/`getUniformBlockIndex` for unknown names (the value is `0xFFFFFFFF`, which does not fit a 32-bit OCaml `int`; only use it for equality tests).


#### New pixel store parameters

```ocaml
method _PACK_ROW_LENGTH_ : int pixelStoreParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PACK_SKIP_PIXELS_ : int pixelStoreParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PACK_SKIP_ROWS_ : int pixelStoreParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_ROW_LENGTH_ : int pixelStoreParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_IMAGE_HEIGHT_ : int pixelStoreParam
                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_SKIP_PIXELS_ : int pixelStoreParam
                                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_SKIP_ROWS_ : int pixelStoreParam Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNPACK_SKIP_IMAGES_ : int pixelStoreParam
                                Js_of_ocaml__.Js.readonly_prop
```

#### New integer query parameters

```ocaml
method _MAX_3D_TEXTURE_SIZE_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_ARRAY_TEXTURE_LAYERS_ : int parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_COLOR_ATTACHMENTS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_DRAW_BUFFERS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_SAMPLES_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_UNIFORM_BUFFER_BINDINGS_ : int parameter
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VERTEX_UNIFORM_BLOCKS_ : int parameter
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_FRAGMENT_UNIFORM_BLOCKS_ : int parameter
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BUFFER_OFFSET_ALIGNMENT_ : int parameter
                                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_ELEMENTS_VERTICES_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_ELEMENTS_INDICES_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VARYING_COMPONENTS_ : int parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VERTEX_UNIFORM_COMPONENTS_ : int parameter
                                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_FRAGMENT_UNIFORM_COMPONENTS_ : int parameter
                                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_VERTEX_OUTPUT_COMPONENTS_ : int parameter
                                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_FRAGMENT_INPUT_COMPONENTS_ : int parameter
                                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MIN_PROGRAM_TEXEL_OFFSET_ : int parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_PROGRAM_TEXEL_OFFSET_ : int parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_COMBINED_UNIFORM_BLOCKS_ : int parameter
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_ : int parameter
                                                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_ : int parameter
                                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_ : int parameter
                                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_ELEMENT_INDEX_ : Js_of_ocaml__.Js.number_t parameter
                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_SERVER_WAIT_TIMEOUT_ : Js_of_ocaml__.Js.number_t parameter
                                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_UNIFORM_BLOCK_SIZE_ : Js_of_ocaml__.Js.number_t parameter
                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS_ : Js_of_ocaml__.Js.number_t
                                                    parameter
                                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS_ : Js_of_ocaml__.Js.number_t
                                                      parameter
                                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_TEXTURE_LOD_BIAS_ : Js_of_ocaml__.Js.number_t parameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _MAX_CLIENT_WAIT_TIMEOUT_WEBGL_ : Js_of_ocaml__.Js.number_t parameter
                                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _READ_BUFFER_ : attachmentPoint parameter Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAGMENT_SHADER_DERIVATIVE_HINT_ : hintTarget
                                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_ACTIVE_ : bool Js_of_ocaml__.Js.t parameter
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_PAUSED_ : bool Js_of_ocaml__.Js.t parameter
                                       Js_of_ocaml__.Js.readonly_prop
```

#### Binding-point queries

```ocaml
method _VERTEX_ARRAY_BINDING_ : vertexArrayObject Js_of_ocaml__.Js.t
                                  Js_of_ocaml__.Js.opt
                                  parameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_BINDING_ : sampler Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                             parameter
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _READ_FRAMEBUFFER_BINDING_ : framebuffer Js_of_ocaml__.Js.t
                                      Js_of_ocaml__.Js.opt
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_FRAMEBUFFER_BINDING_ : framebuffer Js_of_ocaml__.Js.t
                                      Js_of_ocaml__.Js.opt
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COPY_READ_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                      Js_of_ocaml__.Js.opt
                                      parameter
                                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _COPY_WRITE_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                       Js_of_ocaml__.Js.opt
                                       parameter
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PIXEL_PACK_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                       Js_of_ocaml__.Js.opt
                                       parameter
                                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _PIXEL_UNPACK_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                         Js_of_ocaml__.Js.opt
                                         parameter
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_BINDING_ : transformFeedback Js_of_ocaml__.Js.t
                                        Js_of_ocaml__.Js.opt
                                        parameter
                                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER0_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER1_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER2_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER3_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER4_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER5_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER6_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER7_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER8_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER9_ : attachmentPoint parameter
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER10_ : attachmentPoint parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER11_ : attachmentPoint parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER12_ : attachmentPoint parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER13_ : attachmentPoint parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER14_ : attachmentPoint parameter
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _DRAW_BUFFER15_ : attachmentPoint parameter
                           Js_of_ocaml__.Js.readonly_prop
```

#### Indexed parameters

```ocaml
method _TRANSFORM_FEEDBACK_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                               Js_of_ocaml__.Js.opt
                                               indexedParameter
                                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_BUFFER_START_ : int indexedParameter
                                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_BUFFER_SIZE_ : int indexedParameter
                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BUFFER_BINDING_ : buffer Js_of_ocaml__.Js.t
                                    Js_of_ocaml__.Js.opt
                                    indexedParameter
                                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BUFFER_START_ : int indexedParameter
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNIFORM_BUFFER_SIZE_ : int indexedParameter
                                 Js_of_ocaml__.Js.readonly_prop
```

#### Internal format queries

```ocaml
method _SAMPLES_IFP : Js_of_ocaml__.Typed_array.int32Array Js_of_ocaml__.Js.t
                        internalformatParam
                        Js_of_ocaml__.Js.readonly_prop
```

#### Program and vertex attribute parameters

```ocaml
method _TRANSFORM_FEEDBACK_BUFFER_MODE_ : transformFeedbackMode programParam
                                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _TRANSFORM_FEEDBACK_VARYINGS_ : int programParam
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _ACTIVE_UNIFORM_BLOCKS_ : int programParam
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_INTEGER_ : bool Js_of_ocaml__.Js.t
                                         vertexAttribParam
                                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _VERTEX_ATTRIB_ARRAY_DIVISOR_ : int vertexAttribParam
                                         Js_of_ocaml__.Js.readonly_prop
```

#### New uniform types

```ocaml
method _UNSIGNED_INT_UT : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_VEC2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_VEC3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_VEC4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT2x3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT2x4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT3x2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT3x4_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT4x2_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_MAT4x3_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_3D_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_2D_SHADOW_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_2D_ARRAY_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_2D_ARRAY_SHADOW_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SAMPLER_CUBE_SHADOW_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_SAMPLER_2D_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_SAMPLER_3D_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_SAMPLER_CUBE_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_SAMPLER_2D_ARRAY_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_SAMPLER_2D_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_SAMPLER_3D_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_SAMPLER_CUBE_ : uniformType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_SAMPLER_2D_ARRAY_ : uniformType
                                           Js_of_ocaml__.Js.readonly_prop
```

#### Framebuffer introspection

```ocaml
method _FRAMEBUFFER_ATTACHMENT_RED_SIZE_ : int attachParam
                                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_GREEN_SIZE_ : int attachParam
                                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_BLUE_SIZE_ : int attachParam
                                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE_ : int attachParam
                                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE_ : int attachParam
                                               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE_ : int attachParam
                                                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_ : componentType attachParam
                                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_ : colorEncoding attachParam
                                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_ : int attachParam
                                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FLOAT_CT : componentType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _INT_CT : componentType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_INT_CT : componentType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SIGNED_NORMALIZED_ : componentType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _UNSIGNED_NORMALIZED_ : componentType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _LINEAR_CE : colorEncoding Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _SRGB_CE : colorEncoding Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_DEFAULT_ : objectType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_ : framebufferStatus
                                                Js_of_ocaml__.Js.readonly_prop
```