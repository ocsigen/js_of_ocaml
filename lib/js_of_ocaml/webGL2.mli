(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Ocsigen team
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

(** WebGL2 binding.

    WebGL2 ([WebGL2RenderingContext]) is a strict superset of WebGL1, so the
    context {!type:renderingContext} inherits every method and constant of
    {!WebGL.renderingContext} and adds the WebGL2 ones on top.

    Following the pragmatic style of {!WebGL}, the enumerations are kept as
    abstract types shared between related uses.  In particular the many WebGL2
    sized internal formats and the plain formats share the single type
    {!type:pixelFormat}; the binding therefore does not enforce, at compile
    time, that the (internalformat, format, type) triples passed to e.g.
    {!renderingContext.texImage3D} are mutually coherent (WebGL itself is not
    type safe there). *)

open Js

(** {2 Types reused from WebGL1} *)

type sizei = WebGL.sizei

type sizeiptr = WebGL.sizeiptr

type intptr = WebGL.intptr

type uint = WebGL.uint

type clampf = WebGL.clampf

type void = WebGL.void

type clearBufferMask = WebGL.clearBufferMask

type beginMode = WebGL.beginMode

type bufferTarget = WebGL.bufferTarget

type bufferUsage = WebGL.bufferUsage

type enableCap = WebGL.enableCap

type dataType = WebGL.dataType

type pixelType = WebGL.pixelType

type pixelFormat = WebGL.pixelFormat

type format = WebGL.format

type texTarget = WebGL.texTarget

type texFilter = WebGL.texFilter

type rbTarget = WebGL.rbTarget

type fbTarget = WebGL.fbTarget

type attachmentPoint = WebGL.attachmentPoint

type 'a parameter = 'a WebGL.parameter

type 'a pixelStoreParam = 'a WebGL.pixelStoreParam

type buffer = WebGL.buffer

type framebuffer = WebGL.framebuffer

type program = WebGL.program

type renderbuffer = WebGL.renderbuffer

type shader = WebGL.shader

type texture = WebGL.texture

type 'a uniformLocation = 'a WebGL.uniformLocation

type 'a texParam = 'a WebGL.texParam

type wrapMode = WebGL.wrapMode

type depthFunction = WebGL.depthFunction

(** {2 New WebGL2 objects} *)

type query

type sampler

type sync

type transformFeedback

type vertexArrayObject

(** {2 New WebGL2 enumerations} *)

type queryTarget

type currentQueryParam

type 'a queryParam

type syncCondition

type 'a syncParam

type syncStatus

type clientWaitSyncStatus

type clearBuffer

type transformFeedbackMode

type samplerParameterName

type 'a activeUniformParam

type 'a uniformBlockParam

type transformFeedbackTarget

type textureCompareMode

class type renderingContext = object
  inherit WebGL.renderingContext

  (** {2 5.14.2 Setting and getting state (buffers)} *)

  method copyBufferSubData :
    bufferTarget -> bufferTarget -> intptr -> intptr -> sizeiptr -> unit meth

  method getBufferSubData :
    bufferTarget -> intptr -> #Typed_array.arrayBufferView t -> unit meth

  method bindBufferBase : bufferTarget -> uint -> buffer t -> unit meth

  method bindBufferRange :
    bufferTarget -> uint -> buffer t -> intptr -> sizeiptr -> unit meth

  (** {2 5.14.3 Programs and shaders} *)

  method getFragDataLocation : program t -> js_string t -> int meth

  (** {2 5.14.4 3D textures and texture storage} *)

  method texImage3D :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> void opt
    -> unit meth

  method texImage3D_fromView :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method texImage3D_fromImageData :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.imageData t
    -> unit meth

  method texImage3D_fromImage :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.imageElement t
    -> unit meth

  method texImage3D_fromCanvas :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.canvasElement t
    -> unit meth

  method texImage3D_fromVideo :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.videoElement t
    -> unit meth

  method texSubImage3D_fromView :
       texTarget
    -> int
    -> int
    -> int
    -> int
    -> sizei
    -> sizei
    -> sizei
    -> pixelFormat
    -> pixelType
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method copyTexSubImage3D :
    texTarget -> int -> int -> int -> int -> int -> int -> sizei -> sizei -> unit meth

  method compressedTexImage3D :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> sizei
    -> int
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method compressedTexSubImage3D :
       texTarget
    -> int
    -> int
    -> int
    -> int
    -> sizei
    -> sizei
    -> sizei
    -> pixelFormat
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method texStorage2D : texTarget -> sizei -> pixelFormat -> sizei -> sizei -> unit meth

  method texStorage3D :
    texTarget -> sizei -> pixelFormat -> sizei -> sizei -> sizei -> unit meth

  (** {2 5.14.5 Framebuffer objects} *)

  method blitFramebuffer :
       int
    -> int
    -> int
    -> int
    -> int
    -> int
    -> int
    -> int
    -> clearBufferMask
    -> texFilter
    -> unit meth

  method framebufferTextureLayer :
    fbTarget -> attachmentPoint -> texture t -> int -> int -> unit meth

  method invalidateFramebuffer : fbTarget -> attachmentPoint js_array t -> unit meth

  method invalidateSubFramebuffer :
    fbTarget -> attachmentPoint js_array t -> int -> int -> sizei -> sizei -> unit meth

  method readBuffer : attachmentPoint -> unit meth

  (** {2 5.14.6 Renderbuffer objects} *)

  method renderbufferStorageMultisample :
    rbTarget -> sizei -> pixelFormat -> sizei -> sizei -> unit meth

  method renderbufferStorage_ : rbTarget -> pixelFormat -> sizei -> sizei -> unit meth
  (** [renderbufferStorage] accepting the WebGL2 sized internal formats
      ({!type:pixelFormat}); the WebGL1 [renderbufferStorage] taking a
      {!type:format} is still available through inheritance. *)

  (** {2 5.14.8 Texture objects} *)

  method texParameterf : texTarget -> number_t texParam -> number_t -> unit meth
  (** [texParameterf] for the float texture parameters {!_TEXTURE_MIN_LOD_} and
      {!_TEXTURE_MAX_LOD_}; the integer/enum parameters go through the inherited
      [texParameteri]. *)

  (** {2 5.14.9 Multiple render targets} *)

  method drawBuffers : attachmentPoint js_array t -> unit meth

  method clearBufferiv : clearBuffer -> int -> #Typed_array.arrayBufferView t -> unit meth

  method clearBufferuiv :
    clearBuffer -> int -> #Typed_array.arrayBufferView t -> unit meth

  method clearBufferfv : clearBuffer -> int -> #Typed_array.arrayBufferView t -> unit meth

  method clearBufferfi : clearBuffer -> int -> number_t -> int -> unit meth

  (** {2 5.14.10 Query objects} *)

  method createQuery : query t meth

  method deleteQuery : query t -> unit meth

  method isQuery : query t -> bool t meth

  method beginQuery : queryTarget -> query t -> unit meth

  method endQuery : queryTarget -> unit meth

  method getQuery : queryTarget -> currentQueryParam -> query t opt meth

  method getQueryParameter : 'a. query t -> 'a queryParam -> 'a meth

  (** {2 5.14.11 Sampler objects} *)

  method createSampler : sampler t meth

  method deleteSampler : sampler t -> unit meth

  method isSampler : sampler t -> bool t meth

  method bindSampler : uint -> sampler t opt -> unit meth

  method samplerParameteri : sampler t -> samplerParameterName -> int -> unit meth

  method samplerParameterf : sampler t -> samplerParameterName -> number_t -> unit meth

  method getSamplerParameter : 'a. sampler t -> samplerParameterName -> 'a meth

  (** {2 5.14.12 Sync objects} *)

  method fenceSync : syncCondition -> int -> sync t meth

  method isSync : sync t -> bool t meth

  method deleteSync : sync t -> unit meth

  method clientWaitSync : sync t -> int -> number_t -> clientWaitSyncStatus meth

  method waitSync : sync t -> int -> number_t -> unit meth

  method getSyncParameter : 'a. sync t -> 'a syncParam -> 'a meth

  (** {2 5.14.13 Transform feedback} *)

  method createTransformFeedback : transformFeedback t meth

  method deleteTransformFeedback : transformFeedback t -> unit meth

  method isTransformFeedback : transformFeedback t -> bool t meth

  method bindTransformFeedback :
    transformFeedbackTarget -> transformFeedback t opt -> unit meth

  method beginTransformFeedback : beginMode -> unit meth

  method endTransformFeedback : unit meth

  method transformFeedbackVaryings :
    program t -> js_string t js_array t -> transformFeedbackMode -> unit meth

  method getTransformFeedbackVarying : program t -> uint -> WebGL.activeInfo t opt meth

  method pauseTransformFeedback : unit meth

  method resumeTransformFeedback : unit meth

  (** {2 5.14.14 Uniform buffer objects} *)

  method getUniformIndices :
    program t -> js_string t js_array t -> uint js_array t opt meth

  method getActiveUniforms :
    'a. program t -> uint js_array t -> 'a activeUniformParam -> 'a js_array t meth

  method getUniformBlockIndex : program t -> js_string t -> uint meth

  method getActiveUniformBlockParameter :
    'a. program t -> uint -> 'a uniformBlockParam -> 'a meth

  method getActiveUniformBlockName : program t -> uint -> js_string t opt meth

  method uniformBlockBinding : program t -> uint -> uint -> unit meth

  (** {2 5.14.15 Vertex array objects} *)

  method createVertexArray : vertexArrayObject t meth

  method deleteVertexArray : vertexArrayObject t -> unit meth

  method isVertexArray : vertexArrayObject t -> bool t meth

  method bindVertexArray : vertexArrayObject t opt -> unit meth

  (** {2 5.14.16 Instanced and range drawing} *)

  method drawArraysInstanced : beginMode -> int -> sizei -> sizei -> unit meth

  method drawElementsInstanced :
    beginMode -> sizei -> dataType -> intptr -> sizei -> unit meth

  method drawRangeElements :
    beginMode -> uint -> uint -> sizei -> dataType -> intptr -> unit meth

  method vertexAttribDivisor : uint -> uint -> unit meth

  (** {2 5.14.17 Integer vertex attributes} *)

  method vertexAttribI4i : uint -> int -> int -> int -> int -> unit meth

  method vertexAttribI4ui : uint -> uint -> uint -> uint -> uint -> unit meth

  method vertexAttribI4iv : uint -> Typed_array.int32Array t -> unit meth

  method vertexAttribI4uiv : uint -> Typed_array.uint32Array t -> unit meth

  method vertexAttribIPointer : uint -> int -> dataType -> sizei -> intptr -> unit meth

  (** {2 5.14.18 Unsigned integer uniforms} *)

  method uniform1ui : int uniformLocation t -> uint -> unit meth

  method uniform2ui : [ `uvec2 ] uniformLocation t -> uint -> uint -> unit meth

  method uniform3ui : [ `uvec3 ] uniformLocation t -> uint -> uint -> uint -> unit meth

  method uniform4ui :
    [ `uvec4 ] uniformLocation t -> uint -> uint -> uint -> uint -> unit meth

  method uniform1uiv : int uniformLocation t -> Typed_array.uint32Array t -> unit meth

  method uniform2uiv :
    [ `uvec2 ] uniformLocation t -> Typed_array.uint32Array t -> unit meth

  method uniform3uiv :
    [ `uvec3 ] uniformLocation t -> Typed_array.uint32Array t -> unit meth

  method uniform4uiv :
    [ `uvec4 ] uniformLocation t -> Typed_array.uint32Array t -> unit meth

  method uniformMatrix2x3fv :
    [ `mat2x3 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix3x2fv :
    [ `mat3x2 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix2x4fv :
    [ `mat2x4 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix4x2fv :
    [ `mat4x2 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix3x4fv :
    [ `mat3x4 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix4x3fv :
    [ `mat4x3 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  (** {2 New constants} *)

  (** {3 Buffer targets and usages} *)

  method _COPY_READ_BUFFER_ : bufferTarget readonly_prop

  method _COPY_WRITE_BUFFER_ : bufferTarget readonly_prop

  method _PIXEL_PACK_BUFFER_ : bufferTarget readonly_prop

  method _PIXEL_UNPACK_BUFFER_ : bufferTarget readonly_prop

  method _TRANSFORM_FEEDBACK_BUFFER_ : bufferTarget readonly_prop

  method _UNIFORM_BUFFER_ : bufferTarget readonly_prop

  method _STREAM_READ_ : bufferUsage readonly_prop

  method _STREAM_COPY_ : bufferUsage readonly_prop

  method _STATIC_READ_ : bufferUsage readonly_prop

  method _STATIC_COPY_ : bufferUsage readonly_prop

  method _DYNAMIC_READ_ : bufferUsage readonly_prop

  method _DYNAMIC_COPY_ : bufferUsage readonly_prop

  (** {3 Framebuffer targets} *)

  method _READ_FRAMEBUFFER_ : fbTarget readonly_prop

  method _DRAW_FRAMEBUFFER_ : fbTarget readonly_prop

  (** {3 New capabilities} *)

  method _RASTERIZER_DISCARD_ : enableCap readonly_prop

  (** {3 Texture targets} *)

  method _TEXTURE_3D_ : texTarget readonly_prop

  method _TEXTURE_2D_ARRAY_ : texTarget readonly_prop

  (** {3 New texture parameters} *)

  method _TEXTURE_WRAP_R_ : wrapMode texParam readonly_prop

  method _TEXTURE_BASE_LEVEL_ : int texParam readonly_prop

  method _TEXTURE_MAX_LEVEL_ : int texParam readonly_prop

  method _TEXTURE_MIN_LOD_ : number_t texParam readonly_prop

  method _TEXTURE_MAX_LOD_ : number_t texParam readonly_prop

  method _TEXTURE_COMPARE_MODE_ : textureCompareMode texParam readonly_prop

  method _TEXTURE_COMPARE_FUNC_ : depthFunction texParam readonly_prop

  method _COMPARE_REF_TO_TEXTURE_ : textureCompareMode readonly_prop

  method _NONE_TCM : textureCompareMode readonly_prop
  (** The [NONE] value for {!_TEXTURE_COMPARE_MODE_}. *)

  (** {3 New external formats} *)

  method _RED_ : pixelFormat readonly_prop

  method _RG_ : pixelFormat readonly_prop

  method _RED_INTEGER_ : pixelFormat readonly_prop

  method _RG_INTEGER_ : pixelFormat readonly_prop

  method _RGB_INTEGER_ : pixelFormat readonly_prop

  method _RGBA_INTEGER_ : pixelFormat readonly_prop

  (** {3 New sized internal formats} *)

  method _R8_ : pixelFormat readonly_prop

  method _R8_SNORM_ : pixelFormat readonly_prop

  method _R16F_ : pixelFormat readonly_prop

  method _R32F_ : pixelFormat readonly_prop

  method _R8UI_ : pixelFormat readonly_prop

  method _R8I_ : pixelFormat readonly_prop

  method _R16UI_ : pixelFormat readonly_prop

  method _R16I_ : pixelFormat readonly_prop

  method _R32UI_ : pixelFormat readonly_prop

  method _R32I_ : pixelFormat readonly_prop

  method _RG8_ : pixelFormat readonly_prop

  method _RG8_SNORM_ : pixelFormat readonly_prop

  method _RG16F_ : pixelFormat readonly_prop

  method _RG32F_ : pixelFormat readonly_prop

  method _RG8UI_ : pixelFormat readonly_prop

  method _RG8I_ : pixelFormat readonly_prop

  method _RG16UI_ : pixelFormat readonly_prop

  method _RG16I_ : pixelFormat readonly_prop

  method _RG32UI_ : pixelFormat readonly_prop

  method _RG32I_ : pixelFormat readonly_prop

  method _RGB8_ : pixelFormat readonly_prop

  method _SRGB8_ : pixelFormat readonly_prop

  method _RGB8_SNORM_ : pixelFormat readonly_prop

  method _R11F_G11F_B10F_ : pixelFormat readonly_prop

  method _RGB9_E5_ : pixelFormat readonly_prop

  method _RGB16F_ : pixelFormat readonly_prop

  method _RGB32F_ : pixelFormat readonly_prop

  method _RGB8UI_ : pixelFormat readonly_prop

  method _RGB8I_ : pixelFormat readonly_prop

  method _RGB16UI_ : pixelFormat readonly_prop

  method _RGB16I_ : pixelFormat readonly_prop

  method _RGB32UI_ : pixelFormat readonly_prop

  method _RGB32I_ : pixelFormat readonly_prop

  method _RGBA8_ : pixelFormat readonly_prop

  method _SRGB8_ALPHA8_ : pixelFormat readonly_prop

  method _RGBA8_SNORM_ : pixelFormat readonly_prop

  method _RGB10_A2_ : pixelFormat readonly_prop

  method _RGBA16F_ : pixelFormat readonly_prop

  method _RGBA32F_ : pixelFormat readonly_prop

  method _RGBA8UI_ : pixelFormat readonly_prop

  method _RGBA8I_ : pixelFormat readonly_prop

  method _RGB10_A2UI_ : pixelFormat readonly_prop

  method _RGBA16UI_ : pixelFormat readonly_prop

  method _RGBA16I_ : pixelFormat readonly_prop

  method _RGBA32UI_ : pixelFormat readonly_prop

  method _RGBA32I_ : pixelFormat readonly_prop

  method _DEPTH_COMPONENT24_ : pixelFormat readonly_prop

  method _DEPTH_COMPONENT32F_ : pixelFormat readonly_prop

  method _DEPTH24_STENCIL8_ : pixelFormat readonly_prop

  method _DEPTH32F_STENCIL8_ : pixelFormat readonly_prop

  (** {3 New pixel types} *)

  method _HALF_FLOAT_ : pixelType readonly_prop

  method _UNSIGNED_INT_2_10_10_10_REV_ : pixelType readonly_prop

  method _UNSIGNED_INT_10F_11F_11F_REV_ : pixelType readonly_prop

  method _UNSIGNED_INT_5_9_9_9_REV_ : pixelType readonly_prop

  method _UNSIGNED_INT_24_8_ : pixelType readonly_prop

  method _FLOAT_32_UNSIGNED_INT_24_8_REV_ : pixelType readonly_prop

  (** {3 New data types (vertex attributes)} *)

  method _HALF_FLOAT_DT : dataType readonly_prop

  (** {3 Draw / read buffers} *)

  method _BACK_ : attachmentPoint readonly_prop

  method _NONE_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT1_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT2_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT3_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT4_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT5_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT6_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT7_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT8_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT9_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT10_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT11_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT12_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT13_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT14_ : attachmentPoint readonly_prop

  method _COLOR_ATTACHMENT15_ : attachmentPoint readonly_prop

  (** {3 clearBuffer targets} *)

  method _COLOR_CB : clearBuffer readonly_prop

  method _DEPTH_CB : clearBuffer readonly_prop

  method _STENCIL_CB : clearBuffer readonly_prop

  method _DEPTH_STENCIL_CB : clearBuffer readonly_prop

  (** {3 Query targets and parameters} *)

  method _ANY_SAMPLES_PASSED_ : queryTarget readonly_prop

  method _ANY_SAMPLES_PASSED_CONSERVATIVE_ : queryTarget readonly_prop

  method _TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_ : queryTarget readonly_prop

  method _CURRENT_QUERY_ : currentQueryParam readonly_prop

  method _QUERY_RESULT_ : int queryParam readonly_prop

  method _QUERY_RESULT_AVAILABLE_ : bool t queryParam readonly_prop

  (** {3 Sampler parameters} *)

  method _TEXTURE_MAG_FILTER_SP : samplerParameterName readonly_prop

  method _TEXTURE_MIN_FILTER_SP : samplerParameterName readonly_prop

  method _TEXTURE_WRAP_S_SP : samplerParameterName readonly_prop

  method _TEXTURE_WRAP_T_SP : samplerParameterName readonly_prop

  method _TEXTURE_WRAP_R_SP : samplerParameterName readonly_prop

  method _TEXTURE_MIN_LOD_SP : samplerParameterName readonly_prop

  method _TEXTURE_MAX_LOD_SP : samplerParameterName readonly_prop

  method _TEXTURE_COMPARE_MODE_SP : samplerParameterName readonly_prop

  method _TEXTURE_COMPARE_FUNC_SP : samplerParameterName readonly_prop

  (** {3 Sync objects} *)

  method _SYNC_GPU_COMMANDS_COMPLETE_ : syncCondition readonly_prop

  method _SYNC_FLUSH_COMMANDS_BIT_ : int readonly_prop

  method _SYNC_STATUS_ : syncStatus syncParam readonly_prop

  method _SYNC_CONDITION_ : syncCondition syncParam readonly_prop

  method _SYNC_FLAGS_ : int syncParam readonly_prop

  method _SIGNALED_ : syncStatus readonly_prop

  method _UNSIGNALED_ : syncStatus readonly_prop

  method _ALREADY_SIGNALED_ : clientWaitSyncStatus readonly_prop

  method _TIMEOUT_EXPIRED_ : clientWaitSyncStatus readonly_prop

  method _CONDITION_SATISFIED_ : clientWaitSyncStatus readonly_prop

  method _WAIT_FAILED_ : clientWaitSyncStatus readonly_prop

  (** {3 Transform feedback} *)

  method _TRANSFORM_FEEDBACK_ : transformFeedbackTarget readonly_prop

  method _INTERLEAVED_ATTRIBS_ : transformFeedbackMode readonly_prop

  method _SEPARATE_ATTRIBS_ : transformFeedbackMode readonly_prop

  (** {3 Uniform buffer objects} *)

  method _UNIFORM_TYPE_ : int activeUniformParam readonly_prop

  method _UNIFORM_SIZE_ : int activeUniformParam readonly_prop

  method _UNIFORM_BLOCK_INDEX_ : int activeUniformParam readonly_prop

  method _UNIFORM_OFFSET_ : int activeUniformParam readonly_prop

  method _UNIFORM_ARRAY_STRIDE_ : int activeUniformParam readonly_prop

  method _UNIFORM_MATRIX_STRIDE_ : int activeUniformParam readonly_prop

  method _UNIFORM_IS_ROW_MAJOR_ : bool t activeUniformParam readonly_prop

  method _UNIFORM_BLOCK_BINDING_ : int uniformBlockParam readonly_prop

  method _UNIFORM_BLOCK_DATA_SIZE_ : int uniformBlockParam readonly_prop

  method _UNIFORM_BLOCK_ACTIVE_UNIFORMS_ : int uniformBlockParam readonly_prop

  method _UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES_ :
    Typed_array.uint32Array t uniformBlockParam readonly_prop

  method _UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER_ :
    bool t uniformBlockParam readonly_prop

  method _UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER_ :
    bool t uniformBlockParam readonly_prop

  (** {3 New pixel store parameters} *)

  method _PACK_ROW_LENGTH_ : int pixelStoreParam readonly_prop

  method _PACK_SKIP_PIXELS_ : int pixelStoreParam readonly_prop

  method _PACK_SKIP_ROWS_ : int pixelStoreParam readonly_prop

  method _UNPACK_ROW_LENGTH_ : int pixelStoreParam readonly_prop

  method _UNPACK_IMAGE_HEIGHT_ : int pixelStoreParam readonly_prop

  method _UNPACK_SKIP_PIXELS_ : int pixelStoreParam readonly_prop

  method _UNPACK_SKIP_ROWS_ : int pixelStoreParam readonly_prop

  method _UNPACK_SKIP_IMAGES_ : int pixelStoreParam readonly_prop

  (** {3 New integer query parameters} *)

  method _MAX_3D_TEXTURE_SIZE_ : int parameter readonly_prop

  method _MAX_ARRAY_TEXTURE_LAYERS_ : int parameter readonly_prop

  method _MAX_COLOR_ATTACHMENTS_ : int parameter readonly_prop

  method _MAX_DRAW_BUFFERS_ : int parameter readonly_prop

  method _MAX_SAMPLES_ : int parameter readonly_prop

  method _MAX_UNIFORM_BUFFER_BINDINGS_ : int parameter readonly_prop

  method _MAX_VERTEX_UNIFORM_BLOCKS_ : int parameter readonly_prop

  method _MAX_FRAGMENT_UNIFORM_BLOCKS_ : int parameter readonly_prop

  method _UNIFORM_BUFFER_OFFSET_ALIGNMENT_ : int parameter readonly_prop
end

(** {2 Getting a WebGL2 context} *)

val getContext : Dom_html.canvasElement t -> renderingContext t opt

val getContextWithAttributes :
  Dom_html.canvasElement t -> WebGL.contextAttributes t -> renderingContext t opt
