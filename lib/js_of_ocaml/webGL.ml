(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2012 Jérôme Vouillon
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
open! Import

(** 5.1 Types *)

type sizei = int

type sizeiptr = int

type intptr = int

type uint = int

type clampf = number_t

type void

type clearBufferMask = int

type beginMode

type blendingFactor

type blendMode

type bufferTarget

type bufferUsage

type cullFaceMode

type depthFunction

type enableCap

type errorCode

type frontFaceDir

type hintTarget

type hintMode

type textureUnit = int

type 'a pixelStoreParam

type stencilOp

type fbTarget

type attachmentPoint

type rbTarget

type texTarget

type 'a parameter

type 'a bufferParameter

type 'a vertexAttribParam

type vertexAttribPointerParam

type 'a attachParam

type framebufferStatus

type 'a renderbufferParam

type format

type pixelFormat

type pixelType

type 'a texParam

type dataType

type shaderType

type 'a programParam

type 'a shaderParam

type textureFilter

type wrapMode

type texFilter

type uniformType

type colorspaceConversion

type shaderPrecisionType

type objectType

(** 5.2 WebGLContextAttributes *)
class type contextAttributes = object
  method alpha : bool t prop

  method depth : bool t prop

  method stencil : bool t prop

  method antialias : bool t prop

  method premultipliedAlpha : bool t prop

  method preserveDrawingBuffer : bool t prop

  method preferLowPowerToHighPerformance : bool t prop

  method failIfMajorPerformanceCaveat : bool t prop
end

let defaultContextAttributes =
  Js.Unsafe.(
    obj
      [| "alpha", inject _true
       ; "depth", inject _true
       ; "stencil", inject _false
       ; "antialias", inject _true
       ; "premultipliedAlpha", inject _false
       ; "preserveDrawingBuffer", inject _false
       ; "preferLowPowerToHighPerformance", inject _false
       ; "failIfMajorPerformanceCaveat", inject _false
      |])

type buffer

type framebuffer

type program

type renderbuffer

type shader

type texture

type 'a uniformLocation

class type activeInfo = object
  method size : int readonly_prop

  method _type : uniformType readonly_prop

  method name : js_string t readonly_prop
end

class type shaderPrecisionFormat = object
  method rangeMin : int readonly_prop

  method rangeMax : int readonly_prop

  method precision : int readonly_prop
end

class type renderingContext = object
  (** 5.13.1 Attributes *)

  method canvas : Dom_html.canvasElement t readonly_prop

  method drawingBufferWidth : sizei readonly_prop

  method drawingBufferHeight : sizei readonly_prop

  (** 5.13.2 Getting information about the context *)

  method getContextAttributes : contextAttributes t meth

  (** 5.13.3 Setting and getting state *)

  method activeTexture : textureUnit -> unit meth

  method blendColor : clampf -> clampf -> clampf -> clampf -> unit meth

  method blendEquation : blendMode -> unit meth

  method blendEquationSeparate : blendMode -> blendMode -> unit meth

  method blendFunc : blendingFactor -> blendingFactor -> unit meth

  method blendFuncSeparate :
    blendingFactor -> blendingFactor -> blendingFactor -> blendingFactor -> unit meth

  method clearColor : clampf -> clampf -> clampf -> clampf -> unit meth

  method clearDepth : clampf -> unit meth

  method clearStencil : int -> unit meth

  method colorMask : bool t -> bool t -> bool t -> bool t -> unit meth

  method cullFace : cullFaceMode -> unit meth

  method depthFunc : depthFunction -> unit meth

  method depthMask : bool t -> unit meth

  method depthRange : clampf -> clampf -> unit meth

  method disable : enableCap -> unit meth

  method enable : enableCap -> unit meth

  method frontFace : frontFaceDir -> unit meth

  method getParameter : 'a. 'a parameter -> 'a meth

  method getError : errorCode meth

  method hint : hintTarget -> hintMode -> unit meth

  method isEnabled : enableCap -> bool t meth

  method lineWidth : number_t -> unit meth

  method pixelStorei : 'a. 'a pixelStoreParam -> 'a -> unit meth

  method polygonOffset : number_t -> number_t -> unit meth

  method sampleCoverage : clampf -> bool t -> unit meth

  method stencilFunc : depthFunction -> int -> uint -> unit meth

  method stencilFuncSeparate : cullFaceMode -> depthFunction -> int -> uint -> unit meth

  method stencilMask : uint -> unit meth

  method stencilMaskSeparate : cullFaceMode -> uint -> unit meth

  method stencilOp : stencilOp -> stencilOp -> stencilOp -> unit meth

  method stencilOpSeparate :
    cullFaceMode -> stencilOp -> stencilOp -> stencilOp -> unit meth

  (** 5.13.4 Viewing and clipping *)

  method scissor : int -> int -> sizei -> sizei -> unit meth

  method viewport : int -> int -> sizei -> sizei -> unit meth

  (** 5.13.5 Buffer objects *)

  method bindBuffer : bufferTarget -> buffer t -> unit meth

  method bindBuffer_ : bufferTarget -> buffer t opt -> unit meth

  method bufferData_create : bufferTarget -> sizeiptr -> bufferUsage -> unit meth

  method bufferData :
    bufferTarget -> #Typed_array.arrayBufferView t -> bufferUsage -> unit meth

  method bufferData_raw :
    bufferTarget -> Typed_array.arrayBuffer t -> bufferUsage -> unit meth

  method bufferSubData :
    bufferTarget -> intptr -> #Typed_array.arrayBufferView t -> unit meth

  method bufferSubData_raw :
    bufferTarget -> intptr -> Typed_array.arrayBuffer t -> unit meth

  method createBuffer : buffer t meth

  method deleteBuffer : buffer t -> unit meth

  method getBufferParameter : 'a. bufferTarget -> 'a bufferParameter -> 'a meth

  method isBuffer : buffer t -> bool t meth

  (** 5.13.6 Framebuffer objects *)

  method bindFramebuffer : fbTarget -> framebuffer t -> unit meth

  method bindFramebuffer_ : fbTarget -> framebuffer t opt -> unit meth

  method checkFramebufferStatus : fbTarget -> framebufferStatus meth

  method createFramebuffer : framebuffer t meth

  method deleteFramebuffer : framebuffer t -> unit meth

  method framebufferRenderbuffer :
    fbTarget -> attachmentPoint -> rbTarget -> renderbuffer t -> unit meth

  method framebufferTexture2D :
    fbTarget -> attachmentPoint -> texTarget -> texture t -> int -> unit meth

  method getFramebufferAttachmentParameter :
    'a. fbTarget -> attachmentPoint -> 'a attachParam -> 'a meth

  method isFramebuffer : framebuffer t -> bool t meth

  (** 5.13.7 Renderbuffer objects *)

  method bindRenderbuffer : rbTarget -> renderbuffer t -> unit meth

  method bindRenderbuffer_ : rbTarget -> renderbuffer t opt -> unit meth

  method createRenderbuffer : renderbuffer t meth

  method deleteRenderbuffer : renderbuffer t -> unit meth

  method getRenderbufferParameter : 'a. rbTarget -> 'a renderbufferParam -> 'a meth

  method isRenderbuffer : renderbuffer t -> bool t meth

  method renderbufferStorage : rbTarget -> format -> sizei -> sizei -> unit meth

  (** 5.13.8 Texture objects *)

  method bindTexture : texTarget -> texture t -> unit meth

  method bindTexture_ : texTarget -> texture t opt -> unit meth

  method compressedTexImage2D :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> int
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method compressedTexSubImage2D :
       texTarget
    -> int
    -> int
    -> int
    -> sizei
    -> sizei
    -> pixelFormat
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method copyTexImage2D :
    texTarget -> int -> pixelFormat -> int -> int -> sizei -> sizei -> int -> unit meth

  method copyTexSubImage2D :
    texTarget -> int -> int -> int -> int -> int -> sizei -> sizei -> unit meth

  method createTexture : texture t meth

  method deleteTexture : texture t -> unit meth

  method generateMipmap : texTarget -> unit meth

  method getTexParameter : texTarget -> 'a texParam -> 'a meth

  method isTexture : texture t -> bool t meth

  method texImage2D_new :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> void opt
    -> unit meth

  method texImage2D_fromView :
       texTarget
    -> int
    -> pixelFormat
    -> sizei
    -> sizei
    -> int
    -> pixelFormat
    -> pixelType
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method texImage2D_fromImageData :
       texTarget
    -> int
    -> pixelFormat
    -> pixelFormat
    -> pixelType
    -> Dom_html.imageData t
    -> unit meth

  method texImage2D_fromImage :
       texTarget
    -> int
    -> pixelFormat
    -> pixelFormat
    -> pixelType
    -> Dom_html.imageElement t
    -> unit meth

  method texImage2D_fromCanvas :
       texTarget
    -> int
    -> pixelFormat
    -> pixelFormat
    -> pixelType
    -> Dom_html.canvasElement t
    -> unit meth

  method texImage2D_fromVideo :
       texTarget
    -> int
    -> pixelFormat
    -> pixelFormat
    -> pixelType
    -> Dom_html.videoElement t
    -> unit meth

  (* {[
      method texParameterf : texTarget -> texParam -> number_t -> unit meth
     ]}
  *)
  method texParameteri : texTarget -> 'a texParam -> 'a -> unit meth

  method texSubImage2D_fromView :
       texTarget
    -> int
    -> int
    -> int
    -> sizei
    -> sizei
    -> pixelFormat
    -> pixelType
    -> #Typed_array.arrayBufferView t
    -> unit meth

  method texSubImage2D_fromImageData :
       texTarget
    -> int
    -> int
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.imageData t
    -> unit meth

  method texSubImage2D_fromImage :
       texTarget
    -> int
    -> int
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.imageElement t
    -> unit meth

  method texSubImage2D_fromCanvas :
       texTarget
    -> int
    -> int
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.canvasElement t
    -> unit meth

  method texSubImage2D_fromVideo :
       texTarget
    -> int
    -> int
    -> int
    -> pixelFormat
    -> pixelType
    -> Dom_html.videoElement t
    -> unit meth

  (** 5.13.9 Programs and Shaders *)

  method attachShader : program t -> shader t -> unit meth

  method bindAttribLocation : program t -> uint -> js_string t -> unit meth

  method compileShader : shader t -> unit meth

  method createProgram : program t meth

  method createShader : shaderType -> shader t meth

  method deleteProgram : program t -> unit meth

  method deleteShader : shader t -> unit meth

  method detachShader : program t -> shader t -> unit meth

  method getAttachedShaders : program t -> shader t js_array t meth

  method getProgramParameter : 'a. program t -> 'a programParam -> 'a meth

  method getProgramInfoLog : program t -> js_string t meth

  method getShaderParameter : 'a. shader t -> 'a shaderParam -> 'a meth

  method getShaderPrecisionFormat :
    shaderType -> shaderPrecisionType -> shaderPrecisionFormat t meth

  method getShaderInfoLog : shader t -> js_string t meth

  method getShaderSource : shader t -> js_string t meth

  method isProgram : program t -> bool t meth

  method isShader : shader t -> bool t meth

  method linkProgram : program t -> unit meth

  method shaderSource : shader t -> js_string t -> unit meth

  method useProgram : program t -> unit meth

  method validateProgram : program t -> unit meth

  (** 5.13.10 Uniforms and attributes *)

  method disableVertexAttribArray : uint -> unit meth

  method enableVertexAttribArray : uint -> unit meth

  method getActiveAttrib : program t -> uint -> activeInfo t meth

  method getActiveUniform : program t -> uint -> activeInfo t meth

  method getAttribLocation : program t -> js_string t -> int meth

  method getUniform : 'a 'b. program t -> 'a uniformLocation t -> 'b meth

  method getUniformLocation : 'a. program t -> js_string t -> 'a uniformLocation t meth

  method getVertexAttrib : 'a. uint -> 'a vertexAttribParam -> 'a meth

  method getVertexAttribOffset : uint -> vertexAttribPointerParam -> sizeiptr meth

  method uniform1f : number_t uniformLocation t -> number_t -> unit meth

  method uniform1fv_typed :
    number_t uniformLocation t -> Typed_array.float32Array t -> unit meth

  method uniform1fv : number_t uniformLocation t -> number_t js_array t -> unit meth

  method uniform1i : int uniformLocation t -> int -> unit meth

  method uniform1iv_typed : int uniformLocation t -> Typed_array.int32Array t -> unit meth

  method uniform1iv : int uniformLocation t -> int js_array t -> unit meth

  method uniform2f : [ `vec2 ] uniformLocation t -> number_t -> number_t -> unit meth

  method uniform2fv_typed :
    [ `vec2 ] uniformLocation t -> Typed_array.float32Array t -> unit meth

  method uniform2fv : [ `vec2 ] uniformLocation t -> number_t js_array t -> unit meth

  method uniform2i : [ `ivec2 ] uniformLocation t -> int -> int -> unit meth

  method uniform2iv : [ `ivec2 ] uniformLocation t -> int js_array t -> unit meth

  method uniform2iv_typed :
    [ `ivec2 ] uniformLocation t -> Typed_array.int32Array t -> unit meth

  method uniform3f :
    [ `vec3 ] uniformLocation t -> number_t -> number_t -> number_t -> unit meth

  method uniform3fv_typed :
    [ `vec3 ] uniformLocation t -> Typed_array.float32Array t -> unit meth

  method uniform3fv : [ `vec3 ] uniformLocation t -> number_t js_array t -> unit meth

  method uniform3i : [ `ivec3 ] uniformLocation t -> int -> int -> int -> unit meth

  method uniform3iv : [ `ivec3 ] uniformLocation t -> int js_array t -> unit meth

  method uniform3iv_typed :
    [ `ivec3 ] uniformLocation t -> Typed_array.int32Array t -> unit meth

  method uniform4f :
       [ `vec4 ] uniformLocation t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> unit meth

  method uniform4fv_typed :
    [ `vec4 ] uniformLocation t -> Typed_array.float32Array t -> unit meth

  method uniform4fv : [ `vec4 ] uniformLocation t -> number_t js_array t -> unit meth

  method uniform4i : [ `ivec4 ] uniformLocation t -> int -> int -> int -> int -> unit meth

  method uniform4iv : [ `ivec4 ] uniformLocation t -> int js_array t -> unit meth

  method uniform4iv_typed :
    [ `ivec4 ] uniformLocation t -> Typed_array.int32Array t -> unit meth

  method uniformMatrix2fv :
    [ `mat2 ] uniformLocation t -> bool t -> number_t js_array t -> unit meth

  method uniformMatrix2fv_typed :
    [ `mat2 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix3fv :
    [ `mat3 ] uniformLocation t -> bool t -> number_t js_array t -> unit meth

  method uniformMatrix3fv_typed :
    [ `mat3 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method uniformMatrix4fv :
    [ `mat4 ] uniformLocation t -> bool t -> number_t js_array t -> unit meth

  method uniformMatrix4fv_typed :
    [ `mat4 ] uniformLocation t -> bool t -> Typed_array.float32Array t -> unit meth

  method vertexAttrib1f : uint -> number_t -> unit meth

  method vertexAttrib1fv : uint -> number_t js_array t -> unit meth

  method vertexAttrib1fv_typed : uint -> Typed_array.float32Array t -> unit meth

  method vertexAttrib2f : uint -> number_t -> number_t -> unit meth

  method vertexAttrib2fv : uint -> number_t js_array t -> unit meth

  method vertexAttrib2fv_typed : uint -> Typed_array.float32Array t -> unit meth

  method vertexAttrib3f : uint -> number_t -> number_t -> number_t -> unit meth

  method vertexAttrib3fv : uint -> number_t js_array t -> unit meth

  method vertexAttrib3fv_typed : uint -> Typed_array.float32Array t -> unit meth

  method vertexAttrib4f :
    uint -> number_t -> number_t -> number_t -> number_t -> unit meth

  method vertexAttrib4fv : uint -> number_t js_array t -> unit meth

  method vertexAttrib4fv_typed : uint -> Typed_array.float32Array t -> unit meth

  method vertexAttribPointer :
    uint -> int -> dataType -> bool t -> sizei -> intptr -> unit meth

  (** 5.13.11 Writing to the drawing buffer *)

  method clear : clearBufferMask -> unit meth

  method drawArrays : beginMode -> int -> sizei -> unit meth

  method drawElements : beginMode -> sizei -> dataType -> intptr -> unit meth

  method finish : unit meth

  method flush : unit meth

  (** 5.13.12 Reading back pixels *)

  method readPixels :
       int
    -> int
    -> sizei
    -> sizei
    -> pixelFormat
    -> pixelType
    -> #Typed_array.arrayBufferView t
    -> unit meth

  (** 5.13.13 Detecting context lost events *)

  method isContextLost : bool t meth

  (** 5.13.14 Detecting and enabling extensions *)

  method getSupportedExtensions : js_string t js_array t meth

  method getExtension : 'a. js_string t -> 'a t opt meth

  (* Untyped! *)
  (** Constants *)

  method _DEPTH_BUFFER_BIT_ : clearBufferMask readonly_prop

  method _STENCIL_BUFFER_BIT_ : clearBufferMask readonly_prop

  method _COLOR_BUFFER_BIT_ : clearBufferMask readonly_prop

  method _POINTS : beginMode readonly_prop

  method _LINES : beginMode readonly_prop

  method _LINE_LOOP_ : beginMode readonly_prop

  method _LINE_STRIP_ : beginMode readonly_prop

  method _TRIANGLES : beginMode readonly_prop

  method _TRIANGLE_STRIP_ : beginMode readonly_prop

  method _TRIANGLE_FAN_ : beginMode readonly_prop

  method _ZERO : blendingFactor readonly_prop

  method _ONE : blendingFactor readonly_prop

  method _SRC_COLOR_ : blendingFactor readonly_prop

  method _ONE_MINUS_SRC_COLOR_ : blendingFactor readonly_prop

  method _SRC_ALPHA_ : blendingFactor readonly_prop

  method _ONE_MINUS_SRC_ALPHA_ : blendingFactor readonly_prop

  method _DST_ALPHA_ : blendingFactor readonly_prop

  method _ONE_MINUS_DST_ALPHA_ : blendingFactor readonly_prop

  method _DST_COLOR_ : blendingFactor readonly_prop

  method _ONE_MINUS_DST_COLOR_ : blendingFactor readonly_prop

  method _SRC_ALPHA_SATURATE_ : blendingFactor readonly_prop

  method _FUNC_ADD_ : blendMode readonly_prop

  method _FUNC_SUBTRACT_ : blendMode readonly_prop

  method _FUNC_REVERSE_SUBTRACT_ : blendMode readonly_prop

  method _CONSTANT_COLOR_ : blendMode readonly_prop

  method _ONE_MINUS_CONSTANT_COLOR_ : blendMode readonly_prop

  method _CONSTANT_ALPHA_ : blendMode readonly_prop

  method _ONE_MINUS_CONSTANT_ALPHA_ : blendMode readonly_prop

  method _ARRAY_BUFFER_ : bufferTarget readonly_prop

  method _ELEMENT_ARRAY_BUFFER_ : bufferTarget readonly_prop

  method _STREAM_DRAW_ : bufferUsage readonly_prop

  method _STATIC_DRAW_ : bufferUsage readonly_prop

  method _DYNAMIC_DRAW_ : bufferUsage readonly_prop

  method _FRONT : cullFaceMode readonly_prop

  method _BACK : cullFaceMode readonly_prop

  method _FRONT_AND_BACK_ : cullFaceMode readonly_prop

  method _CULL_FACE_ : enableCap readonly_prop

  method _BLEND : enableCap readonly_prop

  method _DITHER : enableCap readonly_prop

  method _STENCIL_TEST_ : enableCap readonly_prop

  method _DEPTH_TEST_ : enableCap readonly_prop

  method _SCISSOR_TEST_ : enableCap readonly_prop

  method _POLYGON_OFFSET_FILL_ : enableCap readonly_prop

  method _SAMPLE_ALPHA_TO_COVERAGE_ : enableCap readonly_prop

  method _SAMPLE_COVERAGE_ : enableCap readonly_prop

  method _NO_ERROR_ : errorCode readonly_prop

  method _INVALID_ENUM_ : errorCode readonly_prop

  method _INVALID_VALUE_ : errorCode readonly_prop

  method _INVALID_OPERATION_ : errorCode readonly_prop

  method _OUT_OF_MEMORY_ : errorCode readonly_prop

  method _CONTEXT_LOST_WEBGL_ : errorCode readonly_prop

  method _INVALID_FRAMEBUFFER_OPERATION_ : errorCode readonly_prop

  method _CW : frontFaceDir readonly_prop

  method _CCW : frontFaceDir readonly_prop

  method _DONT_CARE_ : hintMode readonly_prop

  method _FASTEST : hintMode readonly_prop

  method _NICEST : hintMode readonly_prop

  method _GENERATE_MIPMAP_HINT_ : hintTarget readonly_prop

  method _BLEND_EQUATION_ : blendMode parameter readonly_prop

  method _BLEND_EQUATION_RGB_ : blendMode parameter readonly_prop

  method _BLEND_EQUATION_ALPHA_ : blendMode parameter readonly_prop

  method _BLEND_DST_RGB_ : blendingFactor parameter readonly_prop

  method _BLEND_SRC_RGB_ : blendingFactor parameter readonly_prop

  method _BLEND_DST_ALPHA_ : blendingFactor parameter readonly_prop

  method _BLEND_SRC_ALPHA_ : blendingFactor parameter readonly_prop

  method _BLEND_COLOR_ : Typed_array.float32Array t parameter readonly_prop

  method _ARRAY_BUFFER_BINDING_ : buffer t opt parameter readonly_prop

  method _ELEMENT_ARRAY_BUFFER_BINDING_ : buffer t opt parameter readonly_prop

  method _CULL_FACE_PARAM : bool t parameter readonly_prop

  method _BLEND_PARAM : bool t parameter readonly_prop

  method _DITHER_PARAM : bool t parameter readonly_prop

  method _STENCIL_TEST_PARAM : bool t parameter readonly_prop

  method _DEPTH_TEST_PARAM : bool t parameter readonly_prop

  method _SCISSOR_TEST_PARAM : bool t parameter readonly_prop

  method _POLYGON_OFFSET_FILL_PARAM : bool t parameter readonly_prop

  method _LINE_WIDTH_ : number_t parameter readonly_prop

  method _ALIASED_POINT_SIZE_RANGE_ : Typed_array.float32Array t parameter readonly_prop

  method _ALIASED_LINE_WIDTH_RANGE_ : Typed_array.float32Array t parameter readonly_prop

  method _CULL_FACE_MODE_ : cullFaceMode parameter readonly_prop

  method _FRONT_FACE_ : frontFaceDir parameter readonly_prop

  method _DEPTH_RANGE_ : Typed_array.float32Array t parameter readonly_prop

  method _DEPTH_WRITEMASK_ : bool t parameter readonly_prop

  method _DEPTH_CLEAR_VALUE_ : number_t parameter readonly_prop

  method _DEPTH_FUNC_ : depthFunction parameter readonly_prop

  method _STENCIL_CLEAR_VALUE_ : int parameter readonly_prop

  method _STENCIL_FUNC_ : int parameter readonly_prop

  method _STENCIL_FAIL_ : int parameter readonly_prop

  method _STENCIL_PASS_DEPTH_FAIL_ : int parameter readonly_prop

  method _STENCIL_PASS_DEPTH_PASS_ : int parameter readonly_prop

  method _STENCIL_REF_ : int parameter readonly_prop

  method _STENCIL_VALUE_MASK_ : int parameter readonly_prop

  method _STENCIL_WRITEMASK_ : int parameter readonly_prop

  method _STENCIL_BACK_FUNC_ : int parameter readonly_prop

  method _STENCIL_BACK_FAIL_ : int parameter readonly_prop

  method _STENCIL_BACK_PASS_DEPTH_FAIL_ : int parameter readonly_prop

  method _STENCIL_BACK_PASS_DEPTH_PASS_ : int parameter readonly_prop

  method _STENCIL_BACK_REF_ : int parameter readonly_prop

  method _STENCIL_BACK_VALUE_MASK_ : int parameter readonly_prop

  method _STENCIL_BACK_WRITEMASK_ : int parameter readonly_prop

  method _VIEWPORT : Typed_array.int32Array t parameter readonly_prop

  method _SCISSOR_BOX_ : Typed_array.int32Array t parameter readonly_prop

  method _COLOR_CLEAR_VALUE_ : Typed_array.float32Array t parameter readonly_prop

  method _COLOR_WRITEMASK_ : bool t js_array t parameter readonly_prop

  method _UNPACK_ALIGNMENT_PARAM : int parameter readonly_prop

  method _PACK_ALIGNMENT_ : int parameter readonly_prop

  method _MAX_TEXTURE_SIZE_ : int parameter readonly_prop

  method _MAX_VIEWPORT_DIMS_ : Typed_array.int32Array t parameter readonly_prop

  method _SUBPIXEL_BITS_ : int parameter readonly_prop

  method _RED_BITS_ : int parameter readonly_prop

  method _GREEN_BITS_ : int parameter readonly_prop

  method _BLUE_BITS_ : int parameter readonly_prop

  method _ALPHA_BITS_ : int parameter readonly_prop

  method _DEPTH_BITS_ : int parameter readonly_prop

  method _STENCIL_BITS_ : int parameter readonly_prop

  method _POLYGON_OFFSET_UNITS_ : number_t parameter readonly_prop

  method _POLYGON_OFFSET_FACTOR_ : number_t parameter readonly_prop

  method _TEXTURE_BINDING_2D_ : texture t opt parameter readonly_prop

  method _TEXTURE_BINDING_CUBE_MAP_ : texture t opt parameter readonly_prop

  method _SAMPLE_BUFFERS_ : int parameter readonly_prop

  method _SAMPLES_ : int parameter readonly_prop

  method _SAMPLE_COVERAGE_VALUE_ : number_t parameter readonly_prop

  method _SAMPLE_COVERAGE_INVERT_ : bool t parameter readonly_prop

  method _NUM_COMPRESSED_TEXTURE_FORMATS_ : int parameter readonly_prop

  method _COMPRESSED_TEXTURE_FORMATS_ : Typed_array.uint32Array t parameter readonly_prop

  method _GENERATE_MIPMAP_HINT_PARAM_ : hintMode parameter readonly_prop

  method _BUFFER_SIZE_ : int bufferParameter readonly_prop

  method _BUFFER_USAGE_ : bufferUsage bufferParameter readonly_prop

  method _BYTE : dataType readonly_prop

  method _UNSIGNED_BYTE_DT : dataType readonly_prop

  method _SHORT : dataType readonly_prop

  method _UNSIGNED_SHORT_ : dataType readonly_prop

  method _INT : dataType readonly_prop

  method _UNSIGNED_INT_ : dataType readonly_prop

  method _FLOAT : dataType readonly_prop

  method _UNSIGNED_BYTE_ : pixelType readonly_prop

  method _UNSIGNED_SHORT_4_4_4_4_ : pixelType readonly_prop

  method _UNSIGNED_SHORT_5_5_5_1_ : pixelType readonly_prop

  method _UNSIGNED_SHORT_5_6_5_ : pixelType readonly_prop

  method _ALPHA : pixelFormat readonly_prop

  method _RGB : pixelFormat readonly_prop

  method _RGBA : pixelFormat readonly_prop

  method _LUMINANCE : pixelFormat readonly_prop

  method _LUMINANCE_ALPHA_ : pixelFormat readonly_prop

  method _STENCIL_INDEX_ : pixelFormat readonly_prop

  method _DEPTH_STENCIL_ : pixelFormat readonly_prop

  method _DEPTH_COMPONENT_ : pixelFormat readonly_prop

  method _FRAGMENT_SHADER_ : shaderType readonly_prop

  method _VERTEX_SHADER_ : shaderType readonly_prop

  method _MAX_VERTEX_ATTRIBS_ : int parameter readonly_prop

  method _MAX_VERTEX_UNIFORM_VECTORS_ : int parameter readonly_prop

  method _MAX_VARYING_VECTORS_ : int parameter readonly_prop

  method _MAX_COMBINED_TEXTURE_IMAGE_UNITS_ : int parameter readonly_prop

  method _MAX_VERTEX_TEXTURE_IMAGE_UNITS_ : int parameter readonly_prop

  method _MAX_TEXTURE_IMAGE_UNITS_ : int parameter readonly_prop

  method _MAX_FRAGMENT_UNIFORM_VECTORS_ : int parameter readonly_prop

  method _SHADER_TYPE_ : shaderType shaderParam readonly_prop

  method _DELETE_STATUS_ : bool t shaderParam readonly_prop

  method _COMPILE_STATUS_ : bool t shaderParam readonly_prop

  method _DELETE_STATUS_PROG : bool t programParam readonly_prop

  method _LINK_STATUS_ : bool t programParam readonly_prop

  method _VALIDATE_STATUS_ : bool t programParam readonly_prop

  method _ATTACHED_SHADERS_ : int programParam readonly_prop

  method _ACTIVE_UNIFORMS_ : int programParam readonly_prop

  method _ACTIVE_ATTRIBUTES_ : int programParam readonly_prop

  method _SHADING_LANGUAGE_VERSION_ : js_string t parameter readonly_prop

  method _CURRENT_PROGRAM_ : program t opt parameter readonly_prop

  method _VENDOR : js_string t parameter readonly_prop

  method _RENDERER : js_string t parameter readonly_prop

  method _VERSION : js_string t parameter readonly_prop

  method _MAX_CUBE_MAP_TEXTURE_SIZE_ : int parameter readonly_prop

  method _ACTIVE_TEXTURE_ : int parameter readonly_prop

  method _FRAMEBUFFER_BINDING_ : framebuffer t opt parameter readonly_prop

  method _RENDERBUFFER_BINDING_ : renderbuffer t opt parameter readonly_prop

  method _MAX_RENDERBUFFER_SIZE : int parameter readonly_prop

  method _NEVER : depthFunction readonly_prop

  method _LESS : depthFunction readonly_prop

  method _EQUAL : depthFunction readonly_prop

  method _LEQUAL : depthFunction readonly_prop

  method _GREATER : depthFunction readonly_prop

  method _NOTEQUAL : depthFunction readonly_prop

  method _GEQUAL : depthFunction readonly_prop

  method _ALWAYS : depthFunction readonly_prop

  method _KEEP : stencilOp readonly_prop

  method _REPLACE : stencilOp readonly_prop

  method _INCR : stencilOp readonly_prop

  method _DECR : stencilOp readonly_prop

  method _INVERT : stencilOp readonly_prop

  method _INCR_WRAP_ : stencilOp readonly_prop

  method _DECR_WRAP_ : stencilOp readonly_prop

  method _ZERO_ : stencilOp readonly_prop

  method _NEAREST : texFilter readonly_prop

  method _LINEAR : texFilter readonly_prop

  method _NEAREST_MIPMAP_NEAREST_ : texFilter readonly_prop

  method _LINEAR_MIPMAP_NEAREST_ : texFilter readonly_prop

  method _NEAREST_MIPMAP_LINEAR_ : texFilter readonly_prop

  method _LINEAR_MIPMAP_LINEAR_ : texFilter readonly_prop

  method _TEXTURE_MAG_FILTER_ : texFilter texParam readonly_prop

  method _TEXTURE_MIN_FILTER_ : texFilter texParam readonly_prop

  method _TEXTURE_WRAP_S_ : wrapMode texParam readonly_prop

  method _TEXTURE_WRAP_T_ : wrapMode texParam readonly_prop

  method _NONE_OT : objectType readonly_prop

  method _TEXTURE_OT : objectType readonly_prop

  method _RENDERBUFFER_OT : objectType readonly_prop

  method _TEXTURE_2D_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_POSITIVE_X_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_NEGATIVE_X_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_POSITIVE_Y_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_NEGATIVE_Y_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_POSITIVE_Z_ : texTarget readonly_prop

  method _TEXTURE_CUBE_MAP_NEGATIVE_Z_ : texTarget readonly_prop

  method _TEXTURE0 : textureUnit readonly_prop

  method _TEXTURE1 : textureUnit readonly_prop

  method _TEXTURE2 : textureUnit readonly_prop

  method _TEXTURE3 : textureUnit readonly_prop

  method _TEXTURE4 : textureUnit readonly_prop

  method _TEXTURE5 : textureUnit readonly_prop

  method _TEXTURE6 : textureUnit readonly_prop

  method _TEXTURE7 : textureUnit readonly_prop

  method _TEXTURE8 : textureUnit readonly_prop

  method _TEXTURE9 : textureUnit readonly_prop

  method _TEXTURE10 : textureUnit readonly_prop

  method _TEXTURE11 : textureUnit readonly_prop

  method _TEXTURE12 : textureUnit readonly_prop

  method _TEXTURE13 : textureUnit readonly_prop

  method _TEXTURE14 : textureUnit readonly_prop

  method _TEXTURE15 : textureUnit readonly_prop

  method _TEXTURE16 : textureUnit readonly_prop

  method _TEXTURE17 : textureUnit readonly_prop

  method _TEXTURE18 : textureUnit readonly_prop

  method _TEXTURE19 : textureUnit readonly_prop

  method _TEXTURE20 : textureUnit readonly_prop

  method _TEXTURE21 : textureUnit readonly_prop

  method _TEXTURE22 : textureUnit readonly_prop

  method _TEXTURE23 : textureUnit readonly_prop

  method _TEXTURE24 : textureUnit readonly_prop

  method _TEXTURE25 : textureUnit readonly_prop

  method _TEXTURE26 : textureUnit readonly_prop

  method _TEXTURE27 : textureUnit readonly_prop

  method _TEXTURE28 : textureUnit readonly_prop

  method _TEXTURE29 : textureUnit readonly_prop

  method _TEXTURE30 : textureUnit readonly_prop

  method _TEXTURE31 : textureUnit readonly_prop

  method _REPEAT : wrapMode readonly_prop

  method _CLAMP_TO_EDGE_ : wrapMode readonly_prop

  method _MIRRORED_REPEAT_ : wrapMode readonly_prop

  method _FLOAT_ : uniformType readonly_prop

  method _FLOAT_VEC2_ : uniformType readonly_prop

  method _FLOAT_VEC3_ : uniformType readonly_prop

  method _FLOAT_VEC4_ : uniformType readonly_prop

  method _INT_ : uniformType readonly_prop

  method _INT_VEC2_ : uniformType readonly_prop

  method _INT_VEC3_ : uniformType readonly_prop

  method _INT_VEC4_ : uniformType readonly_prop

  method _BOOL_ : uniformType readonly_prop

  method _BOOL_VEC2_ : uniformType readonly_prop

  method _BOOL_VEC3_ : uniformType readonly_prop

  method _BOOL_VEC4_ : uniformType readonly_prop

  method _FLOAT_MAT2_ : uniformType readonly_prop

  method _FLOAT_MAT3_ : uniformType readonly_prop

  method _FLOAT_MAT4_ : uniformType readonly_prop

  method _SAMPLER_2D_ : uniformType readonly_prop

  method _SAMPLER_CUBE_ : uniformType readonly_prop

  method _VERTEX_ATTRIB_ARRAY_ENABLED_ : bool t vertexAttribParam readonly_prop

  method _VERTEX_ATTRIB_ARRAY_SIZE_ : int vertexAttribParam readonly_prop

  method _VERTEX_ATTRIB_ARRAY_STRIDE_ : int vertexAttribParam readonly_prop

  method _VERTEX_ATTRIB_ARRAY_TYPE_ : int vertexAttribParam readonly_prop

  method _VERTEX_ATTRIB_ARRAY_NORMALIZED_ : bool t vertexAttribParam readonly_prop

  method _VERTEX_ATTRIB_ARRAY_POINTER_ : vertexAttribPointerParam readonly_prop

  method _VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ :
    buffer t opt vertexAttribParam readonly_prop

  method _CURRENT_VERTEX_ATTRIB_ :
    Typed_array.float32Array t vertexAttribParam readonly_prop

  method _LOW_FLOAT_ : shaderPrecisionType readonly_prop

  method _MEDIUM_FLOAT_ : shaderPrecisionType readonly_prop

  method _HIGH_FLOAT_ : shaderPrecisionType readonly_prop

  method _LOW_INT_ : shaderPrecisionType readonly_prop

  method _MEDIUM_INT_ : shaderPrecisionType readonly_prop

  method _HIGH_INT_ : shaderPrecisionType readonly_prop

  method _FRAMEBUFFER : fbTarget readonly_prop

  method _RENDERBUFFER : rbTarget readonly_prop

  method _RGBA4 : format readonly_prop

  method _RGB5_A1_ : format readonly_prop

  method _RGB565 : format readonly_prop

  method _DEPTH_COMPONENT16_ : format readonly_prop

  method _STENCIL_INDEX8_ : format readonly_prop

  method _RENDERBUFFER_WIDTH_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_HEIGHT_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_INTERNAL_FORMAT_ : format renderbufferParam readonly_prop

  method _RENDERBUFFER_RED_SIZE_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_GREEN_SIZE_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_BLUE_SIZE_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_ALPHA_SIZE_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_DEPTH_SIZE_ : int renderbufferParam readonly_prop

  method _RENDERBUFFER_STENCIL_SIZE_ : int renderbufferParam readonly_prop

  method _FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_ : objectType attachParam readonly_prop

  method _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_RENDERBUFFER :
    renderbuffer t attachParam readonly_prop

  method _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_TEXTURE : texture t attachParam readonly_prop

  method _FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_ : int attachParam readonly_prop

  method _FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_ : int attachParam readonly_prop

  method _COLOR_ATTACHMENT0_ : attachmentPoint readonly_prop

  method _DEPTH_ATTACHMENT_ : attachmentPoint readonly_prop

  method _STENCIL_ATTACHMENT_ : attachmentPoint readonly_prop

  method _DEPTH_STENCIL_ATTACHMENT_ : attachmentPoint readonly_prop

  method _FRAMEBUFFER_COMPLETE_ : framebufferStatus readonly_prop

  method _FRAMEBUFFER_INCOMPLETE_ATTACHMENT_ : framebufferStatus readonly_prop

  method _FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_ : framebufferStatus readonly_prop

  method _FRAMEBUFFER_INCOMPLETE_DIMENSIONS_ : framebufferStatus readonly_prop

  method _FRAMEBUFFER_UNSUPPORTED_ : framebufferStatus readonly_prop

  method _UNPACK_FLIP_Y_WEBGL_PARAM : bool t parameter readonly_prop

  method _UNPACK_PREMULTIPLY_ALPHA_WEBGL_PARAM : bool t parameter readonly_prop

  method _UNPACK_COLORSPACE_CONVERSION_WEBGL_PARAM :
    colorspaceConversion parameter readonly_prop

  method _NONE : colorspaceConversion readonly_prop

  method _BROWSER_DEFAULT_WEBGL_ : colorspaceConversion readonly_prop

  method _UNPACK_ALIGNMENT_ : int pixelStoreParam readonly_prop

  method _UNPACK_FLIP_Y_WEBGL_ : bool t pixelStoreParam readonly_prop

  method _UNPACK_PREMULTIPLY_ALPHA_WEBGL_ : bool t pixelStoreParam readonly_prop

  method _UNPACK_COLORSPACE_CONVERSION_WEBGL_ : int pixelStoreParam readonly_prop
end

(** 5.14 WebGLContextEvent *)

class type contextEvent = object
  inherit Dom_html.event

  method statusMessage : js_string t readonly_prop
end

module Event = struct
  let webglcontextlost = Dom_html.Event.make "webglcontextlost"

  let webglcontextrestored = Dom_html.Event.make "webglcontextrestored"

  let webglcontextcreationerror = Dom_html.Event.make "webglcontextcreationerror"
end

(****)

class type canvasElement = object
  method getContext : js_string t -> renderingContext t opt meth

  method getContext_ : js_string t -> contextAttributes t -> renderingContext t opt meth
end

let getContext (c : Dom_html.canvasElement t) =
  let c : canvasElement t = Js.Unsafe.coerce c in
  let ctx = c##getContext (Js.string "webgl") in
  if Opt.test ctx then ctx else c##(getContext (Js.string "experimental-webgl"))

let getContextWithAttributes (c : Dom_html.canvasElement t) attribs =
  let c : canvasElement t = Js.Unsafe.coerce c in
  let ctx = c##getContext_ (Js.string "webgl") attribs in
  if Opt.test ctx then ctx else c##getContext_ (Js.string "experimental-webgl") attribs
