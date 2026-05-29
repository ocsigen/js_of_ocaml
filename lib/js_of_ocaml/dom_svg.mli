(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
 * Copyright (C) 2014 Jérôme Vouillon
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

(** DOM SVG binding

This is a partial binding to the DOM SVG API.
*)

open Js

val xmlns : js_string t
(** {2 Types} *)

(** @deprecated Removed in SVG 2. *)
type error_code =
  | WRONG_TYPE_ERR
  | INVALID_VALUE_ERR
  | MATRIX_NOT_INVERTABLE

(** @deprecated Removed in SVG 2. *)
class type svg_error = object
  inherit Js.error

  method code : error_code t readonly_prop
end

exception SVGError of svg_error
(** @deprecated Removed in SVG 2. *)

type lengthUnitType =
  | LENGTHTYPE_UNKNOWN
  | LENGTHTYPE_NUMBER
  | LENGTHTYPE_PERCENTAGE
  | LENGTHTYPE_EMS
  | LENGTHTYPE_EXS
  | LENGTHTYPE_PX
  | LENGTHTYPE_CM
  | LENGTHTYPE_MM
  | LENGTHTYPE_IN
  | LENGTHTYPE_PT
  | LENGTHTYPE_PC

type angleUnitType =
  | ANGLETYPE_UNKNOWN
  | ANGLETYPE_UNSPECIFIED
  | ANGLETYPE_DEG
  | ANGLETYPE_RAD
  | ANGLETYPE_GRAD

type colorType =
  | COLORTYPE_UNKNOWN
  | COLORTYPE_RGBCOLOR
  | COLORTYPE_RGBCOLOR_ICCCOLOR
  | COLORTYPE_CURRENTCOLOR

type alignmentType =
  | PRESERVEASPECTRATIO_UNKNOWN
  | PRESERVEASPECTRATIO_NONE
  | PRESERVEASPECTRATIO_XMINYMIN
  | PRESERVEASPECTRATIO_XMIDYMIN
  | PRESERVEASPECTRATIO_XMAXYMIN
  | PRESERVEASPECTRATIO_XMINYMID
  | PRESERVEASPECTRATIO_XMIDYMID
  | PRESERVEASPECTRATIO_XMAXYMID
  | PRESERVEASPECTRATIO_XMINYMAX
  | PRESERVEASPECTRATIO_XMIDYMAX
  | PRESERVEASPECTRATIO_XMAXYMAX

type meetOrSliceType =
  | MEETORSLICE_UNKNOWN
  | MEETORSLICE_MEET
  | MEETORSLICE_SLICE

type transformType =
  | TRANSFORM_UNKNOWN
  | TRANSFORM_MATRIX
  | TRANSFORM_TRANSLATE
  | TRANSFORM_SCALE
  | TRANSFORM_ROTATE
  | TRANSFORM_SKEWX
  | TRANSFORM_SKEWY

type zoomAndPanType =
  | ZOOMANDPAN_UNKNOWN
  | ZOOMANDPAN_DISABLE
  | ZOOMANDPAN_MAGNIFY

type lengthAdjust =
  | LENGTHADJUST_UNKNOWN
  | LENGTHADJUST_SPACING
  | LENGTHADJUST_SPACINGANDGLYPHS

type unitType =
  | UNIT_TYPE_UNKNOWN
  | UNIT_TYPE_USERSPACEONUSE
  | UNIT_TYPE_OBJECTBOUNDINGBOX

(* interface SVGRenderingIntent *)
type intentType =
  | RENDERING_INTENT_UNKNOWN
  | RENDERING_INTENT_AUTO
  | RENDERING_INTENT_PERCEPTUAL
  | RENDERING_INTENT_RELATIVE_COLORIMETRIC
  | RENDERING_INTENT_SATURATION
  | RENDERING_INTENT_ABSOLUTE_COLORIMETRIC

(* Path Segment Types *)
type pathSegmentType =
  | PATHSEG_UNKNOWN
  | PATHSEG_CLOSEPATH
  | PATHSEG_MOVETO_ABS
  | PATHSEG_MOVETO_REL
  | PATHSEG_LINETO_ABS
  | PATHSEG_LINETO_REL
  | PATHSEG_CURVETO_CUBIC_ABS
  | PATHSEG_CURVETO_CUBIC_REL
  | PATHSEG_CURVETO_QUADRATIC_ABS
  | PATHSEG_CURVETO_QUADRATIC_REL
  | PATHSEG_ARC_ABS
  | PATHSEG_ARC_REL
  | PATHSEG_LINETO_HORIZONTAL_ABS
  | PATHSEG_LINETO_HORIZONTAL_REL
  | PATHSEG_LINETO_VERTICAL_ABS
  | PATHSEG_LINETO_VERTICAL_REL
  | PATHSEG_CURVETO_CUBIC_SMOOTH_ABS
  | PATHSEG_CURVETO_CUBIC_SMOOTH_REL
  | PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS
  | PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL

(* textPath Method Types *)
type textPathMethodType =
  | TEXTPATH_METHODTYPE_UNKNOWN
  | TEXTPATH_METHODTYPE_ALIGN
  | TEXTPATH_METHODTYPE_STRETCH

(* textPath Spacing Types *)
type textPathSpacingType =
  | TEXTPATH_SPACINGTYPE_UNKNOWN
  | TEXTPATH_SPACINGTYPE_AUTO
  | TEXTPATH_SPACINGTYPE_EXACT

(* Spread Method Types *)
type spreadMethodType =
  | SPREADMETHOD_UNKNOWN
  | SPREADMETHOD_PAD
  | SPREADMETHOD_REFLECT
  | SPREADMETHOD_REPEAT

type markerUnitType =
  | SVG_MARKERUNITS_UNKNOWN
  | SVG_MARKERUNITS_USERSPACEONUSE
  | SVG_MARKERUNITS_STROKEWIDTH

type markerOrientType =
  | SVG_MARKER_ORIENT_UNKNOWN
  | SVG_MARKER_ORIENT_AUTO
  | SVG_MARKER_ORIENT_ANGLE

type blendModeType =
  | SVG_FEBLEND_MODE_UNKNOWN
  | SVG_FEBLEND_MODE_NORMAL
  | SVG_FEBLEND_MODE_MULTIPLY
  | SVG_FEBLEND_MODE_SCREEN
  | SVG_FEBLEND_MODE_DARKEN
  | SVG_FEBLEND_MODE_LIGHTEN

type colorMatrixType =
  | SVG_FECOLORMATRIX_TYPE_UNKNOWN
  | SVG_FECOLORMATRIX_TYPE_MATRIX
  | SVG_FECOLORMATRIX_TYPE_SATURATE
  | SVG_FECOLORMATRIX_TYPE_HUEROTATE
  | SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA

type componentTransferType =
  | SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN
  | SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY
  | SVG_FECOMPONENTTRANSFER_TYPE_TABLE
  | SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE
  | SVG_FECOMPONENTTRANSFER_TYPE_LINEAR
  | SVG_FECOMPONENTTRANSFER_TYPE_GAMMA

type compositeOperatorType =
  | SVG_FECOMPOSITE_OPERATOR_UNKNOWN
  | SVG_FECOMPOSITE_OPERATOR_OVER
  | SVG_FECOMPOSITE_OPERATOR_IN
  | SVG_FECOMPOSITE_OPERATOR_OUT
  | SVG_FECOMPOSITE_OPERATOR_ATOP
  | SVG_FECOMPOSITE_OPERATOR_XOR
  | SVG_FECOMPOSITE_OPERATOR_ARITHMETIC

type edgeModeType =
  | SVG_EDGEMODE_UNKNOWN
  | SVG_EDGEMODE_DUPLICATE
  | SVG_EDGEMODE_WRAP
  | SVG_EDGEMODE_NONE

type channelSelectorType =
  | SVG_CHANNEL_UNKNOWN
  | SVG_CHANNEL_R
  | SVG_CHANNEL_G
  | SVG_CHANNEL_B
  | SVG_CHANNEL_A

type morphologyOperatorType =
  | SVG_MORPHOLOGY_OPERATOR_UNKNOWN
  | SVG_MORPHOLOGY_OPERATOR_ERODE
  | SVG_MORPHOLOGY_OPERATOR_DILATE

type turbulenceType =
  | SVG_TURBULENCE_TYPE_UNKNOWN
  | SVG_TURBULENCE_TYPE_FRACTALNOISE
  | SVG_TURBULENCE_TYPE_TURBULENCE

type stitchType =
  | SVG_STITCHTYPE_UNKNOWN
  | SVG_STITCHTYPE_STITCH
  | SVG_STITCHTYPE_NOSTITCH

type suspendHandleID

(****)

class type ['a] animated = object
  method baseVal : 'a prop

  method animVal : 'a prop
end

class type ['a] list = object
  method length : int readonly_prop
  (** SVG 2 alias of {!numberOfItems}. *)

  method numberOfItems : int readonly_prop

  method clear : unit meth

  method initialize : 'a -> 'a meth

  method getItem : int -> 'a meth

  method insertItemBefore : 'a -> int -> 'a meth

  method replaceItem : 'a -> int -> 'a meth

  method removeItem : int -> 'a meth

  method appendItem : 'a -> 'a meth
end

(****)
(** {2 Elements } *)

(* interface SVGElement *)
class type element = object
  inherit Dom.element

  method id : js_string t prop

  method xmlbase : js_string t optdef prop
  (** @deprecated Removed in SVG 2. *)

  method ownerSVGElement : svgElement t opt readonly_prop

  method viewportElement : element t opt readonly_prop

  method tabIndex : int prop
  (** SVG 2 addition. *)

  method focus : unit meth
  (** SVG 2 addition. *)

  method blur : unit meth
  (** SVG 2 addition. *)

  method style : Dom_html.cssStyleDeclaration t readonly_prop
  (** SVG 2: merged in from [SVGStylable]. *)

  method className : animatedString t readonly_prop
  (** SVG 2: merged in from [SVGStylable].
      @deprecated Use [classList] (from {!Dom.element}) instead. *)

  method dataset : Dom_html.domStringMap t readonly_prop
  (** SVG 2 addition (from [HTMLOrForeignElement]). *)

  method autofocus : bool t prop
  (** SVG 2 addition (from [HTMLOrForeignElement]). *)

  method nonce : js_string t prop
  (** SVG 2 addition (from [HTMLOrForeignElement]). *)
end

(* interface SVGAnimatedString *)
and animatedString = [js_string t] animated

(* interface SVGAnimatedBoolean *)
and animatedBoolean = [bool t] animated

(* interface SVGStringList *)
and stringList = [js_string t] list

(* interface SVGAnimatedEnumeration *)
and animatedEnumeration = [int (*short*)] animated

(* SVGAnimatedEnumeration specialised to SVGUnitTypes *)
and animatedUnitType = [unitType] animated

(* interface SVGAnimatedInteger *)
and animatedInteger = [int] animated

(* interface SVGAnimatedNumber *)
and animatedNumber = [number_t] animated

(* interface SVGNumberList *)
and numberList = [number_t] list

(* interface SVGAnimatedNumberList *)
and animatedNumberList = [numberList t] animated

(* interface SVGLength *)
and length = object
  method unitType : lengthUnitType readonly_prop

  method value : number_t prop

  method valueInSpecifiedUnits : number_t prop

  method valueAsString : js_string t prop

  method newValueSpecifiedUnits : lengthUnitType -> number_t -> unit meth

  method convertToSpecifiedUnits : lengthUnitType -> unit meth
end

(* interface SVGAnimatedLength *)
and animatedLength = [length t] animated

(* interface SVGLengthList *)
and lengthList = [length t] list

(* interface SVGAnimatedLengthList *)
and animatedLengthList = [lengthList t] animated

(* interface SVGAngle *)
and angle = object
  method unitType : angleUnitType readonly_prop

  method value : number_t prop

  method valueInSpecifiedUnits : number_t prop

  method valueAsString : js_string t prop

  method newValueSpecifiedUnits : angleUnitType -> number_t -> unit meth

  method convertToSpecifiedUnits : angleUnitType -> unit meth
end

(* interface SVGAnimatedAngle *)
and animatedAngle = [angle t] animated

(* XXXXX Move it *)
and rgbColor = object end (* interface SVGColor *)

(** @deprecated Removed in SVG 2. *)
and color = object
  (* XXX inherit cssValue *)
  method colorType : colorType readonly_prop

  method rgbColor : rgbColor t readonly_prop

  method iccColor : iccColor t readonly_prop

  method setRGBColor : js_string t -> unit meth

  method setRGBColorICCColor : js_string t -> js_string t -> unit meth

  method setColor : colorType -> js_string t -> js_string t -> unit meth
end (* interface SVGICCColor *)

(** @deprecated Removed in SVG 2. *)
and iccColor = object
  method colorProfile : js_string t prop

  method colors : numberList t readonly_prop
end

(* interface SVGRect *)
and rect = object
  method x : number_t prop

  method y : number_t prop

  method width : number_t prop

  method height : number_t prop
end

(* interface SVGAnimatedRect *)
and animatedRect = [rect t] animated (* interface SVGStylable *)

(** @deprecated Merged into {!element} (SVGElement) in SVG 2; retained only for
    backward compatibility. Use the [style]/[className] methods on {!element}. *)
and stylable = object
  method className : animatedString t readonly_prop

  method style : Dom_html.cssStyleDeclaration t readonly_prop
  (*   CSSValue getPresentationAttribute(in DOMString name); *)
end (* interface SVGLocatable *)

(** @deprecated Replaced by SVGGraphicsElement in SVG 2. *)
and locatable = object
  method nearestViewportElement : element t opt readonly_prop

  method farthestViewportElement : element t opt readonly_prop

  method getBBox : rect t meth

  method getCTM : matrix t meth

  method getScreenCTM : matrix t meth

  method getTransformToElement : element t -> matrix t meth
  (** @deprecated Removed in SVG 2. Use getScreenCTM and matrix inversion. *)
end (* interface SVGTransformable *)

(** @deprecated Replaced by SVGGraphicsElement in SVG 2. *)
and transformable = object
  inherit locatable

  method transform : animatedTransformList t readonly_prop
end

(* interface SVGTests *)
and tests = object
  method requiredFeatures : stringList t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method requiredExtensions : stringList t readonly_prop

  method systemLanguage : stringList t readonly_prop

  method hasExtension : js_string t -> bool t meth
  (** @deprecated Removed in SVG 2. *)
end (* interface SVGLangSpace *)

(** @deprecated Merged into SVGElement in SVG 2. Use lang/xml:lang attributes. *)
and langSpace = object
  method xmllang : js_string t prop

  method xmlspace : js_string t prop
end (* interface SVGExternalResourcesRequired *)

(** @deprecated Removed in SVG 2. *)
and externalResourcesRequired = object
  method externalResourcesRequired : animatedBoolean t readonly_prop
end

(* interface SVGGraphicsElement - SVG 2 *)
and graphicsElement = object
  inherit element

  inherit tests

  method transform : animatedTransformList t readonly_prop

  method getBBox : rect t meth

  method getCTM : matrix t meth

  method getScreenCTM : matrix t meth

  method nearestViewportElement : element t opt optdef readonly_prop
  (** @deprecated Removed in SVG 2 (and from Safari); gated behind a pref in
      Firefox but still present, deprecated, in Chrome/Edge. [optdef] reflects
      its absence in engines that dropped it. *)

  method farthestViewportElement : element t opt optdef readonly_prop
  (** @deprecated Removed in SVG 2 (and from Safari); gated behind a pref in
      Firefox but still present, deprecated, in Chrome/Edge. [optdef] reflects
      its absence in engines that dropped it. *)
end

(* interface SVGGeometryElement - SVG 2 *)
and geometryElement = object
  inherit graphicsElement

  method pathLength : animatedNumber t readonly_prop

  method isPointInFill : point t -> bool t meth

  method isPointInStroke : point t -> bool t meth

  method getTotalLength : number_t meth

  method getPointAtLength : number_t -> point t meth
end

(* interface SVGUnknownElement - SVG 2 *)
and unknownElement = graphicsElement

(* interface SVGFitToViewBox *)
and fitToViewBox = object
  method viewBox : animatedRect t readonly_prop

  method preserveAspectRatio : animatedPreserveAspectRatio t readonly_prop
end

(* interface SVGZoomAndPan *)
and zoomAndPan = object
  method zoomAndPan : zoomAndPanType prop
end (* interface SVGViewSpec *)

(** @deprecated Removed in SVG 2. *)
and viewSpec = object
  inherit zoomAndPan

  inherit fitToViewBox

  method transform : transformList t readonly_prop

  method viewTarget : element t readonly_prop

  method viewBoxString : js_string t readonly_prop

  method preserveAspectRatioString : js_string t readonly_prop

  method transformString : js_string t readonly_prop

  method viewTargetString : js_string t readonly_prop
end

(* interface SVGURIReference *)
and uriReference = object
  method href : animatedString t readonly_prop
end

(* interface SVGCSSRule : CSSRule *)
(*   const unsigned short COLOR_PROFILE_RULE = 7; *)
(* }; *)

(* interface SVGDocument *)
and document = object
  inherit [element] Dom.document

  (*XXX inherit documentEvent *)
  method title : js_string t prop

  method referrer : js_string t readonly_prop

  method domain : js_string t prop

  method _URL : js_string t readonly_prop

  method rootElement : svgElement t opt readonly_prop
end

(* interface SVGSVGElement *)
and svgElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit fitToViewBox

  inherit zoomAndPan

  (*XXX inherit documentevent, viewcss, documentcss *)
  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method contentScriptType : js_string t optdef prop
  (** @deprecated Removed in SVG 2. *)

  method contentStyleType : js_string t optdef prop
  (** @deprecated Removed in SVG 2. *)

  method viewport : rect t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method pixelUnitToMillimeterX : number_t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method pixelUnitToMillimeterY : number_t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method screenPixelUnitToMillimeterX : number_t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method screenPixelUnitToMillimeterY : number_t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method useCurrentView : bool t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method currentView : viewSpec t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method currentScale : number_t prop

  method currentTranslate : point t readonly_prop

  method suspendRedraw : int -> suspendHandleID meth
  (** @deprecated Deprecated in SVG 2. Has no effect. *)

  method unsuspendRedraw : suspendHandleID -> unit meth
  (** @deprecated Deprecated in SVG 2. Has no effect. *)

  method unsuspendRedrawAll : unit meth
  (** @deprecated Deprecated in SVG 2. Has no effect. *)

  method forceRedraw : unit meth
  (** @deprecated Deprecated in SVG 2. Has no effect. *)

  method pauseAnimations : unit meth

  method unpauseAnimations : unit meth

  method animationsPaused : bool t meth

  method getCurrentTime : number_t meth

  method setCurrentTime : int -> unit meth

  method getIntersectionList : rect t -> element t opt -> element Dom.nodeList t meth

  method getEnclosureList : rect t -> element t opt -> element Dom.nodeList t meth

  method checkIntersection : element t -> rect t -> bool t meth

  method checkEnclosure : element t -> rect t -> bool t meth

  method deselectAll : unit meth
  (** @deprecated Deprecated in SVG 2. Use Selection API. *)

  method createSVGNumber : number t meth

  method createSVGLength : length t meth

  method createSVGAngle : angle t meth

  method createSVGPoint : point t meth

  method createSVGMatrix : matrix t meth

  method createSVGRect : rect t meth

  method createSVGTransform : transform t meth

  method createSVGTransformFromMatrix : matrix t -> transform t meth

  method getElementById : js_string t -> Dom.element t meth
end

(* interface SVGGElement *)
and gElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit Dom_html.eventTarget
end

(* interface SVGDefsElement *)
and defsElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired
  (* XXXXXXX ? inherit Dom_html.eventTarget *)
end

(* interface SVGDescElement *)
and descElement = object
  inherit element

  inherit langSpace

  (* XXXXXXX ? inherit Dom_html.eventTarget *)
end

(* interface SVGTitleElement *)
and titleElement = object
  inherit element

  inherit langSpace
end

(* interface SVGSymbolElement *)
and symbolElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit fitToViewBox

  inherit Dom_html.eventTarget

  method x : animatedLength t readonly_prop
  (** SVG 2 addition. *)

  method y : animatedLength t readonly_prop
  (** SVG 2 addition. *)

  method width : animatedLength t readonly_prop
  (** SVG 2 addition. *)

  method height : animatedLength t readonly_prop
  (** SVG 2 addition. *)

  method refX : animatedLength t readonly_prop
  (** SVG 2 addition. *)

  method refY : animatedLength t readonly_prop
  (** SVG 2 addition. *)
end

(* interface SVGUseElement *)
and useElement = object
  inherit graphicsElement

  inherit uriReference

  inherit langSpace

  inherit externalResourcesRequired

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method instanceRoot : element t opt readonly_prop
  (** @deprecated In SVG 2 typed [SVGElement?]; in practice browsers expose a
      shadow tree from [use] rather than populating this. *)

  method animatedInstanceRoot : element t opt readonly_prop
  (** @deprecated In SVG 2 typed [SVGElement?]; in practice browsers expose a
      shadow tree from [use] rather than populating this. *)
end

(** @deprecated Removed in SVG 2. *)
and elementInstance = object
  inherit Dom_html.eventTarget

  method correspondingElement : element t readonly_prop

  method correspondingUseElement : useElement t readonly_prop

  method parentNode : elementInstance t readonly_prop

  method childNodes : elementInstanceList t readonly_prop

  method firstChild : elementInstance t readonly_prop

  method lastChild : elementInstance t readonly_prop

  method previousSibling : elementInstance t readonly_prop

  method nextSibling : elementInstance t readonly_prop
end (* interface SVGElementInstanceList *)

(** @deprecated Removed in SVG 2. *)
and elementInstanceList = object
  method length : int readonly_prop

  method item : int -> elementInstance t meth
end

(* interface SVGImageElement *)
and imageElement = object
  inherit graphicsElement

  inherit uriReference

  inherit langSpace

  inherit externalResourcesRequired

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method preserveAspectRatio : animatedPreserveAspectRatio t readonly_prop

  method crossOrigin : js_string t opt prop
  (** SVG 2 addition. *)
end

and switchElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired
end

(* XXX deprecated => interface GetSVGDocument => SVGDocument getSVGDocument() *)

(* interface SVGStyleElement *)
and styleElement = object
  inherit element

  inherit langSpace

  method type_ : js_string t prop

  method media : js_string t prop

  method title : js_string t prop

  method disabled : bool t prop
  (** SVG 2 addition (from [LinkStyle]). *)

  method sheet : cssStyleSheet t opt readonly_prop
  (** SVG 2 addition (from [LinkStyle]). *)
end

(* interface StyleSheet (the [sheet] property) *)
and cssStyleSheet = object
  method type_ : js_string t readonly_prop

  method href : js_string t opt readonly_prop

  method title : js_string t opt readonly_prop

  method media : js_string t readonly_prop

  method disabled : bool t prop
end

(* interface SVGPoint *)
and point = object
  method x : number_t readonly_prop

  method y : number_t readonly_prop

  method matrixTransform : matrix t -> point t meth
end

(* interface SVGPointList *)
and pointList = [point t] list

(* interface SVGMatrix *)
and matrix = object
  method a : number_t readonly_prop

  method b : number_t readonly_prop

  method c : number_t readonly_prop

  method d : number_t readonly_prop

  method e : number_t readonly_prop

  method f : number_t readonly_prop

  method multiply : matrix t -> matrix t meth

  method inverse : matrix t meth

  method translate : number_t -> number_t -> matrix t meth

  method scale : number_t -> matrix t meth

  method scaleNonUniform : number_t -> number_t -> matrix t meth

  method rotate : number_t -> matrix t meth

  method rotateFromVector : number_t -> number_t -> matrix t meth

  method flipX : matrix t meth

  method flipY : matrix t meth

  method skewX : number_t -> matrix t meth

  method skewY : number_t -> matrix t meth
end

(* interface SVGTransform *)
and transform = object
  method _type : transformType readonly_prop

  method matrix : matrix t readonly_prop

  method angle : number_t readonly_prop

  method setMatrix : matrix t -> unit meth

  method setTranslate : number_t -> number_t -> unit meth

  method setScale : number_t -> number_t -> unit meth

  method setRotate : number_t -> number_t -> number_t -> unit meth

  method setSkewX : number_t -> unit meth

  method setSkewY : number_t -> unit meth
end

(* interface SVGTransformList *)
and transformList = object
  inherit [transform t] list

  method createSVGTransformFromMatrix : matrix -> transform t meth

  method consolidate : transform t meth
end

(* interface SVGAnimatedTransformList *)
and animatedTransformList = [transformList t] animated

(* interface SVGPreserveAspectRatio *)
and preserveAspectRatio = object
  method align : alignmentType readonly_prop

  method meetOrSlice : meetOrSliceType readonly_prop
end

(* interface SVGAnimatedPreserveAspectRatio *)
and animatedPreserveAspectRatio = [preserveAspectRatio t] animated

(* interface SVGPathSeg *)
and pathSeg = object
  method pathSegType : pathSegmentType readonly_prop

  method pathSegTypeAsLetter : js_string t readonly_prop
end

(* interface SVGPathSegClosePath *)
and pathSegClosePath = pathSeg

(* interface SVGPathSegMovetoAbs *)
(* interface SVGPathSegMovetoRel *)
and pathSegMoveto = object
  inherit pathSeg

  method x : number_t prop

  method y : number_t prop
end

(* interface SVGPathSegLinetoAbs *)
(* interface SVGPathSegLinetoRel *)
and pathSegLineto = object
  inherit pathSeg

  method x : number_t prop

  method y : number_t prop
end

(* interface SVGPathSegCurvetoCubicAbs *)
(* interface SVGPathSegCurvetoCubicRel *)
and pathSegCurvetoCubic = object
  inherit pathSeg

  method x : number_t prop

  method y : number_t prop

  method x1 : number_t prop

  method y1 : number_t prop

  method x2 : number_t prop

  method y2 : number_t prop
end

(* interface SVGPathSegCurvetoQuadraticAbs *)
(* interface SVGPathSegCurvetoQuadraticRel *)
and pathSegCurvetoQuadratic = object
  inherit pathSeg

  method x : number_t prop

  method y : number_t prop

  method x1 : number_t prop

  method y1 : number_t prop
end

(* interface SVGPathSegArcAbs *)
(* interface SVGPathSegArcRel*)
and pathSegArc = object
  inherit pathSeg

  method y : number_t prop

  method r1 : number_t prop

  method r2 : number_t prop

  method angle : number_t prop

  method largeArcFlag : bool t prop

  method sweepFlag : bool t prop
end

(* interface SVGPathSegLinetoHorizontalAbs *)
(* interface SVGPathSegLinetoHorizontalRel *)
and pathSegLinetoHorizontal = object
  inherit pathSeg

  method x : number_t prop
end

(* interface SVGPathSegLinetoVerticalAbs *)
(* interface SVGPathSegLinetoVerticalRel *)
and pathSegLinetoVertical = object
  inherit pathSeg

  method y : number_t prop
end

and pathSegCurvetoCubicSmooth = object
  inherit pathSeg

  method x : number_t prop

  method y : number_t prop

  method x2 : number_t prop

  method y2 : number_t prop
end

(* interface SVGPathSegCurvetoQuadraticSmoothAbs *)
(* interface SVGPathSegCurvetoQuadraticSmoothRel  *)
and pathSegCurvetoQuadraticSmooth = object
  inherit pathSeg

  method x : number_t prop

  method y : number_t prop
end

and pathSegList = [pathSeg t] list

(* interface SVGAnimatedPathData *)
and animatedPathData = object
  method pathSegList : pathSegList t optdef prop
  (** @deprecated SVGPathSeg API is deprecated in SVG 2. *)

  method normalizedPathSegList : pathSegList t optdef prop
  (** @deprecated SVGPathSeg API is deprecated in SVG 2. Not implemented in most browsers. *)

  method animatedPathSegList : pathSegList t optdef prop
  (** @deprecated SVGPathSeg API is deprecated in SVG 2. *)

  method animatedNormalizedPathSegList : pathSegList t optdef prop
  (** @deprecated SVGPathSeg API is deprecated in SVG 2. Not implemented in most browsers. *)
end

(* interface SVGPathElement *)
and pathElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit animatedPathData

  method getPathSegAtLength : number_t -> int meth

  method createSVGPathSegClosePath : pathSegClosePath meth

  method createSVGPathSegMovetoAbs : number_t -> number_t -> pathSegMoveto meth

  method createSVGPathSegMovetoRel : number_t -> number_t -> pathSegMoveto meth

  method createSVGPathSegLinetoAbs : number_t -> number_t -> pathSegLineto meth

  method createSVGPathSegLinetoRel : number_t -> number_t -> pathSegLineto meth

  method createSVGPathSegCurvetoCubicAbs :
       number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> pathSegCurvetoCubic meth

  method createSVGPathSegCurvetoCubicRel :
       number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> pathSegCurvetoCubic meth

  method createSVGPathSegCurvetoQuadraticAbs :
    number_t -> number_t -> number_t -> number_t -> pathSegCurvetoQuadratic meth

  method createSVGPathSegCurvetoQuadraticRel :
    number_t -> number_t -> number_t -> number_t -> pathSegCurvetoQuadratic meth

  method createSVGPathSegArcAbs :
       number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> bool t
    -> bool t
    -> pathSegArc meth

  method createSVGPathSegArcRel :
       number_t
    -> number_t
    -> number_t
    -> number_t
    -> number_t
    -> bool t
    -> bool t
    -> pathSegArc meth

  method createSVGPathSegLinetoHorizontalAbs : number_t -> pathSegLinetoHorizontal meth

  method createSVGPathSegLinetoHorizontalRel : number_t -> pathSegLinetoHorizontal meth

  method createSVGPathSegLinetoVerticalAbs : number_t -> pathSegLinetoVertical meth

  method createSVGPathSegLinetoVerticalRel : number_t -> pathSegLinetoVertical meth

  method createSVGPathSegCurvetoCubicSmoothAbs :
    number_t -> number_t -> number_t -> number_t -> pathSegCurvetoCubicSmooth meth

  method createSVGPathSegCurvetoCubicSmoothRel :
    number_t -> number_t -> number_t -> number_t -> pathSegCurvetoCubicSmooth meth

  method createSVGPathSegCurvetoQuadraticSmoothAbs :
    number_t -> number_t -> pathSegCurvetoQuadraticSmooth meth

  method createSVGPathSegCurvetoQuadraticSmoothRel :
    number_t -> number_t -> pathSegCurvetoQuadraticSmooth meth
end

(* interface SVGRectElement *)
and rectElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method rx : animatedLength t readonly_prop

  method ry : animatedLength t readonly_prop
end

(* interface SVGCircleElement *)
and circleElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  method cx : animatedLength t readonly_prop

  method cy : animatedLength t readonly_prop

  method r : animatedLength t readonly_prop
end

(* interface SVGEllipseElement *)
and ellipseElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  method cx : animatedLength t readonly_prop

  method cy : animatedLength t readonly_prop

  method rx : animatedLength t readonly_prop

  method ry : animatedLength t readonly_prop
end

(* interface SVGLineElement *)
class type lineElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit Dom_html.eventTarget

  method x1 : animatedLength t readonly_prop

  method y1 : animatedLength t readonly_prop

  method x2 : animatedLength t readonly_prop

  method y2 : animatedLength t readonly_prop
end

(* interface SVGAnimatedPoints *)
and animatedPoints = object
  method points : pointList t readonly_prop

  method animatedPoints : pointList t readonly_prop
end

(* interface SVGPolylineElement *)
and polyLineElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit animatedPoints
end

(* interface SVGPolygonElement *)
and polygonElement = object
  inherit geometryElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit animatedPoints
end

(* interface SVGTextContentElement *)
and textContentElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired

  inherit Dom_html.eventTarget

  method textLength : animatedLength t readonly_prop

  method lengthAdjust : lengthAdjust animated t readonly_prop

  method getNumberOfChars : int meth

  method getComputedTextLength : number_t meth

  method getSubStringLength : int -> int -> number_t meth

  method getStartPositionOfChar : int -> point t meth

  method getEndPositionOfChar : int -> point t meth

  method getExtentOfChar : int -> rect t meth

  method getRotationOfChar : int -> number_t meth

  method getCharNumAtPosition : point -> int meth

  method selectSubString : int -> int -> unit meth
end

(* interface SVGTextPositioningElement *)
and textPositioningElement = object
  inherit textContentElement

  method x : animatedLengthList t readonly_prop

  method y : animatedLengthList t readonly_prop

  method dx : animatedLengthList t readonly_prop

  method dy : animatedLengthList t readonly_prop

  method rotate : animatedNumberList t readonly_prop
end

(* interface SVGTextElement *)
and textElement = object
  inherit textPositioningElement
end

and tspanElement = textPositioningElement

(** @deprecated Removed in SVG 2. Use tspan with href instead. *)
and trefElement = object
  inherit textPositioningElement

  inherit uriReference
end

(* interface SVGTextPathElement *)
and textPathElementMethod = [textPathMethodType] animated

and textPathElementSpacing = [textPathSpacingType] animated

and textPathElement = object
  inherit textContentElement

  inherit uriReference

  method startOffset : animatedLength t readonly_prop

  method method_ : textPathElementMethod readonly_prop

  method spacing : textPathElementSpacing readonly_prop
end (* interface SVGAltGlyphElement *)

(** @deprecated Removed in SVG 2. SVG fonts replaced by WOFF. *)
and altGlyphElement = object
  inherit textPositioningElement

  inherit uriReference

  method glyphRef : js_string t prop

  method format : js_string t prop
end (* interface SVGAltGlyphDefElement *)

and altGlyphDefElement = element
(** @deprecated Removed in SVG 2. SVG fonts replaced by WOFF. *)
(* interface SVGAltGlyphItemElement *)

and altGlyphItemElement = element
(** @deprecated Removed in SVG 2. SVG fonts replaced by WOFF. *)
(* interface SVGGlyphRefElement *)

(** @deprecated Removed in SVG 2. SVG fonts replaced by WOFF. *)
and glyphRefElement = object
  inherit element

  inherit uriReference

  method glyphRef : js_string t prop

  method format : js_string t prop

  method x : number_t prop

  method y : number_t prop

  method dx : number_t prop

  method dy : number_t prop
end

(* interface SVGPaint : SVGColor { *)

(*   // Paint Types *)
(*   const unsigned short SVG_PAINTTYPE_UNKNOWN = 0; *)
(*   const unsigned short SVG_PAINTTYPE_RGBCOLOR = 1; *)
(*   const unsigned short SVG_PAINTTYPE_RGBCOLOR_ICCCOLOR = 2; *)
(*   const unsigned short SVG_PAINTTYPE_NONE = 101; *)
(*   const unsigned short SVG_PAINTTYPE_CURRENTCOLOR = 102; *)
(*   const unsigned short SVG_PAINTTYPE_URI_NONE = 103; *)
(*   const unsigned short SVG_PAINTTYPE_URI_CURRENTCOLOR = 104; *)
(*   const unsigned short SVG_PAINTTYPE_URI_RGBCOLOR = 105; *)
(*   const unsigned short SVG_PAINTTYPE_URI_RGBCOLOR_ICCCOLOR = 106; *)
(*   const unsigned short SVG_PAINTTYPE_URI = 107; *)

(*   readonly attribute unsigned short paintType; *)
(*   readonly attribute DOMString uri; *)

(*   void setUri(in DOMString uri); *)
(*   void setPaint(in unsigned short paintType, in DOMString uri, in DOMString rgbColor, in DOMString iccColor) raises(SVGException); *)
(* }; *)
and animatedMarkerUnit = [markerUnitType] animated

and animatedMarkerOrient = [markerOrientType] animated

(* interface SVGMarkerElement *)
and markerElement = object
  inherit element

  inherit langSpace

  inherit externalResourcesRequired

  inherit fitToViewBox

  method refX : animatedLength t readonly_prop

  method refY : animatedLength t readonly_prop

  method markerUnits : animatedMarkerUnit t readonly_prop

  method markerWidth : animatedLength t readonly_prop

  method markerHeight : animatedLength t readonly_prop

  method orientType : animatedMarkerOrient t readonly_prop

  method orientAngle : animatedAngle t readonly_prop

  method orient : js_string t prop
  (** SVG 2 addition (string reflection of the [orient] attribute). *)

  method setOrientToAuto : unit meth

  method setOrientToAngle : angle t -> unit meth
end

(* interface SVGColorProfileElement : SVGElement, *)
(*                                    SVGURIReference, *)
(*                                    SVGRenderingIntent { *)
(*   attribute DOMString local; *)
(*   attribute DOMString name; *)
(*   attribute unsigned short renderingIntent; *)
(* }; *)

(* interface SVGColorProfileRule : SVGCSSRule, *)
(*                                 SVGRenderingIntent { *)
(*   attribute DOMString src setraises(DOMException); *)
(*   attribute DOMString name setraises(DOMException); *)
(*   attribute unsigned short renderingIntent setraises(DOMException); *)
(* }; *)

(* interface SVGGradientElement *)
and animatedSpreadMethod = [spreadMethodType] animated

and gradientElement = object
  inherit element

  inherit uriReference

  method gradientUnits : animatedUnitType t readonly_prop

  method gradientTransform : animatedTransformList t readonly_prop

  method spreadMethod : animatedSpreadMethod t readonly_prop
end

(* interface SVGLinearGradientElement *)
and linearGradientElement = object
  inherit gradientElement

  method x1 : animatedLength t readonly_prop

  method y1 : animatedLength t readonly_prop

  method x2 : animatedLength t readonly_prop

  method y2 : animatedLength t readonly_prop
end

(* interface SVGRadialGradientElement *)
and radialGradientElement = object
  inherit gradientElement

  method cx : animatedLength t readonly_prop

  method cy : animatedLength t readonly_prop

  method r : animatedLength t readonly_prop

  method fx : animatedLength t readonly_prop

  method fy : animatedLength t readonly_prop

  method fr : animatedLength t readonly_prop
  (** SVG 2 addition (focal radius). *)
end

(* interface SVGStopElement *)
and stopElement = object
  inherit element

  method offset : animatedNumber t readonly_prop
end

(* interface SVGPatternElement *)
and patternElement = object
  inherit element

  inherit uriReference

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit fitToViewBox

  method patternUnits : animatedUnitType t readonly_prop

  method patternContentUnits : animatedUnitType t readonly_prop

  method patternTransform : animatedTransformList t readonly_prop

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop
end

(* interface SVGClipPathElement *)
and clipPathElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit transformable

  method clipPathUnits : animatedUnitType t readonly_prop
end

(* interface SVGMaskElement *)
and maskElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  method maskUnits : animatedUnitType t readonly_prop

  method maskContentUnits : animatedUnitType t readonly_prop

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop
end

(* interface SVGFilterElement *)
and filterElement = object
  inherit element

  inherit uriReference

  inherit langSpace

  inherit externalResourcesRequired

  method filterUnits : animatedUnitType t readonly_prop

  method primitiveUnits : animatedUnitType t readonly_prop

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method filterResX : animatedInteger t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method filterResY : animatedInteger t optdef readonly_prop
  (** @deprecated Removed in SVG 2. *)

  method setFilterRes : int -> int -> unit meth
  (** @deprecated Removed in SVG 2. *)
end

(* interface SVGFilterPrimitiveStandardAttributes *)
and filterPrimitiveStandardAttributes = object
  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method result : animatedString t readonly_prop
end

and animatedBlendMode = [blendModeType] animated

and animatedColorMatrixType = [colorMatrixType] animated

and animatedComponentTransferType = [componentTransferType] animated

and animatedCompositeOperator = [compositeOperatorType] animated

and animatedEdgeMode = [edgeModeType] animated

and animatedChannelSelector = [channelSelectorType] animated

and animatedMorphologyOperator = [morphologyOperatorType] animated

and animatedTurbulenceType = [turbulenceType] animated

and animatedStitchType = [stitchType] animated

(* interface SVGFEBlendElement *)
and feBlendElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method in2 : animatedString t readonly_prop

  method mode : animatedBlendMode t readonly_prop
end

(* interface SVGFEColorMatrixElement *)
and feColorMatrixElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method _type : animatedColorMatrixType t readonly_prop

  method values : animatedNumberList t readonly_prop
end

(* interface SVGFEComponentTransferElement *)
and feComponentTransferElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop
end

(* interface SVGComponentTransferFunctionElement *)
and componentTransferFunctionElement = object
  inherit element

  method _type : animatedComponentTransferType t readonly_prop

  method tableValues : animatedNumberList t readonly_prop

  method slope : animatedNumber t readonly_prop

  method intercept : animatedNumber t readonly_prop

  method amplitude : animatedNumber t readonly_prop

  method exponent : animatedNumber t readonly_prop

  method offset : animatedNumber t readonly_prop
end

(* interface SVGFEFuncRElement *)
and feFuncRElement = componentTransferFunctionElement

(* interface SVGFEFuncGElement *)
and feFuncGElement = componentTransferFunctionElement

(* interface SVGFEFuncBElement *)
and feFuncBElement = componentTransferFunctionElement

(* interface SVGFEFuncAElement *)
and feFuncAElement = componentTransferFunctionElement

(* interface SVGFECompositeElement *)
and feCompositeElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method in2 : animatedString t readonly_prop

  method operator : animatedCompositeOperator t readonly_prop

  method k1 : animatedNumber t readonly_prop

  method k2 : animatedNumber t readonly_prop

  method k3 : animatedNumber t readonly_prop

  method k4 : animatedNumber t readonly_prop
end

(* interface SVGFEConvolveMatrixElement *)
and feConvolveMatrixElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method orderX : animatedInteger t readonly_prop

  method orderY : animatedInteger t readonly_prop

  method kernelMatrix : animatedNumberList t readonly_prop

  method divisor : animatedNumber t readonly_prop

  method bias : animatedNumber t readonly_prop

  method targetX : animatedInteger t readonly_prop

  method targetY : animatedInteger t readonly_prop

  method edgeMode : animatedEdgeMode t readonly_prop

  method kernelUnitLengthX : animatedNumber t readonly_prop

  method kernelUnitLengthY : animatedNumber t readonly_prop

  method preserveAlpha : animatedBoolean t readonly_prop
end

(* interface SVGFEDiffuseLightingElement *)
and feDiffuseLightingElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method surfaceScale : animatedNumber t readonly_prop

  method diffuseConstant : animatedNumber t readonly_prop

  method kernelUnitLengthX : animatedNumber t readonly_prop

  method kernelUnitLengthY : animatedNumber t readonly_prop
end

(* interface SVGFEDistantLightElement *)
and feDistantLightElement = object
  inherit element

  method azimuth : animatedNumber t readonly_prop

  method elevation : animatedNumber t readonly_prop
end

(* interface SVGFEPointLightElement *)
and fePointLightElement = object
  inherit element

  method x : animatedNumber t readonly_prop

  method y : animatedNumber t readonly_prop

  method z : animatedNumber t readonly_prop
end

(* interface SVGFESpotLightElement *)
and feSpotLightElement = object
  inherit element

  method x : animatedNumber t readonly_prop

  method y : animatedNumber t readonly_prop

  method z : animatedNumber t readonly_prop

  method pointsAtX : animatedNumber t readonly_prop

  method pointsAtY : animatedNumber t readonly_prop

  method pointsAtZ : animatedNumber t readonly_prop

  method specularExponent : animatedNumber t readonly_prop

  method limitingConeAngle : animatedNumber t readonly_prop
end

(* interface SVGFEDisplacementMapElement *)
and feDisplacementMapElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method in2 : animatedString t readonly_prop

  method scale : animatedNumber t readonly_prop

  method xChannelSelector : animatedChannelSelector t readonly_prop

  method yChannelSelector : animatedChannelSelector t readonly_prop
end

(* interface SVGFEFloodElement *)
and feFloodElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes
end

(* interface SVGFEGaussianBlurElement *)
and feGaussianBlurElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method stdDeviationX : animatedNumber t readonly_prop

  method stdDeviationY : animatedNumber t readonly_prop

  method edgeMode : animatedEdgeMode t readonly_prop

  method setStdDeviation : number_t -> number_t -> unit meth
end

(* interface SVGFEImageElement *)
and feImageElement = object
  inherit element

  inherit uriReference

  inherit langSpace

  inherit externalResourcesRequired

  inherit filterPrimitiveStandardAttributes

  method preserveAspectRatio : animatedPreserveAspectRatio t readonly_prop
end

(* interface SVGFEMergeElement *)
and feMergeElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes
end

(* interface SVGFEMergeNodeElement *)
and feMergeNodeElement = object
  inherit element

  method in1 : animatedString t readonly_prop
end

(* interface SVGFEMorphologyElement *)
and feMorphologyElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method operator : animatedMorphologyOperator t readonly_prop

  method radiusX : animatedNumber t readonly_prop

  method radiusY : animatedNumber t readonly_prop
end

(* interface SVGFEOffsetElement *)
and feOffsetElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method dx : animatedNumber t readonly_prop

  method dy : animatedNumber t readonly_prop
end

(* interface SVGFESpecularLightingElement *)
and feSpecularLightingElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method surfaceScale : animatedNumber t readonly_prop

  method specularConstant : animatedNumber t readonly_prop

  method specularExponent : animatedNumber t readonly_prop

  method kernelUnitLengthX : animatedNumber t readonly_prop

  method kernelUnitLengthY : animatedNumber t readonly_prop
end

(* interface SVGFETileElement *)
and feTileElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop
end

(* interface SVGFETurbulenceElement *)
and feTurbulenceElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method baseFrequencyX : animatedNumber t readonly_prop

  method baseFrequencyY : animatedNumber t readonly_prop

  method numOctaves : animatedInteger t readonly_prop

  method seed : animatedNumber t readonly_prop

  method stitchTiles : animatedStitchType t readonly_prop

  method _type : animatedTurbulenceType t readonly_prop
end

(* interface SVGFEDropShadowElement - Filter Effects *)
and feDropShadowElement = object
  inherit element

  inherit filterPrimitiveStandardAttributes

  method in1 : animatedString t readonly_prop

  method dx : animatedNumber t readonly_prop

  method dy : animatedNumber t readonly_prop

  method stdDeviationX : animatedNumber t readonly_prop

  method stdDeviationY : animatedNumber t readonly_prop

  method setStdDeviation : number_t -> number_t -> unit meth
end (* interface SVGCursorElement *)

(** @deprecated Removed in SVG 2. Use CSS cursor property. *)
and cursorElement = object
  inherit element

  inherit uriReference

  inherit tests

  inherit externalResourcesRequired

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop
end

(* interface SVGAElement *)
and aElement = object
  inherit graphicsElement

  inherit uriReference

  inherit langSpace

  inherit externalResourcesRequired

  method target : animatedString t readonly_prop

  method download : js_string t prop
  (** SVG 2 addition. *)

  method ping : js_string t prop
  (** SVG 2 addition. *)

  method rel : js_string t prop
  (** SVG 2 addition. *)

  method relList : Dom_html.tokenList t readonly_prop
  (** SVG 2 addition. *)

  method hreflang : js_string t prop
  (** SVG 2 addition. *)

  method type_ : js_string t prop
  (** SVG 2 addition. *)

  method referrerPolicy : js_string t prop
  (** SVG 2 addition. *)

  method text : js_string t prop
  (** SVG 2 addition (synonym for [textContent]). *)

  method protocol : js_string t prop
  (** SVG 2 addition (from [HTMLHyperlinkElementUtils]). *)

  method username : js_string t prop

  method password : js_string t prop

  method host : js_string t prop

  method hostname : js_string t prop

  method port : js_string t prop

  method pathname : js_string t prop

  method search : js_string t prop

  method hash : js_string t prop

  method origin : js_string t readonly_prop
end

(* interface SVGViewElement *)
and viewElement = object
  inherit element

  inherit externalResourcesRequired

  inherit fitToViewBox

  inherit zoomAndPan

  method viewTarget : stringList t readonly_prop
  (** @deprecated Removed in SVG 2. *)
end

(* interface SVGScriptElement *)
and scriptElement = object
  inherit element

  inherit uriReference

  inherit externalResourcesRequired

  method type_ : js_string t prop

  method crossOrigin : js_string t opt prop
  (** SVG 2 addition. *)
end

(* interface SVGZoomEvent : UIEvent *)
(*   readonly attribute SVGRect zoomRectScreen; *)
(*   readonly attribute float previousScale; *)
(*   readonly attribute SVGPoint previousTranslate; *)
(*   readonly attribute float newScale; *)
(*   readonly attribute SVGPoint newTranslate; *)
(* }; *)

(* interface SVGAnimationElement *)
and animationElement = object
  inherit element

  inherit tests

  inherit externalResourcesRequired

  method targetElement : element t readonly_prop

  method getStartTime : number_t meth

  method getCurrentTime : number_t meth

  method getSimpleDuration : number_t meth

  method beginElement : unit meth
  (** SVG 2 addition (from [ElementTimeControl]). *)

  method beginElementAt : number_t -> unit meth
  (** SVG 2 addition (from [ElementTimeControl]). *)

  method endElement : unit meth
  (** SVG 2 addition (from [ElementTimeControl]). *)

  method endElementAt : number_t -> unit meth
  (** SVG 2 addition (from [ElementTimeControl]). *)
end

(* interface SVGAnimateElement *)
and animateElement = object
  inherit animationElement
end

(* interface SVGSetElement *)
and setElement = animationElement

(* interface SVGAnimateMotionElement *)
and animateMotionElement = animationElement

(* interface SVGMPathElement *)
and mPathElement = object
  inherit element

  inherit uriReference

  inherit externalResourcesRequired
end

(* interface SVGAnimateColorElement *)
and animateColorElement = object
  inherit animationElement
end

(* interface SVGAnimateTransformElement *)
and animateTransformElement = animationElement (* interface SVGFontElement *)

(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)
and fontElement = object
  inherit element
end
(* interface SVGGlyphElement *)
(* interface SVGMissingGlyphElement*)

(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)
and glyphElement = object
  inherit element
end

(* interface SVGHKernElement : SVGElement *)
(* interface SVGVKernElement : SVGElement *)

(* interface SVGFontFaceElement *)

class type fontFaceElement = element
(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)

(* interface SVGFontFaceSrcElement *)

class type fontFaceSrcElement = element
(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)

(* interface SVGFontFaceUriElement *)

class type fontFaceUriElement = element
(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)

(* interface SVGFontFaceFormatElement *)

class type fontFaceFormatElement = element
(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)

(* interface SVGFontFaceNameElement *)

class type fontFaceNameElement = element
(** @deprecated Removed in SVG 2. Use WOFF/WOFF2 fonts. *)

(* interface SVGMetadataElement *)
class type metadataElement = element

(* interface SVGForeignObjectElement *)
class type foreignObjectElement = object
  inherit graphicsElement

  inherit langSpace

  inherit externalResourcesRequired

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop
end

(** {2 Helper functions for creating Svg elements} *)

val createElement : document t -> string -> element t

val createA : document t -> aElement t

val createAltGlyph : document t -> altGlyphElement t

val createAltGlyphDef : document t -> altGlyphDefElement t

val createAltGlyphItem : document t -> altGlyphItemElement t

val createAnimate : document t -> animateElement t

val createAnimateColor : document t -> animateColorElement t

val createAnimateMotion : document t -> animateMotionElement t

val createAnimateTransform : document t -> animateTransformElement t

val createCircle : document t -> circleElement t

val createClipPath : document t -> clipPathElement t

(* val createColorProfile : document t -> colorProfile t *)
val createCursor : document t -> cursorElement t

val createDefs : document t -> defsElement t

val createDesc : document t -> descElement t

val createEllipse : document t -> ellipseElement t

(* val createFe* *)
val createFilter : document t -> filterElement t

val createFont : document t -> fontElement t

val createFontFace : document t -> fontElement t

val createFontFaceFormat : document t -> fontElement t

val createFontFaceName : document t -> fontElement t

val createFontFaceSrc : document t -> fontElement t

val createFontFaceUri : document t -> fontElement t

val createForeignObject : document t -> foreignObjectElement t

val createG : document t -> gElement t

val createGlyph : document t -> glyphElement t

val createGlyphRef : document t -> glyphElement t

val createhkern : document t -> element t

val createImage : document t -> imageElement t

val createLineElement : document t -> lineElement t

val createLinearElement : document t -> linearGradientElement t

val createMask : document t -> maskElement t

val createMetaData : document t -> metadataElement t

val createMissingGlyph : document t -> glyphElement t

val createMPath : document t -> mPathElement t

val createPath : document t -> pathElement t

val createPattern : document t -> patternElement t

val createPolygon : document t -> polygonElement t

val createPolyline : document t -> polyLineElement t

val createRadialgradient : document t -> radialGradientElement t

val createRect : document t -> rectElement t

val createScript : document t -> scriptElement t

val createSet : document t -> setElement t

val createStop : document t -> stopElement t

val createStyle : document t -> styleElement t

val createSvg : document t -> svgElement t

val createSwitch : document t -> switchElement t

val createSymbol : document t -> symbolElement t

val createTextElement : document t -> textElement t

val createTextpath : document t -> textPathElement t

val createTitle : document t -> titleElement t

val createTref : document t -> trefElement t

val createTspan : document t -> tspanElement t

val createUse : document t -> useElement t

val createView : document t -> viewElement t

val createvkern : document t -> element t

val createMarker : document t -> markerElement t

val createFeBlend : document t -> feBlendElement t

val createFeColorMatrix : document t -> feColorMatrixElement t

val createFeComponentTransfer : document t -> feComponentTransferElement t

val createFeFuncR : document t -> feFuncRElement t

val createFeFuncG : document t -> feFuncGElement t

val createFeFuncB : document t -> feFuncBElement t

val createFeFuncA : document t -> feFuncAElement t

val createFeComposite : document t -> feCompositeElement t

val createFeConvolveMatrix : document t -> feConvolveMatrixElement t

val createFeDiffuseLighting : document t -> feDiffuseLightingElement t

val createFeDistantLight : document t -> feDistantLightElement t

val createFePointLight : document t -> fePointLightElement t

val createFeSpotLight : document t -> feSpotLightElement t

val createFeDisplacementMap : document t -> feDisplacementMapElement t

val createFeFlood : document t -> feFloodElement t

val createFeGaussianBlur : document t -> feGaussianBlurElement t

val createFeImage : document t -> feImageElement t

val createFeMerge : document t -> feMergeElement t

val createFeMergeNode : document t -> feMergeNodeElement t

val createFeMorphology : document t -> feMorphologyElement t

val createFeOffset : document t -> feOffsetElement t

val createFeSpecularLighting : document t -> feSpecularLightingElement t

val createFeTile : document t -> feTileElement t

val createFeTurbulence : document t -> feTurbulenceElement t

val createFeDropShadow : document t -> feDropShadowElement t

(****)

val svg_element : element t constr

val document : document t
(** The current document *)

val getElementById : string -> element t
(** [getElementById id] returns the element with the id [id] in the
    current document. It raises [Not_found] if there are no such element *)

(** {2 Coercion functions} *)
module CoerceTo : sig
  val element : #Dom.node t -> element t opt

  val a : #element t -> aElement t opt

  val altGlyph : #element t -> altGlyphElement t opt

  val altGlyphDef : #element t -> altGlyphDefElement t opt

  val altGlyphItem : #element t -> altGlyphItemElement t opt

  val animate : #element t -> animateElement t opt

  val animateColor : #element t -> animateColorElement t opt

  val animateMotion : #element t -> animateMotionElement t opt

  val animateTransform : #element t -> animateTransformElement t opt

  val circle : #element t -> circleElement t opt

  val clipPath : #element t -> clipPathElement t opt

  (* val ColorProfile : #element t -> colorProfile t opt *)
  val cursor : #element t -> cursorElement t opt

  val defs : #element t -> defsElement t opt

  val desc : #element t -> descElement t opt

  val ellipse : #element t -> ellipseElement t opt

  val filter : #element t -> filterElement t opt

  val font : #element t -> fontElement t opt

  val fontFace : #element t -> fontElement t opt

  val fontFaceFormat : #element t -> fontElement t opt

  val fontFaceName : #element t -> fontElement t opt

  val fontFaceSrc : #element t -> fontElement t opt

  val fontFaceUri : #element t -> fontElement t opt

  val foreignObject : #element t -> foreignObjectElement t opt

  val g : #element t -> gElement t opt

  val glyph : #element t -> glyphElement t opt

  val glyphRef : #element t -> glyphElement t opt

  val hkern : #element t -> element t opt

  val image : #element t -> imageElement t opt

  val lineElement : #element t -> lineElement t opt

  val linearElement : #element t -> linearGradientElement t opt

  val mask : #element t -> maskElement t opt

  val metaData : #element t -> metadataElement t opt

  val missingGlyph : #element t -> glyphElement t opt

  val mPath : #element t -> mPathElement t opt

  val path : #element t -> pathElement t opt

  val pattern : #element t -> patternElement t opt

  val polygon : #element t -> polygonElement t opt

  val polyline : #element t -> polyLineElement t opt

  val radialgradient : #element t -> radialGradientElement t opt

  val rect : #element t -> rectElement t opt

  val script : #element t -> scriptElement t opt

  val set : #element t -> setElement t opt

  val stop : #element t -> stopElement t opt

  val style : #element t -> styleElement t opt

  val svg : #element t -> svgElement t opt

  val switch : #element t -> switchElement t opt

  val symbol : #element t -> symbolElement t opt

  val textElement : #element t -> textElement t opt

  val textpath : #element t -> textPathElement t opt

  val title : #element t -> titleElement t opt

  val tref : #element t -> trefElement t opt

  val tspan : #element t -> tspanElement t opt

  val use : #element t -> useElement t opt

  val view : #element t -> viewElement t opt

  val vkern : #element t -> element t opt

  val marker : #element t -> markerElement t opt

  val feBlend : #element t -> feBlendElement t opt

  val feColorMatrix : #element t -> feColorMatrixElement t opt

  val feComponentTransfer : #element t -> feComponentTransferElement t opt

  val feFuncR : #element t -> feFuncRElement t opt

  val feFuncG : #element t -> feFuncGElement t opt

  val feFuncB : #element t -> feFuncBElement t opt

  val feFuncA : #element t -> feFuncAElement t opt

  val feComposite : #element t -> feCompositeElement t opt

  val feConvolveMatrix : #element t -> feConvolveMatrixElement t opt

  val feDiffuseLighting : #element t -> feDiffuseLightingElement t opt

  val feDistantLight : #element t -> feDistantLightElement t opt

  val fePointLight : #element t -> fePointLightElement t opt

  val feSpotLight : #element t -> feSpotLightElement t opt

  val feDisplacementMap : #element t -> feDisplacementMapElement t opt

  val feFlood : #element t -> feFloodElement t opt

  val feGaussianBlur : #element t -> feGaussianBlurElement t opt

  val feImage : #element t -> feImageElement t opt

  val feMerge : #element t -> feMergeElement t opt

  val feMergeNode : #element t -> feMergeNodeElement t opt

  val feMorphology : #element t -> feMorphologyElement t opt

  val feOffset : #element t -> feOffsetElement t opt

  val feSpecularLighting : #element t -> feSpecularLightingElement t opt

  val feTile : #element t -> feTileElement t opt

  val feTurbulence : #element t -> feTurbulenceElement t opt

  val feDropShadow : #element t -> feDropShadowElement t opt
end
