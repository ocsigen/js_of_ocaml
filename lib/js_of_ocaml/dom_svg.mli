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

    This is a partial binding to the DOM SVG API. *)

open Js

val xmlns : js_string t
(** {2 Types} *)

type error_code =
  | WRONG_TYPE_ERR
  | INVALID_VALUE_ERR
  | MATRIX_NOT_INVERTABLE

class type svg_error = object
  inherit Js.error

  method code : error_code t readonly_prop
end

exception SVGError of svg_error

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

type suspendHandleID

(****)

class type ['a] animated = object
  method baseVal : 'a prop

  method animVal : 'a prop
end

class type ['a] list = object
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
(** {2 Elements} *)

(* interface SVGElement *)
class type element = object
  inherit Dom.element

  method id : js_string t prop

  method xmlbase : js_string t prop

  method ownerSVGElement : svgElement t readonly_prop

  method viewportElement : element t readonly_prop
end

(* interface SVGAnimatedString *)
and animatedString = [js_string t] animated

(* interface SVGAnimatedBoolean *)
and animatedBoolean = [bool t] animated

(* interface SVGStringList *)
and stringList = [js_string t] list

(* interface SVGAnimatedEnumeration *)
and animatedEnumeration = [int (*short*)] animated

(* interface SVGAnimatedInteger *)
and animatedInteger = [int] animated

(* interface SVGAnimatedNumber *)
and animatedNumber = [number_t] animated

(* interface SVGNumberList *)
and numberList = [number t] list

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
and rgbColor = object end

(* interface SVGColor *)
and color = object
  (* XXX inherit cssValue *)
  method colorType : colorType readonly_prop

  method rgbColor : rgbColor t readonly_prop

  method iccColor : iccColor t readonly_prop

  method setRGBColor : js_string t -> unit meth

  method setRGBColorICCColor : js_string t -> js_string t -> unit meth

  method setColor : colorType -> js_string t -> js_string t -> unit meth
end

(* interface SVGICCColor *)
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
and animatedRect = [rect t] animated

(* interface SVGStylable *)
and stylable = object
  method className : animatedString t readonly_prop

  method style : Dom_html.cssStyleDeclaration t readonly_prop
  (*   CSSValue getPresentationAttribute(in DOMString name); *)
end

(* interface SVGLocatable *)
and locatable = object
  method nearestViewportElement : element t readonly_prop

  method farthestViewportElement : element t readonly_prop

  method getBBox : rect t meth

  method getCTM : matrix t meth

  method getScreenCTM : matrix t meth

  method getTransformToElement : element t -> matrix t meth
end

(* interface SVGTransformable *)
and transformable = object
  inherit locatable

  method transform : animatedTransformList t readonly_prop
end

(* interface SVGTests *)
and tests = object
  method requiredFeatures : stringList t readonly_prop

  method requiredExtensions : stringList t readonly_prop

  method systemLanguage : stringList t readonly_prop

  method hasExtension : js_string t -> bool t meth
end

(* interface SVGLangSpace *)
and langSpace = object
  method xmllang : js_string t prop

  method xmlspace : js_string t prop
end

(* interface SVGExternalResourcesRequired *)
and externalResourcesRequired = object
  method externalResourcesRequired : animatedBoolean t readonly_prop
end

(* interface SVGFitToViewBox *)
and fitToViewBox = object
  method viewBox : animatedRect t readonly_prop

  method preserveAspectRatio : animatedPreserveAspectRatio t readonly_prop
end

(* interface SVGZoomAndPan *)
and zoomAndPan = object
  method zoomAndPan : zoomAndPanType prop
end

(* interface SVGViewSpec *)
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
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit locatable

  inherit fitToViewBox

  inherit zoomAndPan

  (*XXX inherit documentevent, viewcss, documentcss *)
  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method contentScriptType : js_string t prop

  method contentStyleType : js_string t prop

  method viewport : rect t readonly_prop

  method pixelUnitToMillimeterX : number_t readonly_prop

  method pixelUnitToMillimeterY : number_t readonly_prop

  method screenPixelUnitToMillimeterX : number_t readonly_prop

  method screenPixelUnitToMillimeterY : number_t readonly_prop

  method useCurrentView : bool t readonly_prop

  method currentView : viewSpec t readonly_prop

  method currentScale : number_t prop

  method currentTranslate : point t readonly_prop

  method suspendRedraw : int -> suspendHandleID meth

  method unsuspendRedraw : suspendHandleID -> unit meth

  method unsuspendRedrawAll : unit meth

  method forceRedraw : unit meth

  method pauseAnimations : unit meth

  method unpauseAnimations : unit meth

  method animationsPaused : bool t meth

  method getCurrentTime : number_t meth

  method setCurrentTime : int -> unit meth

  method getIntersectionList : rect t -> element t -> element Dom.nodeList t meth

  method getEnclosureList : rect t -> element t -> element Dom.nodeList t meth

  method checkIntersection : element t -> rect t -> bool t

  method checkEnclosure : element t -> rect t -> bool t

  method deselectAll : unit meth

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
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  inherit Dom_html.eventTarget
end

(* interface SVGDefsElement *)
and defsElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable
  (* XXXXXXX ? inherit Dom_html.eventTarget *)
end

(* interface SVGDescElement *)
and descElement = object
  inherit element

  inherit langSpace

  inherit stylable
  (* XXXXXXX ? inherit Dom_html.eventTarget *)
end

(* interface SVGTitleElement *)
and titleElement = object
  inherit element

  inherit langSpace

  inherit stylable
end

(* interface SVGSymbolElement *)
and symbolElement = object
  inherit element

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit fitToViewBox

  inherit Dom_html.eventTarget
end

(* interface SVGUseElement *)
and useElement = object
  inherit element

  inherit uriReference

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method instanceRoot : elementInstance t readonly_prop

  method animatedInstanceRoot : elementInstance t readonly_prop
end

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
end

(* interface SVGElementInstanceList *)
and elementInstanceList = object
  method length : int readonly_prop

  method item : int -> elementInstance t
end

(* interface SVGImageElement *)
and imageElement = object
  inherit element

  inherit uriReference

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop
  (* readonly attribute SVGAnimatedPreserveAspectRatio preserveAspectRatio *)
end

and switchElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable
end

(* XXX deprecated => interface GetSVGDocument => SVGDocument getSVGDocument() *)

(* interface SVGStyleElement *)
and styleElement = object
  inherit element

  inherit langSpace

  method type_ : js_string t prop

  method media : js_string t prop

  method title : js_string t prop
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

  method x : number_t
end

(* interface SVGPathSegLinetoVerticalAbs *)
(* interface SVGPathSegLinetoVerticalRel *)
and pathSegLinetoVertical = object
  inherit pathSeg

  method y : number_t
end

and pathSegCurvetoCubicSmooth = object
  inherit pathSeg

  method x : number_t

  method y : number_t

  method x2 : number_t

  method y2 : number_t
end

(* interface SVGPathSegCurvetoQuadraticSmoothAbs *)
(* interface SVGPathSegCurvetoQuadraticSmoothRel  *)
and pathSegCurvetoQuadraticSmooth = object
  inherit pathSeg

  method x : number_t

  method y : number_t
end

and pathSegList = [pathSeg t] list

(* interface SVGAnimatedPathData *)
and animatedPathData = object
  method pathSegList : pathSegList t prop

  method normalizedPathSegList : pathSegList t prop

  method animatedPathSegList : pathSegList t prop

  method animatedNormalizedPathSegList : pathSegList t prop
end

(* interface SVGPathElement *)
and pathElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  inherit animatedPathData

  method pathLength : animatedNumber t readonly_prop

  method getTotalLength : number_t meth

  method getPointAtLength : number_t -> point t meth

  method getPathSegAtLength : number_t -> int

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
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method rx : animatedLength t readonly_prop

  method ry : animatedLength t readonly_prop
end

(* interface SVGCircleElement *)
and circleElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  method cx : animatedLength t readonly_prop

  method cy : animatedLength t readonly_prop

  method r : animatedLength t readonly_prop
end

(* interface SVGEllipseElement *)
and ellipseElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  method cx : animatedLength t readonly_prop

  method cy : animatedLength t readonly_prop

  method rx : animatedLength t readonly_prop

  method ry : animatedLength t readonly_prop
end

(* interface SVGLineElement *)
class type lineElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  inherit Dom_html.eventTarget

  method x1 : animatedLength t readonly_prop

  method y1 : animatedLength t readonly_prop

  method x2 : animatedLength t readonly_prop

  method y2 : animatedLength t readonly_prop
end

(* interface SVGAnimatedPoints *)
and animatedPoints = object
  method points : pointList t readonly_prop

  method animatedpoints : pointList t readonly_prop
end

(* interface SVGPolylineElement *)
and polyLineElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  inherit animatedPoints
end

(* interface SVGPolygonElement *)
and polygonElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  inherit animatedPoints
end

(* interface SVGTextContentElement *)
and textContentElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

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

  inherit transformable
end

and tspanElement = textPositioningElement

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
end

(* interface SVGAltGlyphElement *)
and altGlyphElement = object
  inherit textPositioningElement

  inherit uriReference

  method glyphRef : js_string t prop

  method format : js_string t prop
end

(* interface SVGAltGlyphDefElement *)
and altGlyphDefElement = element

(* interface SVGAltGlyphItemElement *)
and altGlyphItemElement = element

(* interface SVGGlyphRefElement *)
and glyphRefElement = object
  inherit element

  inherit uriReference

  inherit stylable

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

(* interface SVGMarkerElement : SVGElement, *)
(*                              SVGLangSpace, *)
(*                              SVGExternalResourcesRequired, *)
(*                              SVGStylable, *)
(*                              SVGFitToViewBox { *)

(*   // Marker Unit Types *)
(*   const unsigned short SVG_MARKERUNITS_UNKNOWN = 0; *)
(*   const unsigned short SVG_MARKERUNITS_USERSPACEONUSE = 1; *)
(*   const unsigned short SVG_MARKERUNITS_STROKEWIDTH = 2; *)

(*   // Marker Orientation Types *)
(*   const unsigned short SVG_MARKER_ORIENT_UNKNOWN = 0; *)
(*   const unsigned short SVG_MARKER_ORIENT_AUTO = 1; *)
(*   const unsigned short SVG_MARKER_ORIENT_ANGLE = 2; *)

(*   readonly attribute SVGAnimatedLength refX; *)
(*   readonly attribute SVGAnimatedLength refY; *)
(*   readonly attribute SVGAnimatedEnumeration markerUnits; *)
(*   readonly attribute SVGAnimatedLength markerWidth; *)
(*   readonly attribute SVGAnimatedLength markerHeight; *)
(*   readonly attribute SVGAnimatedEnumeration orientType; *)
(*   readonly attribute SVGAnimatedAngle orientAngle; *)

(*   void setOrientToAuto() raises(DOMException); *)
(*   void setOrientToAngle(in SVGAngle angle) raises(DOMException); *)
(* }; *)

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

  inherit stylable

  (*   readonly attribute SVGAnimatedEnumeration gradientUnits; *)
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
end

(* interface SVGStopElement *)
and stopElement = object
  inherit element

  inherit stylable

  method offset : animatedNumber t readonly_prop
end

(* interface SVGPatternElement *)
and patternElement = object
  inherit element

  inherit uriReference

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit fitToViewBox

  (*   readonly attribute SVGAnimatedEnumeration patternUnits; *)
  (*   readonly attribute SVGAnimatedEnumeration patternContentUnits; *)
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

  inherit stylable

  inherit transformable
  (*   readonly attribute SVGAnimatedEnumeration clipPathUnits; *)
end

(* interface SVGMaskElement *)
and maskElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  (*   readonly attribute SVGAnimatedEnumeration maskUnits; *)
  (*   readonly attribute SVGAnimatedEnumeration maskContentUnits; *)
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

  inherit stylable

  (*   readonly attribute SVGAnimatedEnumeration filterUnits; *)
  (*   readonly attribute SVGAnimatedEnumeration primitiveUnits; *)
  method x : animatedLength t readonly_prop

  method y : animatedLength t readonly_prop

  method width : animatedLength t readonly_prop

  method height : animatedLength t readonly_prop

  method filterResX : animatedInteger t readonly_prop

  method filterResY : animatedInteger t readonly_prop

  method setFilterRes : int -> int -> unit meth
end

(* interface SVGFilterPrimitiveStandardAttributes : SVGStylable { *)
(*   readonly attribute SVGAnimatedLength x; *)
(*   readonly attribute SVGAnimatedLength y; *)
(*   readonly attribute SVGAnimatedLength width; *)
(*   readonly attribute SVGAnimatedLength height; *)
(*   readonly attribute SVGAnimatedString result; *)
(* }; *)

(* interface SVGFEBlendElement : SVGElement, *)
(*                               SVGFilterPrimitiveStandardAttributes { *)

(*   // Blend Mode Types *)
(*   const unsigned short SVG_FEBLEND_MODE_UNKNOWN = 0; *)
(*   const unsigned short SVG_FEBLEND_MODE_NORMAL = 1; *)
(*   const unsigned short SVG_FEBLEND_MODE_MULTIPLY = 2; *)
(*   const unsigned short SVG_FEBLEND_MODE_SCREEN = 3; *)
(*   const unsigned short SVG_FEBLEND_MODE_DARKEN = 4; *)
(*   const unsigned short SVG_FEBLEND_MODE_LIGHTEN = 5; *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedString in2; *)
(*   readonly attribute SVGAnimatedEnumeration mode; *)
(* }; *)

(* interface SVGFEColorMatrixElement : SVGElement, *)
(*                                     SVGFilterPrimitiveStandardAttributes { *)

(*   // Color Matrix Types *)
(*   const unsigned short SVG_FECOLORMATRIX_TYPE_UNKNOWN = 0; *)
(*   const unsigned short SVG_FECOLORMATRIX_TYPE_MATRIX = 1; *)
(*   const unsigned short SVG_FECOLORMATRIX_TYPE_SATURATE = 2; *)
(*   const unsigned short SVG_FECOLORMATRIX_TYPE_HUEROTATE = 3; *)
(*   const unsigned short SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA = 4; *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedEnumeration type; *)
(*   readonly attribute SVGAnimatedNumberList values; *)
(* }; *)

(* interface SVGFEComponentTransferElement : SVGElement, *)
(*                                           SVGFilterPrimitiveStandardAttributes { *)
(*   readonly attribute SVGAnimatedString in1; *)
(* }; *)

(* interface SVGComponentTransferFunctionElement : SVGElement { *)

(*   // Component Transfer Types *)
(*   const unsigned short SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN = 0; *)
(*   const unsigned short SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY = 1; *)
(*   const unsigned short SVG_FECOMPONENTTRANSFER_TYPE_TABLE = 2; *)
(*   const unsigned short SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE = 3; *)
(*   const unsigned short SVG_FECOMPONENTTRANSFER_TYPE_LINEAR = 4; *)
(*   const unsigned short SVG_FECOMPONENTTRANSFER_TYPE_GAMMA = 5; *)

(*   readonly attribute SVGAnimatedEnumeration type; *)
(*   readonly attribute SVGAnimatedNumberList tableValues; *)
(*   readonly attribute SVGAnimatedNumber slope; *)
(*   readonly attribute SVGAnimatedNumber intercept; *)
(*   readonly attribute SVGAnimatedNumber amplitude; *)
(*   readonly attribute SVGAnimatedNumber exponent; *)
(*   readonly attribute SVGAnimatedNumber offset; *)
(* }; *)

(* interface SVGFEFuncRElement : SVGComponentTransferFunctionElement { *)
(* }; *)

(* interface SVGFEFuncGElement : SVGComponentTransferFunctionElement { *)
(* }; *)

(* interface SVGFEFuncBElement : SVGComponentTransferFunctionElement { *)
(* }; *)

(* interface SVGFEFuncAElement : SVGComponentTransferFunctionElement { *)
(* }; *)

(* interface SVGFECompositeElement : SVGElement, *)
(*                                   SVGFilterPrimitiveStandardAttributes { *)

(*   // Composite Operators *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_UNKNOWN = 0; *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_OVER = 1; *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_IN = 2; *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_OUT = 3; *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_ATOP = 4; *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_XOR = 5; *)
(*   const unsigned short SVG_FECOMPOSITE_OPERATOR_ARITHMETIC = 6; *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedString in2; *)
(*   readonly attribute SVGAnimatedEnumeration operator; *)
(*   readonly attribute SVGAnimatedNumber k1; *)
(*   readonly attribute SVGAnimatedNumber k2; *)
(*   readonly attribute SVGAnimatedNumber k3; *)
(*   readonly attribute SVGAnimatedNumber k4; *)
(* }; *)

(* interface SVGFEConvolveMatrixElement : SVGElement, *)
(*                                        SVGFilterPrimitiveStandardAttributes { *)

(*   // Edge Mode Values *)
(*   const unsigned short SVG_EDGEMODE_UNKNOWN = 0; *)
(*   const unsigned short SVG_EDGEMODE_DUPLICATE = 1; *)
(*   const unsigned short SVG_EDGEMODE_WRAP = 2; *)
(*   const unsigned short SVG_EDGEMODE_NONE = 3; *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedInteger orderX; *)
(*   readonly attribute SVGAnimatedInteger orderY; *)
(*   readonly attribute SVGAnimatedNumberList kernelMatrix; *)
(*   readonly attribute SVGAnimatedNumber divisor; *)
(*   readonly attribute SVGAnimatedNumber bias; *)
(*   readonly attribute SVGAnimatedInteger targetX; *)
(*   readonly attribute SVGAnimatedInteger targetY; *)
(*   readonly attribute SVGAnimatedEnumeration edgeMode; *)
(*   readonly attribute SVGAnimatedNumber kernelUnitLengthX; *)
(*   readonly attribute SVGAnimatedNumber kernelUnitLengthY; *)
(*   readonly attribute SVGAnimatedBoolean preserveAlpha; *)
(* }; *)

(* interface SVGFEDiffuseLightingElement : SVGElement, *)
(*                                         SVGFilterPrimitiveStandardAttributes { *)
(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedNumber surfaceScale; *)
(*   readonly attribute SVGAnimatedNumber diffuseConstant; *)
(*   readonly attribute SVGAnimatedNumber kernelUnitLengthX; *)
(*   readonly attribute SVGAnimatedNumber kernelUnitLengthY; *)
(* }; *)

(* interface SVGFEDistantLightElement : SVGElement { *)
(*   readonly attribute SVGAnimatedNumber azimuth; *)
(*   readonly attribute SVGAnimatedNumber elevation; *)
(* }; *)

(* interface SVGFEPointLightElement : SVGElement { *)
(*   readonly attribute SVGAnimatedNumber x; *)
(*   readonly attribute SVGAnimatedNumber y; *)
(*   readonly attribute SVGAnimatedNumber z; *)
(* }; *)

(* interface SVGFESpotLightElement : SVGElement { *)
(*   readonly attribute SVGAnimatedNumber x; *)
(*   readonly attribute SVGAnimatedNumber y; *)
(*   readonly attribute SVGAnimatedNumber z; *)
(*   readonly attribute SVGAnimatedNumber pointsAtX; *)
(*   readonly attribute SVGAnimatedNumber pointsAtY; *)
(*   readonly attribute SVGAnimatedNumber pointsAtZ; *)
(*   readonly attribute SVGAnimatedNumber specularExponent; *)
(*   readonly attribute SVGAnimatedNumber limitingConeAngle; *)
(* }; *)

(* interface SVGFEDisplacementMapElement : SVGElement, *)
(*                                         SVGFilterPrimitiveStandardAttributes { *)

(*   // Channel Selectors *)
(*   const unsigned short SVG_CHANNEL_UNKNOWN = 0; *)
(*   const unsigned short SVG_CHANNEL_R = 1; *)
(*   const unsigned short SVG_CHANNEL_G = 2; *)
(*   const unsigned short SVG_CHANNEL_B = 3; *)
(*   const unsigned short SVG_CHANNEL_A = 4; *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedString in2; *)
(*   readonly attribute SVGAnimatedNumber scale; *)
(*   readonly attribute SVGAnimatedEnumeration xChannelSelector; *)
(*   readonly attribute SVGAnimatedEnumeration yChannelSelector; *)
(* }; *)

(* interface SVGFEFloodElement : SVGElement, *)
(*                               SVGFilterPrimitiveStandardAttributes { *)
(* }; *)

(* interface SVGFEGaussianBlurElement : SVGElement, *)
(*                                      SVGFilterPrimitiveStandardAttributes { *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedNumber stdDeviationX; *)
(*   readonly attribute SVGAnimatedNumber stdDeviationY; *)

(*   void setStdDeviation(in float stdDeviationX, in float stdDeviationY) raises(DOMException); *)
(* }; *)

(* interface SVGFEImageElement : SVGElement, *)
(*                               SVGURIReference, *)
(*                               SVGLangSpace, *)
(*                               SVGExternalResourcesRequired, *)
(*                               SVGFilterPrimitiveStandardAttributes { *)
(*   readonly attribute SVGAnimatedPreserveAspectRatio preserveAspectRatio; *)
(* }; *)

(* interface SVGFEMergeElement : SVGElement, *)
(*                               SVGFilterPrimitiveStandardAttributes { *)
(* }; *)

(* interface SVGFEMergeNodeElement : SVGElement { *)
(*   readonly attribute SVGAnimatedString in1; *)
(* }; *)

(* interface SVGFEMorphologyElement : SVGElement, *)
(*                                    SVGFilterPrimitiveStandardAttributes { *)

(*   // Morphology Operators *)
(*   const unsigned short SVG_MORPHOLOGY_OPERATOR_UNKNOWN = 0; *)
(*   const unsigned short SVG_MORPHOLOGY_OPERATOR_ERODE = 1; *)
(*   const unsigned short SVG_MORPHOLOGY_OPERATOR_DILATE = 2; *)

(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedEnumeration operator; *)
(*   readonly attribute SVGAnimatedNumber radiusX; *)
(*   readonly attribute SVGAnimatedNumber radiusY; *)
(* }; *)

(* interface SVGFEOffsetElement : SVGElement, *)
(*                                SVGFilterPrimitiveStandardAttributes { *)
(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedNumber dx; *)
(*   readonly attribute SVGAnimatedNumber dy; *)
(* }; *)

(* interface SVGFESpecularLightingElement : SVGElement, *)
(*                                          SVGFilterPrimitiveStandardAttributes { *)
(*   readonly attribute SVGAnimatedString in1; *)
(*   readonly attribute SVGAnimatedNumber surfaceScale; *)
(*   readonly attribute SVGAnimatedNumber specularConstant; *)
(*   readonly attribute SVGAnimatedNumber specularExponent; *)
(*   readonly attribute SVGAnimatedNumber kernelUnitLengthX; *)
(*   readonly attribute SVGAnimatedNumber kernelUnitLengthY; *)
(* }; *)

(* interface SVGFETileElement : SVGElement, *)
(*                              SVGFilterPrimitiveStandardAttributes { *)
(*   readonly attribute SVGAnimatedString in1; *)
(* }; *)

(* interface SVGFETurbulenceElement : SVGElement, *)
(*                                    SVGFilterPrimitiveStandardAttributes { *)

(*   // Turbulence Types *)
(*   const unsigned short SVG_TURBULENCE_TYPE_UNKNOWN = 0; *)
(*   const unsigned short SVG_TURBULENCE_TYPE_FRACTALNOISE = 1; *)
(*   const unsigned short SVG_TURBULENCE_TYPE_TURBULENCE = 2; *)

(*   // Stitch Options *)
(*   const unsigned short SVG_STITCHTYPE_UNKNOWN = 0; *)
(*   const unsigned short SVG_STITCHTYPE_STITCH = 1; *)
(*   const unsigned short SVG_STITCHTYPE_NOSTITCH = 2; *)

(*   readonly attribute SVGAnimatedNumber baseFrequencyX; *)
(*   readonly attribute SVGAnimatedNumber baseFrequencyY; *)
(*   readonly attribute SVGAnimatedInteger numOctaves; *)
(*   readonly attribute SVGAnimatedNumber seed; *)
(*   readonly attribute SVGAnimatedEnumeration stitchTiles; *)
(*   readonly attribute SVGAnimatedEnumeration type; *)
(* }; *)

(* interface SVGCursorElement *)
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
  inherit element

  inherit uriReference

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

  method target : animatedString t readonly_prop
end

(* interface SVGViewElement *)
and viewElement = object
  inherit element

  inherit externalResourcesRequired

  inherit fitToViewBox

  inherit zoomAndPan

  method viewTarget : stringList t readonly_prop
end

(* interface SVGScriptElement *)
and scriptElement = object
  inherit element

  inherit uriReference

  inherit externalResourcesRequired

  method type_ : js_string t prop
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

  (* inherit elementTimeControl *)
  method targetElement : element t readonly_prop

  method getStartTime : number_t meth

  method getCurrentTime : number_t meth

  method getSimpleDuration : number_t meth
end

(* interface SVGAnimateElement *)
and animateElement = object
  inherit animationElement

  inherit stylable
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

  inherit stylable
end

(* interface SVGAnimateTransformElement *)
and animateTransformElement = animationElement

(* interface SVGFontElement *)
and fontElement = object
  inherit element

  inherit stylable
end

(* interface SVGGlyphElement *)
(* interface SVGMissingGlyphElement*)
and glyphElement = object
  inherit element

  inherit stylable
end

(* interface SVGHKernElement : SVGElement *)
(* interface SVGVKernElement : SVGElement *)

(* interface SVGFontFaceElement *)
class type fontFaceElement = element

(* interface SVGFontFaceSrcElement *)
class type fontFaceSrcElement = element

(* interface SVGFontFaceUriElement *)
class type fontFaceUriElement = element

(* interface SVGFontFaceFormatElement *)
class type fontFaceFormatElement = element

(* interface SVGFontFaceNameElement *)
class type fontFaceNameElement = element

(* interface SVGMetadataElement *)
class type metadataElement = element

(* interface SVGForeignObjectElement *)
class type foreignObjectElement = object
  inherit element

  inherit tests

  inherit langSpace

  inherit externalResourcesRequired

  inherit stylable

  inherit transformable

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

(* val createMarker : document t -> markerElement *)
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

(****)

val svg_element : element t constr

val document : document t
(** The current document *)

val getElementById : string -> element t
(** [getElementById id] returns the element with the id [id] in the current document. It
    raises [Not_found] if there are no such element *)

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

  (* val Fe* *)
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

  (* val Marker : #element t -> markerElement *)
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
end
