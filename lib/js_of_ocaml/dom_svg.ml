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

open Js
open! Import

let xmlns = Js.string "http://www.w3.org/2000/svg"

(* translate spec from http://www.w3.org/TR/SVG/idl.html *)
(* http://www.w3.org/TR/SVG/struct.html *)

type error_code =
  | WRONG_TYPE_ERR
  | INVALID_VALUE_ERR
  | MATRIX_NOT_INVERTABLE

class type svg_error =
  object
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

class type ['a] animated =
  object
    method baseVal : 'a prop

    method animVal : 'a prop
  end

class type ['a] list =
  object
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

(* interface SVGElement *)
class type element =
  object
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
and animatedNumber = [number t] animated

(* interface SVGNumberList *)
and numberList = [number t] list

(* interface SVGAnimatedNumberList *)
and animatedNumberList = [numberList t] animated

(* interface SVGLength *)
and length =
  object
    method unitType : lengthUnitType readonly_prop

    method value : number t prop

    method valueInSpecifiedUnits : number t prop

    method valueAsString : js_string t prop

    method newValueSpecifiedUnits : lengthUnitType -> number t -> unit meth

    method convertToSpecifiedUnits : lengthUnitType -> unit meth
  end

(* interface SVGAnimatedLength *)
and animatedLength = [length t] animated

(* interface SVGLengthList *)
and lengthList = [length t] list

(* interface SVGAnimatedLengthList *)
and animatedLengthList = [lengthList t] animated

(* interface SVGAngle *)
and angle =
  object
    method unitType : angleUnitType readonly_prop

    method value : number t prop

    method valueInSpecifiedUnits : number t prop

    method valueAsString : js_string t prop

    method newValueSpecifiedUnits : angleUnitType -> number t -> unit meth

    method convertToSpecifiedUnits : angleUnitType -> unit meth
  end

(* interface SVGAnimatedAngle *)
and animatedAngle = [angle t] animated

(* XXXXX Move it *)
and rgbColor = object end

(* interface SVGColor *)
and color =
  object
    (* XXX inherit cssValue *)
    method colorType : colorType readonly_prop

    method rgbColor : rgbColor t readonly_prop

    method iccColor : iccColor t readonly_prop

    method setRGBColor : js_string t -> unit meth

    method setRGBColorICCColor : js_string t -> js_string t -> unit meth

    method setColor : colorType -> js_string t -> js_string t -> unit meth
  end

(* interface SVGICCColor *)
and iccColor =
  object
    method colorProfile : js_string t prop

    method colors : numberList t readonly_prop
  end

(* interface SVGRect *)
and rect =
  object
    method x : number t prop

    method y : number t prop

    method width : number t prop

    method height : number t prop
  end

(* interface SVGAnimatedRect *)
and animatedRect = [rect t] animated

(* interface SVGStylable *)
and stylable =
  object
    method className : animatedString t readonly_prop

    method style : Dom_html.cssStyleDeclaration t readonly_prop
    (*   CSSValue getPresentationAttribute(in DOMString name); *)
  end

(* interface SVGLocatable *)
and locatable =
  object
    method nearestViewportElement : element t readonly_prop

    method farthestViewportElement : element t readonly_prop

    method getBBox : rect t meth

    method getCTM : matrix t meth

    method getScreenCTM : matrix t meth

    method getTransformToElement : element t -> matrix t meth
  end

(* interface SVGTransformable *)
and transformable =
  object
    inherit locatable

    method transform : animatedTransformList t readonly_prop
  end

(* interface SVGTests *)
and tests =
  object
    method requiredFeatures : stringList t readonly_prop

    method requiredExtensions : stringList t readonly_prop

    method systemLanguage : stringList t readonly_prop

    method hasExtension : js_string t -> bool t meth
  end

(* interface SVGLangSpace *)
and langSpace =
  object
    method xmllang : js_string t prop

    method xmlspace : js_string t prop
  end

(* interface SVGExternalResourcesRequired *)
and externalResourcesRequired =
  object
    method externalResourcesRequired : animatedBoolean t readonly_prop
  end

(* interface SVGFitToViewBox *)
and fitToViewBox =
  object
    method viewBox : animatedRect t readonly_prop

    method preserveAspectRatio : animatedPreserveAspectRatio t readonly_prop
  end

(* interface SVGZoomAndPan *)
and zoomAndPan =
  object
    method zoomAndPan : zoomAndPanType prop
  end

(* interface SVGViewSpec *)
and viewSpec =
  object
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
and uriReference =
  object
    method href : animatedString t readonly_prop
  end

(* interface SVGCSSRule : CSSRule *)
(*   const unsigned short COLOR_PROFILE_RULE = 7; *)
(* }; *)

(* interface SVGDocument *)
and document =
  object
    inherit [element] Dom.document

    (*XXX inherit documentEvent *)
    method title : js_string t prop

    method referrer : js_string t readonly_prop

    method domain : js_string t prop

    method _URL : js_string t readonly_prop

    method rootElement : svgElement t opt readonly_prop
    (* rootElement will be null or undefined in an html context *)
  end

(* interface SVGSVGElement *)
and svgElement =
  object
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

    method pixelUnitToMillimeterX : number t readonly_prop

    method pixelUnitToMillimeterY : number t readonly_prop

    method screenPixelUnitToMillimeterX : number t readonly_prop

    method screenPixelUnitToMillimeterY : number t readonly_prop

    method useCurrentView : bool t readonly_prop

    method currentView : viewSpec t readonly_prop

    method currentScale : number t prop

    method currentTranslate : point t readonly_prop

    method suspendRedraw : int -> suspendHandleID meth

    method unsuspendRedraw : suspendHandleID -> unit meth

    method unsuspendRedrawAll : unit meth

    method forceRedraw : unit meth

    method pauseAnimations : unit meth

    method unpauseAnimations : unit meth

    method animationsPaused : bool t meth

    method getCurrentTime : number t meth

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
and gElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable

    inherit Dom_html.eventTarget
  end

(* interface SVGDefsElement *)
and defsElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable
    (* XXXXXXX ? inherit Dom_html.eventTarget *)
  end

(* interface SVGDescElement *)
and descElement =
  object
    inherit element

    inherit langSpace

    inherit stylable
    (* XXXXXXX ? inherit Dom_html.eventTarget *)
  end

(* interface SVGTitleElement *)
and titleElement =
  object
    inherit element

    inherit langSpace

    inherit stylable
  end

(* interface SVGSymbolElement *)
and symbolElement =
  object
    inherit element

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit fitToViewBox

    inherit Dom_html.eventTarget
  end

(* interface SVGUseElement *)
and useElement =
  object
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

and elementInstance =
  object
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
and elementInstanceList =
  object
    method length : int readonly_prop

    method item : int -> elementInstance t
  end

(* interface SVGImageElement *)
and imageElement =
  object
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

and switchElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable
  end

(* XXX deprecated => interface GetSVGDocument => SVGDocument getSVGDocument() *)

(* interface SVGStyleElement *)
and styleElement =
  object
    inherit element

    inherit langSpace

    method type_ : js_string t prop

    method media : js_string t prop

    method title : js_string t prop
  end

(* interface SVGPoint *)
and point =
  object
    method x : number t readonly_prop

    method y : number t readonly_prop

    method matrixTransform : matrix t -> point t meth
  end

(* interface SVGPointList *)
and pointList = [point t] list

(* interface SVGMatrix *)
and matrix =
  object
    method a : number t readonly_prop

    method b : number t readonly_prop

    method c : number t readonly_prop

    method d : number t readonly_prop

    method e : number t readonly_prop

    method f : number t readonly_prop

    method multiply : matrix t -> matrix t meth

    method inverse : matrix t meth

    method translate : number t -> number t -> matrix t meth

    method scale : number t -> matrix t meth

    method scaleNonUniform : number t -> number t -> matrix t meth

    method rotate : number t -> matrix t meth

    method rotateFromVector : number t -> number t -> matrix t meth

    method flipX : matrix t meth

    method flipY : matrix t meth

    method skewX : number t -> matrix t meth

    method skewY : number t -> matrix t meth
  end

(* interface SVGTransform *)
and transform =
  object
    method _type : transformType readonly_prop

    method matrix : matrix t readonly_prop

    method angle : number t readonly_prop

    method setMatrix : matrix t -> unit meth

    method setTranslate : number t -> number t -> unit meth

    method setScale : number t -> number t -> unit meth

    method setRotate : number t -> number t -> number t -> unit meth

    method setSkewX : number t -> unit meth

    method setSkewY : number t -> unit meth
  end

(* interface SVGTransformList *)
and transformList =
  object
    inherit [transform t] list

    method createSVGTransformFromMatrix : matrix -> transform t meth

    method consolidate : transform t meth
  end

(* interface SVGAnimatedTransformList *)
and animatedTransformList = [transformList t] animated

(* interface SVGPreserveAspectRatio *)
and preserveAspectRatio =
  object
    method align : alignmentType readonly_prop

    method meetOrSlice : meetOrSliceType readonly_prop
  end

(* interface SVGAnimatedPreserveAspectRatio *)
and animatedPreserveAspectRatio = [preserveAspectRatio t] animated

(* interface SVGPathSeg *)
and pathSeg =
  object
    method pathSegType : pathSegmentType readonly_prop

    method pathSegTypeAsLetter : js_string t readonly_prop
  end

(* interface SVGPathSegClosePath *)
and pathSegClosePath = pathSeg

(* interface SVGPathSegMovetoAbs *)
(* interface SVGPathSegMovetoRel *)
and pathSegMoveto =
  object
    inherit pathSeg

    method x : number t prop

    method y : number t prop
  end

(* interface SVGPathSegLinetoAbs *)
(* interface SVGPathSegLinetoRel *)
and pathSegLineto =
  object
    inherit pathSeg

    method x : number t prop

    method y : number t prop
  end

(* interface SVGPathSegCurvetoCubicAbs *)
(* interface SVGPathSegCurvetoCubicRel *)
and pathSegCurvetoCubic =
  object
    inherit pathSeg

    method x : number t prop

    method y : number t prop

    method x1 : number t prop

    method y1 : number t prop

    method x2 : number t prop

    method y2 : number t prop
  end

(* interface SVGPathSegCurvetoQuadraticAbs *)
(* interface SVGPathSegCurvetoQuadraticRel *)
and pathSegCurvetoQuadratic =
  object
    inherit pathSeg

    method x : number t prop

    method y : number t prop

    method x1 : number t prop

    method y1 : number t prop
  end

(* interface SVGPathSegArcAbs *)
(* interface SVGPathSegArcRel*)
and pathSegArc =
  object
    inherit pathSeg

    method y : number t prop

    method r1 : number t prop

    method r2 : number t prop

    method angle : number t prop

    method largeArcFlag : bool t prop

    method sweepFlag : bool t prop
  end

(* interface SVGPathSegLinetoHorizontalAbs *)
(* interface SVGPathSegLinetoHorizontalRel *)
and pathSegLinetoHorizontal =
  object
    inherit pathSeg

    method x : number t
  end

(* interface SVGPathSegLinetoVerticalAbs *)
(* interface SVGPathSegLinetoVerticalRel *)
and pathSegLinetoVertical =
  object
    inherit pathSeg

    method y : number t
  end

and pathSegCurvetoCubicSmooth =
  object
    inherit pathSeg

    method x : number t

    method y : number t

    method x2 : number t

    method y2 : number t
  end

(* interface SVGPathSegCurvetoQuadraticSmoothAbs *)
(* interface SVGPathSegCurvetoQuadraticSmoothRel  *)
and pathSegCurvetoQuadraticSmooth =
  object
    inherit pathSeg

    method x : number t

    method y : number t
  end

and pathSegList = [pathSeg t] list

(* interface SVGAnimatedPathData *)
and animatedPathData =
  object
    method pathSegList : pathSegList t prop

    method normalizedPathSegList : pathSegList t prop

    method animatedPathSegList : pathSegList t prop

    method animatedNormalizedPathSegList : pathSegList t prop
  end

(* interface SVGPathElement *)
and pathElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable

    inherit animatedPathData

    method pathLength : animatedNumber t readonly_prop

    method getTotalLength : number t meth

    method getPointAtLength : number t -> point t meth

    method getPathSegAtLength : number t -> int

    method createSVGPathSegClosePath : pathSegClosePath meth

    method createSVGPathSegMovetoAbs : number t -> number t -> pathSegMoveto meth

    method createSVGPathSegMovetoRel : number t -> number t -> pathSegMoveto meth

    method createSVGPathSegLinetoAbs : number t -> number t -> pathSegLineto meth

    method createSVGPathSegLinetoRel : number t -> number t -> pathSegLineto meth

    method createSVGPathSegCurvetoCubicAbs :
         number t
      -> number t
      -> number t
      -> number t
      -> number t
      -> number t
      -> pathSegCurvetoCubic meth

    method createSVGPathSegCurvetoCubicRel :
         number t
      -> number t
      -> number t
      -> number t
      -> number t
      -> number t
      -> pathSegCurvetoCubic meth

    method createSVGPathSegCurvetoQuadraticAbs :
      number t -> number t -> number t -> number t -> pathSegCurvetoQuadratic meth

    method createSVGPathSegCurvetoQuadraticRel :
      number t -> number t -> number t -> number t -> pathSegCurvetoQuadratic meth

    method createSVGPathSegArcAbs :
         number t
      -> number t
      -> number t
      -> number t
      -> number t
      -> bool t
      -> bool t
      -> pathSegArc meth

    method createSVGPathSegArcRel :
         number t
      -> number t
      -> number t
      -> number t
      -> number t
      -> bool t
      -> bool t
      -> pathSegArc meth

    method createSVGPathSegLinetoHorizontalAbs : number t -> pathSegLinetoHorizontal meth

    method createSVGPathSegLinetoHorizontalRel : number t -> pathSegLinetoHorizontal meth

    method createSVGPathSegLinetoVerticalAbs : number t -> pathSegLinetoVertical meth

    method createSVGPathSegLinetoVerticalRel : number t -> pathSegLinetoVertical meth

    method createSVGPathSegCurvetoCubicSmoothAbs :
      number t -> number t -> number t -> number t -> pathSegCurvetoCubicSmooth meth

    method createSVGPathSegCurvetoCubicSmoothRel :
      number t -> number t -> number t -> number t -> pathSegCurvetoCubicSmooth meth

    method createSVGPathSegCurvetoQuadraticSmoothAbs :
      number t -> number t -> pathSegCurvetoQuadraticSmooth meth

    method createSVGPathSegCurvetoQuadraticSmoothRel :
      number t -> number t -> pathSegCurvetoQuadraticSmooth meth
  end

(* interface SVGRectElement *)
and rectElement =
  object
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
and circleElement =
  object
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
and ellipseElement =
  object
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
class type lineElement =
  object
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
and animatedPoints =
  object
    method points : pointList t readonly_prop

    method animatedpoints : pointList t readonly_prop
  end

(* interface SVGPolylineElement *)
and polyLineElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable

    inherit animatedPoints
  end

(* interface SVGPolygonElement *)
and polygonElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable

    inherit animatedPoints
  end

(* interface SVGTextContentElement *)
and textContentElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit Dom_html.eventTarget

    method textLength : animatedLength t readonly_prop

    method lengthAdjust : lengthAdjust animated t readonly_prop

    method getNumberOfChars : int meth

    method getComputedTextLength : number t meth

    method getSubStringLength : int -> int -> number t meth

    method getStartPositionOfChar : int -> point t meth

    method getEndPositionOfChar : int -> point t meth

    method getExtentOfChar : int -> rect t meth

    method getRotationOfChar : int -> number t meth

    method getCharNumAtPosition : point -> int meth

    method selectSubString : int -> int -> unit meth
  end

(* interface SVGTextPositioningElement *)
and textPositioningElement =
  object
    inherit textContentElement

    method x : animatedLengthList t readonly_prop

    method y : animatedLengthList t readonly_prop

    method dx : animatedLengthList t readonly_prop

    method dy : animatedLengthList t readonly_prop

    method rotate : animatedNumberList t readonly_prop
  end

(* interface SVGTextElement *)
and textElement =
  object
    inherit textPositioningElement

    inherit transformable
  end

and tspanElement = textPositioningElement

and trefElement =
  object
    inherit textPositioningElement

    inherit uriReference
  end

(* interface SVGTextPathElement *)
and textPathElementMethod = [textPathMethodType] animated

and textPathElementSpacing = [textPathSpacingType] animated

and textPathElement =
  object
    inherit textContentElement

    inherit uriReference

    method startOffset : animatedLength t readonly_prop

    method method_ : textPathElementMethod readonly_prop

    method spacing : textPathElementSpacing readonly_prop
  end

(* interface SVGAltGlyphElement *)
and altGlyphElement =
  object
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
and glyphRefElement =
  object
    inherit element

    inherit uriReference

    inherit stylable

    method glyphRef : js_string t prop

    method format : js_string t prop

    method x : number t prop

    method y : number t prop

    method dx : number t prop

    method dy : number t prop
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

and gradientElement =
  object
    inherit element

    inherit uriReference

    inherit stylable

    (*   readonly attribute SVGAnimatedEnumeration gradientUnits; *)
    method gradientTransform : animatedTransformList t readonly_prop

    method spreadMethod : animatedSpreadMethod t readonly_prop
  end

(* interface SVGLinearGradientElement *)
and linearGradientElement =
  object
    inherit gradientElement

    method x1 : animatedLength t readonly_prop

    method y1 : animatedLength t readonly_prop

    method x2 : animatedLength t readonly_prop

    method y2 : animatedLength t readonly_prop
  end

(* interface SVGRadialGradientElement *)
and radialGradientElement =
  object
    inherit gradientElement

    method cx : animatedLength t readonly_prop

    method cy : animatedLength t readonly_prop

    method r : animatedLength t readonly_prop

    method fx : animatedLength t readonly_prop

    method fy : animatedLength t readonly_prop
  end

(* interface SVGStopElement *)
and stopElement =
  object
    inherit element

    inherit stylable

    method offset : animatedNumber t readonly_prop
  end

(* interface SVGPatternElement *)
and patternElement =
  object
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
and clipPathElement =
  object
    inherit element

    inherit tests

    inherit langSpace

    inherit externalResourcesRequired

    inherit stylable

    inherit transformable
    (*   readonly attribute SVGAnimatedEnumeration clipPathUnits; *)
  end

(* interface SVGMaskElement *)
and maskElement =
  object
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
and filterElement =
  object
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

(*   void setStdDeviation(in number t stdDeviationX, in number t stdDeviationY) raises(DOMException); *)
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
and cursorElement =
  object
    inherit element

    inherit uriReference

    inherit tests

    inherit externalResourcesRequired

    method x : animatedLength t readonly_prop

    method y : animatedLength t readonly_prop
  end

(* interface SVGAElement *)
and aElement =
  object
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
and viewElement =
  object
    inherit element

    inherit externalResourcesRequired

    inherit fitToViewBox

    inherit zoomAndPan

    method viewTarget : stringList t readonly_prop
  end

(* interface SVGScriptElement *)
and scriptElement =
  object
    inherit element

    inherit uriReference

    inherit externalResourcesRequired

    method type_ : js_string t prop
  end

(* interface SVGZoomEvent : UIEvent *)
(*   readonly attribute SVGRect zoomRectScreen; *)
(*   readonly attribute number t previousScale; *)
(*   readonly attribute SVGPoint previousTranslate; *)
(*   readonly attribute number t newScale; *)
(*   readonly attribute SVGPoint newTranslate; *)
(* }; *)

(* interface SVGAnimationElement *)
and animationElement =
  object
    inherit element

    inherit tests

    inherit externalResourcesRequired

    (* inherit elementTimeControl *)
    method targetElement : element t readonly_prop

    method getStartTime : number t meth

    method getCurrentTime : number t meth

    method getSimpleDuration : number t meth
  end

(* interface SVGAnimateElement *)
and animateElement =
  object
    inherit animationElement

    inherit stylable
  end

(* interface SVGSetElement *)
and setElement = animationElement

(* interface SVGAnimateMotionElement *)
and animateMotionElement = animationElement

(* interface SVGMPathElement *)
and mPathElement =
  object
    inherit element

    inherit uriReference

    inherit externalResourcesRequired
  end

(* interface SVGAnimateColorElement *)
and animateColorElement =
  object
    inherit animationElement

    inherit stylable
  end

(* interface SVGAnimateTransformElement *)
and animateTransformElement = animationElement

(* interface SVGFontElement *)
and fontElement =
  object
    inherit element

    inherit stylable
  end

(* interface SVGGlyphElement *)
(* interface SVGMissingGlyphElement*)
and glyphElement =
  object
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
class type foreignObjectElement =
  object
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

let createElement (doc : document t) name = doc##createElementNS xmlns (Js.string name)

let unsafeCreateElement doc name = Js.Unsafe.coerce (createElement doc name)

let createA doc : aElement t = unsafeCreateElement doc "a"

let createAltGlyph doc : altGlyphElement t = unsafeCreateElement doc "altglyph"

let createAltGlyphDef doc : altGlyphDefElement t = unsafeCreateElement doc "altglyphdef"

let createAltGlyphItem doc : altGlyphItemElement t =
  unsafeCreateElement doc "altglyphitem"

let createAnimate doc : animateElement t = unsafeCreateElement doc "animate"

let createAnimateColor doc : animateColorElement t =
  unsafeCreateElement doc "animatecolor"

let createAnimateMotion doc : animateMotionElement t =
  unsafeCreateElement doc "animatemotion"

let createAnimateTransform doc : animateTransformElement t =
  unsafeCreateElement doc "animatetransform"

let createCircle doc : circleElement t = unsafeCreateElement doc "circle"

let createClipPath doc : clipPathElement t = unsafeCreateElement doc "clippath"

(* let createColorProfile doc : colorProfile t = unsafeCreateElement doc "color-profile" *)
let createCursor doc : cursorElement t = unsafeCreateElement doc "cursor"

let createDefs doc : defsElement t = unsafeCreateElement doc "defs"

let createDesc doc : descElement t = unsafeCreateElement doc "desc"

let createEllipse doc : ellipseElement t = unsafeCreateElement doc "ellipse"

(* let createFe* *)
let createFilter doc : filterElement t = unsafeCreateElement doc "filter"

let createFont doc : fontElement t = unsafeCreateElement doc "font"

let createFontFace doc : fontElement t = unsafeCreateElement doc "font-face"

let createFontFaceFormat doc : fontElement t = unsafeCreateElement doc "font-face-format"

let createFontFaceName doc : fontElement t = unsafeCreateElement doc "font-face-name"

let createFontFaceSrc doc : fontElement t = unsafeCreateElement doc "font-face-src"

let createFontFaceUri doc : fontElement t = unsafeCreateElement doc "font-face-uri"

let createForeignObject doc : foreignObjectElement t =
  unsafeCreateElement doc "foreignObject"

let createG doc : gElement t = unsafeCreateElement doc "g"

let createGlyph doc : glyphElement t = unsafeCreateElement doc "glyph"

let createGlyphRef doc : glyphElement t = unsafeCreateElement doc "glyphref"

let createhkern doc : element t = unsafeCreateElement doc "hkern"

let createImage doc : imageElement t = unsafeCreateElement doc "image"

let createLineElement doc : lineElement t = unsafeCreateElement doc "line"

let createLinearElement doc : linearGradientElement t =
  unsafeCreateElement doc "lineargradient"

(* let createMarker doc : markerElement *)
let createMask doc : maskElement t = unsafeCreateElement doc "mask"

let createMetaData doc : metadataElement t = unsafeCreateElement doc "metadata"

let createMissingGlyph doc : glyphElement t = unsafeCreateElement doc "missing-glyph"

let createMPath doc : mPathElement t = unsafeCreateElement doc "mpath"

let createPath doc : pathElement t = unsafeCreateElement doc "path"

let createPattern doc : patternElement t = unsafeCreateElement doc "pattern"

let createPolygon doc : polygonElement t = unsafeCreateElement doc "polygon"

let createPolyline doc : polyLineElement t = unsafeCreateElement doc "polyline"

let createRadialgradient doc : radialGradientElement t =
  unsafeCreateElement doc "radialgradient"

let createRect doc : rectElement t = unsafeCreateElement doc "rect"

let createScript doc : scriptElement t = unsafeCreateElement doc "script"

let createSet doc : setElement t = unsafeCreateElement doc "set"

let createStop doc : stopElement t = unsafeCreateElement doc "stop"

let createStyle doc : styleElement t = unsafeCreateElement doc "style"

let createSvg doc : svgElement t = unsafeCreateElement doc "svg"

let createSwitch doc : switchElement t = unsafeCreateElement doc "switch"

let createSymbol doc : symbolElement t = unsafeCreateElement doc "symbol"

let createTextElement doc : textElement t = unsafeCreateElement doc "text"

let createTextpath doc : textPathElement t = unsafeCreateElement doc "textpath"

let createTitle doc : titleElement t = unsafeCreateElement doc "title"

let createTref doc : trefElement t = unsafeCreateElement doc "tref"

let createTspan doc : tspanElement t = unsafeCreateElement doc "tspan"

let createUse doc : useElement t = unsafeCreateElement doc "use"

let createView doc : viewElement t = unsafeCreateElement doc "view"

let createvkern doc : element t = unsafeCreateElement doc "vkern"

(****)

let svg_element : element t constr = Js.Unsafe.global##._SVGElement

let document = Js.Unsafe.global##.document

let getElementById id : element t =
  Js.Opt.case
    (Js.Unsafe.global##.document##getElementById (Js.string id))
    (fun () -> raise Not_found)
    (fun e -> if Js.instanceof e svg_element then e else raise Not_found)

module CoerceTo = struct
  let element (e : #Dom.node Js.t) : element Js.t Js.opt =
    if Js.instanceof e svg_element then Js.some (Js.Unsafe.coerce e) else Js.null

  let unsafeCoerce (e : #element t) tag =
    if Js.equals e##.tagName##toLowerCase (Js.string tag)
    then Js.some (Js.Unsafe.coerce e)
    else Js.null

  let a e : aElement t opt = unsafeCoerce e "a"

  let altGlyph e : altGlyphElement t opt = unsafeCoerce e "altglyph"

  let altGlyphDef e : altGlyphDefElement t opt = unsafeCoerce e "altglyphdef"

  let altGlyphItem e : altGlyphItemElement t opt = unsafeCoerce e "altglyphitem"

  let animate e : animateElement t opt = unsafeCoerce e "animate"

  let animateColor e : animateColorElement t opt = unsafeCoerce e "animatecolor"

  let animateMotion e : animateMotionElement t opt = unsafeCoerce e "animatemotion"

  let animateTransform e : animateTransformElement t opt =
    unsafeCoerce e "animatetransform"

  let circle e : circleElement t opt = unsafeCoerce e "circle"

  let clipPath e : clipPathElement t opt = unsafeCoerce e "clippath"

  (* let ColorProfile e : colorProfile t opt = unsafeCoerce e "color-profile" *)
  let cursor e : cursorElement t opt = unsafeCoerce e "cursor"

  let defs e : defsElement t opt = unsafeCoerce e "defs"

  let desc e : descElement t opt = unsafeCoerce e "desc"

  let ellipse e : ellipseElement t opt = unsafeCoerce e "ellipse"

  (* let Fe* *)
  let filter e : filterElement t opt = unsafeCoerce e "filter"

  let font e : fontElement t opt = unsafeCoerce e "font"

  let fontFace e : fontElement t opt = unsafeCoerce e "font-face"

  let fontFaceFormat e : fontElement t opt = unsafeCoerce e "font-face-format"

  let fontFaceName e : fontElement t opt = unsafeCoerce e "font-face-name"

  let fontFaceSrc e : fontElement t opt = unsafeCoerce e "font-face-src"

  let fontFaceUri e : fontElement t opt = unsafeCoerce e "font-face-uri"

  let foreignObject e : foreignObjectElement t opt = unsafeCoerce e "foreignobject"

  let g e : gElement t opt = unsafeCoerce e "g"

  let glyph e : glyphElement t opt = unsafeCoerce e "glyph"

  let glyphRef e : glyphElement t opt = unsafeCoerce e "glyphref"

  let hkern e : element t opt = unsafeCoerce e "hkern"

  let image e : imageElement t opt = unsafeCoerce e "image"

  let lineElement e : lineElement t opt = unsafeCoerce e "line"

  let linearElement e : linearGradientElement t opt = unsafeCoerce e "lineargradient"

  (* let Marker e : markerElement *)
  let mask e : maskElement t opt = unsafeCoerce e "mask"

  let metaData e : metadataElement t opt = unsafeCoerce e "metadata"

  let missingGlyph e : glyphElement t opt = unsafeCoerce e "missing-glyph"

  let mPath e : mPathElement t opt = unsafeCoerce e "mpath"

  let path e : pathElement t opt = unsafeCoerce e "path"

  let pattern e : patternElement t opt = unsafeCoerce e "pattern"

  let polygon e : polygonElement t opt = unsafeCoerce e "polygon"

  let polyline e : polyLineElement t opt = unsafeCoerce e "polyline"

  let radialgradient e : radialGradientElement t opt = unsafeCoerce e "radialgradient"

  let rect e : rectElement t opt = unsafeCoerce e "rect"

  let script e : scriptElement t opt = unsafeCoerce e "script"

  let set e : setElement t opt = unsafeCoerce e "set"

  let stop e : stopElement t opt = unsafeCoerce e "stop"

  let style e : styleElement t opt = unsafeCoerce e "style"

  let svg e : svgElement t opt = unsafeCoerce e "svg"

  let switch e : switchElement t opt = unsafeCoerce e "switch"

  let symbol e : symbolElement t opt = unsafeCoerce e "symbol"

  let textElement e : textElement t opt = unsafeCoerce e "text"

  let textpath e : textPathElement t opt = unsafeCoerce e "textpath"

  let title e : titleElement t opt = unsafeCoerce e "title"

  let tref e : trefElement t opt = unsafeCoerce e "tref"

  let tspan e : tspanElement t opt = unsafeCoerce e "tspan"

  let use e : useElement t opt = unsafeCoerce e "use"

  let view e : viewElement t opt = unsafeCoerce e "view"

  let vkern e : element t opt = unsafeCoerce e "vkern"
end
