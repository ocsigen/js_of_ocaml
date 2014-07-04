open Js

let xmlns = Js.string "http://www.w3.org/2000/svg"

(* module svg { *)

(* exception SVGException { *)
(*   unsigned short code; *)
(* }; *)

type svg_error =
  | WRONG_TYPE_ERR
  | INVALID_VALUE_ERR
  | MATRIX_NOT_INVERTABLE

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
  method appendItem :  'a -> 'a meth
end

(****)

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
and animatedNumber = [float] animated

(* interface SVGNumberList *)
and numberList = [number t] list

(* interface SVGAnimatedNumberList *)
and animatedNumberList = [numberList t] animated

(* interface SVGLength *)
and length = object
  method unitType : lengthUnitType readonly_prop
  method value : float prop
  method valueInSpecifiedUnits : float prop
  method valueAsString : js_string t prop
  method newValueSpecifiedUnits : lengthUnitType -> float -> unit meth
  method convertToSpecifiedUnits : lengthUnitType -> unit meth
end

(* interface SVGAnimatedLength *)
and animatedLength =  [length] animated

(* interface SVGLengthList *)
and lengthList = [length t] list

(* interface SVGAnimatedLengthList *)
and animatedLengthList = [lengthList t] animated

(* interface SVGAngle *)
and angle = object
  method unitType : angleUnitType readonly_prop
  method value : float prop
  method valueInSpecifiedUnits : float prop
  method valueAsString : js_string t prop
  method newValueSpecifiedUnits : angleUnitType -> float -> unit meth
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
  method x : float prop
  method y : float prop
  method width : float prop
  method height : float prop
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
  method requiredFeatures : stringList readonly_prop
  method requiredExtensions : stringList readonly_prop
  method systemLanguage : stringList readonly_prop
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
  method rootElement : svgElement t readonly_prop
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
  method pixelUnitToMillimeterX : float readonly_prop
  method pixelUnitToMillimeterY : float readonly_prop
  method screenPixelUnitToMillimeterX : float readonly_prop
  method screenPixelUnitToMillimeterY : float readonly_prop
  method useCurrentView : bool t readonly_prop
  method currentView : viewSpec t readonly_prop
  method currentScale : float prop
  method currentTranslate : point t readonly_prop
  method suspendRedraw : int -> suspendHandleID meth
  method unsuspendRedraw : suspendHandleID -> unit meth
  method unsuspendRedrawAll : unit meth
  method forceRedraw : unit meth
  method pauseAnimations : unit meth
  method unpauseAnimations : unit meth
  method animationsPaused : bool t meth
  method getCurrentTime : float meth
  method setCurrentTime : int -> unit meth
  method getIntersectionList :
    rect t -> element t -> element t Dom.nodeList t meth
  method getEnclosureList :
    rect t -> element t -> element t Dom.nodeList t meth
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
  method x : float readonly_prop
  method y : float readonly_prop
  method matrixTransform : matrix t -> point t meth
end

(* interface SVGPointList *)
and pointList = [point t] list

(* interface SVGMatrix *)
and matrix = object
  method a : float readonly_prop
  method b : float readonly_prop
  method c : float readonly_prop
  method d : float readonly_prop
  method e : float readonly_prop
  method f : float readonly_prop

  method multiply : matrix t -> matrix t meth
  method inverse : matrix t meth
  method translate : float -> float -> matrix t meth
  method scale : float -> matrix t meth
  method scaleNonUniform : float -> float -> matrix t meth
  method rotate : float -> matrix t meth
  method rotateFromVector : float -> float -> matrix t meth
  method flipX : matrix t meth
  method flipY : matrix t meth
  method skewX : float -> matrix t meth
  method skewY : float -> matrix t meth
end

(* interface SVGTransform *)
and transform = object
  method _type : transformType readonly_prop
  method matrix : matrix t readonly_prop
  method angle : float readonly_prop
  method setMatrix : matrix t -> unit meth
  method setTranslate : float -> float -> unit meth
  method setScale : float -> float -> unit meth
  method setRotate : float -> float -> float -> unit meth
  method setSkewX : float -> unit meth
  method setSkewY : float -> unit meth
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

(* interface SVGPathSegMovetoAbs *) (* interface SVGPathSegMovetoRel *)
and pathSegMoveto = object
  inherit pathSeg
  method x : float prop
  method y : float prop
end

(* interface SVGPathSegLinetoAbs *) (* interface SVGPathSegLinetoRel *)
and pathSegLineto = object
  inherit pathSeg
  method x : float prop
  method y : float prop
end


(* interface SVGPathSegCurvetoCubicAbs *) (* interface SVGPathSegCurvetoCubicRel *)
and pathSegCurvetoCubic = object
  inherit pathSeg
  method x : float prop
  method y : float prop
  method x1 : float prop
  method y1 : float prop
  method x2 : float prop
  method y2 : float prop
end

(* interface SVGPathSegCurvetoQuadraticAbs *) (* interface SVGPathSegCurvetoQuadraticRel *)
and pathSegCurvetoQuadratic = object
  inherit pathSeg
  method x : float prop
  method y : float prop
  method x1 : float prop
  method y1 : float prop
end

(* interface SVGPathSegArcAbs *) (* interface SVGPathSegArcRel*)
and pathSegArc = object
  inherit pathSeg
  method y : float prop
  method r1 : float prop
  method r2 : float prop
  method angle : float prop
  method largeArcFlag : bool t prop
  method sweepFlag : bool t prop
end

(* interface SVGPathSegLinetoHorizontalAbs *) (* interface SVGPathSegLinetoHorizontalRel *)
and pathSegLinetoHorizontal = object
  inherit pathSeg
  method x : float
end

(* interface SVGPathSegLinetoVerticalAbs *) (* interface SVGPathSegLinetoVerticalRel *)
and pathSegLinetoVertical = object
  inherit pathSeg
  method y : float
end

and pathSegCurvetoCubicSmooth = object
  inherit pathSeg
  method x : float
  method y : float
  method x2 : float
  method y2 : float
end

(* interface SVGPathSegCurvetoQuadraticSmoothAbs *) (* interface SVGPathSegCurvetoQuadraticSmoothRel  *)
and pathSegCurvetoQuadraticSmooth = object
  inherit pathSeg
  method x : float
  method y : float
end

and pathSegList = [pathSeg t] list

(* interface SVGAnimatedPathData *)
and animatedPathData = object
  method pathSegList : pathSegList t prop
  method normalizedPathSegList : pathSegList t prop
  method animatedPathSegList : pathSegList t prop
  method animatedNormalizedPathSegList : pathSegList t prop
end

(* interface SVGPathElement : SVGElement, *)
(*                            SVGTests, *)
(*                            SVGLangSpace, *)
(*                            SVGExternalResourcesRequired, *)
(*                            SVGStylable, *)
(*                            SVGTransformable, *)
(*                            SVGAnimatedPathData { *)

(*   readonly attribute SVGAnimatedNumber pathLength; *)

(*   float getTotalLength(); *)
(*   SVGPoint getPointAtLength(in float distance); *)
(*   unsigned long getPathSegAtLength(in float distance); *)
(*   SVGPathSegClosePath createSVGPathSegClosePath(); *)
(*   SVGPathSegMovetoAbs createSVGPathSegMovetoAbs(in float x, in float y); *)
(*   SVGPathSegMovetoRel createSVGPathSegMovetoRel(in float x, in float y); *)
(*   SVGPathSegLinetoAbs createSVGPathSegLinetoAbs(in float x, in float y); *)
(*   SVGPathSegLinetoRel createSVGPathSegLinetoRel(in float x, in float y); *)
(*   SVGPathSegCurvetoCubicAbs createSVGPathSegCurvetoCubicAbs(in float x, in float y, in float x1, in float y1, in float x2, in float y2); *)
(*   SVGPathSegCurvetoCubicRel createSVGPathSegCurvetoCubicRel(in float x, in float y, in float x1, in float y1, in float x2, in float y2); *)
(*   SVGPathSegCurvetoQuadraticAbs createSVGPathSegCurvetoQuadraticAbs(in float x, in float y, in float x1, in float y1); *)
(*   SVGPathSegCurvetoQuadraticRel createSVGPathSegCurvetoQuadraticRel(in float x, in float y, in float x1, in float y1); *)
(*   SVGPathSegArcAbs createSVGPathSegArcAbs(in float x, in float y, in float r1, in float r2, in float angle, in boolean largeArcFlag, in boolean sweepFlag); *)
(*   SVGPathSegArcRel createSVGPathSegArcRel(in float x, in float y, in float r1, in float r2, in float angle, in boolean largeArcFlag, in boolean sweepFlag); *)
(*   SVGPathSegLinetoHorizontalAbs createSVGPathSegLinetoHorizontalAbs(in float x); *)
(*   SVGPathSegLinetoHorizontalRel createSVGPathSegLinetoHorizontalRel(in float x); *)
(*   SVGPathSegLinetoVerticalAbs createSVGPathSegLinetoVerticalAbs(in float y); *)
(*   SVGPathSegLinetoVerticalRel createSVGPathSegLinetoVerticalRel(in float y); *)
(*   SVGPathSegCurvetoCubicSmoothAbs createSVGPathSegCurvetoCubicSmoothAbs(in float x, in float y, in float x2, in float y2); *)
(*   SVGPathSegCurvetoCubicSmoothRel createSVGPathSegCurvetoCubicSmoothRel(in float x, in float y, in float x2, in float y2); *)
(*   SVGPathSegCurvetoQuadraticSmoothAbs createSVGPathSegCurvetoQuadraticSmoothAbs(in float x, in float y); *)
(*   SVGPathSegCurvetoQuadraticSmoothRel createSVGPathSegCurvetoQuadraticSmoothRel(in float x, in float y); *)
(* }; *)


(* interface SVGRectElement *)
and reactElement = object
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
  method getComputedTextLength : float meth
  method getSubStringLength : int -> int -> float meth
  method getStartPositionOfChar : int -> point t meth
  method getEndPositionOfChar : int -> point t meth
  method getExtentOfChar : int -> rect t meth
  method getRotationOfChar : int -> float meth
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

and spanElement = textPositioningElement

and refElement = object
  inherit textPositioningElement
  inherit uriReference
end

(* interface SVGTextPathElement : SVGTextContentElement, *)
(*                                SVGURIReference { *)

(*   // textPath Method Types *)
(*   const unsigned short TEXTPATH_METHODTYPE_UNKNOWN = 0; *)
(*   const unsigned short TEXTPATH_METHODTYPE_ALIGN = 1; *)
(*   const unsigned short TEXTPATH_METHODTYPE_STRETCH = 2; *)

(*   // textPath Spacing Types *)
(*   const unsigned short TEXTPATH_SPACINGTYPE_UNKNOWN = 0; *)
(*   const unsigned short TEXTPATH_SPACINGTYPE_AUTO = 1; *)
(*   const unsigned short TEXTPATH_SPACINGTYPE_EXACT = 2; *)

(*   readonly attribute SVGAnimatedLength startOffset; *)
(*   readonly attribute SVGAnimatedEnumeration method; *)
(*   readonly attribute SVGAnimatedEnumeration spacing; *)
(* }; *)

(* interface SVGAltGlyphElement : SVGTextPositioningElement, *)
(*                                SVGURIReference { *)
(*   attribute DOMString glyphRef setraises(DOMException); *)
(*   attribute DOMString format setraises(DOMException); *)
(* }; *)

(* interface SVGAltGlyphDefElement : SVGElement { *)
(* }; *)

(* interface SVGAltGlyphItemElement : SVGElement { *)
(* }; *)

(* interface SVGGlyphRefElement : SVGElement, *)
(*                                SVGURIReference, *)
(*                                SVGStylable { *)
(*   attribute DOMString glyphRef setraises(DOMException); *)
(*   attribute DOMString format setraises(DOMException); *)
(*   attribute float x setraises(DOMException); *)
(*   attribute float y setraises(DOMException); *)
(*   attribute float dx setraises(DOMException); *)
(*   attribute float dy setraises(DOMException); *)
(* }; *)

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

(* interface SVGGradientElement : SVGElement, *)
(*                                SVGURIReference, *)
(*                                SVGExternalResourcesRequired, *)
(*                                SVGStylable, *)
(*                                SVGUnitTypes { *)

(*   // Spread Method Types *)
(*   const unsigned short SVG_SPREADMETHOD_UNKNOWN = 0; *)
(*   const unsigned short SVG_SPREADMETHOD_PAD = 1; *)
(*   const unsigned short SVG_SPREADMETHOD_REFLECT = 2; *)
(*   const unsigned short SVG_SPREADMETHOD_REPEAT = 3; *)

(*   readonly attribute SVGAnimatedEnumeration gradientUnits; *)
(*   readonly attribute SVGAnimatedTransformList gradientTransform; *)
(*   readonly attribute SVGAnimatedEnumeration spreadMethod; *)
(* }; *)

(* interface SVGLinearGradientElement : SVGGradientElement { *)
(*   readonly attribute SVGAnimatedLength x1; *)
(*   readonly attribute SVGAnimatedLength y1; *)
(*   readonly attribute SVGAnimatedLength x2; *)
(*   readonly attribute SVGAnimatedLength y2; *)
(* }; *)

(* interface SVGRadialGradientElement : SVGGradientElement { *)
(*   readonly attribute SVGAnimatedLength cx; *)
(*   readonly attribute SVGAnimatedLength cy; *)
(*   readonly attribute SVGAnimatedLength r; *)
(*   readonly attribute SVGAnimatedLength fx; *)
(*   readonly attribute SVGAnimatedLength fy; *)
(* }; *)

(* interface SVGStopElement : SVGElement, *)
(*                            SVGStylable { *)
(*   readonly attribute SVGAnimatedNumber offset; *)
(* }; *)

(* interface SVGPatternElement : SVGElement, *)
(*                               SVGURIReference, *)
(*                               SVGTests, *)
(*                               SVGLangSpace, *)
(*                               SVGExternalResourcesRequired, *)
(*                               SVGStylable, *)
(*                               SVGFitToViewBox, *)
(*                               SVGUnitTypes { *)
(*   readonly attribute SVGAnimatedEnumeration patternUnits; *)
(*   readonly attribute SVGAnimatedEnumeration patternContentUnits; *)
(*   readonly attribute SVGAnimatedTransformList patternTransform; *)
(*   readonly attribute SVGAnimatedLength x; *)
(*   readonly attribute SVGAnimatedLength y; *)
(*   readonly attribute SVGAnimatedLength width; *)
(*   readonly attribute SVGAnimatedLength height; *)
(* }; *)

(* interface SVGClipPathElement : SVGElement, *)
(*                                SVGTests, *)
(*                                SVGLangSpace, *)
(*                                SVGExternalResourcesRequired, *)
(*                                SVGStylable, *)
(*                                SVGTransformable, *)
(*                                SVGUnitTypes { *)
(*   readonly attribute SVGAnimatedEnumeration clipPathUnits; *)
(* }; *)

(* interface SVGMaskElement : SVGElement, *)
(*                            SVGTests, *)
(*                            SVGLangSpace, *)
(*                            SVGExternalResourcesRequired, *)
(*                            SVGStylable, *)
(*                            SVGUnitTypes { *)
(*   readonly attribute SVGAnimatedEnumeration maskUnits; *)
(*   readonly attribute SVGAnimatedEnumeration maskContentUnits; *)
(*   readonly attribute SVGAnimatedLength x; *)
(*   readonly attribute SVGAnimatedLength y; *)
(*   readonly attribute SVGAnimatedLength width; *)
(*   readonly attribute SVGAnimatedLength height; *)
(* }; *)

(* interface SVGFilterElement : SVGElement, *)
(*                              SVGURIReference, *)
(*                              SVGLangSpace, *)
(*                              SVGExternalResourcesRequired, *)
(*                              SVGStylable, *)
(*                              SVGUnitTypes { *)

(*   readonly attribute SVGAnimatedEnumeration filterUnits; *)
(*   readonly attribute SVGAnimatedEnumeration primitiveUnits; *)
(*   readonly attribute SVGAnimatedLength x; *)
(*   readonly attribute SVGAnimatedLength y; *)
(*   readonly attribute SVGAnimatedLength width; *)
(*   readonly attribute SVGAnimatedLength height; *)
(*   readonly attribute SVGAnimatedInteger filterResX; *)
(*   readonly attribute SVGAnimatedInteger filterResY; *)

(*   void setFilterRes(in unsigned long filterResX, in unsigned long filterResY) raises(DOMException); *)
(* }; *)

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

(* interface SVGCursorElement : SVGElement, *)
(*                              SVGURIReference, *)
(*                              SVGTests, *)
(*                              SVGExternalResourcesRequired { *)
(*   readonly attribute SVGAnimatedLength x; *)
(*   readonly attribute SVGAnimatedLength y; *)
(* }; *)

(* interface SVGAElement : SVGElement, *)
(*                         SVGURIReference, *)
(*                         SVGTests, *)
(*                         SVGLangSpace, *)
(*                         SVGExternalResourcesRequired, *)
(*                         SVGStylable, *)
(*                         SVGTransformable { *)
(*   readonly attribute SVGAnimatedString target; *)
(* }; *)

(* interface SVGViewElement : SVGElement, *)
(*                            SVGExternalResourcesRequired, *)
(*                            SVGFitToViewBox, *)
(*                            SVGZoomAndPan { *)
(*   readonly attribute SVGStringList viewTarget; *)
(* }; *)

(* interface SVGScriptElement : SVGElement, *)
(*                              SVGURIReference, *)
(*                              SVGExternalResourcesRequired { *)
(*   attribute DOMString type setraises(DOMException); *)
(* }; *)

(* interface SVGZoomEvent : UIEvent { *)
(*   readonly attribute SVGRect zoomRectScreen; *)
(*   readonly attribute float previousScale; *)
(*   readonly attribute SVGPoint previousTranslate; *)
(*   readonly attribute float newScale; *)
(*   readonly attribute SVGPoint newTranslate; *)
(* }; *)

(* interface SVGAnimationElement : SVGElement, *)
(*                                 SVGTests, *)
(*                                 SVGExternalResourcesRequired, *)
(*                                 ElementTimeControl { *)

(*   readonly attribute SVGElement targetElement; *)

(*   float getStartTime() raises(DOMException); *)
(*   float getCurrentTime(); *)
(*   float getSimpleDuration() raises(DOMException); *)
(* }; *)

(* interface SVGAnimateElement : SVGAnimationElement, *)
(*                               SVGStylable { *)
(* }; *)

(* interface SVGSetElement : SVGAnimationElement { *)
(* }; *)

(* interface SVGAnimateMotionElement : SVGAnimationElement { *)
(* }; *)

(* interface SVGMPathElement : SVGElement, *)
(*                             SVGURIReference, *)
(*                             SVGExternalResourcesRequired { *)

(* interface SVGAnimateColorElement : SVGAnimationElement, *)
(*                                    SVGStylable { *)

(* interface SVGAnimateTransformElement : SVGAnimationElement { *)

(* interface SVGFontElement : SVGElement, *)
(*                            SVGExternalResourcesRequired, *)
(*                            SVGStylable { *)

(* interface SVGGlyphElement : SVGElement, *)
(*                             SVGStylable { *)

(* interface SVGMissingGlyphElement : SVGElement, *)
(*                                    SVGStylable { *)

(* interface SVGHKernElement : SVGElement { *)
(* interface SVGVKernElement : SVGElement { *)

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

(* interface SVGForeignObjectElement : SVGElement, *)
(*                                     SVGTests, *)
(*                                     SVGLangSpace, *)
(*                                     SVGExternalResourcesRequired, *)
(*                                     SVGStylable, *)
(*                                     SVGTransformable { *)
(*   readonly attribute SVGAnimatedLength x; *)
(*   readonly attribute SVGAnimatedLength y; *)
(*   readonly attribute SVGAnimatedLength width; *)
(*   readonly attribute SVGAnimatedLength height; *)
(* }; *)

(* }; *)

let createElement (doc : document t) name =
  doc##createElementNS(xmlns, Js.string name)
let unsafeCreateElement doc name = Js.Unsafe.coerce (createElement doc name)

let createSvg doc : svgElement t = unsafeCreateElement doc "svg"
let createG doc : gElement t = unsafeCreateElement doc "g"
let createTextElement doc : textElement t = unsafeCreateElement doc "text"
let createLineElement doc : lineElement t = unsafeCreateElement doc "line"

(****)

let svg_element : element t constr = Js.Unsafe.global ## _SVGElement

let getElementById id : element t =
  Js.Opt.case (Js.Unsafe.global##document##getElementById (Js.string id))
    (fun () -> raise Not_found)
    (fun e -> if Js.instanceof e svg_element then e else raise Not_found)

module CoerceTo = struct
  let element (e : #Dom.node Js.t) : element Js.t Js.opt =
    if Js.instanceof e svg_element then
      Js.some (Js.Unsafe.coerce e)
    else
      Js.null

  let unsafeCoerce tag (e : #element t) =
    if e##tagName##toLowerCase() == Js.string tag then
      Js.some (Js.Unsafe.coerce e)
    else
      Js.null

  let svg e : svgElement t opt = unsafeCoerce "svg" e
  let g e : gElement t opt = unsafeCoerce "g" e
  let text e : textElement t opt = unsafeCoerce "text" e
  let line e : lineElement t opt = unsafeCoerce "line" e
end
