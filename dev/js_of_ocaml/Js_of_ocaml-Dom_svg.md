
# Module `Js_of_ocaml.Dom_svg`

DOM SVG binding

This is a partial binding to the DOM SVG API.

```ocaml
val xmlns : Js.js_string Js.t
```
Types

```ocaml
type error_code = 
  | WRONG_TYPE_ERR
  | INVALID_VALUE_ERR
  | MATRIX_NOT_INVERTABLE
```
deprecated Removed in SVG 2.
```ocaml
class type  svg_error = object ... end
```
```ocaml
exception SVGError of svg_error
```
deprecated Removed in SVG 2.
```ocaml
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
```
```ocaml
type angleUnitType = 
  | ANGLETYPE_UNKNOWN
  | ANGLETYPE_UNSPECIFIED
  | ANGLETYPE_DEG
  | ANGLETYPE_RAD
  | ANGLETYPE_GRAD
```
```ocaml
type colorType = 
  | COLORTYPE_UNKNOWN
  | COLORTYPE_RGBCOLOR
  | COLORTYPE_RGBCOLOR_ICCCOLOR
  | COLORTYPE_CURRENTCOLOR
```
```ocaml
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
```
```ocaml
type meetOrSliceType = 
  | MEETORSLICE_UNKNOWN
  | MEETORSLICE_MEET
  | MEETORSLICE_SLICE
```
```ocaml
type transformType = 
  | TRANSFORM_UNKNOWN
  | TRANSFORM_MATRIX
  | TRANSFORM_TRANSLATE
  | TRANSFORM_SCALE
  | TRANSFORM_ROTATE
  | TRANSFORM_SKEWX
  | TRANSFORM_SKEWY
```
```ocaml
type zoomAndPanType = 
  | ZOOMANDPAN_UNKNOWN
  | ZOOMANDPAN_DISABLE
  | ZOOMANDPAN_MAGNIFY
```
```ocaml
type lengthAdjust = 
  | LENGTHADJUST_UNKNOWN
  | LENGTHADJUST_SPACING
  | LENGTHADJUST_SPACINGANDGLYPHS
```
```ocaml
type unitType = 
  | UNIT_TYPE_UNKNOWN
  | UNIT_TYPE_USERSPACEONUSE
  | UNIT_TYPE_OBJECTBOUNDINGBOX
```
```ocaml
type intentType = 
  | RENDERING_INTENT_UNKNOWN
  | RENDERING_INTENT_AUTO
  | RENDERING_INTENT_PERCEPTUAL
  | RENDERING_INTENT_RELATIVE_COLORIMETRIC
  | RENDERING_INTENT_SATURATION
  | RENDERING_INTENT_ABSOLUTE_COLORIMETRIC
```
```ocaml
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
```
```ocaml
type textPathMethodType = 
  | TEXTPATH_METHODTYPE_UNKNOWN
  | TEXTPATH_METHODTYPE_ALIGN
  | TEXTPATH_METHODTYPE_STRETCH
```
```ocaml
type textPathSpacingType = 
  | TEXTPATH_SPACINGTYPE_UNKNOWN
  | TEXTPATH_SPACINGTYPE_AUTO
  | TEXTPATH_SPACINGTYPE_EXACT
```
```ocaml
type spreadMethodType = 
  | SPREADMETHOD_UNKNOWN
  | SPREADMETHOD_PAD
  | SPREADMETHOD_REFLECT
  | SPREADMETHOD_REPEAT
```
```ocaml
type markerUnitType = 
  | SVG_MARKERUNITS_UNKNOWN
  | SVG_MARKERUNITS_USERSPACEONUSE
  | SVG_MARKERUNITS_STROKEWIDTH
```
```ocaml
type markerOrientType = 
  | SVG_MARKER_ORIENT_UNKNOWN
  | SVG_MARKER_ORIENT_AUTO
  | SVG_MARKER_ORIENT_ANGLE
```
```ocaml
type blendModeType = 
  | SVG_FEBLEND_MODE_UNKNOWN
  | SVG_FEBLEND_MODE_NORMAL
  | SVG_FEBLEND_MODE_MULTIPLY
  | SVG_FEBLEND_MODE_SCREEN
  | SVG_FEBLEND_MODE_DARKEN
  | SVG_FEBLEND_MODE_LIGHTEN
```
```ocaml
type colorMatrixType = 
  | SVG_FECOLORMATRIX_TYPE_UNKNOWN
  | SVG_FECOLORMATRIX_TYPE_MATRIX
  | SVG_FECOLORMATRIX_TYPE_SATURATE
  | SVG_FECOLORMATRIX_TYPE_HUEROTATE
  | SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA
```
```ocaml
type componentTransferType = 
  | SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN
  | SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY
  | SVG_FECOMPONENTTRANSFER_TYPE_TABLE
  | SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE
  | SVG_FECOMPONENTTRANSFER_TYPE_LINEAR
  | SVG_FECOMPONENTTRANSFER_TYPE_GAMMA
```
```ocaml
type compositeOperatorType = 
  | SVG_FECOMPOSITE_OPERATOR_UNKNOWN
  | SVG_FECOMPOSITE_OPERATOR_OVER
  | SVG_FECOMPOSITE_OPERATOR_IN
  | SVG_FECOMPOSITE_OPERATOR_OUT
  | SVG_FECOMPOSITE_OPERATOR_ATOP
  | SVG_FECOMPOSITE_OPERATOR_XOR
  | SVG_FECOMPOSITE_OPERATOR_ARITHMETIC
```
```ocaml
type edgeModeType = 
  | SVG_EDGEMODE_UNKNOWN
  | SVG_EDGEMODE_DUPLICATE
  | SVG_EDGEMODE_WRAP
  | SVG_EDGEMODE_NONE
```
```ocaml
type channelSelectorType = 
  | SVG_CHANNEL_UNKNOWN
  | SVG_CHANNEL_R
  | SVG_CHANNEL_G
  | SVG_CHANNEL_B
  | SVG_CHANNEL_A
```
```ocaml
type morphologyOperatorType = 
  | SVG_MORPHOLOGY_OPERATOR_UNKNOWN
  | SVG_MORPHOLOGY_OPERATOR_ERODE
  | SVG_MORPHOLOGY_OPERATOR_DILATE
```
```ocaml
type turbulenceType = 
  | SVG_TURBULENCE_TYPE_UNKNOWN
  | SVG_TURBULENCE_TYPE_FRACTALNOISE
  | SVG_TURBULENCE_TYPE_TURBULENCE
```
```ocaml
type stitchType = 
  | SVG_STITCHTYPE_UNKNOWN
  | SVG_STITCHTYPE_STITCH
  | SVG_STITCHTYPE_NOSTITCH
```
```ocaml
type suspendHandleID
```
```ocaml
class type 'a animated = object ... end
```
```ocaml
class type 'a list = object ... end
```

### Elements

```ocaml
class type  element = object ... end
```
```ocaml
class type  animatedString = Js.js_string Js.t animated
```
```ocaml
class type  animatedBoolean = bool Js.t animated
```
```ocaml
class type  stringList = Js.js_string Js.t list
```
```ocaml
class type  animatedEnumeration = int animated
```
```ocaml
class type  animatedUnitType = unitType animated
```
```ocaml
class type  animatedInteger = int animated
```
```ocaml
class type  animatedNumber = Js.number_t animated
```
```ocaml
class type  numberList = Js.number_t list
```
```ocaml
class type  animatedNumberList = numberList Js.t animated
```
```ocaml
class type  length = object ... end
```
```ocaml
class type  animatedLength = length Js.t animated
```
```ocaml
class type  lengthList = length Js.t list
```
```ocaml
class type  animatedLengthList = lengthList Js.t animated
```
```ocaml
class type  angle = object ... end
```
```ocaml
class type  animatedAngle = angle Js.t animated
```
```ocaml
class type  rgbColor = object ... end
```
```ocaml
class type  color = object ... end
```
```ocaml
class type  iccColor = object ... end
```
```ocaml
class type  rect = object ... end
```
```ocaml
class type  animatedRect = rect Js.t animated
```
```ocaml
class type  stylable = object ... end
```
```ocaml
class type  locatable = object ... end
```
```ocaml
class type  transformable = object ... end
```
```ocaml
class type  tests = object ... end
```
```ocaml
class type  langSpace = object ... end
```
```ocaml
class type  externalResourcesRequired = object ... end
```
```ocaml
class type  graphicsElement = object ... end
```
```ocaml
class type  geometryElement = object ... end
```
```ocaml
class type  unknownElement = graphicsElement
```
```ocaml
class type  fitToViewBox = object ... end
```
```ocaml
class type  zoomAndPan = object ... end
```
```ocaml
class type  viewSpec = object ... end
```
```ocaml
class type  uriReference = object ... end
```
```ocaml
class type  document = object ... end
```
```ocaml
class type  svgElement = object ... end
```
```ocaml
class type  gElement = object ... end
```
```ocaml
class type  defsElement = object ... end
```
```ocaml
class type  descElement = object ... end
```
```ocaml
class type  titleElement = object ... end
```
```ocaml
class type  symbolElement = object ... end
```
```ocaml
class type  useElement = object ... end
```
```ocaml
class type  elementInstance = object ... end
```
```ocaml
class type  elementInstanceList = object ... end
```
```ocaml
class type  imageElement = object ... end
```
```ocaml
class type  switchElement = object ... end
```
```ocaml
class type  styleElement = object ... end
```
```ocaml
class type  cssStyleSheet = object ... end
```
```ocaml
class type  point = object ... end
```
```ocaml
class type  pointList = point Js.t list
```
```ocaml
class type  matrix = object ... end
```
```ocaml
class type  transform = object ... end
```
```ocaml
class type  transformList = object ... end
```
```ocaml
class type  animatedTransformList = transformList Js.t animated
```
```ocaml
class type  preserveAspectRatio = object ... end
```
```ocaml
class type  animatedPreserveAspectRatio = preserveAspectRatio Js.t animated
```
```ocaml
class type  pathSeg = object ... end
```
```ocaml
class type  pathSegClosePath = pathSeg
```
```ocaml
class type  pathSegMoveto = object ... end
```
```ocaml
class type  pathSegLineto = object ... end
```
```ocaml
class type  pathSegCurvetoCubic = object ... end
```
```ocaml
class type  pathSegCurvetoQuadratic = object ... end
```
```ocaml
class type  pathSegArc = object ... end
```
```ocaml
class type  pathSegLinetoHorizontal = object ... end
```
```ocaml
class type  pathSegLinetoVertical = object ... end
```
```ocaml
class type  pathSegCurvetoCubicSmooth = object ... end
```
```ocaml
class type  pathSegCurvetoQuadraticSmooth = object ... end
```
```ocaml
class type  pathSegList = pathSeg Js.t list
```
```ocaml
class type  animatedPathData = object ... end
```
```ocaml
class type  pathElement = object ... end
```
```ocaml
class type  rectElement = object ... end
```
```ocaml
class type  circleElement = object ... end
```
```ocaml
class type  ellipseElement = object ... end
```
```ocaml
class type  lineElement = object ... end
```
```ocaml
class type  animatedPoints = object ... end
```
```ocaml
class type  polyLineElement = object ... end
```
```ocaml
class type  polygonElement = object ... end
```
```ocaml
class type  textContentElement = object ... end
```
```ocaml
class type  textPositioningElement = object ... end
```
```ocaml
class type  textElement = object ... end
```
```ocaml
class type  tspanElement = textPositioningElement
```
```ocaml
class type  trefElement = object ... end
```
```ocaml
class type  textPathElementMethod = textPathMethodType animated
```
```ocaml
class type  textPathElementSpacing = textPathSpacingType animated
```
```ocaml
class type  textPathElement = object ... end
```
```ocaml
class type  altGlyphElement = object ... end
```
```ocaml
class type  altGlyphDefElement = element
```
```ocaml
class type  altGlyphItemElement = element
```
```ocaml
class type  glyphRefElement = object ... end
```
```ocaml
class type  animatedMarkerUnit = markerUnitType animated
```
```ocaml
class type  animatedMarkerOrient = markerOrientType animated
```
```ocaml
class type  markerElement = object ... end
```
```ocaml
class type  animatedSpreadMethod = spreadMethodType animated
```
```ocaml
class type  gradientElement = object ... end
```
```ocaml
class type  linearGradientElement = object ... end
```
```ocaml
class type  radialGradientElement = object ... end
```
```ocaml
class type  stopElement = object ... end
```
```ocaml
class type  patternElement = object ... end
```
```ocaml
class type  clipPathElement = object ... end
```
```ocaml
class type  maskElement = object ... end
```
```ocaml
class type  filterElement = object ... end
```
```ocaml
class type  filterPrimitiveStandardAttributes = object ... end
```
```ocaml
class type  animatedBlendMode = blendModeType animated
```
```ocaml
class type  animatedColorMatrixType = colorMatrixType animated
```
```ocaml
class type  animatedComponentTransferType = componentTransferType animated
```
```ocaml
class type  animatedCompositeOperator = compositeOperatorType animated
```
```ocaml
class type  animatedEdgeMode = edgeModeType animated
```
```ocaml
class type  animatedChannelSelector = channelSelectorType animated
```
```ocaml
class type  animatedMorphologyOperator = morphologyOperatorType animated
```
```ocaml
class type  animatedTurbulenceType = turbulenceType animated
```
```ocaml
class type  animatedStitchType = stitchType animated
```
```ocaml
class type  feBlendElement = object ... end
```
```ocaml
class type  feColorMatrixElement = object ... end
```
```ocaml
class type  feComponentTransferElement = object ... end
```
```ocaml
class type  componentTransferFunctionElement = object ... end
```
```ocaml
class type  feFuncRElement = componentTransferFunctionElement
```
```ocaml
class type  feFuncGElement = componentTransferFunctionElement
```
```ocaml
class type  feFuncBElement = componentTransferFunctionElement
```
```ocaml
class type  feFuncAElement = componentTransferFunctionElement
```
```ocaml
class type  feCompositeElement = object ... end
```
```ocaml
class type  feConvolveMatrixElement = object ... end
```
```ocaml
class type  feDiffuseLightingElement = object ... end
```
```ocaml
class type  feDistantLightElement = object ... end
```
```ocaml
class type  fePointLightElement = object ... end
```
```ocaml
class type  feSpotLightElement = object ... end
```
```ocaml
class type  feDisplacementMapElement = object ... end
```
```ocaml
class type  feFloodElement = object ... end
```
```ocaml
class type  feGaussianBlurElement = object ... end
```
```ocaml
class type  feImageElement = object ... end
```
```ocaml
class type  feMergeElement = object ... end
```
```ocaml
class type  feMergeNodeElement = object ... end
```
```ocaml
class type  feMorphologyElement = object ... end
```
```ocaml
class type  feOffsetElement = object ... end
```
```ocaml
class type  feSpecularLightingElement = object ... end
```
```ocaml
class type  feTileElement = object ... end
```
```ocaml
class type  feTurbulenceElement = object ... end
```
```ocaml
class type  feDropShadowElement = object ... end
```
```ocaml
class type  cursorElement = object ... end
```
```ocaml
class type  aElement = object ... end
```
```ocaml
class type  viewElement = object ... end
```
```ocaml
class type  scriptElement = object ... end
```
```ocaml
class type  animationElement = object ... end
```
```ocaml
class type  animateElement = object ... end
```
```ocaml
class type  setElement = animationElement
```
```ocaml
class type  animateMotionElement = animationElement
```
```ocaml
class type  mPathElement = object ... end
```
```ocaml
class type  animateColorElement = object ... end
```
```ocaml
class type  animateTransformElement = animationElement
```
```ocaml
class type  fontElement = object ... end
```
```ocaml
class type  glyphElement = object ... end
```
```ocaml
class type  fontFaceElement = element
```
```ocaml
class type  fontFaceSrcElement = element
```
```ocaml
class type  fontFaceUriElement = element
```
```ocaml
class type  fontFaceFormatElement = element
```
```ocaml
class type  fontFaceNameElement = element
```
```ocaml
class type  metadataElement = element
```
```ocaml
class type  foreignObjectElement = object ... end
```

### Helper functions for creating Svg elements

```ocaml
val createElement : document Js.t -> string -> element Js.t
```
```ocaml
val createA : document Js.t -> aElement Js.t
```
```ocaml
val createAltGlyph : document Js.t -> altGlyphElement Js.t
```
```ocaml
val createAltGlyphDef : document Js.t -> altGlyphDefElement Js.t
```
```ocaml
val createAltGlyphItem : document Js.t -> altGlyphItemElement Js.t
```
```ocaml
val createAnimate : document Js.t -> animateElement Js.t
```
```ocaml
val createAnimateColor : document Js.t -> animateColorElement Js.t
```
```ocaml
val createAnimateMotion : document Js.t -> animateMotionElement Js.t
```
```ocaml
val createAnimateTransform : document Js.t -> animateTransformElement Js.t
```
```ocaml
val createCircle : document Js.t -> circleElement Js.t
```
```ocaml
val createClipPath : document Js.t -> clipPathElement Js.t
```
```ocaml
val createCursor : document Js.t -> cursorElement Js.t
```
```ocaml
val createDefs : document Js.t -> defsElement Js.t
```
```ocaml
val createDesc : document Js.t -> descElement Js.t
```
```ocaml
val createEllipse : document Js.t -> ellipseElement Js.t
```
```ocaml
val createFilter : document Js.t -> filterElement Js.t
```
```ocaml
val createFont : document Js.t -> fontElement Js.t
```
```ocaml
val createFontFace : document Js.t -> fontElement Js.t
```
```ocaml
val createFontFaceFormat : document Js.t -> fontElement Js.t
```
```ocaml
val createFontFaceName : document Js.t -> fontElement Js.t
```
```ocaml
val createFontFaceSrc : document Js.t -> fontElement Js.t
```
```ocaml
val createFontFaceUri : document Js.t -> fontElement Js.t
```
```ocaml
val createForeignObject : document Js.t -> foreignObjectElement Js.t
```
```ocaml
val createG : document Js.t -> gElement Js.t
```
```ocaml
val createGlyph : document Js.t -> glyphElement Js.t
```
```ocaml
val createGlyphRef : document Js.t -> glyphElement Js.t
```
```ocaml
val createhkern : document Js.t -> element Js.t
```
```ocaml
val createImage : document Js.t -> imageElement Js.t
```
```ocaml
val createLineElement : document Js.t -> lineElement Js.t
```
```ocaml
val createLinearElement : document Js.t -> linearGradientElement Js.t
```
```ocaml
val createMask : document Js.t -> maskElement Js.t
```
```ocaml
val createMetaData : document Js.t -> metadataElement Js.t
```
```ocaml
val createMissingGlyph : document Js.t -> glyphElement Js.t
```
```ocaml
val createMPath : document Js.t -> mPathElement Js.t
```
```ocaml
val createPath : document Js.t -> pathElement Js.t
```
```ocaml
val createPattern : document Js.t -> patternElement Js.t
```
```ocaml
val createPolygon : document Js.t -> polygonElement Js.t
```
```ocaml
val createPolyline : document Js.t -> polyLineElement Js.t
```
```ocaml
val createRadialgradient : document Js.t -> radialGradientElement Js.t
```
```ocaml
val createRect : document Js.t -> rectElement Js.t
```
```ocaml
val createScript : document Js.t -> scriptElement Js.t
```
```ocaml
val createSet : document Js.t -> setElement Js.t
```
```ocaml
val createStop : document Js.t -> stopElement Js.t
```
```ocaml
val createStyle : document Js.t -> styleElement Js.t
```
```ocaml
val createSvg : document Js.t -> svgElement Js.t
```
```ocaml
val createSwitch : document Js.t -> switchElement Js.t
```
```ocaml
val createSymbol : document Js.t -> symbolElement Js.t
```
```ocaml
val createTextElement : document Js.t -> textElement Js.t
```
```ocaml
val createTextpath : document Js.t -> textPathElement Js.t
```
```ocaml
val createTitle : document Js.t -> titleElement Js.t
```
```ocaml
val createTref : document Js.t -> trefElement Js.t
```
```ocaml
val createTspan : document Js.t -> tspanElement Js.t
```
```ocaml
val createUse : document Js.t -> useElement Js.t
```
```ocaml
val createView : document Js.t -> viewElement Js.t
```
```ocaml
val createvkern : document Js.t -> element Js.t
```
```ocaml
val createMarker : document Js.t -> markerElement Js.t
```
```ocaml
val createFeBlend : document Js.t -> feBlendElement Js.t
```
```ocaml
val createFeColorMatrix : document Js.t -> feColorMatrixElement Js.t
```
```ocaml
val createFeComponentTransfer : 
  document Js.t ->
  feComponentTransferElement Js.t
```
```ocaml
val createFeFuncR : document Js.t -> feFuncRElement Js.t
```
```ocaml
val createFeFuncG : document Js.t -> feFuncGElement Js.t
```
```ocaml
val createFeFuncB : document Js.t -> feFuncBElement Js.t
```
```ocaml
val createFeFuncA : document Js.t -> feFuncAElement Js.t
```
```ocaml
val createFeComposite : document Js.t -> feCompositeElement Js.t
```
```ocaml
val createFeConvolveMatrix : document Js.t -> feConvolveMatrixElement Js.t
```
```ocaml
val createFeDiffuseLighting : document Js.t -> feDiffuseLightingElement Js.t
```
```ocaml
val createFeDistantLight : document Js.t -> feDistantLightElement Js.t
```
```ocaml
val createFePointLight : document Js.t -> fePointLightElement Js.t
```
```ocaml
val createFeSpotLight : document Js.t -> feSpotLightElement Js.t
```
```ocaml
val createFeDisplacementMap : document Js.t -> feDisplacementMapElement Js.t
```
```ocaml
val createFeFlood : document Js.t -> feFloodElement Js.t
```
```ocaml
val createFeGaussianBlur : document Js.t -> feGaussianBlurElement Js.t
```
```ocaml
val createFeImage : document Js.t -> feImageElement Js.t
```
```ocaml
val createFeMerge : document Js.t -> feMergeElement Js.t
```
```ocaml
val createFeMergeNode : document Js.t -> feMergeNodeElement Js.t
```
```ocaml
val createFeMorphology : document Js.t -> feMorphologyElement Js.t
```
```ocaml
val createFeOffset : document Js.t -> feOffsetElement Js.t
```
```ocaml
val createFeSpecularLighting : document Js.t -> feSpecularLightingElement Js.t
```
```ocaml
val createFeTile : document Js.t -> feTileElement Js.t
```
```ocaml
val createFeTurbulence : document Js.t -> feTurbulenceElement Js.t
```
```ocaml
val createFeDropShadow : document Js.t -> feDropShadowElement Js.t
```
```ocaml
val svg_element : element Js.t Js.constr
```
```ocaml
val document : document Js.t
```
The current document

```ocaml
val getElementById : string -> element Js.t
```
`getElementById id` returns the element with the id `id` in the current document. It raises `Not_found` if there are no such element

```ocaml
module CoerceTo : sig ... end
```