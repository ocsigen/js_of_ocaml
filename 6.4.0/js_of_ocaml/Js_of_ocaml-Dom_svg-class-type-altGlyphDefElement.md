
# Class type `Dom_svg.altGlyphDefElement`

deprecated Removed in SVG 2. SVG fonts replaced by WOFF.
```ocaml
inherit Js_of_ocaml__.Dom.element
```
```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method id : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method xmlbase : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.optdef
                   Js_of_ocaml__.Js.prop
```
deprecated Removed in SVG 2.
```ocaml
method ownerSVGElement : svgElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method viewportElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method tabIndex : int Js_of_ocaml__.Js.prop
```
SVG 2 addition.

```ocaml
method focus : unit Js_of_ocaml__.Js.meth
```
SVG 2 addition.

```ocaml
method blur : unit Js_of_ocaml__.Js.meth
```
SVG 2 addition.

```ocaml
method style : Js_of_ocaml__.Dom_html.cssStyleDeclaration Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
SVG 2: merged in from `SVGStylable`.

```ocaml
method className : animatedString Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.readonly_prop
```
SVG 2: merged in from `SVGStylable`.

deprecated Use classList (from Dom.element) instead.
```ocaml
method dataset : Js_of_ocaml__.Dom_html.domStringMap Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
SVG 2 addition (from `HTMLOrForeignElement`).

```ocaml
method autofocus : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
SVG 2 addition (from `HTMLOrForeignElement`).

```ocaml
method nonce : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
SVG 2 addition (from `HTMLOrForeignElement`).
