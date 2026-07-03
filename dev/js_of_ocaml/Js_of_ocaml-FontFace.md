
# Module `Js_of_ocaml.FontFace`

CSS Font Loading API binding.

This is a partial binding to the CSS Font Loading API.

see [https://developer.mozilla.org/en-US/docs/Web/API/CSS\_Font\_Loading\_API](https://developer.mozilla.org/en-US/docs/Web/API/CSS_Font_Loading_API) 
see [https://www.w3.org/TR/css-font-loading-3/](https://www.w3.org/TR/css-font-loading-3/) 
```ocaml
class type  fontFace = object ... end
```
```ocaml
class type  fontFaceSet = object ... end
```
```ocaml
val is_supported : unit -> bool
```
`is_supported ()` is `true` if the `FontFace` constructor of the CSS Font Loading API is available in the current environment.

```ocaml
val create : Js.js_string Js.t -> Js.js_string Js.t -> fontFace Js.t
```
`create family source` wraps `new FontFace(family, source)`, where `source` is a CSS `src` descriptor string (e.g. `"url(font.woff2)"`).

```ocaml
val create_from_buffer : 
  Js.js_string Js.t ->
  Typed_array.arrayBuffer Js.t ->
  fontFace Js.t
```
`create_from_buffer family data` wraps `new FontFace(family, data)` with the font's binary data as `data`, for fonts loaded from raw bytes.
