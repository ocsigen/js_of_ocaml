
# Module `Js_of_ocaml.Clipboard`

Clipboard API.

A code example:

```ocaml
  if Clipboard.is_supported ()
  then
    ignore
      (Promise.map
         (fun () -> Console.console##log (Js.string "copied"))
         ((Clipboard.clipboard ())##writeText (Js.string "Hello!")))
```
see [https://developer.mozilla.org/en-US/docs/Web/API/Clipboard\_API](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard_API) 
see [https://w3c.github.io/clipboard-apis/](https://w3c.github.io/clipboard-apis/) 
```ocaml
class type  clipboardItem = object ... end
```
```ocaml
val clipboardItem : (Js.Unsafe.any -> clipboardItem Js.t) Js.constr
```
`new%js clipboardItem data` builds a clipboard item. `data` is a plain JavaScript object whose keys are MIME types and whose values are the associated [`File.blob`](./Js_of_ocaml-File-class-type-blob.md), string, or a promise resolving to either.

```ocaml
val item_supports : Js.js_string Js.t -> bool Js.t
```
`item_supports mime` reports whether the given MIME type is supported by the clipboard. Bound to the static `ClipboardItem.supports` method.

```ocaml
class type  clipboard = object ... end
```
```ocaml
val clipboard : unit -> clipboard Js.t
```
The `navigator.clipboard` object.

```ocaml
val is_supported : unit -> bool
```
Whether `navigator.clipboard` is available in the current environment.
