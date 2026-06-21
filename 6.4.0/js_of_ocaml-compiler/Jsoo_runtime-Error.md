
# Module `Jsoo_runtime.Error`

```ocaml
type t
```
```ocaml
val raise_ : t -> 'a
```
```ocaml
val attach_js_backtrace : exn -> force:bool -> exn
```
Attach a JavaScript error to an OCaml exception. if `force = false` and a JavaScript error is already attached, it will do nothing. This function is useful to store and retrieve information about JavaScript stack traces.

Attaching JavaScript errors will happen automatically when compiling with `--enable with-js-error`.

```ocaml
val of_exn : exn -> t option
```
Extract a JavaScript error attached to an OCaml exception, if any. This is useful to inspect an eventual stack trace, especially when sourcemap is enabled.

```ocaml
exception Exn of t
```
The `Error` exception wrap javascript exceptions when caught by OCaml code. In case the javascript exception is not an instance of javascript `Error`, it will be serialized and wrapped into a `Failure` exception.
