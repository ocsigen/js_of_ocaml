
# Module `Url.Current`

This module can be used to handle the Url associated to the current document.

```ocaml
val host : string
```
The host part of the current url.

```ocaml
val port : int option
```
The port of the current url.

```ocaml
val protocol : string
```
The protocol of the current url.

```ocaml
val path_string : string
```
The path of the current url as one long string.

```ocaml
val path : string list
```
The path of the current url as a list of small string.

```ocaml
val arguments : (string * string) list
```
The arguments of the current url as an association list.

```ocaml
val get_fragment : unit -> string
```
Because the `fragment` of the Url for the current document can change dynamically, we use a functional value here.

```ocaml
val set_fragment : string -> unit
```
`set_fragment s` replaces the current fragment by `s`.

```ocaml
val get : unit -> url option
```
`get ()` returns a value of type [`url`](./Js_of_ocaml-Url.md#type-url) with fields reflecting the state of the current Url.

```ocaml
val set : url -> unit
```
`set u` replaces the current Url for `u`. **WARNING:** Causes the document to change.

```ocaml
val as_string : string
```
`as_string` is the original string representation of the current Url. It is NOT necessarily equals to `string_of_url (get ())` but `url_of_string as_string = get ()` holds.
