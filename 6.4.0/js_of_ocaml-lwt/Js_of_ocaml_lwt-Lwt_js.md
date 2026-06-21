
# Module `Js_of_ocaml_lwt.Lwt_js`

Javascript specific Lwt functions.

```ocaml
val sleep : float -> unit Lwt.t
```
`sleep d` is a threads which remain suspended for `d` seconds and then terminates.

```ocaml
val yield : unit -> unit Lwt.t
```
`yield ()` is a threads which suspends itself and then resumes as soon as possible and terminates.
