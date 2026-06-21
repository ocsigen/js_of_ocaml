
# Module `Js_of_ocaml_lwt.Jsonp`

This module provides helpers to perform JSONP calls

```ocaml
val call : 
  ?timeout:float ->
  ?param:string ->
  ?prefix:string ->
  string ->
  'b Lwt.t
```
`call ~timeout ~param url` do a jsonp call using `url`. It uses the named query parameter `param` (default "callback") to pass the name of the callback. It uses `prefix` to generate random string for callback name. If a timeout is given and there are no answer before `timeout` seconds, the lwt thread will be cancelled.

```ocaml
val call_custom_url : 
  ?timeout:float ->
  ?prefix:string ->
  (string -> string) ->
  'b Lwt.t
```
`call_custom_url ~timeout make_url`. Same as `call` but let you build your own url given a callback name
