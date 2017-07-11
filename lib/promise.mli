type 'a promise
val _Promise : 'a
val _catch : 'a promise Js.t -> ('b -> 'a promise Js.t) -> 'a promise Js.t
val _then :
  'a promise Js.t ->
  ?catch:('c -> 'b promise Js.t) ->
  ('a -> 'b promise Js.t) -> 'b promise Js.t
val resolve_value : 'a -> 'a promise Js.t
val resolve_promise : 'a promise Js.t -> 'a promise Js.t
val reject_value : 'a -> 'a promise Js.t
val reject_promise : 'a promise Js.t -> 'a promise Js.t
val race : 'a promise Js.t Js.js_array Js.t -> 'a promise Js.t
val all : 'a promise Js.t Js.js_array Js.t -> 'a promise Js.t
