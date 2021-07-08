type 'a promise 

let _Promise = Js.Unsafe.global##._Promise

let _catch (p : 'a promise Js.t) (f : _ -> 'a promise Js.t) : 'a promise Js.t =
  (Js.Unsafe.coerce p)##_catch (Js.wrap_callback f)

let _then (p : 'a promise Js.t) ?(catch: (_ -> 'b promise Js.t) option)
    (f: 'a -> 'b promise Js.t) : 'b promise Js.t =
  match catch with
  | None ->
    (Js.Unsafe.coerce p)##_then (Js.wrap_callback f)
  | Some catch ->
    (Js.Unsafe.coerce p)##_then (Js.wrap_callback f) (Js.wrap_callback catch)

let resolve_value (v :'a) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "resolve" [|Js.Unsafe.inject v|]

let resolve_promise (v :'a promise Js.t) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "resolve" [|Js.Unsafe.inject v|]

let reject_value (v :'a) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "reject" [|Js.Unsafe.inject v|]

let reject_promise (v :'a promise Js.t) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "rejetct" [|Js.Unsafe.inject v|]

let race (promise_list : 'a promise Js.t Js.js_array Js.t) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "race" [|Js.Unsafe.inject promise_list|]

let all (promise_list : 'a promise Js.t Js.js_array Js.t) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "all" [|Js.Unsafe.inject promise_list|]
