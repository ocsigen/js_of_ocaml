open Js_of_ocaml

let stdin = Js.Unsafe.global##._WScript##._StdIn

type obj

let obj = Js.Unsafe.global##._ActiveXObject

external set_prop : 'a -> string -> Js.Unsafe.any array -> 'd -> unit = "caml_js_set_prop"

type variant =
  | Null : variant
  | Array : _ array -> variant
  | Bool : bool -> variant
  | Number : float -> variant
  | String : string -> variant
  | Object : _ Js.t -> variant

let variant = function
  | Null -> Js.Unsafe.inject Js.null
  | Array a -> Js.Unsafe.inject (Js.array a)
  | Bool b -> Js.Unsafe.inject (Js.bool b)
  | Number f -> Js.Unsafe.inject f
  | String s -> Js.Unsafe.inject (Js.string s)
  | Object o -> Js.Unsafe.inject o

let to_variant x =
  Js.Opt.case x (fun () -> Null) (fun x ->
    if Js.instanceof x Js.Unsafe.global##._Array then
      Array (Js.to_array (Js.Unsafe.coerce x))
    else if Js.typeof x == Js.string "boolean" then
      Bool (Js.to_bool (Js.Unsafe.coerce x))
    else if Js.typeof x == Js.string "string" then
      String (Js.to_string (Js.Unsafe.coerce x))
    else if Js.typeof x == Js.string "number" then
      Number (Js.float_of_number (Js.Unsafe.coerce x))
    else
      Object x
  )

let echo x = Js.Unsafe.global##._WScript##echo (variant x)
