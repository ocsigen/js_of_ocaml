
type +'a t

module Unsafe = struct
  external variable : string -> 'a = "caml_js_var"

  type any
  external inject : 'a -> any = "%identity"
  external extract : any -> 'a = "%identity"
  external coerce : < .. > t -> < ..> t = "%identity"

  external get : 'a -> string -> 'b = "caml_js_get"
  external set : 'a -> string -> 'b -> unit = "caml_js_set"
  external meth_call : 'a -> string -> any array -> 'c = "caml_js_meth_call"
end

type 'a opt = 'a
type 'a optdef = 'a

let null = Unsafe.variable "null"
external some : 'a -> 'a opt = "%identity"

(*FIX: undefined is not a reserved keyword! *)
let undefined = Unsafe.variable "undefined"
external def : 'a -> 'a optdef = "%identity"

module type OPT = sig
  type 'a t
  val ret : 'a -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val case : 'a t -> 'b -> ('a -> 'b) -> 'b
  val get : 'a t -> (unit -> 'a) -> 'a
  val iter : 'a t -> ('a -> unit) -> unit
end

module Opt : OPT with type 'a t = 'a opt = struct
  type 'a t = 'a opt
  let ret = some
  let map x f = if x == null then null else some (f x)
  let bind x f = if x == null then null else f x
  let case x y f = if x == null then y else f x
  let get x f = if x == null then f () else x
  let iter x f = if x != null then f x
end

module Optdef : OPT with type 'a t = 'a optdef = struct
  type 'a t = 'a opt
  let ret = undefined
  let map x f = if x == undefined then undefined else some (f x)
  let bind x f = if x == undefined then undefined else f x
  let case x y f = if x == undefined then y else f x
  let get x f = if x == undefined then f () else x
  let iter x f = if x != undefined then f x
end

let _true = Unsafe.variable "true"
let _false = Unsafe.variable "false"

type readonly
type readwrite
type (+'a, +'b) gen_prop
type 'a readonly_prop = ('a, readonly) gen_prop
type 'a prop = ('a, readwrite) gen_prop
type +'a meth

(* More general types
type +'a gen_prop
type 'a readonly_prop = <read : 'a> gen_prop
type 'a prop = <read: 'a; write : 'a> gen_prop

type 'a optdef_prop = <read: 'a optdef; write : 'a> gen_prop
*)
