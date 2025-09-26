(* TEST
 expect;
*)

type t_any : any
type t_any_non_null : any_non_null
type t_value_or_null : value_or_null
type t_value : value
type t_bits64 : bits64

[%%expect{|
type t_any : any
type t_any_non_null : any_non_null
type t_value_or_null : value_or_null
type t_value
type t_bits64 : bits64
|}]

(* [any_non_null] is not representable *)
let f (x : t_any_non_null) = x

[%%expect{|
Line _, characters 6-26:
Error: This pattern matches values of type t_any_non_null
       but a pattern was expected which matches values of type
         ('a : '_representable_layout_1)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be representable
         because we must know concretely how to pass a function argument.
|}]

type t = { x : t_any_non_null }

[%%expect{|
Line _, characters 11-29:
Error: Record element types must have a representable layout.
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be representable
         because it is the type of record field x.
|}]

module type S1 = sig
  val x : t_any_non_null
end

[%%expect{|
Line _, characters 10-24:
Error: This type signature for x is not a value type.
       The layout of type t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of type t_any_non_null must be a sublayout of value
         because it's the type of something stored in a module structure.
|}]

module type S2 = sig
  val f : unit -> t_any
  val g : unit -> t_any_non_null
end

[%%expect{|
module type S2 = sig val f : unit -> t_any val g : unit -> t_any_non_null end
|}]

module M2 (X : S2) = struct
  let g () = X.g ()
end

[%%expect{|
Line _, characters 13-19:
Error: This expression has type t_any_non_null
       but an expression was expected of type ('a : '_representable_layout_2)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be representable
         because we must know concretely how to return a function result.
|}]

(* [value_or_null] is representable *)

let f (x : t_value_or_null) = x

[%%expect{|
val f : t_value_or_null -> t_value_or_null = <fun>
|}]

type t = { x : t_value_or_null }

[%%expect {|
type t = { x : t_value_or_null; }
|}]

module type S1 = sig
  val x : t_value_or_null
end

[%%expect {|
module type S1 = sig val x : t_value_or_null end
|}]

module type S2 = sig
  val f : unit -> t_value_or_null
end

[%%expect {|
module type S2 = sig val f : unit -> t_value_or_null end
|}]

module M2 (X : S2) = struct
  let f () = X.f ()
end

[%%expect{|
module M2 : functor (X : S2) -> sig val f : unit -> t_value_or_null end
|}]

type ('a : any) id_any = 'a
type ('a : any_non_null) id_any_non_null = 'a
type ('a : value_or_null) id_value_or_null = 'a
type ('a : value) id_value = 'a
type ('a : bits64) id_bits64 = 'a

[%%expect{|
type ('a : any) id_any = 'a
type ('a : any_non_null) id_any_non_null = 'a
type 'a id_value_or_null = 'a
type 'a id_value = 'a
type ('a : bits64) id_bits64 = 'a
|}]

(* [any_non_null] is a sublayout of [any] *)

type t = t_any_non_null id_any

[%%expect{|
type t = t_any_non_null id_any
|}]

module M (X : sig type t : any_non_null end) : sig type t : any end = X

[%%expect{|
module M :
  functor (X : sig type t : any_non_null end) -> sig type t : any end
|}]

(* [any] is not a sublayout of [any_non_null] *)

type t = t_any id_any_non_null

[%%expect{|
Line _, characters 9-14:
Error: This type t_any should be an instance of type ('a : any_non_null)
       The kind of t_any is any
         because of the definition of t_any at file "basics.ml", line 1, characters 0-16.
       But the kind of t_any must be a subkind of any_non_null
         because of the definition of id_any_non_null at file "basics.ml", line 2, characters 0-45.
|}]

module M (X : sig type t : any end) : sig type t : any_non_null end = X

[%%expect{|
Line _, characters 70-71:
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t : any_non_null end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : any_non_null
       The kind of the first is any
         because of the definition of t at file "basics.ml", line 1, characters 18-30.
       But the kind of the first must be a subkind of any_non_null
         because of the definition of t at file "basics.ml", line 1, characters 42-63.
       File "basics.ml", line 1, characters 42-63: Expected declaration
       File "basics.ml", line 1, characters 18-30: Actual declaration
|}]

(* [value] is a sublayout of [value_or_null] *)

type t = t_value id_value_or_null

[%%expect{|
type t = t_value id_value_or_null
|}]

module M (X : sig type t : value end) : sig type t : value_or_null end = X

[%%expect{|
module M : functor (X : sig type t end) -> sig type t : value_or_null end
|}]

(* [value_or_null] is not a sublayout of [value] *)

type t = t_value_or_null id_value

[%%expect{|
Line _, characters 9-24:
Error: This type t_value_or_null should be an instance of type ('a : value)
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at file "basics.ml", line 3, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value
         because of the definition of id_value at file "basics.ml", line 4, characters 0-31.
|}]

module M (X : sig type t : value_or_null end) : sig type t : value end = X

[%%expect{|
Line _, characters 73-74:
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t end
       Type declarations do not match: type t = X.t is not included in type t
       The kind of the first is value_or_null
         because of the definition of t at file "basics.ml", line 1, characters 18-40.
       But the kind of the first must be a subkind of value
         because of the definition of t at file "basics.ml", line 1, characters 52-66.
       File "basics.ml", line 1, characters 52-66: Expected declaration
       File "basics.ml", line 1, characters 18-40: Actual declaration
|}]

(* [value] is a sublayout of [any_non_null] *)

type t = t_value id_any_non_null

[%%expect{|
type t = t_value id_any_non_null
|}]

module M (X : sig type t : value end) : sig type t : any_non_null end = X

[%%expect{|
module M : functor (X : sig type t end) -> sig type t : any_non_null end
|}]

(* [value_or_null] is not a sublayout of [any_non_null] *)

type t = t_value_or_null id_any_non_null

[%%expect{|
Line _, characters 9-24:
Error: This type t_value_or_null should be an instance of type
         ('a : any_non_null)
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at file "basics.ml", line 3, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any_non_null
         because of the definition of id_any_non_null at file "basics.ml", line 2, characters 0-45.
|}]

module M (X : sig type t : value_or_null end) : sig type t : any_non_null end = X

[%%expect{|
Line _, characters 80-81:
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t : any_non_null end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : any_non_null
       The kind of the first is value_or_null
         because of the definition of t at file "basics.ml", line 1, characters 18-40.
       But the kind of the first must be a subkind of any_non_null
         because of the definition of t at file "basics.ml", line 1, characters 52-73.
       File "basics.ml", line 1, characters 52-73: Expected declaration
       File "basics.ml", line 1, characters 18-40: Actual declaration
|}]

(* [value_or_null] is a sublayout of [any] *)

type t = t_value_or_null id_any

[%%expect{|
type t = t_value_or_null id_any
|}]

module M (X : sig type t : value_or_null end) : sig type t : any end = X

[%%expect{|
module M :
  functor (X : sig type t : value_or_null end) -> sig type t : any end
|}]

(* [bits64] (and presumably similar jkinds) is a sublayout of [any_non_null] *)

type t = t_bits64 id_any_non_null

[%%expect{|
type t = t_bits64 id_any_non_null
|}]

module M (X : sig type t : bits64 end) : sig type t : any_non_null end = X

[%%expect{|
module M :
  functor (X : sig type t : bits64 end) -> sig type t : any_non_null end
|}]

(* [any_non_null] is not a sublayout of [value] *)

type t = t_any_non_null id_value

[%%expect{|
Line _, characters 9-23:
Error: This type t_any_non_null should be an instance of type ('a : value)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be a sublayout of value
         because of the definition of id_value at file "basics.ml", line 4, characters 0-31.
|}]

module M (X : sig type t : any_non_null end) : sig type t : value end = X

[%%expect{|
Line _, characters 72-73:
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t end
       Type declarations do not match: type t = X.t is not included in type t
       The layout of the first is any
         because of the definition of t at file "basics.ml", line 1, characters 18-39.
       But the layout of the first must be a sublayout of value
         because of the definition of t at file "basics.ml", line 1, characters 51-65.
       File "basics.ml", line 1, characters 51-65: Expected declaration
       File "basics.ml", line 1, characters 18-39: Actual declaration
|}]

(* [any_non_null] is not a sublayout of [value_or_null] *)

type t = t_any_non_null id_value_or_null

[%%expect{|
Line _, characters 9-23:
Error: This type t_any_non_null should be an instance of type
         ('a : value_or_null)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be a sublayout of value
         because of the definition of id_value_or_null at file "basics.ml", line 3, characters 0-47.
|}]

module M (X : sig type t : any_non_null end) : sig type t : value_or_null end = X

[%%expect{|
Line _, characters 80-81:
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t : value_or_null end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : value_or_null
       The layout of the first is any
         because of the definition of t at file "basics.ml", line 1, characters 18-39.
       But the layout of the first must be a sublayout of value
         because of the definition of t at file "basics.ml", line 1, characters 51-73.
       File "basics.ml", line 1, characters 51-73: Expected declaration
       File "basics.ml", line 1, characters 18-39: Actual declaration
|}]

(* [any_non_null] is not a sublayout of [bits64] (and presumably similar jkinds) *)

type t = t_any_non_null id_bits64

[%%expect{|
Line _, characters 9-23:
Error: This type t_any_non_null should be an instance of type ('a : bits64)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be a sublayout of bits64
         because of the definition of id_bits64 at file "basics.ml", line 5, characters 0-33.
|}]

module M (X : sig type t : any_non_null end) : sig type t : bits64 end = X

[%%expect{|
Line _, characters 73-74:
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t : bits64 end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : bits64
       The layout of the first is any
         because of the definition of t at file "basics.ml", line 1, characters 18-39.
       But the layout of the first must be a sublayout of bits64
         because of the definition of t at file "basics.ml", line 1, characters 51-66.
       File "basics.ml", line 1, characters 51-66: Expected declaration
       File "basics.ml", line 1, characters 18-39: Actual declaration
|}]

(* The meet of [any_non_null] and [value_or_null] is [value] *)

type (_, _) two

type ('a : any) t1 = ('a id_any_non_null, 'a id_value_or_null) two

type should_work = t_value t1

[%%expect{|
type (_, _) two
type 'a t1 = ('a id_any_non_null, 'a id_value_or_null) two
type should_work = t_value t1
|}]

type should_fail = t_value_or_null t1

[%%expect{|
Line _, characters 19-34:
Error: This type t_value_or_null should be an instance of type ('a : value)
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at file "basics.ml", line 3, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value
         because of the definition of t1 at file "basics.ml", line 3, characters 0-66.
|}]

type should_fail = t_any_non_null t1

[%%expect{|
Line _, characters 19-33:
Error: This type t_any_non_null should be an instance of type ('a : value)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "basics.ml", line 2, characters 0-34.
       But the layout of t_any_non_null must be a sublayout of value
         because of the definition of t1 at file "basics.ml", line 3, characters 0-66.
|}]

(* let-rec allows [value_or_null] *)
let u () =
  let rec x : t_value_or_null = assert false in
  ()

[%%expect{|
Line _, characters 10-11:
Warning 26 [unused-var]: unused variable x.

val u : unit -> unit = <fun>
|}]
