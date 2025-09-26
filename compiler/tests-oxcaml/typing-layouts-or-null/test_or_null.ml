(* TEST
 expect;
*)

type ('a : value) t : immediate_or_null with 'a = 'a or_null [@@or_null_reexport]

[%%expect{|
type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
|}]

let to_option (x : 'a or_null) =
  match x with
  | Null -> None
  | This x -> Some x

[%%expect{|
val to_option : 'a or_null -> 'a option = <fun>
|}]

let of_option (x : 'a option) =
  match x with
  | None -> Null
  | Some x -> This x

[%%expect{|
val of_option : 'a option -> 'a t = <fun>
|}]

let pi = This 3.14

[%%expect{|
val pi : float t = This 3.14
|}]

let pi' =
  let value = This 3.14 in
  value

[%%expect{|
val pi' : float t = This 3.14
|}]

type myrec = { x : int; y : int or_null }

[%%expect{|
type myrec = { x : int; y : int or_null; }
|}]

let fst { x; y } = x

[%%expect{|
val fst : myrec -> int = <fun>
|}]

let snd { x; y } = y

[%%expect{|
val snd : myrec -> int or_null = <fun>
|}]

let snd' (a : myrec) = a.y

[%%expect{|
val snd' : myrec -> int or_null = <fun>
|}]

let mk n = { x = n; y = This n }

[%%expect{|
val mk : int -> myrec = <fun>
|}]

let test =
  let a = mk 4 in
  let a' = { a with y = Null } in
  a'.y

[%%expect{|
val test : int or_null = Null
|}]

let mytup = (4, This 5)

[%%expect{|
val mytup : int * int t = (4, This 5)
|}]

type mytup' = int * int t

[%%expect{|
type mytup' = int * int t
|}]

type nested = int or_null or_null

[%%expect{|
Line _, characters 14-25:
Error: This type int or_null should be an instance of type ('a : value)
       The kind of int or_null is immediate_or_null
         because it is the primitive immediate_or_null type or_null.
       But the kind of int or_null must be a subkind of value
         because the type argument of or_null has kind value.
|}]

let should_fail = This (This 5)

[%%expect{|
Line _, characters 23-31:
Error: This expression has type 'a t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because of the definition of t at file "test_or_null.ml", line 1, characters 0-81.
|}]

let should_also_fail = This Null

[%%expect{|
Line _, characters 28-32:
Error: This expression has type 'a t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because of the definition of t at file "test_or_null.ml", line 1, characters 0-81.
|}]

let mk' n = `Foo (This n)

[%%expect{|
val mk' : 'a -> [> `Foo of 'a t ] = <fun>
|}]

module type S = sig
  type a = float or_null

  val x : a
  val f : float -> a
  val g : a -> float
end

[%%expect{|
module type S =
  sig
    type a = float or_null
    val x : a
    val f : float -> a
    val g : a -> float
  end
|}]

module M : S with type a = float t = struct
  type a = float or_null

  let x = This 3.14
  let f x = This x
  let g = function
    | This x -> x
    | Null -> 0.
end

[%%expect{|
module M :
  sig type a = float t val x : a val f : float -> a val g : a -> float end
|}]

external this : 'a -> 'a or_null = "%identity"

[%%expect{|
external this : 'a -> 'a or_null = "%identity"
|}]

external unsafe_get : 'a or_null -> 'a = "%identity"

[%%expect{|
external unsafe_get : 'a or_null -> 'a = "%identity"
|}]

let should_fail = [| Null; This 5 |]

[%%expect{|
Line _, characters 21-25:
Error: This expression has type 'a t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because it's the type of an array element,
         chosen to have kind value.
|}]

type should_fail = float or_null array

[%%expect{|
Line _, characters 19-32:
Error: This type float or_null should be an instance of type
         ('a : any_non_null)
       The kind of float or_null is
         value_or_null mod many unyielding stateless immutable
         because it is the primitive immediate_or_null type or_null.
       But the kind of float or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

let null_list = [ Null; This 5 ]

[%%expect{|
val null_list : int t list = [Null; This 5]
|}]

type null_list = float or_null list

[%%expect{|
type null_list = float or_null list
|}]

(* Immutable arrays should work the same as mutable: *)

let should_fail = [: Null; This 5 :]

[%%expect{|
Line _, characters 21-25:
Error: This expression has type 'a t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because it's the type of an array element,
         chosen to have kind value.
|}]

type should_fail = float or_null array

[%%expect{|
Line _, characters 19-32:
Error: This type float or_null should be an instance of type
         ('a : any_non_null)
       The kind of float or_null is
         value_or_null mod many unyielding stateless immutable
         because it is the primitive immediate_or_null type or_null.
       But the kind of float or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

(* CR layouts v3: object fields should accept null, but it's low priority. *)
type object_with_null = < x : int or_null; .. >

[%%expect{|
Line _, characters 26-42:
Error: Object field types must have layout value.
       The kind of int or_null is immediate_or_null
         because it is the primitive immediate_or_null type or_null.
       But the kind of int or_null must be a subkind of value
         because it's the type of an object field.
|}]

(* CR layouts v3: instance variables should accept null, but it's low priority. *)
class a_with_null =
  object
    val x = Null
  end

[%%expect{|
Line _, characters 8-9:
Error: Variables bound in a class must have layout value.
       The kind of x is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of x must be a subkind of value
         because it's the type of a class field.
|}]

(* just checking printing *)
type t_any_non_null : any_non_null

[%%expect{|
type t_any_non_null : any_non_null
|}]
