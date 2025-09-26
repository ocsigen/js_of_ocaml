(* TEST
 expect;
*)

(* CR layouts v3.5: ['a or_null] can't be re-exported normally,
   because users can't define their own [Null]-like constructors. *)
module Or_null = struct
  type ('a : value) t : immediate_or_null with 'a = 'a or_null =
    | Null
    | This of 'a
end
[%%expect{|
Line _, characters 2-92:
Error: The kind of type t is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of type t must be a subkind of immediate_or_null with 'a
         because of the annotation on the declaration of the type t.
|}]

module Or_null = struct
  type ('a : value) t : immediate_or_null with 'a = 'a or_null
end
[%%expect{|
module Or_null : sig type 'a t = 'a or_null end
|}]

(* Omitting the type representation leaves constructors unexported. *)

let n = Or_null.Null

[%%expect{|
Line _, characters 8-20:
Error: Unbound constructor Or_null.Null
|}]

let t v = Or_null.This v

[%%expect{|
Line _, characters 10-22:
Error: Unbound constructor Or_null.This
|}]

(* [@@or_null_reexport] re-exports those constructors. *)

module Or_null = struct
  type ('a : value) t : immediate_or_null with 'a = 'a or_null [@@or_null_reexport]
end
let n = Or_null.Null
let t v = Or_null.This v
[%%expect{|
module Or_null :
  sig type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport] end
val n : 'a Or_null.t = Or_null.Null
val t : 'a -> 'a Or_null.t = <fun>
|}]

(* The jkind of [Or_null] is still correctly [immediate_or_null with 'a]. *)
let fail = Or_null.This (Or_null.This 5)

[%%expect{|
Line _, characters 24-40:
Error: This expression has type 'a Or_null.t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a Or_null.t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a Or_null.t must be a subkind of value
         because of the definition of t at file "reexport.ml", line 2, characters 2-83.
|}]

(* Type annotations are not required. *)

module Or_null = struct
  type 'a t = 'a or_null [@@or_null_reexport]
end
let fail = Or_null.This (Or_null.This 5)

[%%expect{|
module Or_null :
  sig type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport] end
Line _, characters 24-40:
Error: This expression has type 'a Or_null.t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a Or_null.t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a Or_null.t must be a subkind of value
         because of the definition of t at file "reexport.ml", line 2, characters 2-45.
|}]

(* Incorrect annotations still cause errors. *)

type 'a t : value = 'a or_null [@@or_null_reexport]

[%%expect{|
Line _, characters 0-51:
Error: The kind of type 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of type 'a or_null must be a subkind of value
         because of the definition of t at file "reexport.ml", line 1, characters 0-51.
|}]

type 'a t : float64 = 'a or_null [@@or_null_reexport]

[%%expect{|
Line _, characters 0-53:
Error: The layout of type 'a or_null is value
         because it is the primitive immediate_or_null type or_null.
       But the layout of type 'a or_null must be a sublayout of float64
         because of the definition of t at file "reexport.ml", line 1, characters 0-53.
|}]

type ('a : float64) t = 'a or_null [@@or_null_reexport]

[%%expect{|
Line _, characters 24-26:
Error: This type ('a : float64) should be an instance of type ('b : value)
       The layout of 'a is float64
         because of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value
         because the type argument of or_null has layout value.
|}]

(* The behavior here is correct but confusing: ['a] is restricted to [value],
   which is a subjkind of [value_or_null]. *)

module Or_null = struct
  type ('a : value_or_null) t = 'a or_null [@@or_null_reexport]
end
let fail = Or_null.This (Or_null.This 5)

[%%expect{|
module Or_null :
  sig type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport] end
Line _, characters 24-40:
Error: This expression has type 'a Or_null.t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a Or_null.t is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a Or_null.t must be a subkind of value
         because of the definition of t at file "reexport.ml", line 2, characters 2-63.
|}]

(* This fails, just as [type t = int option = None | Some of int] would. *)

type t = int or_null [@@or_null_reexport]

[%%expect{|
Line _, characters 0-41:
Error: This variant or record definition does not match that of type
         int or_null
       They have different arities.
|}]

(* [@@or_null_reexport] requires a manifest. *)

type 'a t [@@or_null_reexport]

[%%expect{|
Line _, characters 0-30:
Error: Invalid reexport declaration.
       Type t must be defined equal to the primitive type or_null.
|}]

(* CR layouts v3: This would be nice to accept, but it's somewhat complicated
   to implement. So we won't unless we encounter a use-case. *)
module M : sig
  type 'a t [@@or_null_reexport]
end = struct
  type 'a t = 'a or_null [@@or_null_reexport]
end

[%%expect{|
Line _, characters 2-32:
Error: Invalid reexport declaration.
       Type t must be defined equal to the primitive type or_null.
|}]

(* [@@or_null_reexport] forbids explicit representation. *)

type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]

[%%expect{|
Line _, characters 0-63:
Error: Invalid reexport declaration.
       Type t must not define an explicit representation.
|}]

(* [@@or_null_reexport] requires the type to be equal to ['a null]. *)

type 'a t = 'a option [@@or_null_reexport]

[%%expect{|
Line _, characters 0-42:
Error: Invalid reexport declaration.
       Type t must be defined equal to the primitive type or_null.
|}]

(* [@@or_null_reexport] behaves sanely in corner cases. *)

type 'a t = 'a or_null [@@or_null_reexport]
and t' = int or_null

[%%expect{|
type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
and t' = int or_null
|}]

type 'a t = 'b or_null constraint 'b = int * 'a [@@or_null_reexport]

[%%expect{|
Line _, characters 0-68:
Error: This variant or record definition does not match that of type
         (int * 'a) or_null
       Their parameters differ:
       The type int * 'a is not equal to the type 'a
|}]

(* [@@or_null_reexport] expands the type on the right. *)

type 'a t1 = 'a or_null [@@or_null_reexport]

type 'a t2 = 'a t1 [@@or_null_reexport]

[%%expect{|
type 'a t1 = 'a or_null = Null | This of 'a [@@or_null_reexport]
type 'a t2 = 'a t1 = Null | This of 'a [@@or_null_reexport]
|}]

(* Correct injectivity and variance annotations are accepted. *)

type !'a t = 'a or_null [@@or_null_reexport]

type +'a t = 'a or_null [@@or_null_reexport]

[%%expect{|
type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
|}]

(* Incorrect variance annotation fails. *)

type -'a t = 'a or_null [@@or_null_reexport]

[%%expect{|
Line _, characters 0-44:
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective covariant.
|}]

(* The type's arity must be exactly the same. *)

type ('a, 'b) t = 'b or_null [@@or_null_reexport]

[%%expect{|
Line _, characters 0-49:
Error: This variant or record definition does not match that of type
         'b or_null
       They have different arities.
|}]

(* The type parameter must be actually used. *)

type 'a t = int or_null [@@or_null_reexport]
[%%expect{|
Line _, characters 0-44:
Error: This variant or record definition does not match that of type
         int or_null
       Their parameters differ:
       The type int is not equal to the type 'a
|}]


(* [@@or_null_reexport] handles shadowing correctly. *)

type 'a or_null : value = Null | This of 'a

type 'a t = 'a or_null [@@or_null_reexport]

(* CR layouts v3: this error message is somewhat confusing. *)

[%%expect{|
type 'a or_null = Null | This of 'a
Line _, characters 0-43:
Error: Invalid reexport declaration.
       Type t must be defined equal to the primitive type or_null.
|}]

(* Misplaced attribute warnings are not printed in toplevel. *)

let[@or_null_reexport] foo = 5

[%%expect{|
val foo : int = 5
|}]

(* [private] re-export fails. *)

module Or_null = struct
  type ('a : value) t : value_or_null = private 'a or_null [@@or_null_reexport]
end

[%%expect{|
Line _, characters 2-79:
Error: Invalid reexport declaration.
       Type t must be defined equal to the primitive type or_null.
|}]
