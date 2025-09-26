(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Tests for [immediate_or_null]. *)

(* The [immediate_or_null] layout. *)
type t_immediate_or_null : immediate_or_null
[%%expect{|
type t_immediate_or_null : immediate_or_null
|}]

(* The subkind relation. *)

type ('a : immediate_or_null) accept_immediate_or_null
[%%expect{|
type ('a : immediate_or_null) accept_immediate_or_null
|}]

type should_work = t_immediate_or_null accept_immediate_or_null
[%%expect{|
type should_work = t_immediate_or_null accept_immediate_or_null
|}]

type should_work = int accept_immediate_or_null
[%%expect{|
type should_work = int accept_immediate_or_null
|}]

type ('a : immediate) accept_immediate
[%%expect{|
type ('a : immediate) accept_immediate
|}]

type should_fail = t_immediate_or_null accept_immediate
[%%expect{|
Line _, characters 19-38:
Error: This type t_immediate_or_null should be an instance of type
         ('a : immediate)
       The kind of t_immediate_or_null is immediate_or_null
         because of the definition of t_immediate_or_null at file "immediate.ml", line 1, characters 0-44.
       But the kind of t_immediate_or_null must be a subkind of immediate
         because of the definition of accept_immediate at file "immediate.ml", line 1, characters 0-38.
|}]

type ('a : value) accept_value
[%%expect{|
type 'a accept_value
|}]

type should_fail = t_immediate_or_null accept_value
[%%expect{|
Line _, characters 19-38:
Error: This type t_immediate_or_null should be an instance of type
         ('a : value)
       The kind of t_immediate_or_null is immediate_or_null
         because of the definition of t_immediate_or_null at file "immediate.ml", line 1, characters 0-44.
       But the kind of t_immediate_or_null must be a subkind of value
         because of the definition of accept_value at file "immediate.ml", line 1, characters 0-30.
|}]

type ('a : value_or_null) accept_value_or_null

type should_work = t_immediate_or_null accept_value_or_null
[%%expect{|
type 'a accept_value_or_null
type should_work = t_immediate_or_null accept_value_or_null
|}]

(* [int or_null] fits into [immediate_or_null]: *)


type int_or_null : immediate_or_null = int or_null
[%%expect{|
type int_or_null = int or_null
|}]

type should_work = int_or_null accept_immediate_or_null
[%%expect{|
type should_work = int_or_null accept_immediate_or_null
|}]

(* CR layouts v2.8: this is a bug in principal inference with with-kinds. *)

type should_work = int or_null accept_immediate_or_null
[%%expect{|
type should_work = int or_null accept_immediate_or_null
|}, Principal{|
Line 1, characters 19-30:
1 | type should_work = int or_null accept_immediate_or_null
                       ^^^^^^^^^^^
Error: This type "int or_null" should be an instance of type
         "('a : immediate_or_null)"
       The kind of int or_null is immediate_or_null with int
         because it is the primitive immediate_or_null type or_null.
       But the kind of int or_null must be a subkind of immediate_or_null
         because of the definition of accept_immediate_or_null at line 1, characters 0-54.
|}]

(* Values. *)

type ('a : immediate_or_null) myref = { mutable v : 'a }

external read_imm : ('a : immediate_or_null) . 'a myref -> 'a = "%field0"
external write_imm : ('a : immediate_or_null) . 'a myref -> 'a -> unit = "%setfield0"
external equal : ('a : immediate_or_null) . 'a -> 'a -> bool = "%equal"

[%%expect{|
type ('a : immediate_or_null) myref = { mutable v : 'a; }
external read_imm : ('a : immediate_or_null). 'a myref -> 'a = "%field0"
external write_imm : ('a : immediate_or_null). 'a myref -> 'a -> unit
  = "%setfield0"
external equal : ('a : immediate_or_null). 'a -> 'a -> bool = "%equal"
|}]

(* CR layouts v2.8: this is a bug in principal inference with with-kinds. *)

let () =
  let r = { v = (Null : int or_null) } in
  let x = read_imm r in
  assert (equal x Null);
  write_imm r (This 5);
  assert (equal r.v (This 5))
;;

[%%expect{|
|}, Principal{|
Line 2, characters 16-36:
2 |   let r = { v = (Null : int or_null) } in
                    ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int or_null"
       but an expression was expected of type "('a : immediate_or_null)"
       The kind of int or_null is immediate_or_null with int
         because it is the primitive immediate_or_null type or_null.
       But the kind of int or_null must be a subkind of immediate_or_null
         because of the definition of myref at line 1, characters 0-56.
|}]
