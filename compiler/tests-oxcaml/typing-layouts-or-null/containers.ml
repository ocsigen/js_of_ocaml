(* TEST
 expect;
*)

(* Array type arguments are [any_non_null]: *)
type t_any : any

type should_fail = t_any array

[%%expect{|
type t_any : any
Line _, characters 19-24:
Error: This type t_any should be an instance of type ('a : any_non_null)
       The kind of t_any is any
         because of the definition of t_any at file "containers.ml", line 1, characters 0-16.
       But the kind of t_any must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null array

[%%expect{|
type t_value_or_null : value_or_null
Line _, characters 19-34:
Error: This type t_value_or_null should be an instance of type
         ('a : any_non_null)
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at file "containers.ml", line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_any_non_null : any_non_null

type should_work = t_any_non_null array

[%%expect{|
type t_any_non_null : any_non_null
type should_work = t_any_non_null array
|}]

type t_value : value

type should_work = t_value array

[%%expect{|
type t_value
type should_work = t_value array
|}]

(* Test constructing array with or_null elements *)
let should_fail = [| Null; This 3.4 |]

[%%expect{|
Line _, characters 21-25:
Error: This expression has type 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a or_null must be a subkind of value
         because it's the type of an array element,
         chosen to have kind value.
|}]

type t_any : any

type should_fail = t_any iarray

[%%expect{|
type t_any : any
Line _, characters 19-24:
Error: This type t_any should be an instance of type ('a : any_non_null)
       The kind of t_any is any
         because of the definition of t_any at file "containers.ml", line 1, characters 0-16.
       But the kind of t_any must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null iarray

[%%expect{|
type t_value_or_null : value_or_null
Line _, characters 19-34:
Error: This type t_value_or_null should be an instance of type
         ('a : any_non_null)
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at file "containers.ml", line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_any_non_null : any_non_null

type should_work = t_any_non_null iarray

[%%expect{|
type t_any_non_null : any_non_null
type should_work = t_any_non_null iarray
|}]

type t_value : value

type should_work = t_value iarray

[%%expect{|
type t_value
type should_work = t_value iarray
|}]

let should_fail_iarray = [: Null; This 3.4 :]

[%%expect{|
Line _, characters 28-32:
Error: This expression has type 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a or_null must be a subkind of value
         because it's the type of an array element,
         chosen to have kind value.
|}]

(* List type arguments are [value_or_null]: *)
type t_any : any

type should_fail = t_any list

[%%expect{|
type t_any : any
Line _, characters 19-24:
Error: This type t_any should be an instance of type ('a : value_or_null)
       The layout of t_any is any
         because of the definition of t_any at file "containers.ml", line 1, characters 0-16.
       But the layout of t_any must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null list

[%%expect{|
type t_value_or_null : value_or_null
type should_fail = t_value_or_null list
|}]

type t_any_non_null : any_non_null

type should_fail = t_any_non_null list

[%%expect{|
type t_any_non_null : any_non_null
Line _, characters 19-33:
Error: This type t_any_non_null should be an instance of type
         ('a : value_or_null)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "containers.ml", line 1, characters 0-34.
       But the layout of t_any_non_null must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}]

type t_value : value

type should_work = t_value list

[%%expect{|
type t_value
type should_work = t_value list
|}]

let should_work_list = [ Null; This 3.4 ]

[%%expect{|
val should_work_list : float or_null list = [Null; This 3.4]
|}]

let should_work_list_of_list = [ [This 1; Null]; []; [This 2; This 3] ]

[%%expect{|
val should_work_list_of_list : int or_null list list =
  [[This 1; Null]; []; [This 2; This 3]]
|}]

(* Option type arguments are [value_or_null]: *)

type t_any : any

type should_fail = t_any option

[%%expect{|
type t_any : any
Line _, characters 19-24:
Error: This type t_any should be an instance of type ('a : value_or_null)
       The layout of t_any is any
         because of the definition of t_any at file "containers.ml", line 1, characters 0-16.
       But the layout of t_any must be a sublayout of value
         because the type argument of option has layout value_or_null.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null option

[%%expect{|
type t_value_or_null : value_or_null
type should_fail = t_value_or_null option
|}]

type t_any_non_null : any_non_null

type should_fail = t_any_non_null option

[%%expect{|
type t_any_non_null : any_non_null
Line _, characters 19-33:
Error: This type t_any_non_null should be an instance of type
         ('a : value_or_null)
       The layout of t_any_non_null is any
         because of the definition of t_any_non_null at file "containers.ml", line 1, characters 0-34.
       But the layout of t_any_non_null must be a sublayout of value
         because the type argument of option has layout value_or_null.
|}]

type t_value : value

type should_work = t_value option

[%%expect{|
type t_value
type should_work = t_value option
|}]

let should_work_option1 = Some (This 3.4)
let should_work_option2 = Some Null
let should_work_option3 = None

[%%expect{|
val should_work_option1 : float or_null option = Some (This 3.4)
val should_work_option2 : 'a or_null option = Some Null
val should_work_option3 : 'a option = None
|}]

let should_work_list_option = [ Some (This 3.4); None; Some Null ]

[%%expect{|
val should_work_list_option : float or_null option list =
  [Some (This 3.4); None; Some Null]
|}]
