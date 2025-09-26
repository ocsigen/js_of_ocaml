(* TEST
 include stdlib_stable;
 expect;
*)
#directory "+stdlib_stable"

module Or_null = Stdlib_stable.Or_null

[%%expect {|
module Or_null = Stdlib_stable.Or_null
|}]

let _ = Or_null.null

let _ = Or_null.this 3.14

[%%expect {|
- : 'a Or_null.t = Or_null.Null
- : float Or_null.t = Or_null.This 3.14
|}]

let _ = Or_null.value Null ~default:3

let _ = Or_null.value (This 5) ~default:3

[%%expect {|
- : int = 3
- : int = 5
|}]

let _ = Or_null.get (This "foo")

let _ = Or_null.get Null

[%%expect {|
- : string = "foo"
Exception: Invalid_argument "or_null is Null".
|}]

let _ = Or_null.bind Null (fun x -> This (x + 5))

let _ = Or_null.bind (This 7) (fun x -> Null)

let _ = Or_null.bind (This 3) (fun x -> This (x + 5))

[%%expect {|
- : int or_null = Null
- : 'a or_null = Null
- : int or_null = This 8
|}]

let _ = Or_null.map (( + ) 5) Null

let _ = Or_null.map (( + ) 5) (This 3)

[%%expect {|
- : int or_null = Null
- : int or_null = This 8
|}]

let _ = Or_null.fold ~null:3 ~this:(( + ) 5) Null

let _ = Or_null.fold ~null:3 ~this:(( + ) 5) (This 9)

[%%expect {|
- : int = 3
- : int = 14
|}]

let cell = ref 0

let _ = Or_null.iter (( := ) cell) Null

let _ = !cell

let _ = Or_null.iter (( := ) cell) (This 2)

let _ = !cell

[%%expect
{|
val cell : int ref = {contents = 0}
- : unit = ()
- : int = 0
- : unit = ()
- : int = 2
|}]

let _ = Or_null.is_null Null

let _ = Or_null.is_null (This 3)

let _ = Or_null.is_this Null

let _ = Or_null.is_this (This "42")

[%%expect {|
- : bool = true
- : bool = false
- : bool = false
- : bool = true
|}]

let _ = Or_null.equal ( = ) Null Null

let _ = Or_null.equal ( = ) (This 5) Null

let _ = Or_null.equal ( = ) Null (This 8)

let _ = Or_null.equal ( = ) (This 3) (This 3)

let _ = Or_null.equal ( = ) (This 3) (This 5)

[%%expect
{|
- : bool = true
- : bool = false
- : bool = false
- : bool = true
- : bool = false
|}]

let _ = Or_null.compare Int.compare Null Null

let _ = Or_null.compare Int.compare (This 5) Null

let _ = Or_null.compare Int.compare Null (This 8)

let _ = Or_null.compare Int.compare (This 3) (This 3)

let _ = Or_null.compare Int.compare (This 3) (This 5)

let _ = Or_null.compare Int.compare (This 5) (This 0)

[%%expect {|
- : int = 0
- : int = 1
- : int = -1
- : int = 0
- : int = -1
- : int = 1
|}]

let _ = Or_null.to_result ~null:"null" Null

let _ = Or_null.to_result ~null:"null" (This "this")

[%%expect
{|
- : ('a, string) result = Error "null"
- : (string, string) result = Ok "this"
|}]

let _ = Or_null.to_list Null

let _ = Or_null.to_list (This 1.)

[%%expect {|
- : 'a list = []
- : float list = [1.]
|}]

let _ = Or_null.to_seq Null ()

let _ = Or_null.to_seq (This 5.) ()

let _ = Seq.drop 1 (Or_null.to_seq (This 5.)) ()

[%%expect
{|
- : 'a Seq.node = Seq.Nil
- : float Seq.node = Seq.Cons (5., <fun>)
- : float Seq.node = Seq.Nil
|}]

let _ = Or_null.to_option Null

let _ = Or_null.to_option (This 5)

let _ = Or_null.of_option None

let _ = Or_null.of_option (Some "test")

[%%expect
{|
- : 'a option = None
- : int option = Some 5
- : 'a Or_null.t = Or_null.Null
- : string Or_null.t = Or_null.This "test"
|}]
