(* TEST
 flags = "-dlambda -dno-unique-ids -no-extension separability";
 expect;
*)

(* Copy of [non_float_array.ml] with [-no-extension separability] for comparison. *)

let mk_gen (x : 'a) = [| x |]
[%%expect{|
val mk_gen : 'a -> 'a array = <fun>
|}]

let get_gen (xs : 'a array) i = xs.(i)
[%%expect{|
val get_gen : 'a array -> int -> 'a = <fun>
|}]

let set_gen (xs : 'a array) x i = xs.(i) <- x
[%%expect{|
val set_gen : 'a array -> 'a -> int -> unit = <fun>
|}]

(* [non_float] arrays are [addrarray]s. Operations on [addrarray]s
   skip checks related to floats.

   Here we can see that our operations are postfixed with [addr]. *)

let mk (type t : value mod non_float) (x : t) = [| x |]
[%%expect{|
val mk : ('t : value mod non_float). 't -> 't array = <fun>
|}]

let get (type t : value mod non_float) (xs : t array) i = xs.(i)
[%%expect{|
val get : ('t : value mod non_float). 't array -> int -> 't = <fun>
|}]

let set (type t : value mod non_float) (xs : t array) x i = xs.(i) <- x

[%%expect{|
val set : ('t : value mod non_float). 't array -> 't -> int -> unit = <fun>
|}]

(* A concrete example. *)

module X : sig
  type t : immutable_data

  val x1 : t
  val x2 : t
end = struct
  type t = { a: string; b: int }

  let x1 = { a = "first"; b = 1 }
  let x2 = { a = "second"; b = 2 }
end

[%%expect{|
module X : sig type t : immutable_data val x1 : t val x2 : t end
|}]

(* Create an [addrarray] and perform [addr] operations on it. *)

let () =
  let xs = Array.make 4 X.x1 in
  xs.(1) <- X.x2;
  xs.(2) <- X.x2;
  assert (xs.(0) = xs.(3));
  assert (xs.(1) = xs.(2));
  assert (not (xs.(0) = xs.(1)))
;;

[%%expect{|

|}]
