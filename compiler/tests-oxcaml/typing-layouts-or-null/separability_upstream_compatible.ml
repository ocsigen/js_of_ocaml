(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Some [@@unboxed] existentials are non-separable and thus forbidden. *)
(* CR separability: mark them as non-separable instead. *)

type 'a abstract

type packed = P : 'a abstract -> packed [@@unboxed]
[%%expect{|
type 'a abstract
Line _, characters 0-51:
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}]

(* [non_float] annotations allow us to bypass this check, but are erased. *)
type 'a non_float : value mod non_float

type packed = P : 'a non_float -> packed [@@unboxed]

[%%expect{|
type 'a non_float : value mod non_float
Line _, characters 0-52:
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}]

(* [: immediate] gets erased to [@@immediate] and is upstream-compatible. *)

type 'a immediate : immediate

type packed = P : 'a immediate -> packed [@@unboxed]

[%%expect{|
type 'a immediate : immediate
type packed = P : 'a immediate -> packed [@@unboxed]
|}]

(* Annots on existential variables. *)

type exists = E : ('a : value mod non_float) . 'a -> exists [@@unboxed]

[%%expect{|
Line _, characters 0-71:
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}]

(* Non-value layouts. *)

type 'a void : void

type packed_void = P : 'a void -> packed_void [@@unboxed]

[%%expect{|
Line _, characters 15-19:
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

type exists_word = W : ('a : word) . 'a -> exists_word [@@unboxed]

[%%expect{|
Line _, characters 0-66:
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}]
