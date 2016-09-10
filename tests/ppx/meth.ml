(* Test methods. *)

fun (obj : int) -> obj##m
;;

fun (obj : < > Js.t) -> obj##m
;;

fun (obj : < m : float Js.prop > Js.t) -> obj##m
;;

fun (obj : < m : float Js.meth > Js.t) -> obj##m + 1
;;

fun (obj : < m : int -> int Js.meth > Js.t) -> obj##m + 1
;;

fun (obj : < m : int Js.meth > Js.t) -> obj##m 1
;;

fun (obj : < m : int -> float Js.meth > Js.t) -> obj##m 1 + 1
;;

fun (obj : < m : 'a. 'a -> unit Js.meth > Js.t) -> obj##m "string"; obj##m (); obj##w
;;
