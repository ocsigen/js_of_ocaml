(* Test prop reading. *)

fun (obj : int) -> obj##p;;

fun (obj : < > Js.t) -> obj##p;;

fun (obj : < p : float Js.writeonly_prop > Js.t) -> obj##p + 1;;

fun (obj : < p : float Js.prop > Js.t) -> obj##p + 1;;
