(* Test prop writing. *)

fun (obj : int) -> obj##p <- 2;;

fun (obj : < > Js.t) -> obj##p <- 2;;

fun (obj : < p : float Js.readonly_prop > Js.t) -> obj##p <- 1;;

fun (obj : < p : float Js.prop > Js.t) -> obj##p <- 1;;

fun (obj : < p : int Js.prop > Js.t) -> (obj##p <- 1) + 1;;
