(* dummy "let _ = 3" are to work arround ... camlp4 weirdness I guess *)
(* Test prop reading. *)

let x1 = fun (obj : int) -> obj##p;;

let _ = 3;; let _ = 3;;

let x2 = fun (obj : < > Js.t) -> obj##p;;

let _ = 3;; let _ = 3;;

let x3 = fun (obj : < p : float Js.writeonly_prop > Js.t) -> obj##p + 1;;

let _ = 3;; let _ = 3;;

let x4 = fun (obj : < p : float Js.prop > Js.t) -> obj##p + 1;;

let _ = 3;; let _ = 3;;

