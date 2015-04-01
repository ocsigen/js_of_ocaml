(* dummy "let _ = 3" are to work arround ... camlp4 weirdness I guess *)
(* Test methods. *)

let x1 = fun (obj : int) -> obj##m();;

let _ = 3;; let _ = 3;;

let x2 = fun (obj : < > Js.t) -> obj##m();;

let _ = 3;; let _ = 3;;

let x3 = fun (obj : < m : float Js.prop > Js.t) -> obj##m();;

let _ = 3;; let _ = 3;;

let x4 = fun (obj : < m : float Js.meth > Js.t) -> obj##m() + 1;;

let _ = 3;; let _ = 3;;

let x5 = fun (obj : < m : int -> int Js.meth > Js.t) -> obj##m() + 1;;

let _ = 3;; let _ = 3;;

let x6 = fun (obj : < m : int Js.meth > Js.t) -> obj##m(1);;

let _ = 3;; let _ = 3;;

let x7 = fun (obj : < m : int -> float Js.meth > Js.t) -> obj##m(1) + 1;;

let _ = 3;; let _ = 3;;
