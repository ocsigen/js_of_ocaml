(* Test methods. *)

fun (obj : < > Js.t) -> obj = object%js val m = () end
;;

fun (obj : < m : float Js.prop > Js.t) -> obj = object%js val mutable m = 0 end
;;

fun () ->
  (object%js
    val r = 2
    val mutable w = 3.
    method m = ""
  end : < m : int Js.meth; .. > Js.t)
;;


fun () ->
  object%js
    val r = 2
    val _r = 2
  end
;;

fun () ->
  (object%js
    val _r_a = 2
    val _r_b = 2
  end : <_r_a : int Js.readonly_prop > Js.t)
;;

fun () ->
  object%js
    val mutable w = 2
    val mutable _w = 2
  end
;;

fun () ->
  (object%js
    val mutable _w_a = 2
    val mutable _w_b = 2
  end : <_r_a : int Js.prop > Js.t)
;;

fun () ->
  object%js
    method m = ""
    method _m = ""
  end
;;

fun () ->
  (object%js
    method _m_a = ""
    method _m_b = ""
  end : <_m_a : string Js.meth; .. > Js.t)
;;
