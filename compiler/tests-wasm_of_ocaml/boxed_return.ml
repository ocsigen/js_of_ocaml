(* Boxed return of a local function (all call sites known) returning both an
   unboxed number and a value of another type, whose result is only used by
   [Obj.is_int], so that the returned numbers are not boxed eagerly. *)

let () =
  let[@inline never] f b x : Obj.t = if b then Obj.repr (x +. 1.) else Obj.repr b in
  let[@inline never] g b x : Obj.t =
    if b then Obj.repr (Int32.add x 1l) else Obj.repr b
  in
  let[@inline never] h b x : Obj.t =
    if b then Obj.repr (Int64.add x 1L) else Obj.repr b
  in
  let[@inline never] k b x : Obj.t =
    if b then Obj.repr (Nativeint.add x 1n) else Obj.repr b
  in
  for i = 0 to 1 do
    let b = Sys.opaque_identity (i = 0) in
    Printf.printf
      "%b %b %b %b\n"
      (Obj.is_int (f b (Sys.opaque_identity 1.5)))
      (Obj.is_int (g b (Sys.opaque_identity 3l)))
      (Obj.is_int (h b (Sys.opaque_identity 4L)))
      (Obj.is_int (k b (Sys.opaque_identity 5n)))
  done
