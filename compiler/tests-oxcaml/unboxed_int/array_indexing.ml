(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

module By_int64_u = struct
  module I = Stdlib_upstream_compatible.Int64_u
  module A = struct
    external get : 'a array -> int64# -> 'a =
      "%array_safe_get_indexed_by_int64#"
    external set : 'a array -> int64# -> 'a -> unit =
      "%array_safe_set_indexed_by_int64#"
    external unsafe_get : 'a array -> int64# -> 'a =
      "%array_unsafe_get_indexed_by_int64#"
    external unsafe_set : 'a array -> int64# -> 'a -> unit =
      "%array_unsafe_set_indexed_by_int64#"
  end
end

module By_int32_u = struct
  module I = Stdlib_upstream_compatible.Int32_u
  module A = struct
    external get : 'a array -> int32# -> 'a =
      "%array_safe_get_indexed_by_int32#"
    external set : 'a array -> int32# -> 'a -> unit =
      "%array_safe_set_indexed_by_int32#"
    external unsafe_get : 'a array -> int32# -> 'a =
      "%array_unsafe_get_indexed_by_int32#"
    external unsafe_set : 'a array -> int32# -> 'a -> unit =
      "%array_unsafe_set_indexed_by_int32#"
  end
end

module By_nativeint_u = struct
  module I = Stdlib_upstream_compatible.Nativeint_u

  module A = struct
    external get : 'a array -> nativeint# -> 'a =
      "%array_safe_get_indexed_by_nativeint#"
    external set : 'a array -> nativeint# -> 'a -> unit =
      "%array_safe_set_indexed_by_nativeint#"
    external unsafe_get : 'a array -> nativeint# -> 'a =
      "%array_unsafe_get_indexed_by_nativeint#"
    external unsafe_set : 'a array -> nativeint# -> 'a -> unit =
      "%array_unsafe_set_indexed_by_nativeint#"
  end
end

let check_eq arr g =
  for i = 0 to Array.length arr - 1 do
    assert (g arr i =  arr.(i))
  done

let check_inval f =
  try let _ = f () in assert false with
  | Invalid_argument _ -> ()

let pp = Format.printf

let test_get (g: 'a. 'a array -> int -> 'a) =
  check_eq [| 1; 2; 3; 4; 5; 6; 7|] g;
  check_eq [| "a"; "b"; "c"; "d"|] g;
  check_eq [| 1.; 2.; 3.; 4.; 5.|] g;
  ()

let test_set (g: 'a. 'a array -> int -> 'a -> unit) =
  let fill arr v =
    for i = 0 to Array.length arr - 1 do
      g arr i v; assert(Array.get arr i = v)
    done
  in
  let check_all_eq arr v = assert (Array.for_all (fun x -> x = v) arr) in
  let arr = [| 1; 2; 3; 4; 5; 6; 7|] in
  fill arr 0; check_all_eq arr 0;
  let arr = [| "a"; "b"; "c"; "d"|] in
  fill arr "aaa"; check_all_eq arr "aaa";
  let arr = [| 1.; 2.; 3.; 4.; 5.|] in
  fill arr 0.; check_all_eq arr 0.;
  ()

let test_int64_u () =
  let open By_int64_u in

  test_get (fun arr i -> A.get arr (I.of_int i));
  test_get (fun arr i -> A.unsafe_get arr (I.of_int i));

  test_set (fun arr i -> A.set arr (I.of_int i));
  test_set (fun arr i -> A.unsafe_set arr (I.of_int i));

  (* This is
     0b1000000000000000000000000000000000000000000000000000000000000001
     in binary and should be out of bound. *)
  check_inval (fun () -> A.get [| 1; 2; 3|] (-#9223372036854775807L));
  check_inval (fun () -> A.set [| 1; 2; 3|] (-#9223372036854775807L) 0);
  (* no promises when using unsafe_get. int truncation happens. *)
  let arr = [| 1; 2; 3|] in
  assert (A.unsafe_get arr (-#9223372036854775807L) = 2);
  A.unsafe_set arr (-#9223372036854775807L) 11111;
  assert (A.unsafe_get arr #1L = 11111);
  check_inval (fun () -> A.get [| 1; 2; 3|] (-#1L));
  check_inval (fun () -> A.set [| 1; 2; 3|] (-#1L) 1);
  ()

let test_int32_u () =
  let open By_int32_u in

  test_get (fun arr i -> A.get arr (I.of_int i));
  test_get (fun arr i -> A.unsafe_get arr (I.of_int i));

  test_set (fun arr i -> A.set arr (I.of_int i));
  test_set (fun arr i -> A.unsafe_set arr (I.of_int i));

  check_inval (fun () -> A.get [| 1; 2; 3|] (-#2147483647l));
  check_inval (fun () -> A.set [| 1; 2; 3|] (-#2147483647l) 0);
  check_inval (fun () -> A.get [| 1; 2; 3|] (-#1l));
  check_inval (fun () -> A.set [| 1; 2; 3|] (-#1l) 1);
  ()

let test_nativeint_u () =
  let open By_nativeint_u in

  test_get (fun arr i -> A.get arr (I.of_int i));
  test_get (fun arr i -> A.unsafe_get arr (I.of_int i));

  test_set (fun arr i -> A.set arr (I.of_int i));
  test_set (fun arr i -> A.unsafe_set arr (I.of_int i));

  check_inval (fun () -> A.get [| 1; 2; 3|] (-#0x7fffffffn));
  check_inval (fun () -> A.set [| 1; 2; 3|] (-#0x7fffffffn) 0);
  check_inval (fun () -> A.get [| 1; 2; 3|] (-#1n));
  check_inval (fun () -> A.set [| 1; 2; 3|] (-#1n) 1);
  ()

let () =
  test_int64_u ();
  test_int32_u ();
  test_nativeint_u ();
  ()
