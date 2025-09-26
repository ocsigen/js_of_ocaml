(* TEST
 flambda2;
 {
   native;
 } {
   flags = "-O3";
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   bytecode;
 }
*)

external get_int : int or_null Atomic.t -> int or_null = "%atomic_load"

external exchange_int : int or_null Atomic.t -> int or_null -> int or_null
  = "%atomic_exchange"

external compare_and_exchange_int
  : int or_null Atomic.t -> int or_null -> int or_null -> int or_null
  = "%atomic_compare_exchange"

let () =
  let x = Sys.opaque_identity (Atomic.make Null) in
  match get_int x with
  | Null -> ()
  | This _ -> assert false
;;

let () =
  let x = Sys.opaque_identity (Atomic.make (This 47)) in
  match get_int x with
  | This 47 -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let () =
  let x = Sys.opaque_identity (Atomic.make Null) in
  Atomic.set x (This 5);
  Atomic.set x (This 7);
  match get_int x with
  | This 7 -> ()
  | Null -> assert false
  | This _ -> assert false
;;

let () =
  let x = Sys.opaque_identity (Atomic.make Null) in
  match exchange_int x (This 11) with
  | Null -> ()
  | This _ -> assert false
;;

let () =
  let x = Sys.opaque_identity (Atomic.make (This 11)) in
  (match exchange_int x Null with
  | This 11 -> ()
  | This _ -> assert false
  | Null -> assert false);
  match exchange_int x (This 0) with
  | Null -> ()
  | This _ -> assert false
;;

let () =
  let x = Sys.opaque_identity (Atomic.make Null) in
  match compare_and_exchange_int x (This 5) (This 8) with
  | Null -> ()
  | This _ -> assert false
;;

let () =
  let x = Sys.opaque_identity (Atomic.make Null) in
  (match compare_and_exchange_int x Null (This 42) with
  | Null -> ()
  | This _ -> assert false);
  match compare_and_exchange_int x Null (This 45) with
  | This 42 -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let () =
  let aaa = This (ref "aaa") in
  let bb = Sys.opaque_identity (This (ref "bb")) in
  let zzzzz = This (Sys.opaque_identity (ref "zzzzz")) in
  let arr = This (ref "arr") in
  let y = Atomic.make aaa in
  assert (Atomic.exchange y Null == aaa);
  assert (Atomic.exchange y bb == Null);
  assert (Atomic.compare_exchange y aaa zzzzz == bb);
  assert (Atomic.compare_exchange y aaa Null == bb);
  assert (Atomic.compare_exchange y bb arr == bb);
  assert (Atomic.compare_and_set y arr Null)
;;
