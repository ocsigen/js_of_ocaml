(* TEST
*)

let x = Null

let () =
  match x with
  | Null -> ()
  | This _ -> assert false
;;

let y = This 3

let () =
  match y with
  | This 3 -> ()
  | _ -> assert false
;;


external int_as_pointer : int -> int or_null = "%int_as_pointer"

let n = int_as_pointer 0

let () =
  match n with
  | Null -> ()
  | _ -> assert false
;;

external int_as_int : int -> int or_null = "%opaque"

let m = int_as_int 5

let () =
  match m with
  | This 5 -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let x = (Null, This "bar")

let () =
  match x with
  | Null, This "foo" -> assert false
  | Null, This "bar" -> ()
  | _, This "bar" -> assert false
  | Null, _ -> assert false
  | _, _ -> assert false
;;

let y a = fun () -> This a

let d = y 5

let () =
  match d () with
  | This 5 -> ()
  | _ -> assert false
;;

let z = Marshal.to_bytes (This "foo") []

let () =
  match Marshal.from_bytes z 0 with
    | This "foo" -> ()
    | This _ -> assert false
    | Null -> assert false
;;

let w = Marshal.to_bytes Null []

let () =
  match Marshal.from_bytes w 0 with
    | Null -> ()
    | This _ -> assert false
;;

external evil : 'a or_null -> 'a = "%opaque"

let e = This (evil Null)

let () =
  match e with
  | Null -> ()
  | This _ -> assert false
;;

let e' = evil (This 4)

let () =
  match e' with
  | 4 -> ()
  | _ -> assert false
;;

let f a = fun () ->
  match a with
  | This x -> x ^ "bar"
  | Null -> "foo"
;;

let g = f (This "xxx")

let () =
  match g () with
  | "xxxbar" -> ()
  | _ -> assert false
;;

let h = f Null

let () =
  match h () with
  | "foo" -> ()
  | _ -> assert false
;;

let x = ref Null

let () =
  match !x with
  | Null -> ()
  | _ -> assert false
;;

let () = x := This "foo"

let () =
  match !x with
  | This "foo" -> ()
  | _ -> assert false
;;

let () = x := Null

let () =
  match !x with
  | Null -> ()
  | _ -> assert false
;;

let () =
  assert (Null = Null);
  assert (This 4 = This 4);
  assert (Null <> This 4);
  assert (This 8 <> Null);
  assert (This 4 <> This 5);
;;

let () =
  assert (compare Null Null = 0);
  assert (compare (This 4) (This 4) = 0);
  assert (compare Null (This 4) < 0);
  assert (compare (This 8) Null > 0);
  assert (compare (This 4) (This 5) < 0);
  assert (compare (This "abc") (This "xyz") <> 0);
  assert (compare (This "xyz") (This "xyz") = 0);
;;
