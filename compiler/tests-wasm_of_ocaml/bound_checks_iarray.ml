(* An immutable array of constants is compiled to a constant, which the
   global flow analysis does not track: the range analysis must take its
   elements and its length into account. *)

let cell = [| 0 |]

let o x =
  cell.(0) <- x;
  cell.(0)

let test name f =
  match f () with
  | s -> Printf.printf "%s: %s\n" name s
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %s\n" name s

let weights : int iarray = [| 0x3fffffff; 1; 5 |]

let sum () =
  let s = ref 0 in
  for i = 0 to Iarray.length weights - 1 do
    s := !s + Iarray.get weights i
  done;
  !s

let table = Array.make 4 0

let ia : int iarray = [| 0x3fffffff; 7 |]

let () =
  test "sum of constants" (fun () ->
      let s = sum () in
      Printf.sprintf "%d %b %d" s (s < 0) (s / 2));
  test "sum of constants as index" (fun () -> string_of_int table.(sum ()));
  test "element or zero" (fun () ->
      let c = o 1 = 1 and i = o 0 in
      let r = (if c then Iarray.get ia i else 0) + 1 in
      Printf.sprintf "%d %b" r (r < 0));
  test "element as index" (fun () ->
      let c = o 1 = 1 and i = o 1 in
      let b = Array.make 2 42 in
      string_of_int b.(if c then Iarray.get ia i else 0));
  test "block or shorter constant" (fun () ->
      let c = o 1 = 2 and y = o 1 in
      let t : int iarray = if c then [| y; y; y; y |] else [| 5 |] in
      string_of_int (Iarray.get t 3));
  test "block or constant" (fun () ->
      let c = o 1 = 1 and y = o 1 in
      let t : int iarray = if c then [| y; y; y; y |] else [| 5 |] in
      string_of_int (Iarray.get t 3));
  test "rows of a block or of a constant" (fun () ->
      let c = o 1 = 2 and y = o 1 and i = o 1 in
      let r1 : int iarray = [| y; y; y; y |] in
      let t : int iarray iarray = if c then [| r1; r1 |] else [| [| 5 |]; [| 6 |] |] in
      string_of_int (Iarray.get (Iarray.get t i) 3));
  test "constant table" (fun () ->
      let t : int iarray = [| 1; 2; 3; 4; 5; 6; 7; 8 |] in
      string_of_int (Iarray.get t (o 13 land 7)))
