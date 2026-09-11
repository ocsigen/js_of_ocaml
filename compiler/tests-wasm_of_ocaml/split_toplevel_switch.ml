(* Regression test for switch arms assigning a merge local before
   branching to the join, with [toplevel_split_size=5]. The counter
   prevents constant folding; the additions make the arms large enough
   to outline. Avoid printing, which would keep [stdout] live across
   the toplevel and prevent splitting. *)

let counter = ref 0

let next () =
  incr counter;
  !counter

let check ~expected v = assert (v = expected)

(* [next ()] returns 1, 2, 3 and 4 in the successive switches, so each
   arm is selected once, in order. *)

let () =
  check
    ~expected:11
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)

let () =
  check
    ~expected:22
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)

let () =
  check
    ~expected:33
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)

let () =
  check
    ~expected:44
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)
