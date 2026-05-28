(* UGEINT bytecode opcode regression test. Exercises the
   [%int_unsigned_greaterequal] and [%int_unsigned_lessequal]
   primitives, which OxCaml lowers to standalone UGEINT ([Cule] is
   rewritten to [Ugeint] with swapped args). Vanilla ocamlc doesn't
   expose these primitives, so this test is OxCaml-only.

   The (5, 5) boundary rows for [uge] and [ule] catch the previous
   bug, where UGEINT was lowered as [Ult (z, y)] = (z < y) instead
   of [not (Ult (y, z))] = (y >= z): 5 >= 5 is true and 5 <= 5 is
   true; both differ from 5 < 5 (false). *)

external uge : int -> int -> bool = "%int_unsigned_greaterequal"

external ult : int -> int -> bool = "%int_unsigned_lessthan"

external ule : int -> int -> bool = "%int_unsigned_lessequal"

external ugt : int -> int -> bool = "%int_unsigned_greaterthan"

let%expect_test _ =
  Printf.printf "uge 5 5     = %b\n" (uge 5 5);
  Printf.printf "uge 5 3     = %b\n" (uge 5 3);
  Printf.printf "uge 3 5     = %b\n" (uge 3 5);
  Printf.printf "uge (-1) 5  = %b\n" (uge (-1) 5);
  Printf.printf "ult 5 5     = %b\n" (ult 5 5);
  Printf.printf "ult 3 5     = %b\n" (ult 3 5);
  Printf.printf "ult 5 3     = %b\n" (ult 5 3);
  Printf.printf "ult (-1) 5  = %b\n" (ult (-1) 5);
  Printf.printf "ule 5 5     = %b\n" (ule 5 5);
  Printf.printf "ule 3 5     = %b\n" (ule 3 5);
  Printf.printf "ule 5 3     = %b\n" (ule 5 3);
  Printf.printf "ule 5 (-1)  = %b\n" (ule 5 (-1));
  Printf.printf "ugt 5 5     = %b\n" (ugt 5 5);
  Printf.printf "ugt 5 3     = %b\n" (ugt 5 3);
  Printf.printf "ugt 3 5     = %b\n" (ugt 3 5);
  Printf.printf "ugt (-1) 5  = %b\n" (ugt (-1) 5);
  [%expect
    {|
    uge 5 5     = true
    uge 5 3     = true
    uge 3 5     = false
    uge (-1) 5  = true
    ult 5 5     = false
    ult 3 5     = true
    ult 5 3     = false
    ult (-1) 5  = false
    ule 5 5     = true
    ule 3 5     = true
    ule 5 3     = false
    ule 5 (-1)  = true
    ugt 5 5     = false
    ugt 5 3     = true
    ugt 3 5     = false
    ugt (-1) 5  = true
    |}]
