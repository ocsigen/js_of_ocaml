(* TEST *)

(* Unsigned division and remainder: [caml_int_unsigned_div],
   [caml_int_unsigned_mod], [caml_int64_unsigned_div] and
   [caml_int64_unsigned_mod].  What distinguishes them from the signed
   operators is that a negative left-hand side is read as a large positive
   value, so the tests below all pivot on negative inputs. *)

module Int = Stdlib_stable.Int
module Int64_u = Stdlib_upstream_compatible.Int64_u

(* [int] is 32-bit under js_of_ocaml and wasm_of_ocaml but 63-bit natively,
   so the assertions below are written in terms of [min_int]/[max_int] rather
   than literals.  Unsigned, [-1] is [2^n - 1] and [min_int] is [2^(n-1)]. *)

let test_int () =
  (* Positive arguments agree with the signed operators. *)
  assert (Int.unsigned_div 17 5 = 17 / 5);
  assert (Int.unsigned_rem 17 5 = 17 mod 5);
  assert (Int.unsigned_div 0 5 = 0);
  assert (Int.unsigned_rem 0 5 = 0);
  (* Negative arguments do not: [-1] is [2^n - 1], the largest value. *)
  assert (Int.unsigned_div (-1) 1 = -1);
  assert (Int.unsigned_div (-1) 2 = max_int);
  assert (Int.unsigned_rem (-1) 2 = 1);
  assert (Int.unsigned_rem (-1) (-1) = 0);
  (* [min_int] is [2^(n-1)] unsigned: the smallest value with the top bit
     set. *)
  assert (Int.unsigned_div min_int 1 = min_int);
  assert (Int.unsigned_div min_int 2 = (max_int / 2) + 1);
  assert (Int.unsigned_rem min_int 2 = 0);
  (* A divisor that is negative when signed exceeds any dividend that is
     positive when signed, so the quotient is 0. *)
  assert (Int.unsigned_div 1 (-1) = 0);
  assert (Int.unsigned_rem 1 (-1) = 1);
  assert (Int.unsigned_div max_int min_int = 0);
  (* The division identity holds modulo 2^n, whatever n is. *)
  List.iter
    (fun (a, b) ->
      assert ((Int.unsigned_div a b * b) + Int.unsigned_rem a b = a))
    [ -1, 3; -1, 7; min_int, 3; max_int, 3; -5, 2 ]

let i64 = Int64_u.of_int64

let u64 f x y = Int64_u.to_int64 (f (i64 x) (i64 y))

let test_int64 () =
  assert (u64 Int64_u.unsigned_div 17L 5L = 3L);
  assert (u64 Int64_u.unsigned_rem 17L 5L = 2L);
  (* [-1L] read as unsigned is [2^64 - 1]. *)
  assert (u64 Int64_u.unsigned_div (-1L) 3L = 6148914691236517205L);
  assert (u64 Int64_u.unsigned_rem (-1L) 3L = 0L);
  assert (u64 Int64_u.unsigned_div (-1L) 1L = -1L);
  assert (u64 Int64_u.unsigned_div 1L (-1L) = 0L);
  assert (u64 Int64_u.unsigned_rem 1L (-1L) = 1L);
  (* Int64.min_int is [2^63] unsigned: the smallest value with the top bit
     set. *)
  assert (u64 Int64_u.unsigned_div Int64.min_int 2L = 4611686018427387904L);
  assert (u64 Int64_u.unsigned_rem Int64.min_int 3L = 2L)

let () =
  test_int ();
  test_int64 ();
  print_endline "OK"
