(* TEST
 include stdlib_stable;
 flags = "-extension layouts_beta";
*)

(* External declarations for unsigned comparison primitives *)
external unsigned_lt : int16# -> int16# -> bool = "%int16#_unsigned_lessthan"
external unsigned_gt : int16# -> int16# -> bool = "%int16#_unsigned_greaterthan"

module Int16 = Stdlib_stable.Int16
module Int16_u = Stdlib_stable.Int16_u

(* Print all individual successful tests; used for debugging, as it will cause
   this test to fail *)
let debug_tests = false

(* Constant seed for repeatable random-testing properties *)
let () = Random.init 42

let to_ocaml_string s = "\"" ^ String.escaped s ^ "\""

type 'a result = {
  expected : 'a;
  actual : 'a;
  equal : 'a -> 'a -> bool;
  to_string : 'a -> string
}

module type Result = sig
  type t
  val equal : t -> t -> bool
  val to_string : t -> string
end

let mk_result' equal to_string = fun ~expected ~actual ->
  { expected; actual; equal; to_string }

let mk_result (type a) (module M : Result with type t = a) =
  mk_result' M.equal M.to_string

let float_result  = mk_result (module Float)
let bool_result   = mk_result (module Bool)
let int_result    = mk_result (module Int)
let int16_result  = mk_result (module Int16)
let string_result = mk_result' String.equal to_ocaml_string

let option_result (type a) (module M : Result with type t = a)  =
  mk_result'
    (Option.equal M.equal)
    (function
      | None   -> "None"
      | Some x -> "Some (" ^ M.to_string x ^ ")")

type 'a generator =
  | Rand  of (unit -> 'a)
  | Const of 'a

let map_generator f = function
  | Rand  r -> Rand (fun () -> f (r ()))
  | Const c -> Const (f c)

type 'a input = {
  generators : 'a generator list;
  to_string  : 'a -> string
}

module type Integer = sig
  type t
  (* Interesting constants *)
  val zero : t
  val one : t
  val minus_one : t
  val max_int : t
  val min_int : t
  (* String generation *)
  val to_string : t -> string
  (* Comparison (for zero-testing) *)
  val equal : t -> t -> bool
  (* Arithmetic (for generating small numbers) *)
  val sub : t -> t -> t
  val shift_left : t -> int -> t
end

let one_thousand (type a) (module I : Integer with type t = a) =
  let open I in
  let i1024 = shift_left one 10 in
  let i16   = shift_left one 4  in
  let i8    = shift_left one 3  in
  sub (sub i1024 i16) i8

let two_thousand (type a) (module I : Integer with type t = a) =
  I.shift_left (one_thousand (module I)) 1

let unit_input =
  { generators = [Const ()]
  ; to_string = Unit.to_string
  }

let bool_input =
  { generators = [Const false; Const true]
  ; to_string = Bool.to_string
  }

let float_input =
  { generators = [ Const 0.
                 ; Const 1.
                 ; Const (-1.)
                 ; Const Float.max_float
                 ; Const Float.min_float
                 ; Const Float.epsilon
                 ; Const Float.nan
                 ; Const Float.infinity
                 ; Const Float.neg_infinity
                 ; Rand (fun () -> Random.float 2000. -. 1000.)
                 ; Rand (fun () -> Int64.float_of_bits (Random.bits64 ()))
                 ]
  ; to_string = Float.to_string
  }

let integer_input
      (type a) (module I : Integer with type t = a)
      rand_range rand_full =
  let rand_small () =
    let i0_to_2000 = rand_range (two_thousand (module I)) in
    I.sub i0_to_2000 (one_thousand (module I))
  in
  { generators = [ Const I.zero
                 ; Const I.one
                 ; Const I.minus_one
                 ; Const I.max_int
                 ; Const I.min_int
                 ; Rand  rand_small
                 ; Rand  rand_full
                 ]
  ; to_string = I.to_string
  }

let nonzero_integer_input
      (type a) (module I : Integer with type t = a)
      rand_range rand_full =
  let { generators; to_string } =
    integer_input (module I) rand_range rand_full
  in
  let generators =
    generators |>
    List.filter_map
      (function
        | Const c ->
            if I.equal c I.zero
            then None
            else Some (Const c)
        | Rand  r ->
            Some (Rand (fun () ->
              let n = ref I.zero in
              while I.equal !n I.zero do
                n := r ()
              done;
              !n)))
  in
  { generators; to_string }

let random_int16 x = Int16.of_int (Random.int (Int16.to_int x))
let random_bits16 x = Int16.of_int (Random.bits ())

let int_input = integer_input (module Int) Random.int Random.bits
let int16_input = integer_input (module Int16) random_int16 random_bits16
let nonzero_int16_input =
  nonzero_integer_input (module Int16) random_int16 random_bits16

let int16_shift_amount_input =
  { generators = List.init 16 (fun c -> Const c)
  ; to_string  = Int.to_string
  }

let int16_string_input =
  { generators = List.map
                   (map_generator Int16.to_string)
                   int16_input.generators
  ; to_string  = to_ocaml_string
  }

let product2 ~f xs ys =
  List.concat_map (fun x ->
    List.map (fun y ->
        f x y)
      ys)
    xs

let two_inputs in1 in2 =
  { generators = product2 in1.generators in2.generators ~f:(fun gen1 gen2 ->
      match gen1, gen2 with
      | Const c1, Const c2 -> Const (c1, c2)
      | Const c1, Rand  r2 -> Rand (fun () -> c1, r2 ())
      | Rand  r1, Const c2 -> Rand (fun () -> r1 (), c2)
      | Rand  r1, Rand  r2 -> Rand (fun () -> r1 (), r2 ())
    )
  ; to_string = fun (x1, x2) ->
      Printf.sprintf "(%s, %s)" (in1.to_string x1) (in2.to_string x2)
  }

let passed { actual; expected; equal; _ } = equal actual expected

let test ?(n=100) name prop { generators; to_string = input_to_string } =
  let test input =
    let {expected; actual; to_string} as result = prop input in
    let print_test outcome =
      Printf.printf "Test %s: %s. Input = %s; expected = %s; actual = %s\n"
        outcome name
        (input_to_string input) (to_string expected) (to_string actual)
    in
    if passed result then begin
      if debug_tests then print_test "succeeded"
    end
    else
      print_test "failed"
  in
  List.iter
    (function
      | Const c -> test c
      | Rand  r -> for _ = 1 to n do test (r ()) done)
    generators

let test_same
      ~input ~result ~apply_expected ~apply_actual
      ?n name expected actual =
  test ?n name
    (fun x ->
       result
         ~expected:(apply_expected expected x)
         ~actual:(apply_actual actual x))
    input

let test_constant ?n name expected actual result =
  test ?n name (fun () -> result ~expected ~actual) unit_input

let test_same_unary ?n name input result expected actual =
  test_same
    ~input
    ~result
    ~apply_expected:Fun.id
    ~apply_actual:Fun.id
    ?n name expected actual

let test_same_binary ?n name input1 input2 result expected actual =
  test_same
    ~input:(two_inputs input1 input2)
    ~result
    ~apply_expected:(fun f (x,y) -> f x y)
    ~apply_actual:(fun f (x,y) -> f x y)
    ?n name expected actual

let test_unary ?n name f fu =
  test_same_unary ?n name int16_input int16_result f
    (fun x -> Int16_u.to_int16 (fu (Int16_u.of_int16 x)))

let test_unary_of ?n name f fu result =
  test_same_unary ?n name int16_input result f
    (fun x -> fu (Int16_u.of_int16 x))

let test_unary_to ?n name f fu input =
  test_same_unary ?n name input int16_result f
    (fun x -> Int16_u.to_int16 (fu x))

let test_binary' ~second_input ?n name f fu =
  test_same_binary ?n name int16_input second_input int16_result f
    (fun x y -> Int16_u.to_int16
                  (fu
                     (Int16_u.of_int16 x)
                     (Int16_u.of_int16 y)))

let test_binary = test_binary' ~second_input:int16_input

let test_division = test_binary' ~second_input:nonzero_int16_input

let test_binary_of ?n name f fu result =
  test_same_binary ?n name int16_input int16_input result f
    (fun x y -> fu
                  (Int16_u.of_int16 x)
                  (Int16_u.of_int16 y))

let test_shift ?n name shift shiftu =
  test_same_binary
    ?n name int16_input int16_shift_amount_input int16_result shift
    (fun x y -> Int16_u.to_int16
                  (shiftu
                     (Int16_u.of_int16 x)
                     y))

let () =
  test_unary     "neg"                 Int16.neg                 Int16_u.neg;
  test_binary    "add"                 Int16.add                 Int16_u.add;
  test_binary    "sub"                 Int16.sub                 Int16_u.sub;
  test_binary    "mul"                 Int16.mul                 Int16_u.mul;
  test_division  "div"                 Int16.div                 Int16_u.div;
  test_division  "unsigned_div"        Int16.unsigned_div        Int16_u.unsigned_div;
  test_division  "rem"                 Int16.rem                 Int16_u.rem;
  test_division  "unsigned_rem"        Int16.unsigned_rem        Int16_u.unsigned_rem;
  test_unary     "succ"                Int16.succ                Int16_u.succ;
  test_unary     "pred"                Int16.pred                Int16_u.pred;
  test_unary     "abs"                 Int16.abs                 Int16_u.abs;
  test_binary    "logand"              Int16.logand              Int16_u.logand;
  test_binary    "logor"               Int16.logor               Int16_u.logor;
  test_binary    "logxor"              Int16.logxor              Int16_u.logxor;
  test_unary     "lognot"              Int16.lognot              Int16_u.lognot;
  test_shift     "shift_left"          Int16.shift_left          Int16_u.shift_left;
  test_shift     "shift_right"         Int16.shift_right         Int16_u.shift_right;
  test_shift     "shift_right_logical" Int16.shift_right_logical Int16_u.shift_right_logical;
  test_unary_to  "of_int"              Int16.of_int              Int16_u.of_int               int_input;
  test_unary_of  "to_int"              Int16.to_int              Int16_u.to_int               int_result;
  test_unary_of  "unsigned_to_int"     Int16.unsigned_to_int     Int16_u.unsigned_to_int      int_result;
  test_unary_to  "of_float"            Int16.of_float            Int16_u.of_float             float_input;
  test_unary_of  "to_float"            Int16.to_float            Int16_u.to_float             float_result;
  test_unary_to  "of_string"           Int16.of_string           Int16_u.of_string            int16_string_input;
  test_unary_of  "to_string"           Int16.to_string           Int16_u.to_string            string_result;
  test_binary_of "compare"             Int16.compare             Int16_u.compare              int_result;
  test_binary_of "unsigned_compare"    Int16.unsigned_compare    Int16_u.unsigned_compare     int_result;
  test_binary_of "equal"               Int16.equal               Int16_u.equal                bool_result;
  test_binary    "min"                 Int16.min                 Int16_u.min;
  test_binary    "max"                 Int16.max                 Int16_u.max;

  (* Explicit unsigned comparison tests with hardcoded expected values *)
  let module I = Int16_u in

  (* Test that -1 (0xFFFF) > 0 when compared as unsigned *)
  assert (I.unsigned_compare (I.minus_one ()) (I.zero ()) = 1);
  assert (I.unsigned_compare (I.zero ()) (I.minus_one ()) = -1);

  (* Test that -32768 (0x8000) > 32767 (0x7FFF) when compared as unsigned *)
  assert (I.unsigned_compare (I.min_int ()) (I.max_int ()) = 1);
  assert (I.unsigned_compare (I.max_int ()) (I.min_int ()) = -1);

  (* Test ordering: when viewed as unsigned:
     0 < 1 < 32767 < 32768 (min_int) < 65535 (minus_one) *)
  assert (I.unsigned_compare (I.zero ()) (I.one ()) = -1);
  assert (I.unsigned_compare (I.one ()) (I.max_int ()) = -1);
  assert (I.unsigned_compare (I.max_int ()) (I.min_int ()) = -1);
  assert (I.unsigned_compare (I.min_int ()) (I.minus_one ()) = -1);

  (* Test equality *)
  assert (I.unsigned_compare (I.zero ()) (I.zero ()) = 0);
  assert (I.unsigned_compare (I.minus_one ()) (I.minus_one ()) = 0);

  (* Test the unsigned_lt primitive directly *)
  assert (unsigned_lt (I.zero ()) (I.minus_one ()) = true); (* 0 < 65535 *)
  assert (unsigned_lt (I.minus_one ()) (I.zero ()) = false); (* 65535 not < 0 *)
  assert (unsigned_lt (I.max_int ()) (I.min_int ()) = true); (* 32767 < 32768 *)
  assert (unsigned_lt (I.min_int ()) (I.max_int ())
    = false); (* 32768 not < 32767 *)

  (* Test unsigned greater than using primitive comparisons *)
  assert (unsigned_gt (I.minus_one ()) (I.zero ()) = true); (* 65535 > 0 *)
  assert (unsigned_gt (I.zero ()) (I.minus_one ()) = false); (* 0 not > 65535 *)
  assert (unsigned_gt (I.min_int ()) (I.max_int ()) = true); (* 32768 > 32767 *)
  assert (unsigned_gt (I.max_int ()) (I.min_int ())
    = false); (* 32767 not > 32768 *)

  ()
