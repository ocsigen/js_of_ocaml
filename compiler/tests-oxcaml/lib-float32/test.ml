(* TEST *)

open Stdlib_stable
open Float32.Operators

let is_nan2 (x, y) = Float32.is_nan x && Float32.is_nan y

type test =
  | True of (unit -> bool)
  | False of (unit -> bool)
  | Equal of ((unit -> float32) * float32)
  | Pair of ((unit -> float32 * float32) * (float32 * float32))

let cases =
  [ 1, True (fun () -> Float32.is_finite 1.s)
  ; 2, True (fun () -> Float32.is_finite Float32.pi)
  ; 3, False (fun () -> Float32.is_finite Float32.infinity)
  ; 4, False (fun () -> Float32.is_finite Float32.nan)
  ; 5, True (fun () -> Float32.is_infinite Float32.infinity)
  ; 6, False (fun () -> Float32.is_infinite 1.s)
  ; 7, False (fun () -> Float32.is_infinite Float32.nan)
  ; 8, True (fun () -> Float32.is_nan Float32.nan)
  ; 9, False (fun () -> Float32.is_nan 1.s)
  ; 10, False (fun () -> Float32.is_nan Float32.neg_infinity)
  ; 11, True (fun () -> Float32.is_integer 1.s)
  ; 12, True (fun () -> Float32.is_integer (-1e10s))
  ; 13, False (fun () -> Float32.is_integer 1.5s)
  ; 14, False (fun () -> Float32.is_integer Float32.infinity)
  ; 15, False (fun () -> Float32.is_integer Float32.nan)
  ; 16, Equal ((fun () -> Float32.trunc 1.5s), 1.s)
  ; 17, Equal ((fun () -> Float32.trunc (-1.5s)), -1.s)
  ; 18, Equal Float32.((fun () -> trunc infinity), Float32.infinity)
  ; 19, Equal Float32.((fun () -> trunc neg_infinity), Float32.neg_infinity)
  ; 20, True (fun () -> Float32.(is_nan (trunc nan)))
  ; 21, Equal ((fun () -> Float32.round 0.5s), 1.s)
  ; 22, Equal ((fun () -> Float32.round (-0.5s)), -1.s)
  ; 23, Equal ((fun () -> Float32.round 1.5s), 2.s)
  ; 24, Equal ((fun () -> Float32.round (-1.5s)), -2.s)
  ; ( 25
    , let x = 0x1.000002p+23s in
      (* x + 0.5 rounds to x +. 1. *)
      Equal ((fun () -> Float32.round x), x) )
  ; 26, Equal ((fun () -> Float32.round (Float32.next_after 0.5s 0.s)), 0.s)
  ; 27, Equal Float32.((fun () -> round infinity), Float32.infinity)
  ; 28, Equal Float32.((fun () -> round neg_infinity), Float32.neg_infinity)
  ; 29, True (fun () -> Float32.(is_nan (round nan)))
  ; 30, Equal ((fun () -> Float32.next_after 0x1.FFFFFEp-2s 1.s), 0.5s)
  ; 31, Equal ((fun () -> Float32.next_after 0x1.FFFFFEp-2s 0.s), 0x1.FFFFFCp-2s)
  ; 32, Equal Float32.((fun () -> next_after 0x1.FFFFFEp-2s infinity), 0.5s)
  ; 33, Equal Float32.((fun () -> next_after 0x1.FFFFFEp-2s neg_infinity), 0x1.FFFFFCp-2s)
  ; 34, Equal ((fun () -> Float32.next_after 1.s 1.s), 1.s)
  ; 35, True (fun () -> Float32.(is_nan (next_after nan 1.s)))
  ; 36, True (fun () -> Float32.(is_nan (next_after 3.s nan)))
  ; 37, Equal Float32.((fun () -> succ 0x1.FFFFFEp-2s), 0.5s)
  ; 38, Equal Float32.((fun () -> pred 0.5s), 0x1.FFFFFEp-2s)
  ; 39, True Float32.(fun () -> succ 0.s > 0.s)
  ; 40, True Float32.(fun () -> pred 0.s < 0.s)
  ; 41, Equal Float32.((fun () -> succ max_float), infinity)
  ; 42, Equal Float32.((fun () -> pred (-.max_float)), neg_infinity)
  ; 43, True Float32.(fun () -> succ 0.s < min_float)
  ; 44, Equal Float32.((fun () -> succ infinity), infinity)
  ; 45, Equal Float32.((fun () -> pred neg_infinity), neg_infinity)
  ; 46, True Float32.(fun () -> is_nan (succ nan))
  ; 47, True Float32.(fun () -> is_nan (pred nan))
  ; 48, False (fun () -> Float32.sign_bit 1.s)
  ; 49, True (fun () -> Float32.sign_bit (-1.s))
  ; 50, False (fun () -> Float32.sign_bit 0.s)
  ; 51, True (fun () -> Float32.sign_bit (-0.s))
  ; 52, False (fun () -> Float32.sign_bit Float32.infinity)
  ; 53, True (fun () -> Float32.sign_bit Float32.neg_infinity)
  ; 54, Equal ((fun () -> Float32.min 1.s 2.s), 1.s)
  ; 55, Equal ((fun () -> Float32.min 2.s 1.s), 1.s)
  ; 56, True (fun () -> Float32.(is_nan (min 1.s nan)))
  ; 57, True (fun () -> Float32.(is_nan (min nan 2.s)))
  ; 58, True (fun () -> Float32.(is_nan (min nan nan)))
  ; 59, Equal ((fun () -> 1.s /. Float32.min (-0.s) 0.s), Float32.neg_infinity)
  ; 60, Equal ((fun () -> 1.s /. Float32.min 0.s (-0.s)), Float32.neg_infinity)
  ; 61, Equal ((fun () -> Float32.max 1.s 2.s), 2.s)
  ; 62, Equal ((fun () -> Float32.max 2.s 1.s), 2.s)
  ; 63, True (fun () -> Float32.(is_nan (max 1.s nan)))
  ; 64, True (fun () -> Float32.(is_nan (max nan 2.s)))
  ; 65, True (fun () -> Float32.(is_nan (max nan nan)))
  ; 66, Equal ((fun () -> 1.s /. Float32.max (-0.s) 0.s), Float32.infinity)
  ; 67, Equal ((fun () -> 1.s /. Float32.max 0.s (-0.s)), Float32.infinity)
  ; 68, Pair ((fun () -> Float32.min_max 1.s 2.s), (1.s, 2.s))
  ; 69, Pair ((fun () -> Float32.min_max 2.s 1.s), (1.s, 2.s))
  ; 70, True (fun () -> Float32.(is_nan2 (min_max 1.s nan)))
  ; 71, True (fun () -> Float32.(is_nan2 (min_max nan 2.s)))
  ; 72, True (fun () -> Float32.(is_nan2 (min_max nan nan)))
  ; ( 73
    , Pair
        ( (fun () ->
            let x, y = Float32.min_max (-0.s) 0.s in
            1.s /. x, 1.s /. y)
        , (Float32.neg_infinity, Float32.infinity) ) )
  ; ( 74
    , Pair
        ( (fun () ->
            let x, y = Float32.min_max 0.s (-0.s) in
            1.s /. x, 1.s /. y)
        , (Float32.neg_infinity, Float32.infinity) ) )
  ; 75, Equal ((fun () -> Float32.min_num 1.s 2.s), 1.s)
  ; 76, Equal Float32.((fun () -> min_num 1.s nan), 1.s)
  ; 77, Equal Float32.((fun () -> min_num nan 2.s), 2.s)
  ; 78, True (fun () -> Float32.(is_nan (min_num nan nan)))
  ; 79, Equal ((fun () -> 1.s /. Float32.min_num (-0.s) 0.s), Float32.neg_infinity)
  ; 80, Equal ((fun () -> 1.s /. Float32.min_num 0.s (-0.s)), Float32.neg_infinity)
  ; 81, Equal ((fun () -> Float32.max_num 1.s 2.s), 2.s)
  ; 82, Equal Float32.((fun () -> max_num 1.s nan), 1.s)
  ; 83, Equal Float32.((fun () -> max_num nan 2.s), 2.s)
  ; 84, True (fun () -> Float32.(is_nan (max_num nan nan)))
  ; 85, Equal ((fun () -> 1.s /. Float32.max_num (-0.s) 0.s), Float32.infinity)
  ; 86, Equal ((fun () -> 1.s /. Float32.max_num 0.s (-0.s)), Float32.infinity)
  ; 87, Pair ((fun () -> Float32.min_max_num 1.s 2.s), (1.s, 2.s))
  ; 88, Pair ((fun () -> Float32.min_max_num 2.s 1.s), (1.s, 2.s))
  ; 89, Pair ((fun () -> Float32.(min_max_num 1.s nan)), (1.s, 1.s))
  ; 90, Pair ((fun () -> Float32.(min_max_num nan 1.s)), (1.s, 1.s))
  ; 91, True (fun () -> Float32.(is_nan2 (min_max_num nan nan)))
  ; ( 92
    , Pair
        ( (fun () ->
            let x, y = Float32.min_max_num (-0.s) 0.s in
            1.s /. x, 1.s /. y)
        , (Float32.neg_infinity, Float32.infinity) ) )
  ; ( 93
    , Pair
        ( (fun () ->
            let x, y = Float32.min_max_num 0.s (-0.s) in
            1.s /. x, 1.s /. y)
        , (Float32.neg_infinity, Float32.infinity) ) )
  ]

let () =
  let f (n, test) =
    match test with
    | True p -> Printf.printf "%03d: %s\n%!" n (if p () then "OK" else "FAIL")
    | False p -> Printf.printf "%03d: %s\n%!" n (if p () then "FAIL" else "OK")
    | Equal (f, result) ->
        let v = f () in
        if v = result
        then Printf.printf "%03d: OK\n%!" n
        else
          Printf.printf
            "%03d: FAIL (%h returned instead of %h)\n%!"
            n
            (Float32.to_float v)
            (Float32.to_float result)
    | Pair (f, ((l', r') as result)) ->
        let ((l, r) as v) = f () in
        if v = result
        then Printf.printf "%03d: OK\n%!" n
        else
          Printf.printf
            "%03d: FAIL ((%h, %h) returned instead of (%h, %h))\n%!"
            n
            (Float32.to_float l)
            (Float32.to_float r)
            (Float32.to_float l')
            (Float32.to_float r')
  in
  List.iter f cases
