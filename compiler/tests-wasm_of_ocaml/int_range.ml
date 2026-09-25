(* Integer range analysis: values whose range is known to fit in 31 bits
   are represented as 32-bit integers with portable integers. Each result
   is compared with the same computation performed on values whose range
   is unknown, read back from a mutable array ([Sys.opaque_identity] is
   not opaque to the compiler). The two computations must not share code,
   since the ranges would then be unknown in both. *)

let failed = ref false

let check name a b =
  if a <> b
  then (
    Printf.printf "%s: %d <> %d\n" name a b;
    failed := true)

let cell = [| 0 |]

let o x =
  cell.(0) <- x;
  cell.(0)

let max31 = 0x3fffffff

let min31 = -0x40000000

(* Loop counters and their successors crossing the 31-bit bounds *)
let () =
  let s = ref 0 in
  for i = max31 - 3 to max31 do
    s := !s + (i + 1) + (i - 1)
  done;
  let s' = ref 0 in
  for i = o (max31 - 3) to o max31 do
    s' := !s' + (i + o 1) + (i - o 1)
  done;
  check "up" !s !s'

let () =
  let s = ref 0 in
  for i = max31 - 3 to max31 + 2 do
    s := !s + (i + 1)
  done;
  let s' = ref 0 in
  for i = o (max31 - 3) to o (max31 + 2) do
    s' := !s' + (i + o 1)
  done;
  check "up'" !s !s'

let () =
  let s = ref 0 in
  for i = min31 + 3 downto min31 do
    s := !s + (i - 1)
  done;
  let s' = ref 0 in
  for i = o (min31 + 3) downto o min31 do
    s' := !s' + (i - o 1)
  done;
  check "down" !s !s'

(* Counters bounded by a bound check and by loop tests *)
let a = Array.init 10 (fun i -> i * i)

let () =
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do
    s := !s + a.(i) + (i + max31)
  done;
  let i = ref 0 in
  while !i < 10 do
    s := !s + ((!i + 1) * max31);
    incr i
  done;
  let i = ref 20 in
  while !i <> 3 do
    s := !s + (!i * max31);
    decr i
  done;
  let s' = ref 0 in
  for i = 0 to 9 do
    s' := !s' + o a.(i) + (o i + o max31)
  done;
  for i = 0 to 9 do
    s' := !s' + (o (i + 1) * o max31)
  done;
  for i = 20 downto 4 do
    s' := !s' + (o i * o max31)
  done;
  check "bounded" !s !s'

(* A loop counter bounded by a variable, which is only bounded by the loop
   test when its initial value is on the right side of the bound *)
let () =
  let hi = Array.length a - 1 in
  let s = ref 0 in
  for i = 0 to hi do
    s := !s + Array.unsafe_get a i + (i + 1 + max31)
  done;
  let s' = ref 0 in
  for i = 0 to 9 do
    s' := !s' + o a.(i) + (o i + o 1 + o max31)
  done;
  check "variable bound" !s !s';
  (* If the loop test was taken to bound [i] by [hi], the product would be
     assumed to fit in 31 bits *)
  let hi = if o 0 = 0 then 5 else 6 in
  let i = ref (if o 0 = 0 then 10 else 0) in
  let s = ref 0 in
  while !i <> hi && !i <= 20 do
    s := !s + (!i * 0x8000000);
    incr i
  done;
  let s' = ref 0 in
  for i = 10 to 20 do
    s' := !s' + (o i * o 0x8000000)
  done;
  check "initial value above the bound" !s !s'

(* A loop counter bounded by the bound checks of string accesses *)
let () =
  let str = "hello" in
  let s = ref 0 in
  for i = 0 to o 4 do
    s := !s + Char.code str.[i] + (i + 1 + max31)
  done;
  let s' = ref 0 in
  for i = 0 to 4 do
    s' := !s' + o (Char.code str.[i]) + (o i + o 1 + o max31)
  done;
  check "string" !s !s'

(* With 31-bit integers, an index which passed a bound check can still have
   an arbitrary bit 31, as the result of an overflow *)
let () =
  if max_int = max31
  then
    let j = o max31 + o max31 + 5 in
    let v = a.(j) in
    check "unnormalized index" (a.(j + 1) + v) (o a.(4) + o a.(3))

(* A self tail call becomes a loop whose header is the first block of the
   function: the first value of [i] is passed by the closure, not by a branch.
   The function is called twice so that it is not inlined. *)
let rec entry i n acc = if n = 0 then acc else entry 1 (n - 1) (acc + (i * 0x8000000))

let () =
  check "closure entry" (entry (o 20) 3 0) ((o 20 * o 0x8000000) + (2 * o 0x8000000));
  check "closure entry'" (entry (o 30) 2 0) ((o 30 * o 0x8000000) + o 0x8000000)

(* Fields of constant records, and of mutable records which are
   modified *)
type d =
  { dx : int
  ; dy : int
  }

let dirs = [| { dx = 0; dy = 1 }; { dx = 1; dy = 0 }; { dx = 0; dy = -1 } |]

type m = { mutable v : int }

let muts = [| { v = 1 }; { v = 2 } |]

let () = muts.(1).v <- max31

let () =
  let s = ref 0 in
  for k = 0 to 2 do
    let d = dirs.(k) in
    s := !s + (d.dx + d.dy + max31)
  done;
  for k = 0 to 1 do
    s := !s + (muts.(k).v + max31)
  done;
  check
    "fields"
    !s
    (o 0
    + o 1
    + o max31
    + (o 1 + o 0 + o max31)
    + (o 0 + o (-1) + o max31)
    + (o 1 + o max31)
    + (o max31 + o max31))

(* Arithmetic operations on known ranges, with results on both sides of
   the 31-bit bounds *)
let () =
  List.iter
    (fun (x, y) ->
      let name = Printf.sprintf "arith %d %d" x y in
      check name (x land 0xff) (x land o 0xff);
      check name (x lsr 20) (x lsr o 20);
      check name (x asr 20) (x asr o 20);
      check name (x mod 1000) (x mod o 1000);
      check name (x / 1000000) (x / o 1000000);
      check name (x land 0xffff * (y land 0xffff)) (x land o 0xffff * (y land o 0xffff));
      check
        name
        ((x land 0x3fffffff) + (y land 0x3fffffff))
        ((x land o 0x3fffffff) + (y land o 0x3fffffff));
      check name (x land 0x3fffffff * 3) (x land o 0x3fffffff * o 3);
      check name (-(x land 0x3fffffff) - 2) (-(x land o 0x3fffffff) - o 2);
      check
        name
        (x land 0x3fff0000 lor (y land 0xffff))
        (x land o 0x3fff0000 lor (y land o 0xffff));
      check name ((x land 0x7ff) lsl 20) ((x land o 0x7ff) lsl o 20))
    [ 0, 0
    ; max31, max31
    ; min31, min31
    ; max_int, max_int
    ; min_int, min_int
    ; -1, max_int
    ; (123456 * o 1000000) + 789012, (-98765 * o 1000000) - 432109
    ]

let () = if !failed then exit 1
