(* Bound checks are removed when the range analysis proves that the index
   is valid. This checks both the accesses whose checks are removed and
   the ones which must keep them. *)

(* An opaque integer: a read from a mutable array *)
let cell = [| 0 |]

let o x =
  cell.(0) <- x;
  cell.(0)

let test name f =
  match f () with
  | s -> Printf.printf "%s: %s\n" name s
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %s\n" name s

let a = Array.init (o 10) (fun i -> i * i)

let fa = Array.init (o 10) (fun i -> float i)

let s = String.init (o 26) (fun i -> Char.chr (97 + i))

let b = Bytes.make (o 5) 'x'

let ba = Bigarray.(Array1.init int8_unsigned c_layout (o 7) (fun i -> i + 100))

let () =
  (* Checks which can be removed *)
  test "for loop" (fun () ->
      let r = ref 0 in
      for i = 0 to Array.length a - 1 do
        r := !r + a.(i)
      done;
      string_of_int !r);
  test "while loop" (fun () ->
      let r = ref 0. and i = ref 0 in
      while !i < Array.length fa do
        r := !r +. fa.(!i);
        incr i
      done;
      string_of_float !r);
  test "repeated access" (fun () ->
      let c = Array.copy a in
      let i = o 3 in
      c.(i) <- c.(i) + 1;
      string_of_int c.(i));
  test "string loop" (fun () ->
      let n = ref 0 in
      for i = 0 to String.length s - 1 do
        if s.[i] = 'e' || s.[i] = 'z' then n := !n + i
      done;
      string_of_int !n);
  test "bytes loop" (fun () ->
      for i = 0 to Bytes.length b - 1 do
        Bytes.set b i (Char.chr (65 + i))
      done;
      Bytes.to_string b);
  test "bigarray loop" (fun () ->
      let r = ref 0 in
      let i = ref 0 in
      while !i < Bigarray.Array1.dim ba do
        r := !r + ba.{!i};
        incr i
      done;
      string_of_int !r);
  test "checked position" (fun () ->
      let pos = o 4 in
      if pos < 0 || pos >= Array.length a then "out" else string_of_int a.(pos));
  (* Checks which must stay *)
  test "one too far" (fun () ->
      let r = ref 0 in
      for i = 0 to Array.length a do
        r := !r + a.(i)
      done;
      string_of_int !r);
  test "negative start" (fun () ->
      let r = ref 0 in
      for i = o (-1) to Array.length a - 1 do
        r := !r + a.(i)
      done;
      string_of_int !r);
  test "other array" (fun () ->
      let short = Array.make (o 3) 0 in
      let r = ref 0 in
      for i = 0 to Array.length a - 1 do
        r := !r + short.(i)
      done;
      string_of_int !r);
  test "string index too far" (fun () ->
      let pos = o 26 in
      if pos <= String.length s then String.make 1 s.[pos] else "out");
  test "bigarray index too far" (fun () ->
      let pos = o 7 in
      if pos <= Bigarray.Array1.dim ba then string_of_int ba.{pos} else "out");
  test "modified index" (fun () ->
      let r = ref 0 in
      let i = ref 0 in
      while !i < Array.length a do
        i := !i + 2;
        r := !r + a.(!i)
      done;
      string_of_int !r)
