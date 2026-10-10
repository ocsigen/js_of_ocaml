(* Bound checks are removed when the range analysis proves that the index
   is valid. This checks both the accesses whose checks are removed and
   the ones which must keep them. *)

(* An opaque integer: a read from a mutable array *)
let cell = [| 0 |]

let o x =
  cell.(0) <- x;
  cell.(0)

type t = { data : int array }

type u = { mutable mdata : int array }

let test name f =
  match f () with
  | s -> Printf.printf "%s: %s\n" name s
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %s\n" name s

let a = Array.init (o 10) (fun i -> i * i)

let fa = Array.init (o 10) (fun i -> float i)

let s = String.init (o 26) (fun i -> Char.chr (97 + i))

let b = Bytes.make (o 5) 'x'

let ba = Bigarray.(Array1.init int8_unsigned c_layout (o 7) (fun i -> i + 100))

(* Polymorphic accesses, which also apply to float arrays *)
let poly_sum to_float arr =
  let r = ref 0. in
  for i = 0 to Array.length arr - 1 do
    r := !r +. to_float arr.(i)
  done;
  !r

let poly_sum_too_far to_float arr =
  let r = ref 0. in
  for i = 0 to Array.length arr do
    r := !r +. to_float arr.(i)
  done;
  !r

(* The loop bound is only computed when [n > 10] *)
let guarded_bound a =
  let n = Array.length a in
  let rec loop i r =
    let r = r + a.(i) in
    if n > 10
    then
      let hi = n - 1 in
      if i <> hi then loop (i + 1) r else r
    else r
  in
  loop 0 0

let guarded_bound_bigarray
    (b : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t) =
  let n = Bigarray.Array1.dim b in
  let rec loop i r =
    let r = r + b.{i} in
    if n > 10
    then
      let hi = n - 1 in
      if i <> hi then loop (i + 1) r else r
    else r
  in
  loop 0 0

type case =
  | A of int array
  | B of int array
  | C of int array

(* The field read in each case has the same value number, but the global
   flow analysis knows a different array in each case *)
let get_case v i =
  match v with
  | A a -> a.(i land 1)
  | B b -> b.(i land 1) * 2
  | C c -> c.(i land 1) + 1

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
  test "polymorphic loop" (fun () ->
      Printf.sprintf "%g %g" (poly_sum float a) (poly_sum Fun.id fa));
  test "downto loop" (fun () ->
      let r = ref 0 in
      for i = Array.length a - 1 downto 0 do
        r := (2 * !r) + a.(i)
      done;
      string_of_int !r);
  test "decreasing while loop" (fun () ->
      let n = ref 0 and i = ref (String.length s - 1) in
      while !i >= 0 do
        if s.[!i] = 'e' || s.[!i] = 'z' then n := !n + !i;
        decr i
      done;
      string_of_int !n);
  test "while i <> length" (fun () ->
      let rec loop i n =
        if i <> String.length s
        then loop (i + 1) (if s.[i] < 'd' then n + i else n)
        else n
      in
      string_of_int (loop 0 0));
  test "repeated computation" (fun () ->
      let x = o 37 in
      if x land 7 < Array.length a then string_of_int a.(x land 7) else "out");
  test "same immutable field" (fun () ->
      let get r i = if i >= 0 && i < Array.length r.data then r.data.(i) else 0 in
      string_of_int (get { data = a } (o 3) + get { data = Array.make (o 5) 1 } (o 4)));
  test "created array" (fun () ->
      let n = o 6 in
      let p = Array.make n 0 in
      for i = 0 to n - 1 do
        p.(i) <- i * i
      done;
      string_of_int p.(n - 1));
  test "constant table" (fun () ->
      let t = Array.init 16 (fun i -> i) in
      let h = "0123456789abcdef" in
      let x = o 0x3a in
      Printf.sprintf "%d %c" t.(x land 15) h.[x land 15]);
  test "last character" (fun () ->
      if String.length s > 0 then String.make 1 s.[String.length s - 1] else "out");
  test "next element" (fun () ->
      let i = o 7 in
      if i >= 0 && i < Array.length a - 1 then string_of_int a.(i + 1) else "out");
  test "unrolled loop" (fun () ->
      let n = Array.length a in
      let r = ref 0 and i = ref 0 in
      while !i + 3 < n do
        r := !r + a.(!i) + a.(!i + 1) + a.(!i + 2) + a.(!i + 3);
        i := !i + 4
      done;
      while !i < n do
        r := !r + a.(!i);
        incr i
      done;
      string_of_int !r);
  test "unrolled recursive loop" (fun () ->
      let n = Array.length a in
      let rec loop i r = if i + 1 < n then loop (i + 2) (r + a.(i) + a.(i + 1)) else r in
      string_of_int (loop 0 0));
  test "copied constant table" (fun () ->
      let t = [| 1l; 2l; 3l; 4l; 5l; 6l; 7l; 8l |] in
      Int32.to_string t.(o 13 land 7));
  test "rows of a literal array of arrays" (fun () ->
      let t = [| [| 1; 2; 3; 4 |]; [| 5; 6; 7; 8 |] |] in
      let r = ref 0 in
      for i = 0 to 1 do
        for j = 0 to 3 do
          r := !r + t.(i).(j)
        done
      done;
      string_of_int !r);
  test "after a loop guarded by <" (fun () ->
      let n = Array.length a - 1 in
      if n >= 0
      then (
        let i = ref 0 in
        while !i < n do
          incr i
        done;
        string_of_int a.(!i))
      else "out");
  test "after a loop guarded by >" (fun () ->
      let j = ref (Array.length a - 1) in
      if !j >= 0
      then (
        while !j > 0 do
          decr j
        done;
        string_of_int a.(!j))
      else "out");
  test "closure" (fun () ->
      let r = ref 0 in
      let get i = a.(i) in
      for i = 0 to Array.length a - 1 do
        r := !r + get i
      done;
      string_of_int !r);
  test "fortran layout bigarray" (fun () ->
      let f = Bigarray.(Array1.init int fortran_layout (o 3) (fun i -> i * 10)) in
      let r = ref 0 in
      for i = 1 to Bigarray.Array1.dim f do
        r := !r + f.{i}
      done;
      string_of_int !r);
  test "constant index" (fun () ->
      if Array.length a > 2 then string_of_int a.(2) else "out");
  test "constant index into a string" (fun () ->
      if String.length s > 0 then String.make 1 s.[0] else "out");
  test "masked index into a long enough string" (fun () ->
      if String.length s >= 4 then String.make 1 s.[o 7 land 3] else "out");
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
  test "downto from length" (fun () ->
      let r = ref 0 in
      for i = Array.length a downto 0 do
        r := !r + a.(i)
      done;
      string_of_int !r);
  test "downto other array" (fun () ->
      let short = Array.make (o 3) 0 in
      let r = ref 0 in
      for i = Array.length a - 1 downto 0 do
        r := !r + short.(i)
      done;
      string_of_int !r);
  test "while i <> length from a larger index" (fun () ->
      let rec loop i n =
        if i <> String.length s
        then loop (i + 1) (if s.[i] < 'd' then n + i else n)
        else n
      in
      string_of_int (loop (o 30) 0));
  test "next index wrapping around" (fun () ->
      let i = o max_int in
      if i >= 0 && i + 1 < String.length s then String.make 1 s.[i + 1] else "out");
  test "same mutable field" (fun () ->
      let r = { mdata = a } in
      let i = o 3 in
      if i < Array.length r.mdata
      then (
        r.mdata <- [||];
        string_of_int r.mdata.(i))
      else "out");
  test "created array too far" (fun () ->
      let n = o 6 in
      let p = Array.make n 0 in
      for i = 0 to n do
        p.(i) <- i
      done;
      "done");
  test "constant table too far" (fun () ->
      let x = o 0x3a in
      String.make 1 "0123456789abcdef".[x land 31]);
  test "next element too far" (fun () ->
      let i = o 9 in
      if i >= 0 && i < Array.length a then string_of_int a.(i + 1) else "out");
  test "loop bound computed under a condition" (fun () ->
      string_of_int (guarded_bound (Array.make (o 0) 0)));
  test "loop bound computed under a condition, bigarray" (fun () ->
      string_of_int
        (guarded_bound_bigarray Bigarray.(Array1.create int8_unsigned c_layout (o 0))));
  test "field read in another case" (fun () ->
      let x = o 7 in
      let va = A [| x; x; x |] and vb = B [| x |] and vc = C [| x; x; x |] in
      let v = if o 1 = 1 then vb else if o 2 = 3 then va else vc in
      Printf.sprintf "%d %d %d" (get_case va (o 1)) (get_case vc (o 1)) (get_case v (o 1)));
  test "index from an exception handler" (fun () ->
      let i = ref 0 in
      (try
         for k = 0 to 20 do
           i := k;
           if k >= Array.length a then raise Exit
         done
       with Exit -> ());
      string_of_int a.(!i));
  test "fortran layout bigarray index 0" (fun () ->
      let f = Bigarray.(Array1.init int fortran_layout (o 3) (fun i -> i * 10)) in
      string_of_int f.{o 0});
  test "constant index too far" (fun () ->
      if Array.length a > 2 then string_of_int a.(10) else "out");
  test "masked index into a short string" (fun () ->
      let s = String.sub s 0 (o 4) in
      if String.length s >= 4 then String.make 1 s.[o 7 land 7] else "out");
  test "other array" (fun () ->
      let short = Array.make (o 3) 0 in
      let r = ref 0 in
      for i = 0 to Array.length a - 1 do
        r := !r + short.(i)
      done;
      string_of_int !r);
  test "polymorphic loop too far" (fun () -> string_of_float (poly_sum_too_far Fun.id fa));
  test "string index too far" (fun () ->
      let pos = o 26 in
      if pos <= String.length s then String.make 1 s.[pos] else "out");
  test "bigarray index too far" (fun () ->
      let pos = o 7 in
      if pos <= Bigarray.Array1.dim ba then string_of_int ba.{pos} else "out");
  test "decremented ref" (fun () ->
      (* The ref is only modified by [decr] *)
      let c = ref 0 in
      let dec = Sys.opaque_identity (fun () -> decr c) in
      dec ();
      if !c < Array.length a then string_of_int a.(!c) else "out");
  test "variable bound" (fun () ->
      (* The loop exit test is against a bound which is computed at each
         iteration, from a different array *)
      let short = Array.make (o 2) 0 in
      let r = ref 0 in
      let rec loop i =
        let arr = if i < 3 then a else short in
        let hi = Array.length arr - 1 in
        if i <> hi
        then (
          if i >= 0 then r := !r + arr.(i);
          loop (i + 1))
      in
      loop (-1);
      string_of_int !r);
  test "modified index" (fun () ->
      let r = ref 0 in
      let i = ref 0 in
      while !i < Array.length a do
        i := !i + 2;
        r := !r + a.(!i)
      done;
      string_of_int !r);
  test "unrolled loop too far" (fun () ->
      let n = Array.length a in
      let r = ref 0 and i = ref 0 in
      while !i + 2 < n do
        r := !r + a.(!i + 3);
        incr i
      done;
      string_of_int !r);
  test "offset wrapping around" (fun () ->
      let i = o (max_int - 1) in
      if i >= 0 && i + 3 < Array.length a then string_of_int a.(i) else "out");
  test "copied constant table too far" (fun () ->
      let t = [| 1l; 2l; 3l; 4l; 5l; 6l; 7l; 8l |] in
      Int32.to_string t.(o 13 land 15));
  test "offset wrapping around downwards" (fun () ->
      let i = o min_int in
      if i < 50
      then
        if i - 1 >= 0
        then if i < Array.length a then string_of_int a.(i) else "out"
        else "out"
      else "out");
  test "offset fact not dominating" (fun () ->
      let i = o 10 in
      if i >= 0
      then
        let r = if i + 1 < Array.length a then 1 else 0 in
        string_of_int (r + a.(i))
      else "out");
  test "unrolled loop over another array" (fun () ->
      let short = Array.make (o 5) 0 in
      let r = ref 0 and i = ref 0 in
      while !i + 3 < Array.length a do
        r := !r + short.(!i + 3);
        i := !i + 4
      done;
      string_of_int !r);
  test "row too short" (fun () ->
      let t = [| [| 1; 2; 3; 4 |]; [| 5; 6; 7 |] |] in
      string_of_int t.(o 1).(3));
  test "modified row" (fun () ->
      let t = [| [| 1; 2; 3; 4 |]; [| 5; 6; 7; 8 |] |] in
      (Sys.opaque_identity t).(1) <- [||];
      string_of_int t.(o 1).(3));
  test "after a loop not entered" (fun () ->
      let e = Array.make (o 0) 0 in
      let n = Array.length e - 1 in
      let i = ref 0 in
      while !i < n do
        incr i
      done;
      string_of_int e.(!i));
  test "after a loop guarded by <=" (fun () ->
      let n = Array.length a - 1 in
      if n >= 0
      then (
        let i = ref 0 in
        while !i <= n do
          incr i
        done;
        string_of_int a.(!i))
      else "out");
  test "after a loop guarded by >=" (fun () ->
      let j = ref (Array.length a - 1) in
      if !j >= 0
      then (
        while !j >= 0 do
          decr j
        done;
        string_of_int a.(!j))
      else "out");
  test "mixed loop guards" (fun () ->
      let n = Array.length a - 1 in
      let rec loop i k =
        let x = a.(i) in
        if k > 0
        then if i <> n then loop (i + 1) (k - 1) else x
        else if i <= n
        then loop (i + 1) (k - 1)
        else x
      in
      string_of_int (loop 0 (o 0)));
  test "row modified by Array.blit" (fun () ->
      let rows = [| [| 0; 1; 1; 0 |]; [| 1; 1; 1; 1 |] |] in
      Array.blit [| [||] |] 0 rows (o 1) 1;
      string_of_int rows.(o 1).(3));
  test "row maybe empty" (fun () ->
      let rows = [| (if o 0 = 1 then [| 1; 2; 3; 4 |] else [||]) |] in
      string_of_int rows.(o 0).(3));
  test "copy of an empty array" (fun () ->
      let e : int array = Obj.obj (Obj.dup (Obj.repr (Array.make (o 0) 0))) in
      string_of_int e.(0));
  List.iter
    (fun j ->
      test (Printf.sprintf "assigned in a try (%d)" j) (fun () ->
          let a = Array.make 11 0 in
          let i = ref 0 in
          let j = o j in
          try
            if j >= 0 then if j < 11 then i := j;
            if o 0 = 0 then raise Exit;
            "no exception"
          with Exit -> string_of_int a.(!i + 1)))
    [ 9; 10 ];
  List.iter
    (fun j ->
      test (Printf.sprintf "assigned index (%d)" j) (fun () ->
          let a = Array.make 3 0 in
          let i = ref 0 in
          let j = o j in
          try
            if j >= 0 then i := j;
            if o 0 = 0 then raise Exit;
            "no exception"
          with Exit -> if !i <= Array.length a then string_of_int a.(!i) else "out"))
    [ 2; 3 ]
