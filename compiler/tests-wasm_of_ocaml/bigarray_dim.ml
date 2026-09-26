(* Bigarray dimensions are read inline, and the indices of specialized
   bigarray accesses are converted where the conversion can be shared with
   the other uses of the index. This checks the dimensions and accesses
   whose indices come from memory, including out-of-bounds indices that do
   not fit in 32 bits. *)

open Bigarray

(* An opaque integer: a read from a mutable array *)
let cell = [| 0 |]

let o x =
  cell.(0) <- x;
  cell.(0)

(* Out-of-bounds indices, some of which do not fit in 32 bits with 63-bit
   integers *)
let large = if Sys.int_size > 32 then 1 lsl 40 else max_int

let large_neg = if Sys.int_size > 32 then -(1 lsl 31) - 1 else min_int

let just_above_32 = if Sys.int_size > 32 then 1 lsl 32 else max_int - 1

let test name f =
  match f () with
  | s -> Printf.printf "%s: %s\n" name s
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %s\n" name s

let test_dims () =
  let a1 = Array1.create char c_layout (o 7) in
  let f1 = Array1.create float64 fortran_layout (o 3) in
  let a2 = Array2.create int c_layout (o 2) (o 5) in
  let f2 = Array2.create int32 fortran_layout (o 4) (o 1) in
  let a3 = Array3.create int16_signed c_layout (o 3) (o 6) (o 2) in
  let f3 = Array3.create float32 fortran_layout (o 1) (o 2) (o 8) in
  test "dims" (fun () ->
      Printf.sprintf
        "%d %d | %d %d | %d %d | %d %d %d | %d %d %d"
        (Array1.dim a1)
        (Array1.dim f1)
        (Array2.dim1 a2)
        (Array2.dim2 a2)
        (Array2.dim1 f2)
        (Array2.dim2 f2)
        (Array3.dim1 a3)
        (Array3.dim2 a3)
        (Array3.dim3 a3)
        (Array3.dim1 f3)
        (Array3.dim2 f3)
        (Array3.dim3 f3));
  test "dims arithmetic" (fun () ->
      let n = Array1.dim a1 in
      Printf.sprintf "%d %d %d" (n + 1) (n * n) (Array2.dim1 a2 - Array2.dim2 a2))

exception Buffer_short

(* Like bin_prot: the position is kept in a reference, checked against the
   dimension of the buffer, then used as an index *)
let read_byte buf pos_ref =
  let pos = !pos_ref in
  if pos >= Array1.dim buf then raise Buffer_short;
  let c = Char.code (Array1.get buf pos) in
  pos_ref := pos + 1;
  c

let read_all buf pos_ref =
  let sum = ref 0 in
  (try
     while true do
       sum := ((!sum * 31) + read_byte buf pos_ref) land 0xfffff
     done
   with Buffer_short -> ());
  !sum

let test_reads () =
  let buf = Array1.create char c_layout (o 10) in
  for i = 0 to Array1.dim buf - 1 do
    buf.{i} <- Char.chr (65 + i)
  done;
  test "reads" (fun () ->
      let pos_ref = ref (o 2) in
      let s = read_all buf pos_ref in
      Printf.sprintf "%d %d" s !pos_ref);
  List.iteri
    (fun k i ->
      test (Printf.sprintf "read #%d" k) (fun () ->
          let pos_ref = ref (o i) in
          match read_byte buf pos_ref with
          | c -> Printf.sprintf "%d" c
          | exception Buffer_short -> "Buffer_short"))
    [ 0; 9; 10; -1; min_int; max_int; large; large_neg ]

let test_set_get () =
  let a = Array1.create int c_layout (o 6) in
  let f = Array1.create float64 fortran_layout (o 6) in
  let g = Array2.create int32 c_layout (o 3) (o 4) in
  let idx = ref (o 0) in
  while !idx < Array1.dim a do
    a.{!idx} <- !idx * 10;
    f.{!idx + 1} <- float !idx /. 2.;
    idx := !idx + 1
  done;
  for i = 0 to Array2.dim1 g - 1 do
    for j = 0 to Array2.dim2 g - 1 do
      g.{i, j} <- Int32.of_int ((i * 100) + j)
    done
  done;
  test "set and get" (fun () ->
      let r = ref (o 5) in
      Printf.sprintf
        "%d %g %ld %ld"
        a.{!r}
        f.{!r}
        g.{!r - 3, !r - 2}
        g.{Array2.dim1 g - 1, Array2.dim2 g - 1});
  List.iteri
    (fun k i ->
      test (Printf.sprintf "get #%d" k) (fun () -> string_of_int a.{o i});
      test (Printf.sprintf "set #%d" k) (fun () ->
          a.{o i} <- 1;
          "ok");
      test (Printf.sprintf "fortran get #%d" k) (fun () -> string_of_float f.{o i}))
    [ 0; 5; 6; -1; max_int; min_int; just_above_32; just_above_32 + 3 ]

let () =
  test_dims ();
  test_reads ();
  test_set_get ()
