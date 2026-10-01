(* Local references mutated inside a [try] body and read in the handler.
   Without [-g], ocamlc turns them into mutable variables, and the
   bytecode parser produces [Assign] instructions for them. With
   [--effects=cps], the handler and the continuation of the calls in the
   [try] body become closures, which then read or assign these variables. *)

(* A call to an unknown function is a CPS call. The functions below are
   called several times, so that they are not inlined in the toplevel
   code, which is not in CPS. *)
let g = ref (fun () -> ())

let () = if Array.length Sys.argv > 1000 then g := fun () -> print_newline ()

let unknown_call () = !g ()

(* Try inside a loop *)
let f1 n =
  let acc = ref 0. in
  let r = ref 0.25 in
  for i = 1 to n do
    (try
       r := !r +. 1.;
       unknown_call ();
       if i mod 2 = 0 then failwith "x";
       r := !r *. 3.
     with Failure _ -> acc := !acc +. !r);
    acc := !acc +. !r
  done;
  !acc

(* Assignments before and after a CPS call *)
let f2 x =
  let r = ref x in
  try
    r := !r + 1;
    unknown_call ();
    r := !r * 10;
    failwith "x"
  with Failure _ -> !r

(* A variable holding an exception *)
exception A of int

let f3 () =
  try raise (A 1)
  with e -> (
    let r = ref e in
    try
      unknown_call ();
      r := A 2;
      raise Not_found
    with Not_found -> (
      match !r with
      | A n -> n
      | _ -> 0))

(* The variable is a parameter of the continuation of a CPS call *)
let f4 n =
  let r = ref (List.length (List.init n Fun.id)) in
  try
    unknown_call ();
    r := !r * 2;
    if n > 0 then failwith "x";
    r := 0;
    !r
  with Failure _ -> !r + 1

(* Deeply nested continuations: in JavaScript, some of them are lambda
   lifted, and then receive the variables they capture as parameters *)
let f5 () =
  unknown_call ();
  let r = ref 0 in
  try
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    r := !r + 1;
    unknown_call ();
    failwith "x"
  with Failure _ -> !r

(* A variable bound in a loop: in JavaScript, the handler, created in
   the loop, gets a copy of it *)
exception Found of int

let f6 n =
  let i = ref 0 and acc = ref 0 in
  try
    while true do
      incr i;
      let r = ref (!i * 10) in
      (try
         r := !r + 1;
         if !i = n
         then (
           unknown_call ();
           r := !r + 100;
           raise Exit);
         r := !r + 5
       with Exit -> raise (Found !r));
      acc := !acc + !r
    done;
    0
  with Found v -> (v * 1000) + !acc

let () =
  assert (Float.equal (f1 7) 421.5);
  assert (Float.equal (f1 1) 3.75);
  assert (f2 4 = 50);
  assert (f2 0 = 10);
  assert (f3 () = 2);
  assert (f3 () = 2);
  assert (f4 3 = 7);
  assert (f4 0 = 0);
  assert (f5 () = 30);
  assert (f5 () = 30);
  assert (f6 1 = 111000);
  assert (f6 3 = 131042)
