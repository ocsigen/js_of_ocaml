(* Functions which escape but are also called directly are split into
   a function with unboxed parameters and return value, called
   directly, and a wrapper with the generic calling convention, called
   through closures. Parameters are only unboxed with the type
   checker's hints (OCaml 5.6). *)

(* Unboxed return value *)
let[@inline never] h n = sqrt (float n) +. 1.

let r = ref (fun n -> float n)

(* Unboxed parameters (with the type checker's hints) *)
let[@inline never] scale (a : float) (n : int) (b : float) = (a *. float n) +. b

let s = ref (fun _ _ _ -> 0.)

(* A polymorphic function: its parameter must not be unboxed, even
   if it is a float at the direct call sites *)
let[@inline never] first x _ = x

let t = ref (fun _ _ -> 0.)

(* A raising function with unboxed parameters (the exception has no
   argument, which would box [x]) *)
exception Neg

let[@inline never] check (x : float) = if x < 0. then raise Neg else x +. 1.

let c = ref (fun x -> x)

(* An unknown tail call whose result is a float: the function, which
   is reached through a closure, cannot have its return value unboxed
   by a wrapper, or the stack would grow at each iteration. *)
let[@inline never] rec even n x =
  if n = 0 then x +. 1. else (if n land 1 = 0 then odd else even) (n - 1) (x *. 0.5)

and[@inline never] odd n x =
  if n = 0 then x +. 2. else (if n land 1 = 0 then even else odd) (n - 1) (x *. 0.5)

let () =
  let acc = ref 0. in
  for i = 0 to 10 do
    acc := !acc +. h i +. scale 1.5 i 2.5 +. first 0.25 i
  done;
  if !acc > 3. then r := h;
  if !acc > 3. then s := scale;
  t := first;
  Printf.printf "direct: %.3f\n" !acc;
  Printf.printf "return: %.3f\n" (!r 2);
  Printf.printf "parameters: %.3f\n" (!s 2. 3 4.);
  Printf.printf "polymorphic: %s %.3f\n" (Sys.opaque_identity first "a" 1) (!t 0.5 1);
  (match check 1. with
  | x -> Printf.printf "check: %.3f " x
  | exception Neg -> Printf.printf "check: raised ");
  (match check (-1.) with
  | x -> Printf.printf "%.3f\n" x
  | exception Neg -> Printf.printf "raised\n");
  c := check;
  (match !c (-2.) with
  | x -> Printf.printf "through a ref: %.3f\n" x
  | exception Neg -> Printf.printf "through a ref: raised\n");
  (* Partial application *)
  let p = scale 2. in
  Printf.printf
    "partial application: %.3f %.3f\n"
    (p 3 1.)
    (List.fold_left (fun a b -> a +. p b 0.5) 0. [ 1; 2; 3 ]);
  (* The results are used unboxed, so that the return values of [even]
     and [odd] are not boxed *)
  Printf.printf "tail calls: %.3f\n" (even 1_000_000 1. +. odd 1_000_001 1.)
