(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flambda2;
 {
   bytecode;
 } {
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

(* test reading/writing atomic fields using block indices *)

module Basic = struct
  type t = { mutable x: int [@atomic]; mutable y: string [@atomic] }

  let () =
    Printf.printf "== Basic idx_atomic ==\n";
    let t = { x = 1; y = "two" } in
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    Printf.printf "(.x) <- 3\n"; Idx_atomic.set t (.x) 3;
    Printf.printf "(.y) <- four\n"; Idx_atomic.set t (.y) "four";
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    ()
end

(* test read-modify-write operations using block indices *)

module Rmw = struct
  type t = { mutable x: int [@atomic]; mutable y: string [@atomic] }

  let () =
    Printf.printf "== idx_atomic read-modify-write ==\n";
    let t = { x = 1; y = "one" } in
    Printf.printf "exchange (.x) 2 = %d\n" (Idx_atomic.exchange t (.x) 2);
    Printf.printf "exchange (.y) two = %s\n" (Idx_atomic.exchange t (.y) "two");
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    Printf.printf "compare_and_set (.x) 2 3 = %b\n"
      (Idx_atomic.compare_and_set t (.x) 2 3);
    Printf.printf "compare_and_set (.x) 2 4 = %b\n"
      (Idx_atomic.compare_and_set t (.x) 2 4);
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    let y = Idx_atomic.get t (.y) in
    Printf.printf "compare_and_set (.y) y three = %b\n"
      (Idx_atomic.compare_and_set t (.y) y "three");
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    Printf.printf "compare_exchange (.x) 3 5 = %d\n"
      (Idx_atomic.compare_exchange t (.x) 3 5);
    Printf.printf "compare_exchange (.x) 3 6 = %d\n"
      (Idx_atomic.compare_exchange t (.x) 3 6);
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "fetch_and_add (.x) 10 = %d\n"
      (Idx_atomic.fetch_and_add t (.x) 10);
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "add (.x) 4\n"; Idx_atomic.add t (.x) 4;
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "sub (.x) 5\n"; Idx_atomic.sub t (.x) 5;
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "logand (.x) 6\n"; Idx_atomic.logand t (.x) 6;
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "logor (.x) 9\n"; Idx_atomic.logor t (.x) 9;
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "logxor (.x) 3\n"; Idx_atomic.logxor t (.x) 3;
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "incr (.x)\n"; Idx_atomic.incr t (.x);
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    Printf.printf "decr (.x)\n"; Idx_atomic.decr t (.x);
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    ()
end

(* test read-modify-write operations on a mixed record *)

module RmwMixed = struct
  type t = { x: int64_u; mutable y: int [@atomic]; z: int64_u }

  let () =
    Printf.printf "== idx_atomic read-modify-write (mixed record) ==\n";
    let t = { x = #42L; y = 1; z = #67L } in
    Printf.printf "exchange (.y) 2 = %d\n" (Idx_atomic.exchange t (.y) 2);
    Printf.printf "compare_and_set (.y) 2 3 = %b\n"
      (Idx_atomic.compare_and_set t (.y) 2 3);
    Printf.printf "fetch_and_add (.y) 10 = %d\n"
      (Idx_atomic.fetch_and_add t (.y) 10);
    Printf.printf "(.x) = %Ld\n" (Int64_u.to_int64 t.x);
    Printf.printf "(.y) = %d\n" (Idx_atomic.get t (.y));
    Printf.printf "(.z) = %Ld\n" (Int64_u.to_int64 t.z);
    ()
end

(* test read-modify-write operations on a stack-allocated record *)

module RmwStack = struct
  type t = { mutable x: int [@atomic] }

  let () =
    Printf.printf "== idx_atomic read-modify-write (stack-allocated record) ==\n";
    let t = stack_ { x = 1 } in
    Printf.printf "exchange (.x) 2 = %d\n" (Idx_atomic.exchange t (.x) 2);
    Printf.printf "compare_and_set (.x) 2 3 = %b\n"
      (Idx_atomic.compare_and_set t (.x) 2 3);
    Printf.printf "compare_exchange (.x) 3 4 = %d\n"
      (Idx_atomic.compare_exchange t (.x) 3 4);
    Printf.printf "fetch_and_add (.x) 10 = %d\n"
      (Idx_atomic.fetch_and_add t (.x) 10);
    Printf.printf "(.x) = %d\n" (Idx_atomic.get t (.x));
    ()
end

(* test reading/writing unboxed singleton record *)

module UnboxedSingleton = struct
  type inner = { y: int }
  type outer = { mutable x: inner# [@atomic] }

  let print_inner ppf (i : inner#) = Printf.fprintf ppf "#{y = %d}" i.#y

  let () =
    Printf.printf "== idx_atomic with unboxed singleton ==\n";
    let t = { x = #{ y = 1 } } in
    let fst = (.x) in
    let snd = (.x.#y) in
    Printf.printf "(.x) = %a\n" print_inner (Idx_atomic.get t fst);
    Printf.printf "(.x.#y) = %d\n" (Idx_atomic.get t snd);
    Printf.printf "(.x) <- #{y = 2}\n"; Idx_atomic.set t fst #{y = 2};
    Printf.printf "(.x) = %a\n" print_inner (Idx_atomic.get t fst);
    Printf.printf "(.x.#y) = %d\n" (Idx_atomic.get t snd);
    Printf.printf "(.x.#y) <- 3\n"; Idx_atomic.set t snd 3;
    Printf.printf "(.x) = %a\n" print_inner (Idx_atomic.get t fst);
    Printf.printf "(.x.#y) = %d\n" (Idx_atomic.get t snd);
    ()

  (* test deepening idx_atomic *)
  let () =
    Printf.printf "== deepening idx_atomic with unboxed singleton ==\n";
    let t = { x = #{ y = 1 } } in
    let fst = (.x) in
    let snd = (.idx_atomic(fst).#y) in
    Printf.printf "(.x) = %a\n" print_inner (Idx_atomic.get t fst);
    Printf.printf "(.idx_atomic((.x)).#y) = %d\n" (Idx_atomic.get t snd);
    Printf.printf "(.x) <- #{y = 2}\n"; Idx_atomic.set t fst #{y = 2};
    Printf.printf "(.x) = %a\n" print_inner (Idx_atomic.get t fst);
    Printf.printf "(.idx_atomic((.x)).#y) = %d\n" (Idx_atomic.get t snd);
    Printf.printf "(.idx_atomic((.x)).#y) <- 3\n"; Idx_atomic.set t snd 3;
    Printf.printf "(.x) = %a\n" print_inner (Idx_atomic.get t fst);
    Printf.printf "(.idx_atomic((.x)).#y) = %d\n" (Idx_atomic.get t snd);
    ()
end

(* test reading/writing from mixed record *)

module Mixed = struct
  type t = { x: int64_u; mutable y: string [@atomic]; z: int64_u }

  let () =
    Printf.printf "== Basic idx_atomic (mixed record) ==\n";
    let t = { x = #42L; y = "two"; z = #67L } in
    Printf.printf "(.x) = %Ld\n" (Int64_u.to_int64 t.x);
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    Printf.printf "(.z) = %Ld\n" (Int64_u.to_int64 t.z);
    Printf.printf "(.y) <- three\n"; Idx_atomic.set t (.y) "three";
    Printf.printf "(.x) = %Ld\n" (Int64_u.to_int64 t.x);
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    Printf.printf "(.z) = %Ld\n" (Int64_u.to_int64 t.z);
end

(* test reading/writing from all-float record *)

module Float = struct
  [@@@warning "-214"]
  type t = { x: float; mutable y: float [@atomic] }

  let () =
    Printf.printf "== Basic idx_atomic (float record) ==\n";
    let t = { x = 2.0; y = 4.0 } in
    Printf.printf "(.x) = %f\n" t.x;
    Printf.printf "(.y) = %f\n" (Idx_atomic.get t (.y));
    Printf.printf "(.y) <- 6.0\n"; Idx_atomic.set t (.y) 6.0;
    Printf.printf "(.x) = %f\n" t.x;
    Printf.printf "(.y) = %f\n" (Idx_atomic.get t (.y));
    ()
end

(* test reading/writing from record with void fields *)

module Void = struct
  [@@@warning "-214"]
  type t = { x: unit#; mutable y: string [@atomic]; z: unit# }

  let () =
    Printf.printf "== Basic idx_atomic (void record) ==\n";
    let t = { x = #(); y = "hello"; z = #() } in
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    Printf.printf "(.y) <- world\n"; Idx_atomic.set t (.y) "world";
    Printf.printf "(.y) = %s\n" (Idx_atomic.get t (.y));
    ()
end

(* test reading/writing from stack-allocated record *)

module Stack = struct
  type t = { mutable x: string [@atomic] }

  (* because printf does not accept format arguments @ local *)
  external globalize_string : string @ local -> string @ unique @@ portable = "%obj_dup"

  let () =
    Printf.printf "== Basic idx_atomic (stack-allocated record) ==\n";
    let t = stack_ { x = "hello" } in
    Printf.printf "(.x) = %s\n" (Idx_atomic.get t (.x) |> globalize_string);
    Printf.printf "(.x) <- \"world\"\n"; Idx_atomic.set t (.x) "world";
    Printf.printf "(.x) = %s\n" (Idx_atomic.get t (.x) |> globalize_string);
    ()
end
