(* Higher-order functions are cloned for each combination of known
   functions they are passed (see compiler/lib/clone_higher_order.ml).
   The clones are only created at -O2 and -O3. *)

exception Stop

(* Like [Ctype.try_expand_head]: iterate a step function until it raises *)
let rec iterate step x =
  let y = step x in
  try iterate step y with Stop -> y

let[@inline never] step_a x = if x >= 10 then raise Stop else x + 1

let[@inline never] step_b x = if x >= 100 then raise Stop else x * 2

let[@inline never] step_c x = if String.length x >= 8 then raise Stop else x ^ "c"

(* A function whose identity is not known statically: the call sites
   using it must still use the original function *)
let unknown = ref step_a

let () = if Array.length Sys.argv > 999 then unknown := step_b

(* Two function parameters, only one of which is known at some call
   sites *)
let[@inline never] combine f g x = try f (g x) with Stop -> -1

(* The parameter is called from a nested closure *)
let[@inline never] map_all f l = List.map (fun x -> try Some (f x) with Stop -> None) l

let show_opt = function
  | Some x -> string_of_int x
  | None -> "-"

let () =
  Printf.printf "a: %d\n" (iterate step_a 0);
  Printf.printf "b: %d\n" (iterate step_b 1);
  Printf.printf "c: %s\n" (iterate step_c "");
  Printf.printf "unknown: %d\n" (iterate !unknown 5);
  Printf.printf
    "combine: %d %d %d\n"
    (combine step_a step_b 3)
    (combine step_b step_a 7)
    (combine step_a !unknown 10);
  Printf.printf
    "map_all: %s | %s\n"
    (String.concat " " (List.map show_opt (map_all step_a [ 1; 10; 5 ])))
    (String.concat " " (List.map show_opt (map_all step_b [ 1; 200; 50 ])))
