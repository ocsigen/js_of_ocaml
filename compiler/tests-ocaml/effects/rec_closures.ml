(* Functions with a CPS version (double translation) in various positions:
   a self-recursive toplevel function, a mutually recursive group, a nested
   recursive function and a nested closure referring to it, each called
   both from direct-style code and from inside a fiber. *)

open Effect
open Effect.Deep

type _ Effect.t += E : int -> int Effect.t

let l = ref []

let rec sum g acc = function
  | [] -> acc
  | x :: r -> sum g (acc + g x) r

let rec even g n = if n = 0 then true else odd g (g n - 1)

and odd g n = if n = 0 then false else even g (g n - 1)

let outer g n =
  let rec loop acc i =
    if i = 0
    then acc
    else (
      let k () = loop (acc + g i) (i - 1) in
      l := (fun () -> ignore (k ())) :: !l;
      k ())
  in
  let count = ref 0 in
  let rec nested i =
    if i = 0
    then !count
    else (
      count := !count + g i;
      nested (i - 1))
  in
  loop 0 n + nested n

let run f =
  try_with
    f
    ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | E n -> Some (fun (k : (a, _) continuation) -> continue k n)
          | _ -> None)
    }

let () =
  let g x = perform (E x) in
  let id x = x in
  Printf.printf "%d %d\n" (sum id 0 [ 1; 2; 3 ]) (run (fun () -> sum g 0 [ 1; 2; 3 ]));
  Printf.printf "%b %b\n" (even id 4) (run (fun () -> odd g 4));
  Printf.printf "%d %d\n" (outer id 5) (run (fun () -> outer g 5));
  Printf.printf "%d\n" (List.length !l)
