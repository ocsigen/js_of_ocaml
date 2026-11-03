(* Deep mutual recursion between functions which do not need CPS. Under
   --effects=double-translation, such functions only exist in
   whole-program compilation (with separate compilation, exported
   functions get a CPS version), and must also be trampolined. *)

let rec even n = if n = 0 then true else odd (n - 1)

and odd n = if n = 0 then false else even (n - 1)

let () = Printf.printf "%b %b\n" (even 1_000_000) (odd 1_000_001)
