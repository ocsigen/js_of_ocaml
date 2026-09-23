(* A function which does not perform any effect, applied to more arguments
   than it takes: the closure it returns may perform one, and must then be
   applied in CPS. *)

open Effect.Deep

let handle f =
  try_with
    f
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Over_application_helper.E n ->
              Some (fun (k : (a, _) continuation) -> continue k (n + 40))
          | _ -> None)
    }

(* Several functions of different arities may be called: the call is not
   exact, and the analysis must take into account that [make] is applied to
   too many arguments. *)
let g =
  if Array.length Sys.argv > 100 then fun () x -> x else Over_application_helper.make

let%expect_test "over-application of a known function" =
  Printf.printf "%d\n" (handle (fun () -> g () 1));
  [%expect {| 42 |}]

(* When the function is not known statically (it comes back from the
   standard library, which is a separate compilation unit in the effect
   profiles), whether it is applied to too many arguments is checked at
   runtime. *)
let%expect_test "over-application of an unknown function" =
  let g = List.hd [ Over_application_helper.make ] in
  Printf.printf "%d\n" (handle (fun () -> g () 1));
  [%expect {| 42 |}]
