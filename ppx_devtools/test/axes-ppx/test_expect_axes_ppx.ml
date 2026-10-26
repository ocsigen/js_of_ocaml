(* Registered in this order, so [Test_axes_override] takes precedence over
   [Test_axes], which takes precedence over the predefined axes. *)

let () =
  Ppx_devtools_expect.register_axes (function
    | ("enabled" | "disabled" | "flavour" | "level" | "version") as x ->
        Some ("Test_axes." ^ x)
    | _ -> None)

let () =
  Ppx_devtools_expect.register_axes (function
    | ("flavour" | "extra" | "compare" | "cygwin") as x -> Some ("Test_axes_override." ^ x)
    | _ -> None)
