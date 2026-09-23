(* TEST *)

(* [Domain.max_domain_count] goes through the [caml_max_domain_count] stub.
   js_of_ocaml and wasm_of_ocaml are single-domain, so it is 1, and it must be
   consistent with the other domain counts. *)

let () =
  assert (Domain.max_domain_count = 1);
  assert (Domain.recommended_domain_count () = 1);
  assert ((Domain.self () :> int) < Domain.max_domain_count);
  print_endline "OK"
