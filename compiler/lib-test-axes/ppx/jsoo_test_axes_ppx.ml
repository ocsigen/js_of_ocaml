(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Makes the axes of [Jsoo_test_axes] available to [@when] predicates. *)

let () =
  Ppx_devtools_expect.register_axes (function
    | ("target_engine" | "host_engine" | "node" | "quickjs" | "bun" | "wasi" | "effects")
      as x -> Some ("Jsoo_test_axes." ^ x)
    | _ -> None)
