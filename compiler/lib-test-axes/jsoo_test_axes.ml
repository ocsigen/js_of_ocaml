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

let target_engine =
  match Sys.getenv_opt "JSOO_TEST_ENGINE" with
  | Some e when not (String.equal e "") -> e
  | _ -> (
      (* [JSOO_ENGINE] selects the engine that runs the generated code (node,
         bun, ...); reflect it here so engine-specific test guards apply. *)
      match Sys.getenv_opt "JSOO_ENGINE" with
      | Some e when not (String.equal e "") -> e
      | _ -> "node")

let host_engine =
  match Sys.backend_type with
  | Native | Bytecode -> "native"
  | Other _ -> target_engine

let node = String.equal host_engine "node"

let quickjs = String.equal host_engine "quickjs"

let bun = String.equal host_engine "bun"

let wasi = String.equal host_engine "wasi"

let effects =
  match Sys.backend_type with
  | Native | Bytecode -> "native"
  | Other _ -> (
      match Jsoo_runtime.Sys.Config.effects () with
      | `Disabled -> "disabled"
      | `Cps -> "cps"
      | `Double_translation -> "double-translation"
      | `Jspi -> "jspi")
