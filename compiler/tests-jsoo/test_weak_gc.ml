(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Ephemeron liveness: data referencing its own key (a key <-> data
   cycle) must not keep the key alive once nothing else references it. *)

open Js_of_ocaml

let force_gc : unit -> unit =
  let f =
    Js.Unsafe.js_expr
      {|(function () {
        var v8 = require("node:v8");
        var vm = require("node:vm");
        v8.setFlagsFromString("--expose-gc");
        var g = vm.runInNewContext("gc");
        v8.setFlagsFromString("--no-expose-gc");
        return g;
      })()|}
  in
  fun () -> ignore (Js.Unsafe.fun_call f [||])

let e = Obj.Ephemeron.create 1

let () =
  let key = ref 42 in
  Obj.Ephemeron.set_key e 0 (Obj.repr key);
  (* the data holds the only other reference to the key *)
  Obj.Ephemeron.set_data e (Obj.repr (key, "payload"))

let () =
  let check () =
    force_gc ();
    Printf.printf "key alive: %b\n%!" (Obj.Ephemeron.check_key e 0);
    Printf.printf "data alive: %b\n%!" (Obj.Ephemeron.check_data e)
  in
  (* WeakRef targets are kept alive until the end of the turn in which
     they were created or dereferenced; observe from a later turn *)
  ignore (Js.Unsafe.global##setTimeout (Js.wrap_callback check) 0)
