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

(* [Gc.finalise_last] callbacks run inside the FinalizationRegistry
   host callback; they must be invoked through caml_callback so that
   the CPS calling convention (--effects cps, used to compile this
   directory) is respected. *)

open Js_of_ocaml

let force_gc : unit -> unit =
  let f =
    Js.Unsafe.js_expr
      {|(function () {
        // bun (JavaScriptCore) has no node:v8 setFlagsFromString; use Bun.gc.
        if (typeof Bun !== "undefined") return function () { Bun.gc(true); };
        var v8 = require("node:v8");
        var vm = require("node:vm");
        v8.setFlagsFromString("--expose-gc");
        var g = vm.runInNewContext("gc");
        v8.setFlagsFromString("--no-expose-gc");
        return g;
      })()|}
  in
  fun () -> ignore (Js.Unsafe.fun_call f [||])

let () =
  ignore
    (Js.Unsafe.js_expr
       {|process.on("uncaughtException", function (e) {
           console.log("uncaught exception");
         })|})

let set_timeout (f : unit -> unit) =
  ignore (Js.Unsafe.global##setTimeout (Js.wrap_callback f) 0)

(* The JS FinalizationRegistry probe and the OCaml finalizer both fire on
   the same collection, but the host does not guarantee an order between
   them, so we buffer the messages and print them sorted once everything
   has run. *)
let lines = ref []

let record s = lines := s :: !lines

let js_probe : 'a -> unit =
  let f =
    Js.Unsafe.js_expr
      {|(function (o, cb) {
        var r = new FinalizationRegistry(function () {
          cb();
        });
        r.register(o, 0);
        globalThis.__probe_keep = r;
      })|}
  in
  fun v ->
    ignore
      (Js.Unsafe.fun_call
         f
         [| Js.Unsafe.inject v
          ; Js.Unsafe.inject (Js.wrap_callback (fun () -> record "js probe fired"))
         |])

let[@inline never] make () =
  let v = Array.make 16 0 in
  js_probe v;
  Gc.finalise_last (fun () -> record "finalised") v

let () = make ()

let rec wait n =
  force_gc ();
  if n = 0
  then (
    List.iter (fun s -> Printf.printf "%s\n" s) (List.sort compare !lines);
    Printf.printf "done\n%!")
  else set_timeout (fun () -> wait (n - 1))

let () = set_timeout (fun () -> wait 3)
