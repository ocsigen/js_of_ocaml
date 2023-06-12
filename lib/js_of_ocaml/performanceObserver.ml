(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2021 Philip White
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

open! Import

class type performanceObserverInit =
  object
    method entryTypes : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
  end

class type performanceEntry =
  object
    method name : Js.js_string Js.t Js.readonly_prop

    method entryType : Js.js_string Js.t Js.readonly_prop

    method startTime : float Js.t Js.readonly_prop

    method duration : float Js.t Js.readonly_prop
  end

class type performanceObserverEntryList =
  object
    method getEntries : performanceEntry Js.t Js.js_array Js.t Js.meth
  end

class type performanceObserver =
  object
    method observe : performanceObserverInit Js.t -> unit Js.meth

    method disconnect : unit Js.meth

    method takeRecords : performanceEntry Js.t Js.js_array Js.t Js.meth
  end

let performanceObserver = Js.Unsafe.global##._PerformanceObserver

let is_supported () = Js.Optdef.test performanceObserver

let performanceObserver :
    (   (performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit) Js.callback
     -> performanceObserver Js.t)
    Js.constr =
  performanceObserver

let observe ~entry_types ~f =
  let entry_types = entry_types |> List.map Js.string |> Array.of_list |> Js.array in
  let performance_observer_init : performanceObserverInit Js.t = Js.Unsafe.obj [||] in
  let () = performance_observer_init##.entryTypes := entry_types in
  let obs = new%js performanceObserver (Js.wrap_callback f) in
  let () = obs##observe performance_observer_init in
  obs
