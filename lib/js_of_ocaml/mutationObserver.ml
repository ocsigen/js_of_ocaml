(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 Stéphane Legrand
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

class type mutationObserverInit = object
  method childList : bool Js.writeonly_prop

  method attributes : bool Js.writeonly_prop

  method characterData : bool Js.writeonly_prop

  method subtree : bool Js.writeonly_prop

  method attributeOldValue : bool Js.writeonly_prop

  method characterDataOldValue : bool Js.writeonly_prop

  method attributeFilter : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
end

class type mutationRecord = object
  method _type : Js.js_string Js.t Js.readonly_prop

  method target : Dom.node Js.t Js.readonly_prop

  method addedNodes : Dom.node Dom.nodeList Js.t Js.readonly_prop

  method removedNodes : Dom.node Dom.nodeList Js.t Js.readonly_prop

  method previousSibling : Dom.node Js.t Js.opt Js.readonly_prop

  method nextSibling : Dom.node Js.t Js.opt Js.readonly_prop

  method attributeName : Js.js_string Js.t Js.opt Js.readonly_prop

  method attributeNamespace : Js.js_string Js.t Js.opt Js.readonly_prop

  method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop
end

class type mutationObserver = object
  method observe : #Dom.node Js.t -> mutationObserverInit Js.t -> unit Js.meth

  method disconnect : unit Js.meth

  method takeRecords : mutationRecord Js.t Js.js_array Js.t Js.meth
end

let empty_mutation_observer_init () : mutationObserverInit Js.t = Js.Unsafe.obj [||]

let mutationObserver = Js.Unsafe.global##._MutationObserver

let is_supported () = Js.Optdef.test mutationObserver

let mutationObserver :
    (   (mutationRecord Js.t Js.js_array Js.t -> mutationObserver Js.t -> unit) Js.callback
     -> mutationObserver Js.t)
    Js.constr =
  mutationObserver

let observe
    ~(node : #Dom.node Js.t)
    ~(f : mutationRecord Js.t Js.js_array Js.t -> mutationObserver Js.t -> unit)
    ?(child_list : bool option)
    ?(attributes : bool option)
    ?(character_data : bool option)
    ?(subtree : bool option)
    ?(attribute_old_value : bool option)
    ?(character_data_old_value : bool option)
    ?(attribute_filter : Js.js_string Js.t list option)
    () : mutationObserver Js.t =
  let opt_iter x f =
    match x with
    | None -> ()
    | Some x -> f x
  in
  let obs = new%js mutationObserver (Js.wrap_callback f) in
  let cfg = empty_mutation_observer_init () in
  let () = opt_iter child_list (fun v -> cfg##.childList := v) in
  let () = opt_iter attributes (fun v -> cfg##.attributes := v) in
  let () = opt_iter character_data (fun v -> cfg##.characterData := v) in
  let () = opt_iter subtree (fun v -> cfg##.subtree := v) in
  let () = opt_iter attribute_old_value (fun v -> cfg##.attributeOldValue := v) in
  let () =
    opt_iter character_data_old_value (fun v -> cfg##.characterDataOldValue := v)
  in
  let () =
    opt_iter attribute_filter (fun l ->
        cfg##.attributeFilter := Js.array (Array.of_list l))
  in
  let () = obs##observe node cfg in
  obs
