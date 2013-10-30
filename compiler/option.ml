(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

(* Optimisation *)

module Debug = struct
  let debugs : (string * bool ref) list ref = ref []

  let find s =
    let state =
      try
        List.assoc s !debugs
      with Not_found ->
        let state = ref false in
        debugs := (s, state) :: !debugs;
        state
    in
    fun () -> !state

  let set s =
    try List.assoc s !debugs := true with Not_found ->
      Format.eprintf "The debug named %S doesn't exist@." s;
      exit 1

end

module Optim = struct

  let optims = ref []

  let o ~name ~default =
    let state =
      try
        List.assoc name !optims
      with Not_found ->
        let state = ref default in
        optims := (name, state) :: !optims;
        state
    in
    fun () -> !state

  let disable s =
    try List.assoc s !optims := false with Not_found -> ()
  let enable s =
    try List.assoc s !optims := true with Not_found -> ()

  let pretty =     o ~name:"pretty" ~default:false
  let debuginfo =  o ~name:"debuginfo" ~default:false
  let deadcode =   o ~name:"deadcode" ~default:true
  let shortvar =  o ~name:"shortvar" ~default:true
  let compact =    o ~name:"compact" ~default:true
  let optcall =    o ~name:"optcall" ~default:true
  let inline =     o ~name:"inline" ~default:true
  let staticeval = o ~name:"staticeval" ~default:true
  let constant =   o ~name:"constant" ~default:true
  let compact_vardecl = o ~name:"vardecl" ~default:true

end
let is_toplevel_ = ref false
let set_toplevel () = is_toplevel_ := true
let is_toplevel () = !is_toplevel_
