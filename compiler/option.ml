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



let global_object_inside_jsoo = "joo_global_object"

let global_object = ref "this"

let extra_js_files = ["+weak.js" ; "+graphics.js"; "+toplevel.js"]

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

  let enable s =
    try List.assoc s !debugs := true with Not_found ->
      failwith (Printf.sprintf "The debug named %S doesn't exist" s)

  let disable s =
    try List.assoc s !debugs := false with Not_found ->
      failwith (Printf.sprintf "The debug named %S doesn't exist" s)

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
    try List.assoc s !optims := false with Not_found ->
      failwith (Printf.sprintf "The option named %S doesn't exist" s)

  let enable s =
    try List.assoc s !optims := true with Not_found ->
      failwith (Printf.sprintf "The option named %S doesn't exist" s)

  let pretty =     o ~name:"pretty" ~default:false
  let debuginfo =  o ~name:"debuginfo" ~default:false
  let deadcode =   o ~name:"deadcode" ~default:true
  let shortvar =   o ~name:"shortvar" ~default:true
  let compact =    o ~name:"compact" ~default:true
  let optcall =    o ~name:"optcall" ~default:true
  let inline =     o ~name:"inline" ~default:true
  let staticeval = o ~name:"staticeval" ~default:true
  let share_constant = o ~name:"share" ~default:true
  let strictmode = o ~name:"strict" ~default:true
  let debugger = o ~name:"debugger" ~default:true
  let genprim = o ~name:"genprim" ~default:true
  let excwrap = o ~name:"excwrap" ~default:true
  let include_cmis = o ~name:"withcmi" ~default: true
  let warn_unused = o ~name:"warn-unused"  ~default: false

  let inline_callgen = o ~name:"callgen" ~default:false

  (* this does not optimize properly *)
  let compact_vardecl = o ~name:"vardecl" ~default:false
end

module Tailcall = struct
  type t =
    | TcNone
    | TcTrampoline
    | TcWhile

  let default = TcTrampoline

  let all = default :: List.filter ((<>) default) [TcNone;TcTrampoline(* ;TcWhile *)]

  let to_string = function
    | TcNone -> "none"
    | TcTrampoline -> "trampoline"
    | TcWhile -> "while"

  let of_string =
    let all_string = List.map (fun x -> to_string x,x) all in
    fun x -> List.assoc x all_string


  let set,get =
    let r = ref default in
    (fun x -> r:=x),(fun () -> !r)

  let maximum () = 50

end
