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



let global_object = "joo_global_object"

let extra_js_files =
  [ "+weak.js"
  ; "+graphics.js"
  ; "+toplevel.js"
  ; "+nat.js"
  ; "+dynlink.js"
  ; "+core_kernel.js"
  ; "+bin_prot.js"
  ; "+bigstring.js"
  ]

(* Optimisation *)

let series = ref None

let stop_profiling () = match !series with
  | Some _x ->
    (* Spacetime.Series.save_and_close x; *)
    series:=None
  | None -> ()

let start_profiling name =
  let path = name ^ ".spacetime" in
  stop_profiling ();
  Format.eprintf "Start profiling %s\n%!" path;
  (* series := Some (Spacetime.Series.create ~path); *)
  ()

let take_snapshot () =
  match !series with
  | None -> ()
  | Some _series ->
    Gc.minor ();
    (* Spacetime.Snapshot.take series; *)
    ()
module Debug = struct
  let debugs : (string * bool ref) list ref = ref []

  let available () = List.map fst !debugs

  let find s =
    let state =
      try
        List.assoc s !debugs
      with Not_found ->
        let state = ref false in
        debugs := (s, state) :: !debugs;
        state
    in
    fun () ->
      if s = "times" then take_snapshot ();
      not !Util.quiet && !state

  let enable s =
    try List.assoc s !debugs := true with Not_found ->
      failwith (Printf.sprintf "The debug named %S doesn't exist" s)

  let disable s =
    try List.assoc s !debugs := false with Not_found ->
      failwith (Printf.sprintf "The debug named %S doesn't exist" s)


  let _ = find "mem"

end

module Optim = struct

  let optims = ref []

  let available () = List.map fst !optims

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
  let stable_var = o ~name:"stable_var" ~default:false
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
  let improved_stacktrace = o ~name:"with-js-error" ~default:false
  let include_cmis = o ~name:"withcmi" ~default: true
  let warn_unused = o ~name:"warn-unused"  ~default: false

  let inline_callgen = o ~name:"callgen" ~default:false

  let check_magic = o ~name:"check-magic-number" ~default:true

  (* this does not optimize properly *)
  let compact_vardecl = o ~name:"vardecl" ~default:false
end

module Param = struct

  let int default =
    default, int_of_string

  let enum : (string * 'a) list -> _ = function
    | ((_,v) :: _) as l ->
      v, (fun x -> List.assoc x l)
    | _ -> assert false


  let params : (string * _) list ref = ref []

  let p ~name ~desc (default,convert) =
    assert(not (List.mem_assoc name !params));
    let state = ref default in
    let set : string -> unit = fun v ->
      try state := convert v with
      | _ -> Util.warn "Warning: malformed option %s=%s. IGNORE@." name v
    in
    params := (name, (set,desc)) :: !params;
    fun () -> !state

  let set s v =
    try fst (List.assoc s !params) v with Not_found ->
      failwith (Printf.sprintf "The option named %S doesn't exist" s)

  let all () = List.map (fun (n,(_,d)) -> n,d) !params

  (* V8 "optimize" switches with less than 128 case.
     60 seams to perform well. *)
  let switch_max_case = p
      ~name:"switch_size"
      ~desc:"set the maximum number of case in a switch"
      (int 60)

  let tailcall_max_depth = p
      ~name:"tc_depth"
      ~desc:"set the maximum number of recursive tailcalls defore returning a trampoline"
      (int 50)

  let constant_max_depth = p
      ~name:"cst_depth"
      ~desc:"set the maximum depth of generated litteral JavaScript values"
      (int 10)


  type tc =
    | TcNone
    | TcTrampoline
    (* | TcWhile *)

  let tc_default = TcTrampoline

  let _tc_all = tc_default :: List.filter ((<>) tc_default) [TcNone;TcTrampoline]

  let tailcall_optim = p
      ~name:"tc"
      ~desc:"Set tailcall optimisation"
      (enum ["trampoline",TcTrampoline;(* default *)
             "none",TcNone])
end
