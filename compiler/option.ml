
(****)


(****)

let disabled_lst = ref []

let disabled ?(init=false) s =
  let state = ref init in
  if not (List.mem_assoc s !disabled_lst)
  then disabled_lst := (s, state) :: !disabled_lst;
  fun () -> !state

let set_disabled s =
  try List.assoc s !disabled_lst := true with Not_found ->
   Format.eprintf "%s: no disable option named '%s'@." Sys.argv.(0) s; exit 1

let set_enabled s =
  try List.assoc s !disabled_lst := false with Not_found ->
   Format.eprintf "%s: no disable option named '%s'@." Sys.argv.(0) s; exit 1

(****)


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
    try List.assoc s !debugs := true with Not_found -> ()

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
  let staticeval = o ~name:"staticeval" ~default:false
  let constant =   o ~name:"constant" ~default:true
end
