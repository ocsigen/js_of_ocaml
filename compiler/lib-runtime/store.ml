let tbl = Hashtbl.create 17

let register name content = Hashtbl.add tbl ("+" ^ name) content

let find name = try Some (Hashtbl.find tbl name) with Not_found -> None

let all () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
