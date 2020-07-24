let table : (string, string) Hashtbl.t = Hashtbl.create 17

let set k v = Hashtbl.add table k v

let get k = try Some (Hashtbl.find table k) with Not_found -> None
