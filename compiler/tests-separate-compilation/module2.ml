let () = try raise Not_found with Not_found -> Module1.hello "world"

let () = Lib1_mod1.hello ()
