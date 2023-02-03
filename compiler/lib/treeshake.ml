open! Stdlib

let name_of_ident = function
  | Javascript.S { name = Utf8 name; _ } -> name
  | V _ -> assert false

let shake (p : Javascript.program) (keeps : StringSet.t) =
  let defines = Hashtbl.create 17 in
  let defines_rev = Hashtbl.create 17 in
  let uses = Hashtbl.create 17 in
  let uses_rev = Hashtbl.create 17 in
  let keep = Hashtbl.create 17 in
  List.iter p ~f:(fun (s, loc) ->
      let free = new Js_traverse.free in
      let _ = free#program [ s, loc ] in
      let u = free#get_free in
      let define = free#get_def in
      Hashtbl.add uses s u;
      Javascript.IdentSet.iter
        (fun u ->
          let u = name_of_ident u in
          Hashtbl.add uses_rev u s)
        u;
      Hashtbl.add defines s define;
      Javascript.IdentSet.iter
        (fun u ->
          let u = name_of_ident u in
          Hashtbl.add defines_rev u s)
        define);
  let rec mark_live (name : string) =
    match Hashtbl.find_opt defines_rev name with
    | None -> ()
    | Some st ->
        mark_live_st st;
        let l = Hashtbl.find_all uses_rev name in
        List.iter l ~f:(function
            | Javascript.Function_declaration _ | Class_declaration _ -> ()
            | s -> mark_live_st s)
  and mark_live_st st =
    Hashtbl.add keep st ();
    let using = Hashtbl.find uses st in
    Javascript.IdentSet.iter (fun s -> mark_live (name_of_ident s)) using
  in

  StringSet.iter mark_live keeps;
  List.concat_map p ~f:(fun (s, loc) -> if Hashtbl.mem keep s then [ s, loc ] else [])
