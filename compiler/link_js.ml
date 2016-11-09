
let sourceMappingURL = "//# sourceMappingURL="
let sourceMappingURL_base64 = "//# sourceMappingURL=data:application/json;base64,"
let drop_prefix ~prefix s =
  let plen = String.length prefix in
  if plen > String.length s
  then None
  else begin
    try
      for i = 0 to String.length prefix - 1 do
        if String.get s i <> String.get prefix i
        then raise Exit
      done;
      Some (String.sub s plen (String.length s - plen))
    with Exit -> None
  end

let _ = drop_prefix ~prefix:"qwe:" "qwe"

let kind ~resolve_sourcemap_url file line =
  let s =
    match drop_prefix ~prefix:sourceMappingURL_base64 line with
    | Some base64 ->
      `Json_base64 base64
    | None ->
      match drop_prefix ~prefix:sourceMappingURL line with
      | Some url -> `Url url
      | None -> `Other
  in
  match s with
  | `Other -> `Other
  | `Json_base64 base64->
    `Source_map (Yojson.Basic.from_string (B64.decode base64))
  | `Url _ when not resolve_sourcemap_url ->
    `Drop
  | `Url url ->
    let base = Filename.dirname file in
    let ic = open_in (Filename.concat base url) in
    let l = in_channel_length ic in
    let content = really_input_string ic l in
    close_in ic;
    `Source_map (Yojson.Basic.from_string content)
;;

let link ~output ~files ~resolve_sourcemap_url ~source_map =
  let sm = ref [] in
  let line_offset = ref 0 in
  let new_line () =
    output_string output "\n";
    incr line_offset
  in
  let source_offset = ref 0 in
  List.iter (fun file ->
    let ic = open_in file in
    (try
       output_string output (Printf.sprintf "//# 1 %S" file);
       new_line ();
       let start_line = !line_offset in
       while true do
         let line = input_line ic in
         match kind ~resolve_sourcemap_url file line, source_map with
         | `Other, _ ->
           output_string output line;
           new_line ()
         | `Drop, _ -> ()
         | `Source_map _, None ->
           ()
         | `Source_map x, Some _->
           let x = Source_map.of_json x in
           source_offset := List.length x.Source_map.sources;
           sm := (start_line, file, x) :: !sm
       done;
     with End_of_file -> ());
    close_in ic;
    new_line ();
  ) files;
  match source_map with
  | None -> ()
  | Some (file, init_sm) ->
    match Source_map.merge ((0,"",init_sm) :: List.rev !sm) with
    | None -> ()
    | Some sm ->
      match file with
      | None ->
        let json = Source_map.json sm in
        let data = Yojson.Basic.to_string json in
        let s = sourceMappingURL_base64 ^ (B64.encode data) in
        output_string output s
      | Some file ->
        let json = Source_map.json sm in
        Yojson.Basic.to_file file json;
        let s = sourceMappingURL ^ file in
        output_string output s
