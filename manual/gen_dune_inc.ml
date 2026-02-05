type desc = string * string * string list

(* --- Minimal S-expression parser for dune files --- *)

type sexp =
  | Atom of string
  | List of sexp list

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Bytes.to_string s)

let parse_sexps s =
  let pos = ref 0 in
  let len = String.length s in
  let rec skip_ws () =
    while
      !pos < len
      &&
      match s.[!pos] with
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false
    do
      incr pos
    done;
    if !pos < len && s.[!pos] = ';'
    then begin
      while !pos < len && s.[!pos] <> '\n' do
        incr pos
      done;
      skip_ws ()
    end
  in
  let read_quoted () =
    incr pos;
    let buf = Buffer.create 64 in
    while !pos < len && s.[!pos] <> '"' do
      if s.[!pos] = '\\' && !pos + 1 < len
      then begin
        incr pos;
        Buffer.add_char buf s.[!pos];
        incr pos
      end
      else begin
        Buffer.add_char buf s.[!pos];
        incr pos
      end
    done;
    if !pos < len then incr pos;
    Atom (Buffer.contents buf)
  in
  let read_atom () =
    let start = !pos in
    while
      !pos < len
      &&
      match s.[!pos] with
      | ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' -> false
      | _ -> true
    do
      incr pos
    done;
    Atom (String.sub s start (!pos - start))
  in
  let rec read_sexp () =
    skip_ws ();
    if !pos >= len
    then failwith "unexpected end of input"
    else if s.[!pos] = '('
    then begin
      incr pos;
      let items = ref [] in
      skip_ws ();
      while !pos < len && s.[!pos] <> ')' do
        items := read_sexp () :: !items;
        skip_ws ()
      done;
      if !pos < len then incr pos;
      List (List.rev !items)
    end
    else if s.[!pos] = '"'
    then read_quoted ()
    else read_atom ()
  in
  let result = ref [] in
  skip_ws ();
  while !pos < len do
    result := read_sexp () :: !result;
    skip_ws ()
  done;
  List.rev !result

let has_field name value fields =
  List.exists
    (function
      | List [ Atom n; Atom v ] -> n = name && v = value
      | _ -> false)
    fields

let has_key name fields =
  List.exists
    (function
      | List (Atom n :: _) -> n = name
      | _ -> false)
    fields

(* Extract file deps from (alias (name default) (deps ...)) stanzas,
   skipping those with enabled_if (which are wasm-only aliases). *)
let extract_default_alias_deps sexps =
  List.fold_left
    (fun acc sexp ->
      match sexp with
      | List (Atom "alias" :: fields)
        when has_field "name" "default" fields && not (has_key "enabled_if" fields) ->
          List.fold_left
            (fun acc field ->
              match field with
              | List (Atom "deps" :: dep_list) ->
                  List.fold_left
                    (fun acc dep ->
                      match dep with
                      | Atom f when not (String.contains f '%') -> f :: acc
                      | List [ Atom "glob_files"; Atom pattern ] -> (
                          match String.rindex_opt pattern '/' with
                          | Some idx -> String.sub pattern 0 (idx + 1) :: acc
                          | None -> acc)
                      | List [ Atom "source_tree"; Atom dir ] -> (dir ^ "/") :: acc
                      | _ -> acc)
                    acc
                    dep_list
              | _ -> acc)
            acc
            fields
      | _ -> acc)
    []
    sexps
  |> List.rev

let is_dir x =
  let len = String.length x in
  len > 0 && String.get x (len - 1) = '/'

(* Collect filenames passed via --file in build_runtime_flags.
   These files are embedded into the .bc.js and do not need to be installed. *)
let extract_embedded_files sexps =
  let files = ref [] in
  let rec scan_list = function
    | [] -> ()
    | Atom "--file" :: Atom f :: rest ->
        let len = String.length f in
        let name =
          if len > 7 && String.sub f 0 6 = "%{dep:" && f.[len - 1] = '}'
          then String.sub f 6 (len - 7)
          else f
        in
        files := name :: !files;
        scan_list rest
    | _ :: rest -> scan_list rest
  in
  let rec walk = function
    | Atom _ -> ()
    | List items ->
        scan_list items;
        List.iter walk items
  in
  List.iter walk sexps;
  !files

(* Scan ../examples/ for subdirectories with a dune file and extract their
   default alias deps. This avoids having to manually update this file
   whenever a new example is added. *)
let discover_examples () : desc list =
  let examples_dir = "../../examples" in
  let entries = Sys.readdir examples_dir in
  Array.sort String.compare entries;
  Array.to_list entries
  |> List.filter_map (fun name ->
      let dir = examples_dir ^ "/" ^ name in
      let dune_path = dir ^ "/dune" in
      if
        String.length name > 0
        && name.[0] <> '.'
        && Sys.is_directory dir
        && Sys.file_exists dune_path
      then
        let content = read_file dune_path in
        let sexps = parse_sexps content in
        let file_deps = extract_default_alias_deps sexps in
        let embedded = extract_embedded_files sexps in
        let file_deps =
          List.filter
            (fun dep ->
              let base =
                if is_dir dep then String.sub dep 0 (String.length dep - 1) else dep
              in
              not (List.mem base embedded))
            file_deps
        in
        if file_deps <> [] then Some (dir, name, file_deps) else None
      else None)

(* The toplevel example lives in a different directory and has special build
   rules, so we keep it hardcoded. *)
let toplevel : desc =
  ( "../../toplevel/examples/lwt_toplevel"
  , "toplevel"
  , [ "index.html"; "toplevel.js"; "toplevel.bc.js"; "test_dynlink.js" ] )

let all : desc list = toplevel :: discover_examples ()

let fmt_dep fmt dep : unit =
  if is_dir dep
  then Format.fprintf fmt "(source_tree %s)" dep
  else Format.fprintf fmt "%s" dep

let cp_src dir f =
  let f = if is_dir f then String.sub f 0 (String.length f - 1) else f in
  dir ^ "/" ^ f

let () = set_binary_mode_out stdout true

let () =
  let pp = Format.std_formatter in
  let pp_sep fmt () = Format.pp_print_break fmt 0 0 in
  List.iter
    (fun (dir, dst, files) ->
      Format.fprintf
        pp
        {|
(rule
 (alias doc-manual)
 (targets (dir %s))
 (deps@;<0 2>@[<v 0>%a@])
 (action (bash "mkdir -p %s && cp -r %s %s")))
|}
        dst
        (Format.pp_print_list ~pp_sep fmt_dep)
        (List.map (fun f -> dir ^ "/" ^ f) files)
        dst
        (String.concat " " (List.map (cp_src dir) files))
        dst)
    all
