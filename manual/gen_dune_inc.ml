type desc = string * string * string list

let all : desc list =
  [ ( "../toplevel/examples/lwt_toplevel"
    , "toplevel"
    , [ "index.html"; "toplevel.js"; "toplevel.bc.js"; "test_dynlink.js" ] )
  ; ( "../examples/boulderdash"
    , "boulderdash"
    , [ "index.html"; "boulderdash.bc.js"; "sprites/" ] )
  ; "../examples/webgl", "webgl", [ "index.html"; "webgldemo.bc.js" ]
  ; "../examples/graph_viewer", "graph_viewer", [ "index.html"; "viewer_js.bc.js" ]
  ; "../examples/planet", "planet", [ "index.html"; "texture.jpg"; "planet.bc.js" ]
  ; "../examples/wiki", "wiki", [ "index.html"; "main.bc.js" ]
  ; "../examples/wysiwyg", "wysiwyg", [ "index.html"; "main.bc.js" ]
  ; ( "../examples/hyperbolic"
    , "hyperbolic"
    , [ "index.html"; "hypertree.bc.js"; "icons/"; "thumbnails/" ] )
  ; "../examples/minesweeper", "minesweeper", [ "index.html"; "main.bc.js"; "sprites/" ]
  ; "../examples/cubes", "cubes", [ "index.html"; "cubes.bc.js" ]
  ]

let deps (dir, _, files) = List.map (fun f -> dir ^ "/" ^ f) files

let is_dir x =
  let len = String.length x in
  len > 0 && String.get x (len - 1) = '/'

let fmt_dep fmt dep : unit =
  if is_dir dep
  then Format.fprintf fmt "(source_tree %s)" dep
  else Format.fprintf fmt "%s" dep

let fmt_copy fmt (dir, dst, files) : unit =
  let srcs =
    List.map
      (fun f ->
        let f = if is_dir f then String.sub f 0 (String.length f - 1) else f in
        dir ^ "/" ^ f)
      files
  in
  Format.fprintf fmt "(bash \"cp -r %s files/%s\")" (String.concat " " srcs) dst

let fmt_mkdir fmt (_, dst, _) : unit =
  Format.fprintf fmt "(bash \"mkdir -p files/%s\")" dst

let () = set_binary_mode_out stdout true

let () =
  Format.fprintf
    Format.std_formatter
    {|
(rule
 (alias doc-manual)
 (deps@;<0 2>@[<v 0>(glob_files *wiki)@;(source_tree files)@;%a@])
 (action (progn@;<0 2>@[<v 0>%a%a@])))
|}
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_break fmt 0 0) fmt_dep)
    (List.flatten (List.map deps all))
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_break fmt 0 0) fmt_mkdir)
    all
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_break fmt 0 0) fmt_copy)
    all
