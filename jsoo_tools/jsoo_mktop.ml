(* #use "topfind" *)
(* #require "findlib" *)

let verbose = ref false

let syntaxes = ref []
let syntaxes_mod = ref []
let add_syntax_pkg p =
  syntaxes:=!syntaxes @ [p]
let add_syntax_mod p =
  syntaxes_mod:=!syntaxes_mod @ [p]

let execute cmd =
  let s = String.concat " " cmd in
  if !verbose then Printf.printf "+ %s\n" s;
  let ret = Sys.command s in
  if ret <> 0
  then failwith (Printf.sprintf "Error: %s" s)

let to_clean = ref []
let clean file = to_clean := file :: !to_clean
let do_clean () =
  List.iter (fun x ->
      if Sys.file_exists x
      then Sys.remove x) !to_clean

module type CAMLP4 = sig   val build : string list -> string list -> string list end
module Camlp4 : CAMLP4 = struct
  let flush_str = Printf.sprintf
      "let _ = JsooTop.register_camlp4_syntax %S Camlp4.Register.iter_and_take_callbacks"

  let make_flush_module =
    let count = ref 0 in
    fun pkg_name ->
      incr count;
      let name = Printf.sprintf "camlp4_flush_%d" !count in
      let name_ml = name ^ ".ml" in
      let oc = open_out name_ml in
      clean name_ml;
      output_string oc (flush_str pkg_name);
      close_out oc;
      execute ["ocamlfind";"ocamlc";"-c";"-I";"+camlp4";"-package";"js_of_ocaml.toplevel";name_ml];
      clean (name ^ ".cmo");
      clean (name ^ ".cmi");
      (name^".cmo")

  let with_flush x archive =
    [ make_flush_module x ; archive]

  let resolve_syntaxes all =
    let preds = ["syntax";"preprocessor"] in
    let all = Findlib.package_deep_ancestors preds all in
    let ll = List.map (fun x ->
        try "-I" :: (Findlib.package_directory x) :: with_flush x (Findlib.package_property preds x "archive") with
        | Not_found -> []) all in
    List.flatten ll

  let build pkgs mods =
    if pkgs = [] && mods = [] then [] else
      let all = resolve_syntaxes pkgs @ mods in
      [
        "-I";"+camlp4";
        "dynlink.cma";
        "camlp4lib.cma";
	      "Camlp4Parsers/Camlp4OCamlRevisedParser.cmo";
	      "Camlp4Parsers/Camlp4OCamlParser.cmo";
      ]
      @ all
      @ ["Camlp4Top/Top.cmo"]
end

let rec scan_args acc = function
  | "-top-syntax" :: x :: xs -> add_syntax_pkg x; scan_args acc xs
  | "-top-syntax-mod" :: x :: xs -> add_syntax_mod x; scan_args acc xs
  | "-verbose"::xs -> verbose:=true; scan_args ("-verbose"::acc) xs
  | x :: xs -> scan_args (x::acc) xs
  | [] -> List.rev acc

let _ =
  try
    let jsoo_top = ["-package";"js_of_ocaml.toplevel"] in
    let base_cmd = ["ocamlfind";"ocamlc";"-linkall";"-linkpkg"] in
    let args = List.tl (Array.to_list (Sys.argv)) in
    let args = scan_args [] args in
    let cmd = base_cmd @ Camlp4.build !syntaxes !syntaxes_mod  @ jsoo_top @ args in
    execute cmd;
    do_clean ()
  with exn -> do_clean (); raise exn
