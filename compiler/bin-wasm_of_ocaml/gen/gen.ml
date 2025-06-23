open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

class check_and_warn =
  object
    inherit Js_traverse.free as super

    method! merge_info from =
      let def = from#get_def in
      let use = from#get_use in
      let diff = Javascript.IdentSet.diff def use in
      let diff =
        Javascript.IdentSet.fold
          (fun x acc ->
            match x with
            | S { name = Utf8_string.Utf8 s; _ } ->
                if String.starts_with s ~prefix:"_" then acc else s :: acc
            | V _ -> acc)
          diff
          []
      in
      (match diff with
      | [] -> ()
      | l ->
          Warning.warn
            `Unused_js_variable
            "unused variable:@. %s@."
            (String.concat ~sep:", " l));
      super#merge_info from
  end

let free_variable code =
  if Warning.enabled `Unused_js_variable
  then
    let o = new check_and_warn in
    let _code = o#program code in
    Javascript.IdentSet.fold
      (fun x acc ->
        match x with
        | S { name = Utf8 x; _ } -> StringSet.add x acc
        | V _ -> acc)
      o#get_free
      StringSet.empty
  else
    let free = ref StringSet.empty in
    let o = new Js_traverse.fast_freevar (fun s -> free := StringSet.add s !free) in
    o#program code;
    !free

let check_js_file fname =
  Warning.werror := true;
  Warning.enable `Unused_js_variable;
  let c = Fs.read_file fname in
  let p =
    try Parse_js.parse (Parse_js.Lexer.of_string ~filename:fname c)
    with Parse_js.Parsing_error pi ->
      failwith (Printf.sprintf "cannot parse file %S (l:%d, c:%d)@." fname pi.line pi.col)
  in
  let freenames = free_variable p in
  let freenames = StringSet.diff freenames Reserved.keyword in
  let freenames = StringSet.diff freenames Reserved.provided in
  if not (StringSet.is_empty freenames)
  then
    Warning.warn
      `Free_variables_in_primitive
      "free variables in %S@.vars: %a@."
      fname
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         Format.pp_print_string)
      (StringSet.elements freenames);
  Warning.process_warnings ();
  ()

(* Keep the two variables below in sync with function build_runtime in
   ../compile.ml *)

let default_flags = []

let interesting_runtimes = [ [ "effects", `S "jspi" ]; [ "effects", `S "cps" ] ]

let name_runtime standard l =
  let flags =
    List.filter_map l ~f:(fun (k, v) ->
        match v with
        | `S s -> Some s
        | `B b -> if b then Some k else None)
  in
  String.concat ~sep:"-" ("runtime" :: (if standard then [ "standard" ] else flags))
  ^ ".wasm"

let print_flags f flags =
  Format.fprintf
    f
    "@[<2>[ %a ]@]"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
       (fun f (k, v) ->
         Format.fprintf
           f
           "@[\"%s\",@ %a@]"
           k
           (fun f v ->
             match v with
             | `S s -> Format.fprintf f "Wat_preprocess.String \"%s\"" s
             | `B b ->
                 Format.fprintf f "Wat_preprocess.Bool %s" (if b then "true" else "false"))
           v))
    flags

let () =
  let () = set_binary_mode_out stdout true in
  let js_runtime, deps, wat_files, runtimes =
    match Array.to_list Sys.argv with
    | _ :: js_runtime :: deps :: rest ->
        assert (Filename.check_suffix js_runtime ".js");
        assert (Filename.check_suffix deps ".json");
        let wat_files, rest =
          List.partition rest ~f:(fun f -> Filename.check_suffix f ".wat")
        in
        let wasm_files, rest =
          List.partition rest ~f:(fun f -> Filename.check_suffix f ".wasm")
        in
        assert (List.is_empty rest);
        js_runtime, deps, wat_files, wasm_files
    | _ -> assert false
  in
  check_js_file js_runtime;
  Format.printf "open Wasm_of_ocaml_compiler@.";
  Format.printf "let js_runtime = {|\n%s\n|}@." (Fs.read_file js_runtime);
  Format.printf "let dependencies = {|\n%s\n|}@." (Fs.read_file deps);
  Format.printf
    "let wat_files = [%a]@."
    (Format.pp_print_list (fun f file ->
         Format.fprintf
           f
           "{|%s|},@;{|%s|};@;"
           Filename.(chop_suffix (basename file) ".wat")
           (Fs.read_file file)))
    wat_files;
  Format.printf
    "let precompiled_runtimes = [%a]@."
    (Format.pp_print_list (fun f (standard, flags) ->
         let flags = flags @ default_flags in
         let name = name_runtime standard flags in
         match
           List.find_opt runtimes ~f:(fun file ->
               String.equal (Filename.basename file) name)
         with
         | None -> failwith ("Missing runtime " ^ name)
         | Some file ->
             Format.fprintf f "%a,@;%S;@;" print_flags flags (Fs.read_file file)))
    (List.mapi interesting_runtimes ~f:(fun i flags -> i = 0, flags))
