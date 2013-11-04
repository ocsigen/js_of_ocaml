open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

(* Options *)
let _ = Options.use_ocamlfind := true
let _ = Options.make_links:=false
(* hack to prevent discovering the wrong ocamlbuild binary *)
(* This should be fix with the latest version of ocaml (4.01) *)
let _ = Pack.Ocamlbuild_where.bindir := "/"

let compiler_main = "compiler/compile"

(* Utils *)

let read_lines ?(empty=false) file =
  let ic = open_in file in
  let rec aux l =
    try
      let s=input_line ic in
      if not empty && s="" then aux l
      else aux (s::l)
    with End_of_file -> List.rev l in
  aux []

let read_mllist file =
  let b = Filename.dirname file in
  let l = read_lines file in
  List.map (fun f -> Filename.concat b (String.uncapitalize f)) l

let read_filelist file =
  let b = Filename.dirname file in
  let l = read_lines file in
  List.map (fun f -> Filename.concat b f) l

(* remove duplicated in sorted list *)
let rec uniq_from_sorted l acc = match l,acc with
  | [],[] -> []
  | [],acc -> acc
  | x::xs , a::l when x = a -> uniq_from_sorted xs acc
  | x::xs , acc -> uniq_from_sorted xs (x::acc)

let find_first_good_or_fail build l =
  let rec find = function
    | [] -> raise Not_found
    | Pack.My_std.Outcome.Good name::_ -> name
    | _ :: xs -> find xs in
  let res = build (List.map (fun x -> [x]) l) in
  find res

(* add syntax extension *)
let add_syntax name path =
  (* hack : not dep when "compile" to avoid the extension syntax to be link with binaries *)
  (* the dep with ocamldep make sure the extension syntax is compiled before *)
  flag ["ocaml";"compile";"pkg_"^name]  (S [A "-ppopt" ;P (path ^ name -.- "cma") ]);
  flag_and_dep ["ocaml";"ocamldep";"pkg_"^name] (S [A "-ppopt" ;P (path ^ name -.- "cma") ])


(* Concat js files into a single js file *)
let init_jslib () =
  let dep = "%.jslib"
  and prod = "%.js" in
  rule "concat"
    ~dep
    ~prod
    (fun env build ->
       let files = read_filelist (env dep) in
       let content = List.map (fun f ->
           let fname = (f ^ ".js") in
           ignore(build [[fname]]);
           read_lines ~empty:true fname) files in
       let content = List.flatten content in
       let content = List.map (fun s -> [s;"\n"]) content in
       let content = List.flatten content in
       Echo (content, env prod)
    )

(* List all external primitives used by a lib *)
let init_joo_external () =
  let dep = "%.mllib"
  and prod = "%.external" in
  rule "joo_external"
    ~dep
    ~prod
    (fun env build ->
       let modules = read_mllist (env dep) in
       let files = List.map (fun m ->
           try P (find_first_good_or_fail build [m -.- "ml"])
           with Not_found -> N
         ) modules in
       Cmd (
         S [
           A "sed"; A "-n" ; A "-e";
           Sh "'s/.*external.*\"\\([^\"%]*\\)\".*/\\1/p'";
           S files;
           Sh ">";
           A (env prod)
         ]
       )
    )

(* Create a fake stubs from external file *)
let init_joo_stubs () =
  let dep = "%.external"
  and prod = "%_stubs.c" in
  rule "joo_stubs"
    ~dep
    ~prod
    (fun env build ->
       let header = [
         "#include <stdlib.h>\n";
	       "#include <stdio.h>\n";
         "#define D(f) void f () { fprintf(stderr, \"Unimplemented Javascript primitive %s!\\\\n\", #f); exit(1); }\n";
       ] in
       let prims = read_lines (env dep) in
       let prims = uniq_from_sorted (List.fast_sort String.compare prims) [] in
       let prims_str = List.map (fun prim -> Printf.sprintf "D(%s)\n" prim) prims in
       Echo (header @ prims_str , env prod)
    )

(* js_of_ocaml rule *)
let init_js_of_ocaml () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env build =
    let dep = env dep in
    let prod = env prod in
    let link_opts = [](* link_opts prod *) in
    let tags = tags_of_pathname dep ++ "js_of_ocaml" in
    let compiler = find_first_good_or_fail build [compiler_main -.- "native";"compiler/main.byte"] in
    Cmd (S [A compiler; A "-noruntime"; T tags; S link_opts; P dep; A "-o"; Px prod])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f;
  flag ["js_of_ocaml"; "debug"] (S [A "-pretty"; A "-debuginfo"; A "-noinline"]);
  pflag ["js_of_ocaml"] "opt" (fun n -> S [A "-opt"; A n]);
  pflag ["js_of_ocaml"] "jsopt" (fun opt -> S[Sh opt]);
  pflag_and_dep ["js_of_ocaml"] "with_js" (fun f -> P f)

(* phantomjs rule *)
let init_phantom_js () =
  let dep = "%.js"
  and prod = "%.jslog" in
  rule "phantomjs"
    ~dep
    ~prod
    (fun env build ->
       Cmd (
         S [
           Sh "phantomjs";
           P (env dep);
           Sh ">";
           P (env prod);
         ]
       )
    )

(* Expunge a list of Module bytecode *)
let init_expunge () =
  let dep_desc = "%.expunge"
  and dep_byte = "%.byte"
  and prod = "%_expunge.byte" in
  rule "expunge"
    ~deps:[dep_desc;dep_byte]
    ~prod
    (fun env build ->
       let units = read_lines (env dep_desc) in
       Cmd (
         S [
           V"OCAMLFIND";
           A"stdlib/expunge";
           P (env dep_byte);
           P (env prod);
           S (List.map (fun u -> A u) units)
         ]
       )
    )

(* META file *)

(* package description rules *)
(* could be replaced by oasis *)
type typ = [`Lib | `Bin | `Meta | `Stubs | `Js | `Doc]
let string_of_section : typ -> string = function
  | `Lib -> "lib"
  | `Stubs -> "stublibs"
  | `Meta -> "lib"
  | `Bin -> "bin"
  | `Js -> "lib"
  | `Doc -> "doc"

let configure  bindir enable_native enable_dynlink =
  let list_file_from_mllib_if_any f ext =
    let mllib = f -.- "mllib" in
    if Sys.file_exists mllib
    then
      List.map (fun f -> f -.- ext ) (read_mllist mllib)
    else [f -.- ext] in
  let expand_files typ native f =
    let ext e = f ^ "." ^ e in
    match typ with
      | `Bin when native -> [ext "native"]
      | `Bin -> [ext "byte"]
      | `Meta -> [f]
      | `Stubs -> [f]
      | `Lib ->
        let l = [ext "cma"] in
        let l = list_file_from_mllib_if_any f "cmi" @ l in
        if native
        then
          let l = list_file_from_mllib_if_any f "cmx" @ l in
          let l = ext "cmxa" :: l in
          if enable_dynlink
          then ext "cmxs":: l
          else l
        else l
      | `Js ->
        [ext "js"]
      | `Doc -> []
  in

  let toinstall = ref [] in

  let install ?name ?dir ?(native=enable_native) (typ : typ) path =
    let fpath = path in
    let files = expand_files typ native fpath in
    let dir = match dir with
      | None | Some "" -> ""
      | Some d -> d^"/" in
    List.iter (fun path ->
        let ext =
          try
            let n = Filename.basename path in
            let pos = String.rindex n '.' in
            String.sub n pos (String.length n - pos)
          with Not_found -> "" in
        let name = match name with
          | None -> Filename.chop_suffix (Filename.basename path) ext ^  ext
          | Some name -> name in
        toinstall:=!toinstall@[typ,path, dir^name]) files
  in

  (* generate %.install file *)
  rule ".install"
    ~prod:"js_of_ocaml.install"
    (fun _ _ ->
       failwith ".install rule not yet implemented";
    );

  (* installation script *)
  rule "ocamlfind installation script"
    ~prod:"install.sh"
    (fun _ build ->
       let l = !toinstall in
       let l = List.map (fun (typ,from,into) ->
           try
             let from = find_first_good_or_fail build [from] in
             `Install (typ,from,into)
           with Not_found ->
             if Sys.file_exists from
             then `Install (typ,from,into)
             else `Don't (typ,from,into)
         ) l in
       let libs,bins = List.fold_left (fun (libs,bins) x ->
           match x with
             | `Install ((`Lib|`Js|`Stubs|`Meta),from, into) ->
               P from::libs,bins
             | `Install (`Bin,from,into) ->
               libs,(from,into)::bins
             | `Install (`Doc as typ,from,into)
             | `Don't (typ,from, into) ->
               Printf.printf "error with %s into %s/%s\n" from (string_of_section typ) into;
               libs,bins
         ) ([],[]) l in
       let version = find_first_good_or_fail build ["VERSION"] in
       let version_str = match read_lines version with
         | x::_ -> x
         | _ -> "dev" in
       let cmd_ocamlfind = [
         S [
           A "ocamlfind";
           A "install";
           A "js_of_ocaml";
           A "-patch-version";
           A version_str;
           S libs;
         ];
         S [
           A "install";
           A "-d";
           A "-m"; A "755";
           A bindir]
       ]
       in
       let cmd_install = List.map (fun (from,into) ->
           S [
             A "install";
             P from;
             P (Filename.concat bindir into)]) bins in
       let header = "#!/usr/bin/env sh\n" in
       let cmd_str = List.map (fun cmd ->
           Command.string_of_command_spec cmd ^ "\n") (cmd_ocamlfind @ cmd_install ) in
       Echo( header::cmd_str ,"install.sh")
    );

  (* generate all.itarget *)
  rule "all.itarget"
    ~prod:"all.itarget"
    (fun env build ->
       Echo (List.map (fun (typ,from,_) ->
           match typ with
             | `Stubs -> "" (* hack *)
             | _ -> from ^ "\n") !toinstall, "all.itarget")
    );
  install

let getenv var default = try Sys.getenv var with Not_found -> default

let dispatcher = function
  | After_rules ->

    add_syntax "pa_js" "lib/syntax/";
    add_syntax "pa_deriving_Json" "lib/syntax/";

    ocaml_lib ~extern:false ~tag_name:"use_js_of_ocaml" ~dir:"lib" "lib/js_of_ocaml";
    ocaml_lib ~extern:false ~tag_name:"use_deriving_json" ~dir:"lib/deriving_json" "lib/deriving_json/deriving_json";
    ocaml_lib ~extern:false ~tag_name:"use_compiler" ~dir:"compiler" "compiler/compiler";

    dep ["use_js_of_ocaml";"ocamldep"] ["lib/js_of_ocaml.cma"];
    dep ["use_deriving_json";"ocamldep"] ["lib/deriving_json/deriving_json.cma"];
    dep ["use_compiler";"ocamldep"] ["compiler/compiler.cma"];


    if Sys.ocaml_version.[0] = '4'
    then begin
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "ocamlcommon";
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "ocamlbytecomp";
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "ocamltoplevel";
    end else begin
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "toplevellib";
    end;

    (* hack to compile js_of_ocaml lib *)
    flag ["use_js_of_ocaml";"link"] (S [A "-I" ; A "lib"]);
    pflag_and_dep ["link"] "linkdep" (fun param -> P param);

    (* ocamldoc intro *)
    pflag_and_dep ["doc"] "with_intro" (fun f -> S [A "-intro"; P f] );

    init_js_of_ocaml ();
    init_joo_stubs ();
    init_joo_external ();
    init_jslib ();
    init_phantom_js ();
    init_expunge ();
    pflag ["ocaml";"parser";"menhir"] "menhir_external_token" (fun m -> S [A "--external-tokens"; A m]);

    let enable_deriving = try ignore(Findlib.query "deriving"); true with _ -> false in
    let enable_native = if getenv "BEST" "opt" = "opt" then true else false in
    let enable_dynlink =
      try
        let stdlib = Findlib.((query "stdlib").location) in
        Sys.file_exists (Filename.concat stdlib "dynlink.cmxa")
      with _ -> false in

    let bindir = getenv "BINDIR" "/usr/local/bin" in

    Printf.printf "bindir: %s\nnative:%b\ndynlink:%b\n" bindir enable_native enable_dynlink;

    let add = configure bindir enable_native enable_dynlink in

    add `Bin ~name:("js_of_ocaml" -.- !Options.exe) compiler_main;
    add `Bin ~name:("js_of_ocaml_minify" -.- !Options.exe) "compiler/minify";
    add `Lib ~native:false "lib/js_of_ocaml";
    add `Lib (* ~dir:"syntax" *) "lib/syntax/pa_js";
    if enable_deriving
    then begin
      add `Lib "lib/deriving_json/deriving_json";
      add `Lib (* ~dir:"syntax" *) "lib/syntax/pa_deriving_Json";
    end;
    add `Lib "ocamlbuild/ocamlbuild_js_of_ocaml";
    add `Stubs ("lib/dlljs_of_ocaml" -.- !Options.ext_dll);
    add `Stubs ("lib/libjs_of_ocaml" -.- !Options.ext_lib);
    add `Meta "META";
    add `Js "runtime/runtime";
    add `Js "runtime/weak";
    add `Js "runtime/unix";
    add `Js "runtime/classlist";
    add `Lib (* ~dir:"compiler" *) "compiler/compiler"

  | _ -> ()

let _ = dispatch dispatcher
