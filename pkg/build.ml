#!/usr/bin/env ocaml;;
#directory "pkg";;
#use "topkg.ml";;
#use "common.ml";;
let with_deriving = Env.bool "deriving"
let with_graphics = Env.bool "graphics"
let with_tyxml = Env.bool "tyxml"
let with_react = Env.bool "react"

module ByteExts = struct
  let interface = [".mli"; ".cmi"; ".cmti"]
  let library = [".cma"]
  let module_library = (interface @ library)
end

let fulllib ?dst ?cond ext_lib ext_int name =
  let l = read_mllist (name ^ ".mllib") in
  let d x = match dst with
    | None -> None
    | Some d -> Some (d ^ Filename.basename x) in
  let cma = Pkg.lib ?cond ?dst:(d name) ~exts:ext_lib name in
  let filelist = List.map (fun f ->
      Pkg.lib ?cond ?dst:(d f) ~exts:ByteExts.interface f) l in
  cma :: filelist


let () =
  Pkg.describe "js_of_ocaml" ~builder:`OCamlbuild (List.flatten [
      (* META *)
      [
        Pkg.lib "pkg/META"
      ];
      (* Libs *)
      fulllib ByteExts.library ByteExts.interface "lib/js_of_ocaml" ;
      (* syntax *)
      [ Pkg.lib ~exts:[".cmo";".cmx";".cmxs"] "lib/syntax/pa_js" ] ;

      (* deriving lib *)
      fulllib ~dst:"deriving/" ~cond:with_deriving Exts.library Exts.interface "lib/deriving_json/deriving_json";
      (* deriving syntax *)
      [
        Pkg.lib ~dst:"deriving/pa_deriving_Json" ~cond:with_deriving ~exts:Exts.module_library "lib/syntax/pa_deriving_Json"
      ];

      (* graphics lib *)
      [
        Pkg.lib ~cond:with_graphics ~dst:"graphics/graphics_js" ~exts:ByteExts.module_library "lib/graphics/graphics_js";
      ];

      (* tyxml lib *)
      Pkg.lib ~dst:"tyxml/tyxml_cast_sigs" ~cond:(with_react && with_tyxml) ~exts:[".mli";".cmi"] "lib/tyxml/tyxml_cast_sigs" ::
      fulllib ~dst:"tyxml/" ~cond:(with_react && with_tyxml) ByteExts.library ByteExts.interface "lib/tyxml/tyxml";

      (* Ocamlbuild *)
      [
        Pkg.lib ~dst:"ocamlbuild/ocamlbuild_js_of_ocaml" ~exts:Exts.module_library "ocamlbuild/ocamlbuild_js_of_ocaml";
      ];
      (* Runtime *)
      [
        Pkg.lib "runtime/runtime.js";
        Pkg.lib "runtime/classlist.js";
        Pkg.lib "runtime/weak.js";
        Pkg.lib "runtime/toplevel.js";
        Pkg.lib "runtime/graphics.js";
      ];
      (* Compiler-libs *)
      [
        Pkg.lib ~dst:"compiler/compiler" ~exts:(".cmi"::Exts.library)  "compiler/compiler";
      ];
      (* Binaries *)
      [
        Pkg.bin ~auto:true ~dst:"jsoo_minify" "compiler/minify";
        Pkg.bin ~auto:true ~dst:"js_of_ocaml" "compiler/compile";
      ];
      (* Other *)
      [
        Pkg.doc "README.md";
        Pkg.doc "CHANGES";
      ]
    ])
