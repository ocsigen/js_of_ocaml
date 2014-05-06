#!/usr/bin/env ocaml;;
#directory "pkg";;
#use "topkg.ml";;
#use "common.ml";;
let with_deriving = Env.bool "deriving"
let with_graphics = Env.bool "graphics"

module ByteExts = struct
  let interface = [".mli"; ".cmi"; ".cmti"]
  let library = [".cma"]
  let module_library = (interface @ library)
end

let fulllib ?cond ext_lib ext_int name =
  let l = read_mllist (name ^ ".mllib") in
  let cma = Pkg.lib ~exts:ext_lib name in
  let filelist = List.map (fun f ->
      Pkg.lib ?cond ~exts:ByteExts.interface f) l in
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
      fulllib ~cond:with_deriving Exts.library Exts.interface "lib/deriving_json/deriving_json";
      (* deriving syntax *)
      [
        Pkg.lib ~cond:with_deriving ~exts:Exts.module_library "lib/syntax/pa_deriving_Json"
      ];

      (* graphics lib *)
      [
        Pkg.lib ~cond:with_graphics ~exts:ByteExts.module_library "lib/graphics/graphics_js";
      ];

      (* Ocamlbuild *)
      [
        Pkg.lib ~exts:Exts.module_library "ocamlbuild/ocamlbuild_js_of_ocaml";
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
        Pkg.lib ~exts:(".cmi"::Exts.library)  "compiler/compiler";
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
