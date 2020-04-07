open Js_of_ocaml_compiler

let printl l = List.iter (fun name -> print_endline name) (List.sort String.compare l)

module SS = Set.Make (String)

let%expect_test _ =
  let all = List.map Builtins.File.name (Builtins.all ()) in
  let runtime = List.map Builtins.File.name Jsoo_runtime.runtime in
  let extra = SS.elements (SS.diff (SS.of_list all) (SS.of_list runtime)) in
  printl all;
  [%expect
    {|
    +bigarray.js
    +bigstring-base_bigstring.js
    +bigstring-core_kernel.js
    +bigstring-cstruct.js
    +bigstring.js
    +dynlink.js
    +fs.js
    +fs_fake.js
    +fs_node.js
    +gc.js
    +graphics.js
    +ieee_754.js
    +int64.js
    +internalMod.js
    +io.js
    +jslib.js
    +jslib_js_of_ocaml.js
    +lexing.js
    +marshal.js
    +md5.js
    +mlString.js
    +nat.js
    +parsing.js
    +stdlib.js
    +stdlib_modern.js
    +toplevel.js
    +unix.js
    +weak.js |}];
  printl runtime;
  [%expect
    {|
    +bigarray.js
    +bigstring-cstruct.js
    +bigstring.js
    +fs.js
    +fs_fake.js
    +fs_node.js
    +gc.js
    +graphics.js
    +ieee_754.js
    +int64.js
    +internalMod.js
    +io.js
    +jslib.js
    +jslib_js_of_ocaml.js
    +lexing.js
    +marshal.js
    +md5.js
    +mlString.js
    +nat.js
    +parsing.js
    +stdlib.js
    +unix.js
    +weak.js |}];
  printl extra;
  [%expect
    {|
    +bigstring-base_bigstring.js
    +bigstring-core_kernel.js
    +dynlink.js
    +stdlib_modern.js
    +toplevel.js |}]
