open Js_of_ocaml_compiler

let printl l = List.iter (fun name -> print_endline name) (List.sort String.compare l)

module SS = Set.Make (String)

let%expect_test _ =
  let all = List.map Builtins.File.name (Builtins.all ()) in
  let runtime = List.map Builtins.File.name Js_of_ocaml_compiler_runtime_files.runtime in
  let extra = SS.elements (SS.diff (SS.of_list all) (SS.of_list runtime)) in
  printl all;
  [%expect
    {|
    +array.js
    +backtrace.js
    +bigarray.js
    +bigstring.js
    +blake2.js
    +compare.js
    +domain.js
    +dynlink.js
    +effect.js
    +fail.js
    +format.js
    +fs.js
    +fs_fake.js
    +fs_node.js
    +gc.js
    +graphics.js
    +hash.js
    +ieee_754.js
    +int64.js
    +ints.js
    +io.js
    +jslib.js
    +jslib_js_of_ocaml.js
    +lexing.js
    +marshal.js
    +md5.js
    +mlBytes.js
    +nat.js
    +obj.js
    +parsing.js
    +prng.js
    +runtime_events.js
    +stdlib.js
    +str.js
    +sync.js
    +sys.js
    +toplevel.js
    +unix.js
    +weak.js
    +zstd.js
    |}];
  printl runtime;
  [%expect
    {|
    +array.js
    +backtrace.js
    +bigarray.js
    +bigstring.js
    +blake2.js
    +compare.js
    +domain.js
    +effect.js
    +fail.js
    +format.js
    +fs.js
    +fs_fake.js
    +fs_node.js
    +gc.js
    +graphics.js
    +hash.js
    +ieee_754.js
    +int64.js
    +ints.js
    +io.js
    +jslib.js
    +jslib_js_of_ocaml.js
    +lexing.js
    +marshal.js
    +md5.js
    +mlBytes.js
    +nat.js
    +obj.js
    +parsing.js
    +prng.js
    +runtime_events.js
    +stdlib.js
    +str.js
    +sync.js
    +sys.js
    +unix.js
    +weak.js
    +zstd.js
    |}];
  printl extra;
  [%expect {|
    +dynlink.js
    +toplevel.js |}]
