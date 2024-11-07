#use "topfind"

#require "opam-format"

#require "unix"

#require "str"

module StringSet = Set.Make (String)

(****)

let repo = "jane-street/opam-repository/packages"

let roots = [ "bonsai"; "string_dict" ]

let omitted_others = StringSet.of_list [ "cohttp-async"; "cohttp"; "uri"; "uri-sexp" ]

let omitted_js = StringSet.of_list [ "sexplib0" ]

let do_not_pin =
  StringSet.of_list
    [ "wasocaml"; "wasm_of_ocaml"; "dune"; "ezjs"; "ezjs_blockies"; "fmlib"; "graphv" ]

let do_pin = StringSet.of_list [ "base"; "ppx_expect"; "ppx_inline_test"; "time_now" ]

let aliases = [ "ocaml-cstruct", "cstruct" ]

let dune_workspace =
  {|(lang dune 3.17)
(env
 (_
  (env-vars (TESTING_FRAMEWORK inline-test))
  (js_of_ocaml (enabled_if false))
  (flags :standard -warn-error -8-32-34-49-52-55 -w -67-69)))
|}

let patches =
  [ ( "sexp_grammar"
    , {|
diff --git a/sexp_grammar_validation.opam b/sexp_grammar_validation.opam
new file mode 100644
index 0000000..e69de29
diff --git a/validation/src/dune b/validation/src/dune
index 91933ec..849e4d7 100644
--- a/validation/src/dune
+++ b/validation/src/dune
@@ -1,5 +1,6 @@
 (library
  (name sexp_grammar_validation)
+ (public_name sexp_grammar_validation)
  (libraries bignum.bigint core
    expect_test_helpers_core.expect_test_helpers_base sexp_grammar)
  (preprocess
|}
    )
  ; ( "bignum"
    , {bignum|
diff --git a/dune-project b/dune-project
index e563d7e..b87e356 100644
--- a/dune-project
+++ b/dune-project
@@ -1,3 +1,3 @@
-(lang dune 2.0)
+(lang dune 3.17)
 
 (formatting disabled)
diff --git a/test/src/dune b/test/src/dune
index f93ae3f..3f00557 100644
--- a/test/src/dune
+++ b/test/src/dune
@@ -2,5 +2,6 @@
  (name bignum_test)
  (libraries bigint bignum core expect_test_helpers_core
    sexp_grammar_validation)
+ (inline_tests (flags -drop-tag no-js -drop-tag no-wasm -drop-tag 64-bits-only) (modes js wasm))
  (preprocess
   (pps ppx_jane)))
diff --git a/test/src/test_bignum.ml b/test/src/test_bignum.ml
index c6d09fb..61b1e5b 100644
--- a/test/src/test_bignum.ml
+++ b/test/src/test_bignum.ml
@@ -3,6 +3,11 @@ open! Expect_test_helpers_core
 open Bignum
 open Bignum.For_testing
 
+module Zarith = struct
+  module Q = Q
+  module Z = Z
+end
+
 let%expect_test "Bignum.abs" =
   let test t =
     let t' = require_no_allocation [%here] (fun () -> abs t) in
@@ -71,7 +76,7 @@ let%expect_test "Bignum.sexp_of_t does use Scientific Notation" =
 let compare_floats ~of_float x =
   let x' = x |> of_float |> Bignum.to_float in
   if not (Float.( = ) x x' || (Float.is_nan x && Float.is_nan x'))
-  then raise_s [%message "mismatch" (x : float) (x' : float)]
+  then raise_s [%message "mismatch" (x : float) (x' : float) (x |> of_float : Bignum.t)]
 ;;
 
 let%expect_test "roundtrip: f |> Bignum.of_float_decimal |> Bignum.to_float" =
@@ -783,7 +788,7 @@ let%test_module _ =
          -1073741825 -> ( 6) \001\253\255\255\255\191 |}]
     ;;
 
-    let%expect_test ("bin_io serialization V2 (javascript)" [@tags "js-only"]) =
+    let%expect_test ("bin_io serialization V2 (javascript)" [@tags "js-only", "no-wasm"]) =
       bin_io_tests (module V2);
       [%expect
         {|
@@ -811,6 +816,34 @@ let%test_module _ =
          -1073741825 -> ( 6) \001\253\255\255\255\191 |}]
     ;;
 
+    let%expect_test ("bin_io serialization V2 (Wasm)" [@tags "wasm-only"]) =
+      bin_io_tests (module V2);
+      [%expect
+        {|
+                   0 -> ( 1) \000
+                   1 -> ( 2) \001\001
+                  -1 -> ( 3) \001\255\255
+           100000001 -> ( 6) \001\253\001\225\245\005
+           1000000.1 -> ( 6) \002\253\129\150\152\000
+           100000.01 -> ( 6) \003\253\129\150\152\000
+           10000.001 -> ( 6) \004\253\129\150\152\000
+           1000.0001 -> ( 6) \005\253\129\150\152\000
+           100.00001 -> ( 6) \006\253\129\150\152\000
+           10.000001 -> ( 6) \007\253\129\150\152\000
+           1.0000001 -> ( 6) \008\253\129\150\152\000
+          0.10000001 -> ( 6) \009\253\129\150\152\000
+         0.010000001 -> (11) \010\253\129\150\152\000\253\000\202\154\059
+        0.0010000001 -> (22) \011\02010000001\04710000000000
+      10000000000000 -> (16) \011\01410000000000000
+     -10000000000000 -> (17) \011\015\04510000000000000
+12345678901234567.12345678901234567 -> (55) \01151234567890123456712345678901234567\047100000000000000000
+       1099511627775 -> (15) \011\0131099511627775
+          1073741823 -> ( 6) \001\253\255\255\255\063
+         -1073741824 -> ( 6) \001\253\000\000\000\192
+          1073741824 -> (12) \011\0101073741824
+         -1073741825 -> (13) \011\011\0451073741825 |}]
+    ;;
+
     let%expect_test "bin_io de-serialization V2" =
|bignum}
    )
  ]

(****)

let read_opam_file filename =
  OpamPp.parse
    OpamPp.Op.(OpamFormat.I.file -| OpamPp.map_snd OpamFile.OPAM.pp_raw_fields)
    ~pos:{ filename; start = 0, 0; stop = 0, 0 }
    (OpamParser.FullPos.file (Filename.concat (Filename.concat repo filename) "opam"))

let dependencies (_, { OpamFile.OPAM.depends }) =
  let open OpamFormula in
  depends
  |> map (fun (nm, _) -> Atom (nm, None))
  |> of_atom_formula
  |> atoms
  |> List.map fst
  |> List.map OpamPackage.Name.to_string

let packages =
  repo
  |> Sys.readdir
  |> Array.to_list
  |> List.map (fun s -> String.sub s 0 (String.index s '.'), read_opam_file s)

let rec traverse visited p =
  if StringSet.mem p visited
  then visited
  else
    let visited = StringSet.add p visited in
    match List.assoc p packages with
    | exception Not_found -> visited
    | opam ->
        let l = dependencies opam in
        List.fold_left traverse visited l

let forked_packages =
  let ch =
    Unix.open_process_in
      "curl -L -H 'Accept: application/vnd.github+json' -H 'X-GitHub-Api-Version: \
       2022-11-28' https://api.github.com/orgs/ocaml-wasm/repos 2> /dev/null | jq -r \
       '.[] | .name'"
  in
  let l = Str.(split (regexp "\n")) (In_channel.input_all ch) in
  close_in ch;
  StringSet.of_list l

let is_forked p = StringSet.mem p forked_packages

let exec_async ~delay cmd =
  let p =
    Unix.open_process_out (Printf.sprintf "sleep %f; %s" (float delay /. 10.) cmd)
  in
  fun () -> ignore (Unix.close_process_out p)

let ( let* ) (f : unit -> 'a) (g : 'a -> unit -> 'b) : unit -> 'b = fun () -> g (f ()) ()

let sync_exec f l =
  let l = List.mapi f l in
  List.iter (fun f -> f ()) l

let pin delay nm =
  exec_async
    ~delay
    (Printf.sprintf
       "opam pin add -n %s https://github.com/ocaml-wasm/%s.git#wasm"
       (try List.assoc nm aliases
        with Not_found -> if List.mem_assoc nm packages then nm ^ ".v0.16.0" else nm)
       nm)

let pin_packages js =
  sync_exec
    pin
    (StringSet.elements
       (StringSet.union
          (StringSet.diff (StringSet.diff forked_packages js) do_not_pin)
          do_pin))

let install_others others =
  let others = StringSet.elements (StringSet.diff others omitted_others) in
  ignore (Sys.command ("opam install -y " ^ String.concat " " others))

let clone delay ?branch ?(depth = 1) nm src =
  exec_async
    ~delay
    (Printf.sprintf
       "git clone -q --depth %d %s%s jane-street/lib/%s"
       depth
       (match branch with
       | None -> ""
       | Some b -> Printf.sprintf "-b %s " b)
       src
       nm)

let clone' delay ?branch ?commit nm src =
  match commit with
  | None -> clone delay ?branch nm src
  | Some commit ->
      let* () = clone delay ?branch ~depth:10 nm src in
      exec_async
        ~delay:0
        (Printf.sprintf "cd jane-street/lib/%s && git checkout -b wasm %s" nm commit)

let () =
  Out_channel.(
    with_open_bin "jane-street/dune-workspace"
    @@ fun ch -> output_string ch dune_workspace)

let () =
  let js, others =
    List.fold_left traverse StringSet.empty roots
    |> StringSet.partition (fun p -> List.mem_assoc p packages)
  in
  pin_packages js;
  install_others others;
  sync_exec (fun i () -> clone i "ocaml-uri" "https://github.com/mirage/ocaml-uri") [ () ];
  sync_exec
    (fun i nm ->
      let branch = if is_forked nm then Some "wasm" else None in
      let commit =
        if is_forked nm
        then None
        else
          Some
            (let _, opam = List.assoc nm packages in
             let url = OpamUrl.to_string (Option.get (OpamFile.OPAM.get_url opam)) in
             let tar_file = Filename.basename url in
             String.sub tar_file 0 (String.index tar_file '.'))
      in
      clone'
        i
        ?branch
        ?commit
        nm
        (Printf.sprintf
           "https://github.com/%s/%s"
           (if is_forked nm then "ocaml-wasm" else "janestreet")
           nm))
    (StringSet.elements (StringSet.diff js omitted_js))

let () =
  List.iter
    (fun (dir, patch) ->
      let ch =
        Unix.open_process_out (Printf.sprintf "cd jane-street/lib/%s && patch -p 1" dir)
      in
      output_string ch patch;
      ignore (Unix.close_process_out ch))
    patches
