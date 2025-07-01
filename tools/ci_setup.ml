module StringSet = Set.Make (String)

(****)

let jane_root, wasmoo_root =
  match Sys.argv with
  | [| _; jane_root; wasmoo_root |] -> jane_root, wasmoo_root
  | _ -> "janestreet", "wasm_of_ocaml"

let repo = Filename.concat jane_root "opam-repository/packages"

let roots =
  [ "bonsai_web_components"; "string_dict"; "ppx_html"; "bonsai_bench"; "float_array" ]

let omitted_others = StringSet.of_list []

let omitted_js = StringSet.of_list [ "basement"; "sexplib0" ]

let do_pin =
  StringSet.of_list
    [ "basement" (* https://github.com/janestreet/basement/pull/3 *); "bigstringaf" ]

let forked_packages =
  StringSet.of_list
    [ "async_js"
    ; "base"
    ; "core"
    ; "core_kernel"
    ; "core_unix"
    ; "bonsai" (* Compatibility with effect syntax *)
    ; "bonsai_test"
    ; "bonsai_web" (* Compatibility with effect syntax *)
    ; "bonsai_web_components"
    ; "bonsai_web_test"
    ; "time_now"
    ; "typerep" (* https://github.com/janestreet/typerep/pull/7 *)
    ; "virtual_dom" (* Compatibility with effect syntax *)
    ; "zarith_stubs_js"
    ]

let dune_workspace =
  {|(lang dune 3.17)
(env
 (_
  (env-vars (TESTING_FRAMEWORK inline-test))
  (js_of_ocaml (enabled_if false))
  (wasm_of_ocaml
   (flags
    (:standard --enable use-js-string)))
  (flags :standard -alert -all -warn-error -7-8-27-30-32-34-37-49-52-55 -w -7-27-30-32-34-37-49-52-55-56-58-67-69)))
|}

let node_wrapper =
  [ ( "node_wrapper/dune"
    , {|(executable
 (public_name node)
 (name node_wrapper)
       (libraries unix))|} )
  ; "node_wrapper/node_wrapper_per_profile.ml", {|let args = []|}
  ; "node_wrapper/dune-project", "(lang dune 3.17)"
  ; "node_wrapper/node_wrapper.opam", ""
  ]

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
diff --git a/test/src/dune b/test/src/dune
index f93ae3f..3f00557 100644
--- a/test/src/dune
+++ b/test/src/dune
@@ -2,5 +2,6 @@
  (name bignum_test)
  (libraries bigint bignum core expect_test_helpers_core expectable
    sexp_grammar_validation zarith)
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
     let t' = require_no_allocation (fun () -> abs t) in
|bignum}
    )
  ; ( "bin_prot"
    , {bp|
diff --git a/test/dune b/test/dune
index bd88b8d..29b3604 100644
--- a/test/dune
+++ b/test/dune
@@ -1,15 +1,8 @@
 (library
  (name bin_prot_test)
  (libraries base base_bigstring bin_prot
-   expect_test_helpers_core.expect_test_helpers_base expect_test_patterns
+   expect_test_helpers_core.expect_test_helpers_base ; expect_test_patterns
    float_array base.md5 sexplib splittable_random stdio)
+ (inline_tests (flags -drop-tag no-js -drop-tag 64-bits-only -drop-tag 32-bits-only -drop-tag no-wasm) (modes js wasm))
  (preprocess
   (pps ppx_jane)))
-
-(rule
- (deps core/blob_stability_tests.ml integers_repr_tests_64bit.ml
-   integers_repr_tests_js.ml integers_repr_tests_wasm.ml)
- (action
-  (bash
-    "diff <(\necho '869e6b3143f14201f406eac9c05c4cdb  core/blob_stability_tests.ml'\necho 'a9ed028fa16f307982c196f647d05afa  integers_repr_tests_64bit.ml'\necho 'a17ffcd3bf1e15dbca0ee54ec5b95c58  integers_repr_tests_js.ml'\necho 'e747bd85320575c771fc62a0d3085d29  integers_repr_tests_wasm.ml'\n  ) <(md5sum %{deps})"))
- (alias runtest))
diff --git a/test/non_integers_repr.ml b/test/non_integers_repr.ml
index cbb9bd5..b5b5a03 100644
--- a/test/non_integers_repr.ml
+++ b/test/non_integers_repr.ml
@@ -811,11 +811,12 @@ let%expect_test "Non-integer bin_prot size tests" =
     00 00 00 00 00 00 00 00 -> 0
     |}];
   gen_tests Tests.float_nan;
+  [%expect
+    {| 7f f8 00 00 00 00 00 01 -> NAN |}];
+(*
   Expect_test_patterns.require_match
     [%here]
-    {|
-    7f f{8,0} 00 00 00 00 00 01 -> NAN (glob)
-    |};
+*)
   gen_tests Tests.vec;
   [%expect
     {|
    |bp}
    )
  ; ( "base_bigstring"
    , {|
diff --git a/test/dune b/test/dune
index 8d23f86..21e83ba 100644
--- a/test/dune
+++ b/test/dune
@@ -2,5 +2,6 @@
  (name base_bigstring_test)
  (libraries base_bigstring core.base_for_tests core expect_test_helpers_core
    stdio)
+ (inline_tests (flags -drop-tag no-js -drop-tag 64-bits-only -drop-tag no-wasm) (modes js wasm))
  (preprocess
   (pps ppx_jane)))
|}
    )
  ; ( "core_kernel"
    , {|
diff --git a/version_util/src/dune b/version_util/src/dune
index 4b2b8bb..f7eb7ba 100644
--- a/version_util/src/dune
+++ b/version_util/src/dune
@@ -10,4 +10,5 @@
  (preprocess
   (pps ppx_jane))
  (wasm_of_ocaml
-  (javascript_files version_util.js)))
+  (javascript_files version_util.js)
+  (wasm_files version_util.wat)))
|}
    )
  ; ( "string_dict"
    , {|
diff --git a/test/dune b/test/dune
index b145cb3..e5fc412 100644
--- a/test/dune
+++ b/test/dune
@@ -1,5 +1,6 @@
 (library
  (name string_dict_test)
  (libraries base core expect_test_helpers_core string_dict)
+ (inline_tests (flags -drop-tag no-js -drop-tag 64-bits-only -drop-tag no-wasm) (modes js wasm))
  (preprocess
   (pps ppx_jane)))
|}
    )
  ; ( "zarith_stubs_js"
    , {zs|
diff --git a/test/bitwise.ml b/test/bitwise.ml
index 5fd0ddc..4833923 100644
--- a/test/bitwise.ml
+++ b/test/bitwise.ml
@@ -86,7 +86,7 @@ module Ml_z_popcount = struct
     Static.quickcheck ~f:(fun x -> [%message (x : t) (popcount x : int)]) ();
     (* Compression rate is low because our quickcheck implementation generates
        integers with a bounded bitcount. *)
-    [%expect {| ((hash 1e429706c701b111d98b6e6e858bbea4) (uniqueness_rate 42.96875)) |}]
+    [%expect {| ((hash d937e61f530ab9c27544e392922d286d) (uniqueness_rate 42.96875)) |}]
   ;;
 end

@@ -102,7 +102,7 @@ module Ml_z_hamdist = struct
       ();
     (* Compression rate is low because our quickcheck implementation generates
        integers with a bounded bitcount. *)
-    [%expect {| ((hash 0a270232628736ee7d47c8b403250989) (uniqueness_rate 33.284457)) |}]
+    [%expect {| ((hash 0d36530b39292e2c31f13d10ec004a38) (uniqueness_rate 33.284457)) |}]
   ;;
 end

diff --git a/test/dune b/test/dune
index 7996514..d0b463a 100644
--- a/test/dune
+++ b/test/dune
@@ -1,7 +1,9 @@
 (library
  (name zarith_stubs_js_test)
- (libraries zarith core base.md5 zarith_stubs_js)
+ (libraries zarith_wrapper core base.md5 zarith_stubs_js)
  (flags :standard -w -60)
+ (inline_tests (flags -drop-tag no-js -drop-tag 64-bits-only -drop-tag no-wasm) (modes js wasm))
+ (modules (:standard \ zarith))
  (preprocess
   (pps ppx_jane)))

@@ -35,10 +37,16 @@
  (deps implemented_externals.txt tested_externals.txt)
  (action
   (bash "diff %{deps}"))
- (alias runtest))
+ (alias runtest-))

 (rule
  (deps implemented_externals.txt zarith_externals.txt)
  (action
   (bash "diff %{deps}"))
- (alias runtest))
+ (alias runtest-))
+
+(subdir zarith
+ (copy_files (files ../zarith.ml))
+ (library (name zarith_wrapper)
+  (wrapped false)
+  (libraries zarith)))
|zs}
    )
  ; ( "ppx_css"
    , {|
diff --git a/css_parser/lexer/ident.ml b/css_parser/lexer/ident.ml
index fdf9926..d0ccf6a 100644
--- a/css_parser/lexer/ident.ml
+++ b/css_parser/lexer/ident.ml
@@ -6,7 +6,7 @@ let css_newline_single_char = [%sedlex.regexp? '\n' | '\r' | "\u{000C}"]
 let css_newline = [%sedlex.regexp? "\r\n" | css_newline_single_char]
 let css_whitespace = [%sedlex.regexp? css_newline | '\t' | " "]
 let css_whitespace_single_char = [%sedlex.regexp? css_newline_single_char | '\t' | " "]
-let ascii = [%sedlex.regexp? '\000' .. '\177']
+let ascii = [%sedlex.regexp? Latin1 '\000' .. '\177']
 let non_ascii = [%sedlex.regexp? Compl ascii]
 let ident_start_code_point = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_' | non_ascii]
 let ident_code_point = [%sedlex.regexp? ident_start_code_point | '0' .. '9' | '-']
|}
    )
  ]

let removes =
  [ "core/core/test/test_sys.ml"
  ; "core/core/test/test_sys.mli"
  ; "core/core/test/test_timezone.ml"
  ; "core/core/test/test_timezone.mli"
  ]
(****)

let read_opam_file filename =
  OpamPp.parse
    OpamPp.Op.(OpamFormat.I.file -| OpamPp.map_snd OpamFile.OPAM.pp_raw_fields)
    ~pos:{ filename; start = 0, 0; stop = 0, 0 }
    (OpamParser.FullPos.file (Filename.concat (Filename.concat repo filename) "opam"))

let dependencies (_, { OpamFile.OPAM.depends; _ }) =
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
  |> List.map (fun s ->
         if String.contains s '.'
         then String.sub s 0 (String.index s '.'), read_opam_file s
         else
           ( s
           , read_opam_file
               (Filename.concat
                  s
                  (List.find
                     (fun f -> String.starts_with ~prefix:s f)
                     (Array.to_list (Sys.readdir (Filename.concat repo s))))) ))

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

let is_forked p = StringSet.mem p forked_packages

let exec_async cmd =
  let p = Unix.open_process_out cmd in
  fun () -> ignore (Unix.close_process_out p)

let ( let* ) (f : unit -> 'a) (g : 'a -> unit -> 'b) : unit -> 'b = fun () -> g (f ()) ()

let sync_exec f l =
  let l = List.map f l in
  List.iter (fun f -> f ()) l

let branch nm =
  if is_forked nm then
    match nm with
    | "async_js"
    | "base"
    | "core"
    | "core_kernel"
    | "core_unix"
    | "time_now"
    | "zarith_stubs_js" -> Some "js-strings"
    | _ -> Some "wasm-latest"
  else
    None

let pin nm =
  let branch = Option.value ~default:"wasm-latest" (branch nm) in
  exec_async
    (Printf.sprintf
       "opam pin add -n %s https://github.com/ocaml-wasm/%s.git#%s"
       nm
       nm
       branch)

let pin_packages () = sync_exec pin (StringSet.elements do_pin)

let install_others others =
  let others = StringSet.elements (StringSet.diff others omitted_others) in
  ignore (Sys.command ("opam install -y " ^ String.concat " " others))

let clone ?branch ?(depth = 1) nm src =
  exec_async
    (Printf.sprintf
       "git clone -q --depth %d %s%s %s/lib/%s"
       depth
       (match branch with
       | None -> ""
       | Some b -> Printf.sprintf "-b %s " b)
       src
       jane_root
       nm)

let clone' ?branch ?commit nm src =
  match commit with
  | None -> clone ?branch nm src
  | Some commit ->
      let* () = clone ?branch ~depth:100 nm src in
      exec_async
        (Printf.sprintf "cd %s/lib/%s && git checkout -b wasm %s" jane_root nm commit)

let () =
  let write f contents =
    Out_channel.(with_open_bin f @@ fun ch -> output_string ch contents)
  in
  let copy f f' =
    let contents = In_channel.(with_open_bin f @@ input_all) in
    Out_channel.(with_open_bin f' @@ fun ch -> output_string ch contents)
  in
  write (Filename.concat jane_root "dune-workspace") dune_workspace;
  Unix.mkdir (Filename.concat jane_root "node_wrapper") 0o755;
  List.iter
    (fun (f, contents) -> write (Filename.concat jane_root f) contents)
    node_wrapper;
  copy
    (Filename.concat wasmoo_root "tools/node_wrapper.ml")
    (Filename.concat jane_root "node_wrapper/node_wrapper.ml")

let () =
  let js, others =
    List.fold_left traverse StringSet.empty roots
    |> StringSet.partition (fun p -> List.mem_assoc p packages)
  in
  pin_packages ();
  install_others others;
  sync_exec (fun () -> clone "ocaml-uri" "https://github.com/mirage/ocaml-uri") [ () ];
  sync_exec (fun () -> exec_async "opam install uri --deps-only") [ () ];
  sync_exec
    (fun nm ->
      let branch = branch nm in
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
      let p = if Sys.win32 then "patch --binary" else "patch" in
      let ch =
        Unix.open_process_out
          (Printf.sprintf "cd %s/lib/%s && %s -p 1 --" jane_root dir p)
      in
      let patch =
        if Sys.win32
        then String.concat "\r\n" (String.split_on_char '\n' patch)
        else patch
      in
      output_string ch patch;
      match Unix.close_process_out ch with
      | WEXITED 0 -> ()
      | e ->
          let name, i =
            match e with
            | WEXITED n -> "exit", n
            | WSIGNALED n -> "signal", n
            | WSTOPPED n -> "stop", n
          in
          failwith (Printf.sprintf "%s %d while patching %s" name i dir))
    patches;
  List.iter (fun p -> Sys.remove (Printf.sprintf "%s/lib/%s" jane_root p)) removes
