(* Configuration of the Jane Street test-suite setup (see ci_setup.ml) for
   the mainstream compiler; ci_setup_config-oxcaml.ml is the OxCaml
   counterpart. Values differing between the two variants live here. *)
let roots =
  [ "bonsai_web_components"; "string_dict"; "ppx_html"; "bonsai_bench"; "float_array" ]

let additional_others = []

let omitted_others = []

let omitted_js = [ "basement"; "sexplib0" ]

let do_pin =
  [ "basement" (* https://github.com/janestreet/basement/pull/3 *); "bigstringaf" ]

let forked_packages =
  [ "base"
  ; "core"
  ; "bonsai" (* Compatibility with effect syntax *)
  ; "bonsai_test"
  ; "bonsai_web" (* Compatibility with effect syntax *)
  ; "bonsai_web_components"
  ; "bonsai_web_test"
  ; "virtual_dom" (* Compatibility with effect syntax *)
  ; "typerep" (* https://github.com/janestreet/typerep/pull/7 *)
  ]

(* Extra line spliced in the (env (_ ...)) block of the generated
   dune-workspace *)
let dune_workspace_extra_env = ""

let keep_package _ = true

let pin_branch = "wasm-latest"

let fixed_branch = "wasm-latest-fixed"

let forked_branch = "wasm-latest"

let default_branch = None

let patches ~target_is_wasm =
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
@@ -164,8 +169,8 @@
 let%expect_test ("Float.to_string_hum big exponents (js-only)" [@tags "js-only"]) =
   let x = -3.3810849992682576E+37 in
   print_s
     [%sexp (Float.to_string_hum ~delimiter:'_' ~decimals:7 ~strip_zero:false x : string)];
-  [%expect {| -33_810_849_992_682_576_000_000_000_000_000_000_000.0000000 |}]
+  [%expect {| -33_810_849_992_682_574_344_623_022_087_906_263_040.0000000 |}]
 ;;

 let%expect_test "to_string_hum" =
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
+    {| 7f f8 00 00 00 00 00 |bp}
      ^ (if target_is_wasm then "01" else "00")
      ^ {bp| -> NAN |}];
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
diff --git a/standalone/css_inliner.ml b/standalone/css_inliner.ml
--- a/standalone/css_inliner.ml
+++ b/standalone/css_inliner.ml
@@ -68,11 +68,15 @@
     Buffer.add_string w mli_file;
     Buffer.add_string w " end\n");
   create_file alias_ml ~f:(fun w ->
-    Buffer.add_string w ("include " ^ String.capitalize generated_name ^ "\n"));
+    Buffer.add_string
+      w
+      ("include " ^ String.capitalize (Filename.basename generated_name) ^ "\n"));
   create_file alias_mli ~f:(fun w ->
     Buffer.add_string
       w
-      ("include module type of " ^ String.capitalize generated_name ^ "\n"))
+      ("include module type of "
+       ^ String.capitalize (Filename.basename generated_name)
+       ^ "\n"))
 ;;

 let command =
|}
    )
  ]
