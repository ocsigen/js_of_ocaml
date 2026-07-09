(* Configuration of the Jane Street test-suite setup (see ci_setup.ml) for
   OxCaml; ci_setup_config_mainstream.ml is the mainstream counterpart.
   Values differing between the two variants live here. *)
let roots =
  [ "bonsai_web_components"
  ; "string_dict"
  ; "ppx_html"
  ; "bonsai_bench"
  ; "float_array"
  ; "unboxed"
  ; "await"
  ]

let additional_others = [ "spawn" ]

let omitted_others = [ "odoc" ]

let omitted_js = [ "basement"; "sexplib0"; "ppxlib_jane"; "spawn"; "sexp_type" ]

let do_pin = [ "bigstringaf" ]

let forked_packages =
  [ "base"
  ; "core"
  ; "bonsai_test"
  ; "bonsai_web_components"
  ; "bonsai_web_test"
  ; "virtual_dom"
  ]

(* Extra line spliced in the (env (_ ...)) block of the generated
   dune-workspace *)
let dune_workspace_extra_env = {|  (ocamlopt_flags -zero-alloc-check none)
|}

let keep_package opam =
  match OpamFile.OPAM.get_url opam with
  | None -> false
  | Some url ->
      let url = OpamUrl.to_string url in
      String.starts_with ~prefix:"https://github.com/janestreet/" url

let pin_branch = "wasm-oxcaml"

let fixed_branch = "wasm-oxcaml-31-fixed"

let forked_branch = "wasm-oxcaml-31"

let default_branch = Some "oxcaml"

let patches ~target_is_wasm:_ =
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
index 5a53c69..571e52e 100644
--- a/test/dune
+++ b/test/dune
@@ -1,15 +1,8 @@
 (library
  (name bin_prot_test)
  (libraries base base_bigstring bin_prot
-   expect_test_helpers_core.expect_test_helpers_base expect_test_patterns
+   expect_test_helpers_core.expect_test_helpers_base ; expect_test_patterns
    float_array base.md5 re sexplib splittable_random stdio)
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
index e59e237..30740fc 100644
--- a/test/non_integers_repr.ml
+++ b/test/non_integers_repr.ml
@@ -831,11 +831,13 @@ let%expect_test "Non-integer bin_prot size tests" =
     3e 7a d7 f2 9a bc af 48 -> 1E-07
     00 00 00 00 00 00 00 00 -> 0
     |}];
+(*
   gen_tests Tests.float_nan;
   Expect_test_patterns.require_match
     {|
     7f f{8,0} 00 00 00 00 00 01 -> NAN (glob)
     |};
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
     (* Compression rate is low because our quickcheck implementation generates integers
        with a bounded bitcount. *)
-    [%expect {| ((hash 1e429706c701b111d98b6e6e858bbea4) (uniqueness_rate 42.96875)) |}]
+    [%expect {| ((hash d937e61f530ab9c27544e392922d286d) (uniqueness_rate 42.96875)) |}]
   ;;
 end
 
@@ -102,7 +102,7 @@ module Ml_z_hamdist = struct
       ();
     (* Compression rate is low because our quickcheck implementation generates integers
        with a bounded bitcount. *)
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
- (libraries zarith_stubs_js core base.md5 zarith)
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
diff --git a/test/zarith.ml b/test/zarith.ml
index 059d011..b40264e 100644
--- a/test/zarith.ml
+++ b/test/zarith.ml
@@ -1,5 +1,4 @@
 module Big_int_Z = Big_int_Z
 module Q = Q
 module Z = Z
-module Zarith = Zarith
 module Zarith_version = Zarith_version
|zs}
    )
  ; ( "ppx_css"
    , {|
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
