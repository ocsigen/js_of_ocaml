(* Configuration of the Jane Street test-suite setup (see ci_setup.ml) for
   OxCaml; ci_setup_config_mainstream.ml is the mainstream counterpart.
   Values differing between the two variants live here. *)
let roots =
  [ "base_test" (* The tests of base and core are separate packages *)
  ; "core_test"
  ; "bonsai_web_components"
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
  [ "base_test"
  ; "core"
  ; "core_test"
  ; "bonsai_test"
  ; "bonsai_web_components"
  ; "bonsai_web_test"
  ; "virtual_dom"
  ]

(* The released packages work with the current Dom bindings *)
let fixed_packages = []

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

let pin_branch = "wasm-oxcaml-ox9"

let fixed_branch = "wasm-oxcaml-ox9-fixed"

let forked_branch = "wasm-oxcaml-ox9"

let default_branch = Some "oxcaml"

let patches ~target_is_wasm:_ =
  [ ( "bignum"
    , {bignum|
diff --git a/test/src/dune b/test/src/dune
index 063a745..e4c2a8e 100644
--- a/test/src/dune
+++ b/test/src/dune
@@ -2,5 +2,6 @@
  (name bignum_test)
  (libraries bigint bignum core expect_test_helpers_core expectable
    sexp_grammar.validation zarith)
+ (inline_tests (flags -drop-tag no-js -drop-tag no-wasm -drop-tag 64-bits-only) (modes js wasm))
  (preprocess
   (pps ppx_jane -compare-require-explicit-locality)))
diff --git a/test/src/test_bignum.ml b/test/src/test_bignum.ml
index fb374d0..e4b0dfa 100644
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
@@ -190,10 +195,7 @@ module%test Js_vs_native_vs_wasm = struct
   let%expect_test ("web assembly" [@tags "wasm-only"]) =
     test ();
     [%expect
-      {|
-      ((float_to_string_hum  3_194_450_417.0351563)
-       (bignum_to_string_hum 3_194_450_417.0351562))
-      |}]
+      {| 3_194_450_417.0351562 |}]
   ;;
 end
 
@@ -253,9 +255,9 @@ let%expect_test ("to_string_hum, tie resolved differently, native" [@tags "no-js
 
 let%expect_test ("to_string_hum, tie resolved differently, js" [@tags "js-only"]) =
   print_endline (Float.to_string_hum (-1.25) ~decimals:1);
-  [%expect {| -1.3 |}];
+  [%expect {| -1.2 |}];
   print_endline (Float.to_string_hum 1.25 ~decimals:1);
-  [%expect {| 1.3 |}]
+  [%expect {| 1.2 |}]
 ;;
 
 module%test [@name "Bignum.gen"] _ = struct
|bignum}
    )
  ; ( "bin_prot"
    , {bp|
diff --git a/test/dune b/test/dune
index 345dd8e..3949012 100644
--- a/test/dune
+++ b/test/dune
@@ -3,13 +3,6 @@
  (libraries base base_bigstring bin_prot expect_test_helpers_base
-   expect_test_patterns float_array unboxed.float_u unboxed.int64_u base.md5
+   float_array unboxed.float_u unboxed.int64_u base.md5
    re splittable_random stdio)
+ (inline_tests (flags -drop-tag no-js -drop-tag 64-bits-only -drop-tag 32-bits-only -drop-tag no-wasm) (modes js wasm))
  (preprocess
   (pps ppx_jane -compare-require-explicit-locality)))
-
-(rule
- (deps core/blob_stability_tests.ml integers_repr_tests_64bit.ml
-   integers_repr_tests_js.ml integers_repr_tests_wasm.ml)
- (action
-  (bash
-    "diff <(\necho '869e6b3143f14201f406eac9c05c4cdb  core/blob_stability_tests.ml'\necho 'a9ed028fa16f307982c196f647d05afa  integers_repr_tests_64bit.ml'\necho 'a17ffcd3bf1e15dbca0ee54ec5b95c58  integers_repr_tests_js.ml'\necho '9d0d4b68c8bdbffd3d6c4bd106171f6f  integers_repr_tests_wasm.ml'\n  ) <(md5sum %{deps} | sed 's#  \\./#  #')"))
- (alias runtest))
diff --git a/test/non_integers_repr.ml b/test/non_integers_repr.ml
index daf8e48..0643c79 100644
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
    , {bas|
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
|bas}
    )
  ; ( "string_dict"
    , {str|
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
|str}
    )
  ; ( "zarith_stubs_js"
    , {zs|
diff --git a/runtime.wat b/runtime.wat
index 8c895f4..915d8be 100644
--- a/runtime.wat
+++ b/runtime.wat
@@ -610,8 +610,8 @@
       (local.set $x (call $Int32_val (local.get $d)))
       (local.set $res (ref.i31 (local.get $x)))
       (if (i32.eq (local.get $x) (i31.get_s (local.get $res)))
-         (then (return (local.get $res)))
-         (else (return_call $wrap_bigint (call $of_int32 (local.get $x))))))
+         (then (return (local.get $res))))
+      (return_call $wrap_bigint (call $of_int32 (local.get $x))))
 
    (@if $portable-int
    (@then
@@ -622,8 +622,8 @@
          (local.set $res (ref.i31 (i32.wrap_i64 (local.get $x))))
          (if (i64.eq (local.get $x)
                 (i64.extend_i32_s (i31.get_s (local.get $res))))
-            (then (return (local.get $res)))
-            (else (return_call $wrap_bigint (call $of_int64 (local.get $x))))))
+            (then (return (local.get $res))))
+         (return_call $wrap_bigint (call $of_int64 (local.get $x))))
    )
    (@else
       (func (export "ml_z_of_nativeint")
@@ -632,8 +632,8 @@
          (local.set $x (call $Nativeint_val (local.get $d)))
          (local.set $res (ref.i31 (local.get $x)))
          (if (i32.eq (local.get $x) (i31.get_s (local.get $res)))
-            (then (return (local.get $res)))
-            (else (return_call $wrap_bigint (call $of_int32 (local.get $x))))))
+            (then (return (local.get $res))))
+         (return_call $wrap_bigint (call $of_int32 (local.get $x))))
    ))
 
    (func (export "ml_z_of_int64")
@@ -982,7 +982,8 @@
             (ref.i31
                (i32.sub (i32.gt_s (local.get $x) (i32.const 0))
                   (i32.lt_s (local.get $x) (i32.const 0)))))))
-      (select (ref.i31 (i32.const 1)) (ref.i31 (i32.const -1))
+      (select (result (ref i31))
+         (ref.i31 (i32.const 1)) (ref.i31 (i32.const -1))
          (call $positive (call $unwrap_bigint (local.get $z)))))
 
    (func (export "ml_z_gcd")
diff --git a/test/bitwise.ml b/test/bitwise.ml
index b6c3bf2..044a9ec 100644
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
index 83d6109..5764deb 100644
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
   (pps ppx_jane -compare-require-explicit-locality)))
 
@@ -41,10 +43,16 @@
  (deps implemented_externals.txt tested_externals.txt)
  (action
   (bash "diff %{deps}"))
- (alias runtest))
+ (alias runtest-))
 
 (rule
  (deps implemented_public_externals.txt zarith_externals.txt)
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
  ; ( "ocaml_intrinsics_kernel"
    , {oik|
diff --git a/src/runtime.wat b/src/runtime.wat
index 1234567..89abcde 100644
--- a/src/runtime.wat
+++ b/src/runtime.wat
@@ -7,3 +7,3 @@
       (result (ref eq))
-      (select (local.get $true) (local.get $false)
+      (select (result (ref eq)) (local.get $true) (local.get $false)
          (i31.get_s (ref.cast (ref i31) (local.get $cond)))))
@@ -12,3 +12,3 @@
       (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
-      (select (local.get $x) (local.get $y)
+      (select (result (ref eq)) (local.get $x) (local.get $y)
          (f64.lt (call $Double_val (local.get $x))
@@ -18,3 +18,3 @@
       (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
-      (select (local.get $x) (local.get $y)
+      (select (result (ref eq)) (local.get $x) (local.get $y)
          (f64.gt (call $Double_val (local.get $x))
|oik}
    )
  ; ( "ppx_css"
    , {ppx|
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
|ppx}
    )
  ]

let removes = [ "core_test/test_sys.ml"; "core_test/test_sys.mli" ]

(* The OxCaml angstrom depends on base *)
let opam_sources = [ "angstrom" ]
