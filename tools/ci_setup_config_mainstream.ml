(* Configuration of the Jane Street test-suite setup (see ci_setup.ml) for
   the mainstream compiler; ci_setup_config_oxcaml.ml is the OxCaml
   counterpart. Values differing between the two variants live here.

   The packages come from the "public release reloaded" opam repository,
   which provides Jane Street's preview packages for OCaml 5.5. They are
   used as is: the inline tests are enabled below, and the remaining
   adjustments are the patches at the end of this file. *)
let roots =
  [ "bonsai_web_components"
  ; "string_dict"
  ; "ppx_html"
  ; "bonsai_bench"
  ; "float_array"
  ; (* Test dependencies *)
    "expect_test_patterns"
  ; "ppx_bin_and_sexp_digest"
  ]

let additional_others = []

let omitted_others = []

let omitted_js = [ "basement"; "sexplib0"; "sexp_type" ]

let do_pin = [ "bigstringaf" ]

let forked_packages = []

let fixed_packages = []

(* Extra line spliced in the (env (_ ...)) block of the generated
   dune-workspace *)
let dune_workspace_extra_env = ""

let keep_package _ = true

let pin_branch = "wasm-latest"

(* No forked packages *)
let fixed_branch = ""

let forked_branch = ""

let default_branch = None

let inline_tests =
  [ "base_bigstring", "test/dune", ""
  ; "bignum", "test/src/dune", ""
  ; "bin_prot", "test/dune", ""
  ; "string_dict", "test/dune", ""
  ; "core", "command/src/dune", ""
  ; "core", "core/src/dune", ""
  ; "core", "core/test/dune", " (deps sample_time_zone_file)"
  ; "core", "validate/test/dune", ""
  ; "bonsai_test", "computation_report/dune", ""
  ; "bonsai_test", "of_bonsai_itself/dune", ""
  ; "bonsai_web_test", "of_bonsai_itself/dune", ""
  ; "bonsai_web_components", "accordion/test/dune", ""
  ; "bonsai_web_components", "auto_generated/test/dune", ""
  ; "bonsai_web_components", "bindings/dygraph/src/dune", ""
  ; "bonsai_web_components", "bonsai_codemirror/themes/dune", ""
  ; "bonsai_web_components", "bonsai_codemirror/themes/test/dune", ""
  ; "bonsai_web_components", "drag_and_drop/test/dune", ""
  ; ( "bonsai_web_components"
    , "element_size_hooks/test/of_element_size_hooks_itself/dune"
    , "" )
  ; "bonsai_web_components", "experimental/dagviz/test/dune", ""
  ; "bonsai_web_components", "experimental/snips/test/dune", ""
  ; "bonsai_web_components", "extendy/test/dune", ""
  ; "bonsai_web_components", "file/test/dune", ""
  ; "bonsai_web_components", "form/test/dune", ""
  ; "bonsai_web_components", "freeform_multiselect/test/dune", ""
  ; "bonsai_web_components", "gauge/test/dune", ""
  ; "bonsai_web_components", "low_level_vdom/test/dune", ""
  ; "bonsai_web_components", "multi_select/focus_ring/src/dune", ""
  ; "bonsai_web_components", "multi_select/test/dune", ""
  ; "bonsai_web_components", "nav_link/test/dune", ""
  ; "bonsai_web_components", "not_connected_warning_box/test/dune", ""
  ; "bonsai_web_components", "partial_render_table/computation_report/dune", ""
  ; "bonsai_web_components", "partial_render_table/test/dune", ""
  ; "bonsai_web_components", "popover_deprecated/test/dune", ""
  ; "bonsai_web_components", "query_box/test/dune", ""
  ; "bonsai_web_components", "reorderable_list/test/dune", ""
  ; "bonsai_web_components", "split_pane/test/dune", ""
  ; "bonsai_web_components", "tabs/test/dune", ""
  ; "bonsai_web_components", "typeahead/test/dune", ""
  ; "virtual_dom", "test/dune", ""
  ; "zarith_stubs_js", "test/dune", ""
  ]

let patches ~target_is_wasm:_ =
  [ ( "zarith_stubs_js"
    , (* The test directory defines a Zarith module, which conflicts with
         the zarith library: use a wrapper library. Disable the checks
         relying on the layout of Jane Street's repository. The quickcheck
         hashes differ in JavaScript and Wasm. *)
      {zs|
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
index 9917cdc..60ddd4a 100644
--- a/test/dune
+++ b/test/dune
@@ -1,7 +1,8 @@
 (library
  (name zarith_stubs_js_test)
- (libraries zarith_stubs_js core base.md5 zarith)
+ (libraries zarith_stubs_js core base.md5 zarith_wrapper)
  (flags :standard -w -60)
+ (modules (:standard \ zarith))
  (preprocess
   (pps ppx_jane -compare-require-explicit-locality)))
 
@@ -34,10 +35,16 @@
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
  ; ( "core"
    , (* Current names of the extern conversion instructions *)
      {|
diff --git a/core/src/runtime.wat b/core/src/runtime.wat
index 1af7decc..c979f16a 100644
--- a/core/src/runtime.wat
+++ b/core/src/runtime.wat
@@ -154,13 +154,13 @@
    (func $bigstring_destroy_stub (export "bigstring_destroy_stub")
       (param $v (ref eq)) (result (ref eq))
       (if (ref.test (ref i31)
-            (extern.internalize (call $caml_ba_get_data (local.get $v))))
+            (any.convert_extern (call $caml_ba_get_data (local.get $v))))
          (then
             (call $caml_invalid_argument
                (array.new_data $string $bigstring_destroy_already_deallocated
                   (i32.const 0) (i32.const 51)))))
       (call $caml_ba_set_data (local.get $v)
-         (extern.externalize (ref.i31 (i32.const 0))))
+         (extern.convert_any (ref.i31 (i32.const 0))))
       (array.set $int_array (call $caml_ba_get_dim (local.get $v)) (i32.const 0)
          (i32.const 0))
       (ref.i31 (i32.const 0)))
@@ -176,7 +176,7 @@
       (local $new_data (ref extern))
       (local.set $size (i31.get_u (ref.cast (ref i31) (local.get $vsize))))
       (if (ref.test (ref i31)
-            (extern.internalize
+            (any.convert_extern
                (call $caml_ba_get_data (local.get $bigstring))))
          (then
             (call $caml_invalid_argument
|}
    )
  ; ( "browser"
    , (* Parameterized Dom_html.messageEvent of js_of_ocaml > 6.3.0
         (https://github.com/janestreet/browser/pull/1) *)
      {|
diff --git a/extra/coercion.ml b/extra/coercion.ml
index bbdcf81..f9a049c 100644
--- a/extra/coercion.ml
+++ b/extra/coercion.ml
@@ -234,11 +234,13 @@ let of_jsoo_media_event (event : Dom_html.mediaEvent Js.t) =
 
 (* Message Event *)
 let to_jsoo_message_event (event : message_event Js.t) =
-  let event : Dom_html.messageEvent Js.t = Js.Unsafe.coerce event in
+  let event : (Js.Unsafe.top, Js.Unsafe.any Js.opt) Dom_html.messageEvent Js.t =
+    Js.Unsafe.coerce event
+  in
   event
 ;;
 
-let of_jsoo_message_event (event : Dom_html.messageEvent Js.t) =
+let of_jsoo_message_event (event : (_, _) Dom_html.messageEvent Js.t) =
   let event : message_event Js.t = Js.Unsafe.coerce event in
   event
 ;;
diff --git a/extra/coercion.mli b/extra/coercion.mli
index 6499688..d8c57ee 100644
--- a/extra/coercion.mli
+++ b/extra/coercion.mli
@@ -112,6 +112,8 @@ val to_jsoo_media_event : event Js.t -> Dom_html.mediaEvent Js.t
 val of_jsoo_media_event : Dom_html.mediaEvent Js.t -> event Js.t
 
 (** Message Event conversions *)
-val to_jsoo_message_event : message_event Js.t -> Dom_html.messageEvent Js.t
+val to_jsoo_message_event
+  :  message_event Js.t
+  -> (Js.Unsafe.top, Js.Unsafe.any Js.opt) Dom_html.messageEvent Js.t
 
-val of_jsoo_message_event : Dom_html.messageEvent Js.t -> message_event Js.t
+val of_jsoo_message_event : (_, _) Dom_html.messageEvent Js.t -> message_event Js.t
|}
    )
  ]
