
(library
 ;; compiler/tests-compiler/double-translation/direct_calls.ml
 (name direct_calls_47)
 (enabled_if true)
 (modules direct_calls)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if true)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/double-translation/effects_continuations.ml
 (name effects_continuations_47)
 (enabled_if true)
 (modules effects_continuations)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if true)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/double-translation/effects_exceptions.ml
 (name effects_exceptions_47)
 (enabled_if true)
 (modules effects_exceptions)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if true)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/double-translation/effects_toplevel.ml
 (name effects_toplevel_47)
 (enabled_if true)
 (modules effects_toplevel)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if true)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))
