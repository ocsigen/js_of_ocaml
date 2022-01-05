
(library
 (name jsooexp_array_access)
 (modules array_access)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_build_path_prefix_map)
 (modules build_path_prefix_map)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_call_gen)
 (modules call_gen)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_cond)
 (modules cond)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_eliminate_exception_handler)
 (modules eliminate_exception_handler)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_empty_cma)
 (modules empty_cma)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_end_to_end)
 (modules end_to_end)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_error)
 (modules error)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_exceptions)
 (modules exceptions)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_exports)
 (modules exports)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_getenv)
 (modules getenv)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_gh1007)
 (modules gh1007)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_gh1051)
 (modules gh1051)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_gl507)
 (modules gl507)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_inlining)
 (modules inlining)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_js_parser_printer)
 (modules js_parser_printer)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_lazy)
 (modules lazy)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_macro)
 (modules macro)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_match_with_exn)
 (modules match_with_exn)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_mutable_closure)
 (modules mutable_closure)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_obj)
 (modules obj)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_obj_dup)
 (modules obj_dup)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_side_effect)
 (modules side_effect)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_sourcemap)
 (modules sourcemap)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_static_eval)
 (modules static_eval)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_sys_command)
 (modules sys_command)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_sys_fs)
 (modules sys_fs)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_tailcall)
 (modules tailcall)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_target_env)
 (modules target_env)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_unix_fs)
 (modules unix_fs)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 (name jsooexp_variable_declaration_output)
 (modules variable_declaration_output)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))
