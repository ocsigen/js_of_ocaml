
(library
 ;; compiler/tests-compiler/array_access.ml
 (name array_access_15)
 (modules array_access)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/build_path_prefix_map.ml
 (name build_path_prefix_map_15)
 (modules build_path_prefix_map)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/call_gen.ml
 (name call_gen_15)
 (modules call_gen)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/cond.ml
 (name cond_15)
 (modules cond)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/eliminate_exception_handler.ml
 (name eliminate_exception_handler_15)
 (modules eliminate_exception_handler)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/empty_cma.ml
 (name empty_cma_15)
 (modules empty_cma)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/end_to_end.ml
 (name end_to_end_15)
 (modules end_to_end)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/error.ml
 (name error_15)
 (modules error)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/exceptions.ml
 (name exceptions_15)
 (modules exceptions)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/exports.ml
 (name exports_15)
 (modules exports)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/getenv.ml
 (name getenv_15)
 (modules getenv)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1007.ml
 (name gh1007_15)
 (modules gh1007)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1051.ml
 (name gh1051_15)
 (modules gh1051)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1320.ml
 (name gh1320_15)
 (modules gh1320)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gl507.ml
 (name gl507_15)
 (modules gl507)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/inlining.ml
 (name inlining_15)
 (modules inlining)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/js_parser_printer.ml
 (name js_parser_printer_15)
 (modules js_parser_printer)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/lazy.ml
 (name lazy_15)
 (modules lazy)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/loops.ml
 (name loops_15)
 (modules loops)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/macro.ml
 (name macro_15)
 (modules macro)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/match_with_exn.ml
 (name match_with_exn_15)
 (modules match_with_exn)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/minify.ml
 (name minify_15)
 (modules minify)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/mutable_closure.ml
 (name mutable_closure_15)
 (modules mutable_closure)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/obj.ml
 (name obj_15)
 (modules obj)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/obj_dup.ml
 (name obj_dup_15)
 (modules obj_dup)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/side_effect.ml
 (name side_effect_15)
 (modules side_effect)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/sourcemap.ml
 (name sourcemap_15)
 (modules sourcemap)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/static_eval.ml
 (name static_eval_15)
 (modules static_eval)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/sys_command.ml
 (name sys_command_15)
 (modules sys_command)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/sys_fs.ml
 (name sys_fs_15)
 (modules sys_fs)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/tailcall.ml
 (name tailcall_15)
 (modules tailcall)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/target_env.ml
 (name target_env_15)
 (modules target_env)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/unix_fs.ml
 (name unix_fs_15)
 (modules unix_fs)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/variable_declaration_output.ml
 (name variable_declaration_output_15)
 (modules variable_declaration_output)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))
