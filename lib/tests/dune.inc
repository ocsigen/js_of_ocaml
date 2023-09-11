
(library
 ;; lib/tests/test_css_angle.ml
 (name test_css_angle_75)
 (enabled_if true)
 (modules test_css_angle)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_css_color.ml
 (name test_css_color_75)
 (enabled_if true)
 (modules test_css_color)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_css_length.ml
 (name test_css_length_75)
 (enabled_if true)
 (modules test_css_length)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_fun_call.ml
 (name test_fun_call_75)
 (enabled_if (<> %{profile} using-effects))
 (modules test_fun_call)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_json.ml
 (name test_json_75)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules test_json)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_nodejs_filesystem_errors.ml
 (name test_nodejs_filesystem_errors_75)
 (enabled_if true)
 (modules test_nodejs_filesystem_errors)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_poly_compare.ml
 (name test_poly_compare_75)
 (enabled_if true)
 (modules test_poly_compare)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_regexp.ml
 (name test_regexp_75)
 (enabled_if true)
 (modules test_regexp)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_sys.ml
 (name test_sys_75)
 (enabled_if (>= %{ocaml_version} 5))
 (modules test_sys)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_typed_array.ml
 (name test_typed_array_75)
 (enabled_if true)
 (modules test_typed_array)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_unsafe_set_get.ml
 (name test_unsafe_set_get_75)
 (enabled_if true)
 (modules test_unsafe_set_get)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/test_url.ml
 (name test_url_75)
 (enabled_if true)
 (modules test_url)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js))
 (preprocess
  (pps ppx_js_internal ppx_expect)))
