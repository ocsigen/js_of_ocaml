
(library
 ;; lib/tests/css_a.ml
 (name css_a_75)
 (enabled_if true)
 (modules css_a)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/css_c.ml
 (name css_c_75)
 (enabled_if true)
 (modules css_c)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/css_l.ml
 (name css_l_75)
 (enabled_if true)
 (modules css_l)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/fcall.ml
 (name fcall_75)
 (enabled_if true)
 (modules fcall)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/fcall2.ml
 (name fcall2_75)
 (enabled_if true)
 (modules fcall2)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/fserr.ml
 (name fserr_75)
 (enabled_if true)
 (modules fserr)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/json.ml
 (name json_75)
 (enabled_if true)
 (modules json)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/poly_comp.ml
 (name poly_comp_75)
 (enabled_if true)
 (modules poly_comp)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/poly_eq.ml
 (name poly_eq_75)
 (enabled_if true)
 (modules poly_eq)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/regexp.ml
 (name regexp_75)
 (enabled_if true)
 (modules regexp)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/sys.ml
 (name sys_75)
 (enabled_if true)
 (modules sys)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/typedarray.ml
 (name typedarray_75)
 (enabled_if true)
 (modules typedarray)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/uget_uset.ml
 (name uget_uset_75)
 (enabled_if true)
 (modules uget_uset)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))

(library
 ;; lib/tests/url.ml
 (name url_75)
 (enabled_if true)
 (modules url)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js wasm))
 (preprocess
  (pps ppx_js_internal ppx_expect)))
