A float converted at toplevel and passed to a JavaScript call a long way
after is stored in a global, so the conversion is not folded into the call:
[fun_call_1] takes a JavaScript value. The conversions passed directly to a
call are still folded: [fun_call_2] takes two f64.

  $ ocamlc -g -c globalize_js_float.ml
  $ wasm_of_ocaml --debug wat globalize_js_float.cmo -o globalize_js_float.wasmo
  $ grep -A1 '"fun_call_[12]"' Globalize_js_float.wat
    (import "Globalize_js_float.fragments" "fun_call_1"
      (func $fun_call_1 (param (ref null extern) (ref null extern))
  --
    (import "Globalize_js_float.fragments" "fun_call_2"
      (func $fun_call_2 (param (ref null extern) f64 f64)
