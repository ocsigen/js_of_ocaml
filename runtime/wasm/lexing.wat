(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_new_lex_engine")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_new_lex_engine"))
      (i31.new (i32.const 0)))

   (func (export "caml_lex_engine")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_lex_engine"))
      (i31.new (i32.const 0)))
)
