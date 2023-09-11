(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_dynlink_close_lib")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_dynlink_close_lib"))
      (ref.i31 (i32.const 0)))

   (func (export "caml_dynlink_lookup_symbol")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_dynlink_lookup_symbol"))
      (ref.i31 (i32.const 0)))
)
