(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_gc_quick_stat")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_gc_quick_stat"))
      (i31.new (i32.const 0)))

   (func (export "caml_final_register")
     (param (ref eq) (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)
