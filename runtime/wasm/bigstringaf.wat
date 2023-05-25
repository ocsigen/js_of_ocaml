(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "bigstringaf_blit_from_bytes")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bigstringaf_blit_from_bytes"))
      (i31.new (i32.const 0)))

   (func (export "bigstringaf_blit_to_bytes")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bigstringaf_blit_to_bytes"))
      (i31.new (i32.const 0)))
)
