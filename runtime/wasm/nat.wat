(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "create_nat")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "create_nat"))
      (ref.i31 (i32.const 0)))

   (func (export "incr_nat")
      (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "incr_nat"))
      (ref.i31 (i32.const 0)))

   (func (export "initialize_nat")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "set_digit_nat")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "set_digit_nat"))
      (ref.i31 (i32.const 0)))

   (func (export "set_to_zero_nat")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "set_to_zero_nat"))
      (ref.i31 (i32.const 0)))
)
