(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_md5_string")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_string"))
      (i31.new (i32.const 0)))

   (func (export "caml_md5_chan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_chan"))
      (i31.new (i32.const 0)))
)
