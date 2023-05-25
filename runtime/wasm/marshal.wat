(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_marshal_data_size")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_marshal_data_size"))
      (unreachable))

   (func (export "caml_input_value_from_bytes")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value_from_bytes"))
      (unreachable))

   (func (export "caml_output_value_to_buffer")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value_to_buffer"))
      (unreachable))

   (func (export "caml_output_value_to_string")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value_to_string"))
      (unreachable))
)
