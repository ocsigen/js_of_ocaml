(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $string (array (mut i8)))

   (func (export "caml_marshal_data_size")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_marshal_data_size"))
      (i31.new (i32.const 0)))

   (func (export "caml_input_value_from_bytes")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value_from_bytes"))
      (i31.new (i32.const 0)))

   (func (export "caml_output_value_to_buffer")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value_to_buffer"))
      (i31.new (i32.const 0)))

   (func (export "caml_output_value_to_string")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value_to_string"))
      (array.new_fixed $string))
)
