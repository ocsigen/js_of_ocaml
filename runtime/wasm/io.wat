(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_sys_open")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_open"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_close")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_close"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_set_channel_name")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_set_channel_name"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_out_channels_list")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_out_channels_list"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_open_descriptor_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_open_descriptor_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_close_channel")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_close_channel"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input"))
      (i31.new (i32.const 0)))

   (func (export "caml_input_value") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value"))
      (unreachable))

   (func (export "caml_ml_input_char")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_char"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_int")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_int"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in_64")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in_64"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_out")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_scan_line")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_scan_line"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_flush") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_flush"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_bytes")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_bytes"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_char")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_char"))
      (i31.new (i32.const 0)))

   (func (export "caml_output_value")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value"))
      (unreachable))

   (func (export "caml_ml_output_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_int"))
      (i31.new (i32.const 0)))
)
