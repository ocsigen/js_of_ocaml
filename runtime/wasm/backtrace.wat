(module
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (func (export "caml_get_exception_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_backtrace_status")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_convert_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_raw_backtrace_next_slot")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (data $raw_backtrace_slot_err
      "Printexc.get_raw_backtrace_slot: index out of bounds")

   (func (export "caml_raw_backtrace_slot")
      (param (ref eq) (ref eq)) (result (ref eq))
      (call $caml_invalid_argument
          (array.new_data $string $raw_backtrace_slot_err
             (i32.const 0) (i32.const 52)))
      (i31.new (i32.const 0)))

   (func (export "caml_convert_raw_backtrace_slot")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_restore_raw_backtrace")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_get_current_callstack")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_ml_debug_info_status")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_record_backtrace") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)
