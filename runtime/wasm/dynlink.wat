(module
   (import "jslib" "log_str" (func $log_str (param (ref $string))))

   (type $string (array (mut i8)))

   (data $caml_dynlink_close_lib "caml_dynlink_close_lib")

   (func (export "caml_dynlink_close_lib")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $caml_dynlink_close_lib
            (i32.const 0) (i32.const 22)))
      (ref.i31 (i32.const 0)))

   (data $caml_dynlink_lookup_symbol "caml_dynlink_lookup_symbol")

   (func (export "caml_dynlink_lookup_symbol")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $caml_dynlink_lookup_symbol
            (i32.const 0) (i32.const 26)))
      (ref.i31 (i32.const 0)))
)
