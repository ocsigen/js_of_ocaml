(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "obj" "abstract_tag" (global $abstract_tag i32))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param $arg (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (func (export "caml_ephe_create")
      (param (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_create"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_get_data")
      (param (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_get_data"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_set_data")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_set_data"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_set_key")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_set_key"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_unset_key")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_unset_key"))
      (i31.new (i32.const 0)))

   (global $caml_ephe_none (ref eq)
      (array.new_fixed $block (i31.new (global.get $abstract_tag))))

   (data $Weak_create "Weak.create")

   (func (export "caml_weak_create")
      (param $vlen (ref eq)) (result (ref eq))
      (local $len i32)
      (local $res (ref $block))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (if (i32.lt_s (local.get $len) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Weak_create
                  (i32.const 0) (i32.const 11)))))
      (local.set $res
         (array.new $block (global.get $caml_ephe_none)
            (i32.add (local.get $len) (i32.const 3))))
      (array.set $block (local.get $res) (i32.const 0)
         (i31.new (global.get $abstract_tag)))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_create"))
      (local.get $res))

   (func (export "caml_weak_blit")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_blit"))
      (i31.new (i32.const 0)))

   (func (export "caml_weak_check")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_check"))
      (i31.new (i32.const 0)))

   (func (export "caml_weak_get")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_get"))
      (i31.new (i32.const 0)))

   (func (export "caml_weak_get_copy")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_get_copy"))
      (i31.new (i32.const 0)))
)
