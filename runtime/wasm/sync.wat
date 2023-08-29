(module
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "custom" "custom_compare_id"
      (func $custom_compare_id
        (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (import "custom" "custom_hash_id"
      (func $custom_hash_id (param (ref eq)) (result i32)))
   (import "custom" "custom_next_id" (func $custom_next_id (result i64)))

   (type $string (array (mut i8)))
   (type $value->value->int->int
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $value->int
      (func (param (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field $cust_id (ref $string))
         (field $cust_compare (ref null $value->value->int->int))
         (field $cust_compare_ext (ref null $value->value->int->int))
         (field $cust_hash (ref null $value->int))
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))
   (type $custom_with_id
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $id i64))))

   (global $mutex_ops (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 6 ;; "_mutex"
            (i32.const 95) (i32.const 109) (i32.const 117) (i32.const 116)
            (i32.const 101) (i32.const 120))
         (ref.func $custom_compare_id)
         (ref.null $value->value->int->int)
         (ref.func $custom_hash_id)))

   (type $mutex
      (sub final $custom_with_id
         (struct
            (field (ref $custom_operations))
            (field i64)
            (field $state (mut i32)))))

   (func (export "caml_ml_mutex_new") (param (ref eq)) (result (ref eq))
      (struct.new $mutex
         (global.get $mutex_ops) (call $custom_next_id) (i32.const 0)))

   (data $lock_failure "Mutex.lock: mutex already locked. Cannot wait.")

   (func (export "caml_ml_mutex_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast (ref $mutex) (local.get 0)))
      (if (struct.get $mutex $state (local.get $t))
         (then
            (call $caml_failwith
               (array.new_data $string $lock_failure
                  (i32.const 0) (i32.const 46)))))
      (struct.set $mutex $state (local.get $t) (i32.const 1))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_try_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast (ref $mutex) (local.get 0)))
      (if (result (ref eq)) (struct.get $mutex $state (local.get $t))
         (then
            (i31.new (i32.const 0)))
         (else
            (struct.set $mutex $state (local.get $t) (i32.const 1))
            (i31.new (i32.const 1)))))

   (func (export "caml_ml_mutex_unlock") (param (ref eq)) (result (ref eq))
      (struct.set $mutex $state
         (ref.cast (ref $mutex) (local.get 0)) (i32.const 0))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_condition_new") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (data $condition_failure "Condition.wait: cannot wait")

   (func (export "caml_ml_condition_wait")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $condition_failure
            (i32.const 0) (i32.const 27)))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_condition_signal") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_condition_broadcast")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)
