(module
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $string (array (mut i8)))
   (type $value->value->int
      (func (param (ref eq)) (param (ref eq)) (result i32)))
   (type $value->int
      (func (param (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field (ref $string)) ;; identifier
         (field (ref $value->value->int)) ;; compare
         (field (ref null $value->int)) ;; hash
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))

   (global $mutex_ops (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string ;; "_mutex"
            (i32.const 95) (i32.const 109) (i32.const 117) (i32.const 116)
            (i32.const 101) (i32.const 120))
         (ref.func $mutex_cmp)
         (ref.func $mutex_hash)))

   (type $mutex
      (sub $custom
         (struct
            (field (ref $custom_operations)) (field i32) (field (mut i32)))))

   (func $mutex_cmp (param (ref eq)) (param (ref eq)) (result i32)
      (local $i1 i32) (local $i2 i32)
      (local.set $i1 (struct.get $mutex 1 (ref.cast $mutex (local.get 0))))
      (local.set $i2 (struct.get $mutex 1 (ref.cast $mutex (local.get 1))))
      (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
               (i32.lt_s (local.get $i1) (local.get $i2))))

   (func $mutex_hash (param (ref eq)) (result i32)
      (struct.get $mutex 1 (ref.cast $mutex (local.get 0))))

   (global $next_mutex_id (mut i32) (i32.const 0))

   (func (export "caml_ml_mutex_new") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $next_mutex_id))
      (global.set $next_mutex_id (i32.add (local.get $id) (i32.const 1)))
      (struct.new $mutex (global.get $mutex_ops) (local.get $id) (i32.const 0)))

   (data $lock_failure "Mutex.lock: mutex already locked. Cannot wait.")

   (func (export "caml_ml_mutex_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast $mutex (local.get 0)))
      (if (struct.get $mutex 2 (local.get $t))
         (then
            (call $caml_failwith
               (array.new_data $string $lock_failure
                  (i32.const 0) (i32.const 46)))))
      (struct.set $mutex 2 (local.get $t) (i32.const 1))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_try_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast $mutex (local.get 0)))
      (if (result (ref eq)) (struct.get $mutex 2 (local.get $t))
         (then
            (i31.new (i32.const 0)))
         (else
            (struct.set $mutex 2 (local.get $t) (i32.const 1))
            (i31.new (i32.const 1)))))

   (func (export "caml_ml_mutex_unlock") (param (ref eq)) (result (ref eq))
      (struct.set $mutex 2 (ref.cast $mutex (local.get 0)) (i32.const 0))
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
