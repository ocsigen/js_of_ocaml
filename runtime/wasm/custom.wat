(module
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

   (func (export "custom_compare_id")
      (param (ref eq)) (param (ref eq)) (param i32) (result i32)
      (local $i1 i64) (local $i2 i64)
      (local.set $i1
         (struct.get $custom_with_id $id
            (ref.cast $custom_with_id (local.get 0))))
      (local.set $i2
         (struct.get $custom_with_id $id
            (ref.cast $custom_with_id (local.get 1))))
      (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
               (i64.lt_s (local.get $i1) (local.get $i2))))

   (func (export "custom_hash_id") (param (ref eq)) (result i32)
      (i32.wrap_i64
         (struct.get $custom_with_id $id
           (ref.cast $custom_with_id (local.get 0)))))

   (global $next_id (mut i64) (i64.const 0))

   (func (export "custom_next_id") (result i64)
      (local $id i64)
      (local.set $id (global.get $next_id))
      (global.set $next_id (i64.add (local.get $id) (i64.const 1)))
      (local.get $id))
)
