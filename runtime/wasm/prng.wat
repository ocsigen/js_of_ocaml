(module
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "bindings" "ta_get_i32"
      (func $ta_get_i32 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_set_i32"
      (func $ta_set_i32 (param (ref extern)) (param i32) (param i32)))

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
   (type $int_array (array (mut i32)))
   (type $bigarray
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field (ref extern)) ;; data
            (field (ref $int_array)) ;; size in each dimension
            (field i8) ;; number of dimensions
            (field i8) ;; kind
            (field i8)))) ;; layout

   (func (export "caml_lxm_next") (param $v (ref eq)) (result (ref eq))
      (local $data (ref extern))
      (local $a i64) (local $s i64) (local $q0 i64) (local $q1 i64)
      (local $z i64)
      (local.set $data
         (struct.get $bigarray 1 (ref.cast $bigarray (local.get $v))))
      (local.set $a
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 0)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 1)))
               (i64.const 32))))
      (local.set $s
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 2)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 3)))
               (i64.const 32))))
      (local.set $q0
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 4)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 5)))
               (i64.const 32))))
      (local.set $q1
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 6)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 7)))
               (i64.const 32))))
      (local.set $z (i64.add (local.get $s) (local.get $q0)))
      (local.set $z
         (i64.mul (i64.xor (local.get $z)
                           (i64.shr_u (local.get $z) (i64.const 32)))
                  (i64.const 0xdaba0b6eb09322e3)))
      (local.set $z
         (i64.mul (i64.xor (local.get $z)
                           (i64.shr_u (local.get $z) (i64.const 32)))
                  (i64.const 0xdaba0b6eb09322e3)))
      (local.set $z
         (i64.xor (local.get $z) (i64.shr_u (local.get $z) (i64.const 32))))
      (local.set $s
         (i64.add (i64.mul (local.get $s) (i64.const 0xd1342543de82ef95))
                  (local.get $a)))
      (call $ta_set_i32 (local.get $data) (i32.const 2)
         (i32.wrap_i64 (local.get $s)))
      (call $ta_set_i32 (local.get $data) (i32.const 3)
         (i32.wrap_i64 (i64.shr_u (local.get $s) (i64.const 32))))
      (local.set $q1 (i64.xor (local.get $q1) (local.get $q0)))
      (local.set $q0 (i64.rotl (local.get $q0) (i64.const 24)))
      (local.set $q0 (i64.xor (i64.xor (local.get $q0) (local.get $q1))
                              (i64.shl (local.get $q1) (i64.const 16))))
      (local.set $q1 (i64.rotl (local.get $q1) (i64.const 37)))
      (call $ta_set_i32 (local.get $data) (i32.const 4)
         (i32.wrap_i64 (local.get $q0)))
      (call $ta_set_i32 (local.get $data) (i32.const 5)
         (i32.wrap_i64 (i64.shr_u (local.get $q0) (i64.const 32))))
      (call $ta_set_i32 (local.get $data) (i32.const 6)
         (i32.wrap_i64 (local.get $q1)))
      (call $ta_set_i32 (local.get $data) (i32.const 7)
         (i32.wrap_i64 (i64.shr_u (local.get $q1) (i64.const 32))))
      (return_call $caml_copy_int64 (local.get $z)))
)
