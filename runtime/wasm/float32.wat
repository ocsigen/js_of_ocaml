(module
    (import "fail" "caml_failwith"
        (func $caml_failwith (param (ref eq))))
    (import "marshal" "caml_serialize_int_4"
        (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
    (import "marshal" "caml_deserialize_int_4"
        (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))
    (import "int32" "Int32_val"
        (func $Int32_val (param (ref eq)) (result i32)))
    (import "int32" "caml_copy_int32"
        (func $caml_copy_int32 (param i32) (result (ref eq))))
    (import "int64" "Int64_val"
        (func $Int64_val (param (ref eq)) (result i64)))
    (import "int64" "caml_copy_int64"
        (func $caml_copy_int64 (param i64) (result (ref eq))))
    (import "float" "caml_float_of_string"
        (func $caml_float_of_string (param (ref eq)) (result (ref eq))))
    (import "float" "caml_format_float"
        (func $caml_format_float (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "float" "caml_fma_float"
        (func $caml_fma_float (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "float" "caml_erf_float"
        (func $caml_erf_float (param f64) (result f64)))
    (import "float" "caml_erfc_float"
        (func $caml_erfc_float (param f64) (result f64)))
    (import "float" "caml_frexp_float"
        (func $caml_frexp_float (param (ref eq)) (result (ref eq))))
    (import "float" "caml_ldexp_float"
      (func $caml_ldexp_float (param f64) (param (ref eq)) (result f64)))
    (import "bigarray" "caml_ba_get_1"
        (func $caml_ba_get_1 (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "bigarray" "caml_ba_get_2"
        (func $caml_ba_get_2 (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "bigarray" "caml_ba_get_3"
        (func $caml_ba_get_3 (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "bigarray" "caml_ba_set_1"
        (func $caml_ba_set_1 (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "bigarray" "caml_ba_set_2"
        (func $caml_ba_set_2 (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "bigarray" "caml_ba_set_3"
        (func $caml_ba_set_3 (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
    (import "bigarray" "caml_ba_uint8_get32"
        (func $caml_ba_uint8_get32 (param (ref eq)) (param (ref eq)) (result i32)))
    (import "bigarray" "caml_ba_uint8_set32"
        (func $caml_ba_uint8_set32 (param (ref eq)) (param (ref eq)) (param i32) (result (ref eq))))
    (import "string" "caml_string_get32"
        (func $caml_string_get32 (param (ref eq)) (param (ref eq)) (result i32)))
    (import "string" "caml_bytes_get32"
        (func $caml_bytes_get32 (param (ref eq)) (param (ref eq)) (result i32)))
    (import "string" "caml_bytes_set32"
        (func $caml_bytes_set32 (param (ref eq)) (param (ref eq)) (param i32) (result (ref eq))))
    (import "array" "caml_make_vect"
        (func $caml_make_vect (param (ref eq)) (param (ref eq)) (result (ref eq))))

    (import "Math" "sin" (func $sin (param f64) (result f64)))
    (import "Math" "cos" (func $cos (param f64) (result f64)))
    (import "Math" "tan" (func $tan (param f64) (result f64)))
    (import "Math" "asin" (func $asin (param f64) (result f64)))
    (import "Math" "acos" (func $acos (param f64) (result f64)))
    (import "Math" "atan" (func $atan (param f64) (result f64)))
    (import "Math" "atan2" (func $atan2 (param f64) (param f64) (result f64)))
    (import "Math" "exp" (func $exp (param f64) (result f64)))
    (import "Math" "log" (func $log (param f64) (result f64)))
    (import "Math" "pow" (func $pow (param f64) (param f64) (result f64)))
    (import "Math" "fmod" (func $fmod (param f64) (param f64) (result f64)))
    (import "Math" "expm1" (func $expm1 (param f64) (result f64)))
    (import "Math" "log1p" (func $log1p (param f64) (result f64)))
    (import "Math" "log2" (func $log2 (param f64) (result f64)))
    (import "Math" "hypot" (func $hypot (param f64) (param f64) (result f64)))
    (import "Math" "log10" (func $log10 (param f64) (result f64)))
    (import "Math" "cosh" (func $cosh (param f64) (result f64)))
    (import "Math" "acosh" (func $acosh (param f64) (result f64)))
    (import "Math" "sinh" (func $sinh (param f64) (result f64)))
    (import "Math" "asinh" (func $asinh (param f64) (result f64)))
    (import "Math" "tanh" (func $tanh (param f64) (result f64)))
    (import "Math" "atanh" (func $atanh (param f64) (result f64)))
    (import "Math" "cbrt" (func $cbrt (param f64) (result f64)))

    (type $float (struct (field f64)))

    (func $box_float
        (param $f f64) (result (ref eq))
        (struct.new $float (local.get $f)))

    (func $unbox_float
        (param $f (ref eq)) (result f64)
        (struct.get $float 0 (ref.cast (ref $float) (local.get $f))))

    (type $block (array (mut (ref eq))))
    (type $bytes (array (mut i8)))
    (type $compare
        (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
    (type $hash
        (func (param (ref eq)) (result i32)))
    (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
    (type $serialize
        (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
    (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
    (type $dup (func (param (ref eq)) (result (ref eq))))
    (type $custom_operations
        (struct
            (field $id (ref $bytes))
            (field $compare (ref null $compare))
            (field $compare_ext (ref null $compare))
            (field $hash (ref null $hash))
            (field $fixed_length (ref null $fixed_length))
            (field $serialize (ref null $serialize))
            (field $deserialize (ref null $deserialize))
            (field $dup (ref null $dup))))
    (type $custom (sub (struct (field (ref $custom_operations)))))

    (global $float32_ops (export "float32_ops") (ref $custom_operations)
        (struct.new $custom_operations
            (array.new_fixed $bytes 4 (i32.const 95) (i32.const 102) (i32.const 51) (i32.const 50)) ;; "_f32"
            (ref.func $float32_cmp)
            (ref.null $compare)
            (ref.func $float32_hash)
            (struct.new $fixed_length (i32.const 4) (i32.const 4))
            (ref.func $float32_serialize)
            (ref.func $float32_deserialize)
            (ref.func $float32_dup)))

    (type $float32
        (sub final $custom (struct (field (ref $custom_operations)) (field f32))))

    (func $box_float32
        (param $f f32) (result (ref eq))
        (struct.new $float32 (global.get $float32_ops) (local.get $f)))

    (func $unbox_float32
        (param $f (ref eq)) (result f32)
        (struct.get $float32 1 (ref.cast (ref $float32) (local.get $f))))

    (func $float32_cmp
        (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get $v1)))
        (local.set $y (call $unbox_float32 (local.get $v2)))
        (i32.add
            (i32.sub (f32.gt (local.get $x) (local.get $y))
                     (f32.lt (local.get $x) (local.get $y)))
            (i32.sub (f32.eq (local.get $x) (local.get $x))
                     (f32.eq (local.get $y) (local.get $y)))))

    (func $float32_hash
        (param $v (ref eq)) (result i32)
        (i32.reinterpret_f32 (call $unbox_float32 (local.get $v))))

    (func $float32_serialize
        (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
        (call $caml_serialize_int_4 (local.get $s)
            (i32.reinterpret_f32 (call $unbox_float32 (local.get $v))))
        (tuple.make 2 (i32.const 4) (i32.const 4)))

    (func $float32_deserialize
        (param $s (ref eq)) (result (ref eq)) (result i32)
        (tuple.make 2
            (call $box_float32 (f32.reinterpret_i32 (call $caml_deserialize_int_4 (local.get $s))))
            (i32.const 4)))

    (func $float32_dup
        (param $v (ref eq)) (result (ref eq))
        (local $d (ref $float32))
        (local.set $d (ref.cast (ref $float32) (local.get $v)))
        (struct.new $float32
            (struct.get $float32 0 (local.get $d))
            (struct.get $float32 1 (local.get $d))))

    (func $caml_float_of_float32 (export "caml_float_of_float32")
        (param $f32 (ref eq)) (result (ref eq))
        (call $box_float
            (f64.promote_f32 (call $unbox_float32 (local.get $f32)))))

    (func $caml_float32_of_float (export "caml_float32_of_float")
        (param $f64 (ref eq)) (result (ref eq))
        (call $box_float32
            (f32.demote_f64 (call $unbox_float (local.get $f64)))))

    (func (export "caml_float32_of_int")
        (param $i (ref eq)) (result (ref eq))
        (call $box_float32
            (f32.convert_i32_s (i31.get_s (ref.cast (ref i31) (local.get $i))))))

    (func (export "caml_int_of_float32")
        (param $f (ref eq)) (result (ref eq))
        (ref.i31
            (i32.trunc_sat_f32_s (call $unbox_float32 (local.get $f)))))

    (func (export "caml_float32_of_bits_bytecode")
        (param $i (ref eq)) (result (ref eq))
        (call $box_float32
            (f32.reinterpret_i32 (call $Int32_val (local.get $i)))))

    (func (export "caml_float32_to_bits_bytecode")
        (param $f (ref eq)) (result (ref eq))
        (call $caml_copy_int32
            (i32.reinterpret_f32 (call $unbox_float32 (local.get $f)))))

    (func (export "caml_float32_of_int64_bytecode")
        (param $i (ref eq)) (result (ref eq))
        (call $box_float32
            (f32.convert_i64_s (call $Int64_val (local.get $i)))))

    (func (export "caml_float32_to_int64_bytecode")
        (param $f (ref eq)) (result (ref eq))
        (call $caml_copy_int64
            (i64.trunc_sat_f32_s (call $unbox_float32 (local.get $f)))))

    (func (export "caml_float32_of_string")
        (param $s (ref eq)) (result (ref eq))
        (call $caml_float32_of_float (call $caml_float_of_string (local.get $s))))

    (func (export "caml_format_float32")
        (param $s (ref eq)) (param $f (ref eq)) (result (ref eq))
        (call $caml_format_float (local.get $s) (call $caml_float_of_float32 (local.get $f))))

    (func (export "caml_float32_compare")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31
            (i32.add
            (i32.sub (f32.gt (local.get $x) (local.get $y))
                     (f32.lt (local.get $x) (local.get $y)))
            (i32.sub (f32.eq (local.get $x) (local.get $x))
                     (f32.eq (local.get $y) (local.get $y))))))

    (func (export "caml_eq_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31 (f32.eq (local.get $x) (local.get $y))))

    (func (export "caml_neq_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31 (f32.ne (local.get $x) (local.get $y))))

    (func (export "caml_ge_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31 (f32.ge (local.get $x) (local.get $y))))

   (func (export "caml_le_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31 (f32.le (local.get $x) (local.get $y))))

    (func (export "caml_gt_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31 (f32.gt (local.get $x) (local.get $y))))

    (func (export "caml_lt_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (ref.i31 (f32.lt (local.get $x) (local.get $y))))

    (func (export "caml_add_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32 (f32.add (local.get $x) (local.get $y))))

    (func (export "caml_sub_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32 (f32.sub (local.get $x) (local.get $y))))

    (func (export "caml_mul_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32 (f32.mul (local.get $x) (local.get $y))))

    (func (export "caml_div_float32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32 (f32.div (local.get $x) (local.get $y))))

    (func (export "caml_neg_float32")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.neg (local.get $x))))

    (func (export "caml_abs_float32")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.abs (local.get $x))))

    (func (export "caml_modf_float32")
        (param (ref eq)) (result (ref eq))
        (local $x f32) (local $a f32) (local $i f32) (local $f f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $a (f32.abs (local.get $x)))
        (if (f32.ge (local.get $a) (f32.const 0))
            (then
            (if (f32.lt (local.get $a) (f32.const inf))
                (then ;; normal
                    (local.set $i (f32.floor (local.get $a)))
                    (local.set $f (f32.sub (local.get $a) (local.get $i)))
                    (local.set $i (f32.copysign (local.get $i) (local.get $x)))
                    (local.set $f (f32.copysign (local.get $f) (local.get $x))))
                (else ;; infinity
                    (local.set $i (local.get $x))
                    (local.set $f (f32.copysign (f32.const 0) (local.get $x))))))
            (else ;; zero or nan
            (local.set $i (local.get $x))
            (local.set $f (local.get $x))))
        (array.new_fixed $block 3 (ref.i31 (i32.const 0))
            (call $box_float32 (local.get $f)) (call $box_float32 (local.get $i))))

    (func (export "caml_fmod_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f64) (local $y f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (local.set $y (f64.promote_f32 (call $unbox_float32 (local.get 1))))
        (call $box_float32 (f32.demote_f64 (call $fmod (local.get $x) (local.get $y)))))

    (func (export "caml_acos_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $acos (local.get $x)))))

    (func (export "caml_asin_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $asin (local.get $x)))))

    (func (export "caml_atan_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $atan (local.get $x)))))

    (func (export "caml_atan2_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f64) (local $y f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (local.set $y (f64.promote_f32 (call $unbox_float32 (local.get 1))))
        (call $box_float32 (f32.demote_f64 (call $atan2 (local.get $x) (local.get $y)))))

    (func (export "caml_ceil_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.ceil (local.get $x))))

    (func (export "caml_floor_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.floor (local.get $x))))

    (func (export "caml_exp_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $exp (local.get $x)))))

    (func (export "caml_log_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $log (local.get $x)))))

    (func (export "caml_power_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f64) (local $y f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (local.set $y (f64.promote_f32 (call $unbox_float32 (local.get 1))))
        (call $box_float32 (f32.demote_f64 (call $pow (local.get $x) (local.get $y)))))

    (func (export "caml_cos_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $cos (local.get $x)))))

    (func (export "caml_sin_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $sin (local.get $x)))))

    (func (export "caml_tan_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $tan (local.get $x)))))

    (func (export "caml_sqrt_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.sqrt (local.get $x))))

    (func (export "caml_nextafter_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32) (local $i i32) (local $j i32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (if (f32.ne (local.get $x) (local.get $x)) (then (return (local.get 0))))
        (if (f32.ne (local.get $y) (local.get $y)) (then (return (local.get 1))))
        (if (f32.eq (local.get $x) (local.get $y))
            (then (return (local.get 1))))
        (if (result (ref eq)) (f32.eq (local.get $x) (f32.const 0))
            (then
            (if (f32.ge (local.get $y) (f32.const 0))
                (then (return (call $box_float32 (f32.const 0x1p-149))))
                (else (return (call $box_float32 (f32.const -0x1p-149))))))
            (else
            (local.set $i (i32.reinterpret_f32 (local.get $x)))
            (local.set $j (i32.reinterpret_f32 (local.get $y)))
            (if (i32.and (i32.lt_s (local.get $i) (local.get $j))
                         (i32.lt_u (local.get $i) (local.get $j)))
                (then (local.set $i (i32.add (local.get $i) (i32.const 1))))
                (else (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
            (return (call $box_float32 (f32.reinterpret_i32 (local.get $i)))))))

    (func (export "caml_trunc_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.trunc (local.get $x))))

    (func (export "caml_classify_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $a f32)
        (local.set $a
            (f32.abs (call $unbox_float32 (local.get 0))))
        (ref.i31
            (if (result i32) (f32.ge (local.get $a) (f32.const 0x1p-126))
            (then
                (if (result i32) (f32.lt (local.get $a) (f32.const inf))
                    (then (i32.const 0)) ;; normal
                    (else (i32.const 3)))) ;; infinity
            (else
                (if (result i32) (f32.eq (local.get $a) (f32.const 0))
                    (then (i32.const 2)) ;; zero
                    (else
                        (if (result i32) (f32.eq (local.get $a) (local.get $a))
                        (then (i32.const 1)) ;; subnormal
                        (else (i32.const 4))))))))) ;; nan

    (func (export "caml_ldexp_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_float32_of_float
            (call $box_float
                (call $caml_ldexp_float
                    (call $unbox_float (call $caml_float_of_float32 (local.get 0)))
                    (local.get 1)))))

    (func (export "caml_frexp_float32")
        (param (ref eq)) (result (ref eq))
        (local $frexp (ref $block))
        (local.set $frexp (ref.cast (ref $block)
            (call $caml_frexp_float (call $caml_float_of_float32 (local.get 0)))))
        (array.new_fixed $block 3 (ref.i31 (i32.const 0))
            (call $caml_float32_of_float (array.get $block (local.get $frexp) (i32.const 1)))
            (array.get $block (local.get $frexp) (i32.const 2))))

    (func (export "caml_copysign_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32 (f32.copysign (local.get $x) (local.get $y))))

    (func (export "caml_signbit_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (ref.i31 (i32.shr_u
            (i32.reinterpret_f32 (call $unbox_float32 (local.get 0)))
            (i32.const 31))))

    (func (export "caml_expm1_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $expm1 (local.get $x)))))

    (func (export "caml_exp2_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $pow (f64.const 2) (local.get $x)))))

    (func (export "caml_log1p_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $log1p (local.get $x)))))

    (func (export "caml_log2_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $log2 (local.get $x)))))

    (func (export "caml_log10_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $log10 (local.get $x)))))

    (func (export "caml_cosh_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $cosh (local.get $x)))))

    (func (export "caml_acosh_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $acosh (local.get $x)))))

    (func (export "caml_sinh_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $sinh (local.get $x)))))

    (func (export "caml_asinh_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $asinh (local.get $x)))))

    (func (export "caml_tanh_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $tanh (local.get $x)))))

    (func (export "caml_atanh_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $atanh (local.get $x)))))

    (func (export "caml_round_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.nearest (local.get $x))))

    (func (export "caml_cbrt_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (call $box_float32 (f32.demote_f64 (call $cbrt (local.get $x)))))

    (func (export "caml_erf_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x (ref eq))
        (local.set $x (call $box_float (f64.promote_f32 (call $unbox_float32 (local.get 0)))))
        (call $box_float32 (f32.demote_f64
             (call $caml_erf_float (call $unbox_float (local.get $x))))))

    (func (export "caml_erfc_float32_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x (ref eq))
        (local.set $x (call $box_float (f64.promote_f32 (call $unbox_float32 (local.get 0)))))
        (call $box_float32 (f32.demote_f64
            (call $caml_erfc_float (call $unbox_float (local.get $x))))))

    (func (export "caml_hypot_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f64) (local $y f64)
        (local.set $x (f64.promote_f32 (call $unbox_float32 (local.get 0))))
        (local.set $y (f64.promote_f32 (call $unbox_float32 (local.get 1))))
        (call $box_float32 (f32.demote_f64 (call $hypot (local.get $x) (local.get $y)))))

    (func (export "caml_fma_float32_bytecode")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x (ref eq)) (local $y (ref eq)) (local $z (ref eq))
        (local.set $x (call $box_float (f64.promote_f32 (call $unbox_float32 (local.get 0)))))
        (local.set $y (call $box_float (f64.promote_f32 (call $unbox_float32 (local.get 1)))))
        (local.set $z (call $box_float (f64.promote_f32 (call $unbox_float32 (local.get 2)))))
        (call $box_float32 (f32.demote_f64
            (call $unbox_float (call $caml_fma_float (local.get $x) (local.get $y) (local.get $z))))))

    (func (export "caml_simd_float32_min_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32
            (if (result f32) (f32.lt (local.get $x) (local.get $y))
                (then (local.get $x))
                (else (local.get $y)))))

    (func (export "caml_simd_float32_max_bytecode")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (local $x f32) (local $y f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (local.set $y (call $unbox_float32 (local.get 1)))
        (call $box_float32
            (if (result f32) (f32.gt (local.get $x) (local.get $y))
                (then (local.get $x))
                (else (local.get $y)))))

    (func (export "caml_simd_cast_float32_int64_bytecode")
        (param $f (ref eq)) (result (ref eq))
        (call $caml_copy_int64
            (i64.trunc_sat_f32_s (call $unbox_float32 (local.get $f)))))

    (func (export "caml_simd_float32_round_current_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.nearest (local.get $x))))

    (func (export "caml_simd_float32_round_neg_inf_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.floor (local.get $x))))

    (func (export "caml_simd_float32_round_pos_inf_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.ceil (local.get $x))))

    (func (export "caml_simd_float32_round_towards_zero_bytecode")
        (param (ref eq)) (result (ref eq))
        (local $x f32)
        (local.set $x (call $unbox_float32 (local.get 0)))
        (call $box_float32 (f32.trunc (local.get $x))))

    (func (export "caml_make_unboxed_float32_vect_bytecode")
        (param (ref eq)) (result (ref eq))
        (call $caml_make_vect (local.get 0) (call $box_float32 (f32.const 0))))

    (func (export "caml_ba_float32_get_1")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_float32_of_float (call $caml_ba_get_1 (local.get 0) (local.get 1))))

    (func (export "caml_ba_float32_get_2")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_float32_of_float (call $caml_ba_get_2 (local.get 0) (local.get 1) (local.get 2))))

    (func (export "caml_ba_float32_get_3")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_float32_of_float (call $caml_ba_get_3 (local.get 0) (local.get 1) (local.get 2) (local.get 3))))

    (func (export "caml_ba_float32_set_1")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_ba_set_1 (local.get 0) (local.get 1) (call $caml_float_of_float32 (local.get 2))))

    (func (export "caml_ba_float32_set_2")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_ba_set_2 (local.get 0) (local.get 1) (local.get 2) (call $caml_float_of_float32 (local.get 3))))

    (func (export "caml_ba_float32_set_3")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_ba_set_3 (local.get 0) (local.get 1) (local.get 2) (local.get 3) (call $caml_float_of_float32 (local.get 4))))

    (func (export "caml_ba_uint8_getf32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $box_float32 (f32.reinterpret_i32
            (call $caml_ba_uint8_get32 (local.get 0) (local.get 1)))))

    (func (export "caml_ba_uint8_setf32")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_ba_uint8_set32 (local.get 0) (local.get 1)
            (i32.reinterpret_f32 (call $unbox_float32 (local.get 2)))))

    (func (export "caml_string_getf32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $box_float32 (f32.reinterpret_i32
            (call $caml_string_get32 (local.get 0) (local.get 1)))))

    (func (export "caml_bytes_getf32")
        (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $box_float32 (f32.reinterpret_i32
            (call $caml_bytes_get32 (local.get 0) (local.get 1)))))

    (func (export "caml_bytes_setf32")
        (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
        (call $caml_bytes_set32 (local.get 0) (local.get 1)
            (i32.reinterpret_f32 (call $unbox_float32 (local.get 2)))))
)
