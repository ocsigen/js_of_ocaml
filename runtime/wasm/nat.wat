(module
   (import "jslib" "log_str" (func $log_str (param (ref $string))))
   (import "custom" "caml_register_custom_operations"
      (func $caml_register_custom_operations
         (param $ops (ref $custom_operations))))
   (import "int32" "Nativeint_val"
      (func $Nativeint_val (param (ref eq)) (result i32)))
   (import "int32" "caml_copy_nativeint"
      (func $caml_copy_nativeint (param i32) (result (ref eq))))

   (type $string (array (mut i8)))
   (type $data (array (mut i32)))
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
         (field $id (ref $string))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))
         (field $dup (ref null $dup))))
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (global $nat_ops (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 3
            (i32.const 110) (i32.const 97) (i32.const 116)) ;; "_nat"
         (ref.null $compare)
         (ref.null $compare)
         (ref.func $hash_nat)
         (ref.null $fixed_length)
         (ref.func $serialize_nat)
         (ref.func $deserialize_nat)
         (ref.null $dup)))

   (type $nat
      (sub final $custom
         (struct
            (field (ref $custom_operations))
            (field $data (ref $data)))))

   (func (export "initialize_nat")
      (param (ref eq)) (result (ref eq))
      (call $caml_register_custom_operations (global.get $nat_ops))
      (ref.i31 (i32.const 0)))

   (func (export "create_nat")
      (param $sz (ref eq)) (result (ref eq))
      (struct.new $nat
         (global.get $nat_ops)
         (array.new $data (i32.const 0)
            (i31.get_u (ref.cast (ref i31) (local.get $sz))))))

   (func (export "incr_nat")
      (param $nat (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (param $carry_in (ref eq)) (result (ref eq))
      (local $data (ref $data))
      (local $carry i32) (local $i i32) (local $ofs i32) (local $len i32)
      (local $x i32)
      (local.set $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat))))
      (local.set $carry (i31.get_s (ref.cast (ref i31) (local.get $carry_in))))
      (local.set $ofs (i31.get_s (ref.cast (ref i31) (local.get $vofs))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.eqz (local.get $carry))
         (then (return (ref.i31 (i32.const 0)))))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $x
                  (i32.add
                     (array.get $data (local.get $data) (local.get $ofs))
                     (i32.const 1)))
               (array.set $data (local.get $data) (local.get $ofs)
                  (local.get $x))
               (if (local.get $x)
                  (then
                     (return (ref.i31 (i32.const 0)))))
               (local.set $ofs (i32.add (local.get $ofs) (i32.const 1)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 1)))

   (func (export "decr_nat")
      (param $nat (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (param $carry_in (ref eq)) (result (ref eq))
      (local $data (ref $data))
      (local $carry i32) (local $i i32) (local $ofs i32) (local $len i32)
      (local $x i32)
      (local.set $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat))))
      (local.set $carry (i31.get_s (ref.cast (ref i31) (local.get $carry_in))))
      (local.set $ofs (i31.get_s (ref.cast (ref i31) (local.get $vofs))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.eqz (local.get $carry))
         (then (return (ref.i31 (i32.const 0)))))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $x
                  (array.get $data (local.get $data) (local.get $ofs)))
               (array.set $data (local.get $data) (local.get $ofs)
                  (i32.sub (local.get $x) (i32.const 1)))
               (if (local.get $x)
                  (then
                     (return (ref.i31 (i32.const 0)))))
               (local.set $ofs (i32.add (local.get $ofs) (i32.const 1)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 1)))

   (func (export "set_digit_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $digit (ref eq))
      (result (ref eq))
      (array.set $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
         (i31.get_s (ref.cast (ref i31) (local.get $ofs)))
         (i31.get_s (ref.cast (ref i31) (local.get $digit))))
      (ref.i31 (i32.const 0)))

   (func (export "set_digit_nat_native")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $digit (ref eq))
      (result (ref eq))
      (array.set $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
         (i31.get_s (ref.cast (ref i31) (local.get $ofs)))
         (call $Nativeint_val (local.get $digit)))
      (ref.i31 (i32.const 0)))

   (func (export "nth_digit_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (array.get $data
            (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
            (i31.get_s (ref.cast (ref i31) (local.get $ofs))))))

   (func (export "nth_digit_nat_native")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (call $caml_copy_nativeint
         (array.get $data
            (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
            (i31.get_s (ref.cast (ref i31) (local.get $ofs))))))

   (func (export "is_digit_zero")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.eqz
            (array.get $data
               (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
               (i31.get_s (ref.cast (ref i31) (local.get $ofs)))))))

   (func (export "num_leading_zero_bits_in_digit")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.clz
            (array.get $data
               (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
               (i31.get_s (ref.cast (ref i31) (local.get $ofs)))))))

   (func (export "is_digit_odd")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.and (i32.const 1)
            (array.get $data
               (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
               (i31.get_s (ref.cast (ref i31) (local.get $ofs)))))))

   (func (export "is_digit_int")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.ge_u (i32.const 0x40000000)
            (array.get $data
               (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
               (i31.get_s (ref.cast (ref i31) (local.get $ofs)))))))

   (func (export "set_to_zero_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (array.fill $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat)))
         (i31.get_s (ref.cast (ref i31) (local.get $ofs)))
         (i32.const 0)
         (i31.get_s (ref.cast (ref i31) (local.get $len))))
      (ref.i31 (i32.const 0)))

   (func (export "blit_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (array.copy $data $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1)))
         (i31.get_s (ref.cast (ref i31) (local.get $ofs1)))
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2)))
         (i31.get_s (ref.cast (ref i31) (local.get $ofs2)))
         (i31.get_s (ref.cast (ref i31) (local.get $len))))
      (ref.i31 (i32.const 0)))

   (func (export "num_digits_nat")
      (param $nat (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $ofs i32) (local $len i32) (local $data (ref $data))
      (local.set $ofs (i31.get_s (ref.cast (ref i31) (local.get $vofs))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (local.set $data
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat))))
      (local.set $ofs
         (i32.add (local.get $ofs) (i32.sub (local.get $len) (i32.const 1))))
      (loop $loop
         (if (i32.eqz (local.get $len)) (then (return (ref.i31 (i32.const 1)))))
         (if (array.get $data (local.get $data) (local.get $ofs))
            (then (return (ref.i31 (local.get $len)))))
         (local.set $len (i32.sub (local.get $len) (i32.const 1)))
         (local.set $ofs (i32.sub (local.get $ofs) (i32.const 1)))
         (br $loop)))

   (func (export "compare_digits_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      (local $d1 i32) (local $d2 i32)
      (local.set $d1
         (array.get $data
            (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1)))
            (i31.get_s (ref.cast (ref i31) (local.get $ofs1)))))
      (local.set $d2
         (array.get $data
            (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2)))
            (i31.get_s (ref.cast (ref i31) (local.get $ofs2)))))
      (if (i32.gt_u (local.get $d1) (local.get $d2))
         (then (return (ref.i31 (i32.const 1)))))
      (if (i32.lt_u (local.get $d1) (local.get $d2))
         (then (return (ref.i31 (i32.const -1)))))
      (ref.i31 (i32.const 0)))

   (func (export "compare_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (result (ref eq))
      (local $ofs1 i32) (local $len1 i32) (local $data1 (ref $data))
      (local $ofs2 i32) (local $len2 i32) (local $data2 (ref $data))
      (local $d1 i32) (local $d2 i32)
      (local.set $ofs1 (i31.get_s (ref.cast (ref i31) (local.get $vofs1))))
      (local.set $len1 (i31.get_s (ref.cast (ref i31) (local.get $vlen1))))
      (local.set $data1
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1))))
      (local.set $ofs1
         (i32.add (local.get $ofs1) (i32.sub (local.get $len1) (i32.const 1))))
      (local.set $ofs2 (i31.get_s (ref.cast (ref i31) (local.get $vofs2))))
      (local.set $len2 (i31.get_s (ref.cast (ref i31) (local.get $vlen2))))
      (local.set $data2
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2))))
      (local.set $ofs2
         (i32.add (local.get $ofs2) (i32.sub (local.get $len2) (i32.const 1))))
      (loop $loop
         (if (local.get $len1)
            (then
               (if (i32.eqz
                      (array.get $data (local.get $data1) (local.get $ofs1)))
                  (then
                     (local.set $len1 (i32.sub (local.get $len1) (i32.const 1)))
                     (local.set $ofs1 (i32.sub (local.get $ofs1) (i32.const 1)))
                     (br $loop))))))
      (loop $loop
         (if (local.get $len2)
            (then
               (if (i32.eqz
                      (array.get $data (local.get $data2) (local.get $ofs2)))
                  (then
                     (local.set $len2 (i32.sub (local.get $len2) (i32.const 1)))
                     (local.set $ofs2 (i32.sub (local.get $ofs2) (i32.const 1)))
                     (br $loop))))))
      (if (i32.gt_u (local.get $len1) (local.get $len2))
         (then (return (ref.i31 (i32.const 1)))))
      (if (i32.lt_u (local.get $len2) (local.get $len1))
         (then (return (ref.i31 (i32.const -1)))))
      (loop $loop
         (if (local.get $len1)
            (then
               (local.set $len1 (i32.sub (local.get $len1) (i32.const 1)))
               (local.set $d1
                  (array.get $data (local.get $data1) (local.get $ofs1)))
               (local.set $d2
                  (array.get $data (local.get $data2) (local.get $ofs2)))
               (if (i32.gt_u (local.get $d1) (local.get $d2))
                  (then (return (ref.i31 (i32.const 1)))))
               (if (i32.lt_u (local.get $d1) (local.get $d2))
                  (then (return (ref.i31 (i32.const -1)))))
               (local.set $ofs1 (i32.sub (local.get $ofs1) (i32.const 1)))
               (local.set $ofs2 (i32.sub (local.get $ofs2) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 0)))

   (func (export "mult_digit_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (param $nat3 (ref eq)) (param $vofs3 (ref eq)) (result (ref eq))
      (local $ofs1 i32) (local $len1 i32) (local $data1 (ref $data))
      (local $ofs2 i32) (local $len2 i32) (local $data2 (ref $data))
      (local $i i32) (local $d i64) (local $x i64) (local $carry i64)
      (local $y i32)
      (local.set $d
         (i64.extend_i32_u
            (array.get $data
               (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat3)))
               (i31.get_s (ref.cast (ref i31) (local.get $vofs3))))))
      (local.set $ofs1 (i31.get_s (ref.cast (ref i31) (local.get $vofs1))))
      (local.set $len1 (i31.get_s (ref.cast (ref i31) (local.get $vlen1))))
      (local.set $data1
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1))))
      (local.set $ofs2 (i31.get_s (ref.cast (ref i31) (local.get $vofs2))))
      (local.set $len2 (i31.get_s (ref.cast (ref i31) (local.get $vlen2))))
      (local.set $data2
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2))))
      (local.set $len1 (i32.sub (local.get $len1) (local.get $len2)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len2))
            (then
               (local.set $x
                  (i64.add
                     (i64.add (local.get $carry)
                        (i64.extend_i32_u
                           (array.get $data (local.get $data1)
                              (local.get $ofs1))))
                     (i64.mul (local.get $d)
                        (i64.extend_i32_u
                           (array.get $data (local.get $data2)
                              (local.get $ofs2))))))
               (array.set $data (local.get $data1) (local.get $ofs1)
                  (i32.wrap_i64 (local.get $x)))
               (local.set $carry (i64.shr_u (local.get $x) (i64.const 32)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (local.set $ofs1 (i32.add (local.get $ofs1) (i32.const 1)))
               (local.set $ofs2 (i32.add (local.get $ofs2) (i32.const 1)))
               (br $loop))))
      (if (i32.eqz (local.get $len1))
         (then (return (ref.i31 (i32.wrap_i64 (local.get $carry))))))
      (local.set $x
         (i64.add (local.get $carry)
            (i64.extend_i32_u
               (array.get $data (local.get $data1) (local.get $ofs1)))))
      (array.set $data (local.get $data1) (local.get $ofs1)
         (i32.wrap_i64 (local.get $x)))
      (local.set $carry (i64.shr_u (local.get $x) (i64.const 32)))
      (local.set $len1 (i32.sub (local.get $len1) (i32.const 1)))
      (if (i64.eqz (local.get $carry)) (then (return (ref.i31 (i32.const 0)))))
      (if (i32.eqz (local.get $len1))
         (then (return (ref.i31 (i32.wrap_i64 (local.get $carry))))))
      (local.set $ofs1 (i32.add (local.get $ofs1) (i32.const 1)))
      (loop $loop
         (local.set $y
            (i32.add
               (array.get $data (local.get $data1) (local.get $ofs1))
                  (i32.const 1)))
         (array.set $data (local.get $data1) (local.get $ofs1) (local.get $y))
         (if (local.get $y) (then (return (ref.i31 (i32.const 0)))))
         (local.set $len1 (i32.sub (local.get $len1) (i32.const 1)))
         (local.set $ofs1 (i32.add (local.get $ofs1) (i32.const 1)))
         (if (local.get $len1) (then (br $loop))))
      (i31.new (i32.const 1)))

   (data $mult_nat "mult_nat")

   (func (export "mult_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (param $nat3 (ref eq)) (param $vofs3 (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $mult_nat (i32.const 0) (i32.const 8)))
      (unreachable))

   (data $square_nat "square_nat")

   (func (export "square_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $square_nat (i32.const 0) (i32.const 10)))
      (unreachable))

   (data $shift_left_nat "shift_left_nat")

   (func (export "shift_left_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vnbits (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $shift_left_nat (i32.const 0) (i32.const 14)))
      (unreachable))

   (data $shift_right_nat "shift_right_nat")

   (func (export "shift_right_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vnbits (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $shift_right_nat (i32.const 0) (i32.const 15)))
      (unreachable))

   (data $div_digit_nat "div_digit_nat")

   (func (export "div_digit_nat")
      (param $natq (ref eq)) (param $ofsq (ref eq))
      (param $natr (ref eq)) (param $ofsr (ref eq))
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $div_digit_nat (i32.const 0) (i32.const 13)))
      (unreachable))

   (data $div_nat "div_nat")

   (func (export "div_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $div_nat (i32.const 0) (i32.const 7)))
      (unreachable))

   (data $add_nat "add_nat")

   (func (export "add_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (param $carry_in (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $add_nat (i32.const 0) (i32.const 7)))
      (unreachable))

   (data $sub_nat "sub_nat")

   (func (export "sub_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq)) (param $vlen1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq)) (param $vlen2 (ref eq))
      (param $carry_in (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $sub_nat (i32.const 0) (i32.const 7)))
      (unreachable))

   (data $complement_nat "complement_nat")

   (func (export "complement_nat")
      (param $nat (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $complement_nat (i32.const 0) (i32.const 14)))
      (unreachable))

   (func (export "land_digit_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq))
      (result (ref eq))
      (local $ofs1 i32) (local $data1 (ref $data))
      (local $ofs2 i32) (local $data2 (ref $data))
      (local.set $data1
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1))))
      (local.set $ofs1 (i31.get_s (ref.cast (ref i31) (local.get $vofs1))))
      (local.set $data2
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2))))
      (local.set $ofs2 (i31.get_s (ref.cast (ref i31) (local.get $vofs2))))
      (array.set $data (local.get $data1) (local.get $ofs1)
         (i32.and (array.get $data (local.get $data1) (local.get $ofs1))
            (array.get $data (local.get $data2) (local.get $ofs2))))
      (ref.i31 (i32.const 0)))

   (func (export "lxor_digit_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq))
      (result (ref eq))
      (local $ofs1 i32) (local $data1 (ref $data))
      (local $ofs2 i32) (local $data2 (ref $data))
      (local.set $data1
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1))))
      (local.set $ofs1 (i31.get_s (ref.cast (ref i31) (local.get $vofs1))))
      (local.set $data2
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2))))
      (local.set $ofs2 (i31.get_s (ref.cast (ref i31) (local.get $vofs2))))
      (array.set $data (local.get $data1) (local.get $ofs1)
         (i32.xor (array.get $data (local.get $data1) (local.get $ofs1))
            (array.get $data (local.get $data2) (local.get $ofs2))))
      (ref.i31 (i32.const 0)))

   (func (export "lor_digit_nat")
      (param $nat1 (ref eq)) (param $vofs1 (ref eq))
      (param $nat2 (ref eq)) (param $vofs2 (ref eq))
      (result (ref eq))
      (local $ofs1 i32) (local $data1 (ref $data))
      (local $ofs2 i32) (local $data2 (ref $data))
      (local.set $data1
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat1))))
      (local.set $ofs1 (i31.get_s (ref.cast (ref i31) (local.get $vofs1))))
      (local.set $data2
         (struct.get $nat $data (ref.cast (ref $nat) (local.get $nat2))))
      (local.set $ofs2 (i31.get_s (ref.cast (ref i31) (local.get $vofs2))))
      (array.set $data (local.get $data1) (local.get $ofs1)
         (i32.or (array.get $data (local.get $data1) (local.get $ofs1))
            (array.get $data (local.get $data2) (local.get $ofs2))))
      (ref.i31 (i32.const 0)))

   (data $hash_nat "hash_nat")

   (func $hash_nat (param (ref eq)) (result i32)
      ;; ZZZ
      (call $log_str
         (array.new_data $string $hash_nat (i32.const 0) (i32.const 8)))
      (unreachable))

   (data $serialize_nat "serialize_nat")

   (func $serialize_nat
      (param (ref eq)) (param (ref eq)) (result i32) (result i32)
      ;; ZZZ
      (call $log_str
         (array.new_data $string $serialize_nat (i32.const 0) (i32.const 13)))
      (unreachable))

   (data $deserialize_nat "deserialize_nat")

   (func $deserialize_nat (param (ref eq)) (result (ref eq)) (result i32)
      ;; ZZZ
      (call $log_str
         (array.new_data $string $serialize_nat (i32.const 0) (i32.const 15)))
      (unreachable))
)
