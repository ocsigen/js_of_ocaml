(module
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "fail" "caml_raise_end_of_file" (func $caml_raise_end_of_file))
   (import "obj" "double_array_tag" (global $double_array_tag i32))
   (import "string" "caml_string_cat"
      (func $caml_string_cat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "obj" "caml_is_closure"
      (func $caml_is_closure (param (ref eq)) (result i32)))
   (import "effect" "caml_is_continuation"
      (func $caml_is_continuation (param (ref eq)) (result i32)))
   (import "bindings" "weak_map_new" (func $weak_map_new (result (ref any))))
   (import "bindings" "weak_map_get"
      (func $weak_map_get (param (ref any)) (param (ref eq)) (result i31ref)))
   (import "bindings" "weak_map_set"
      (func $weak_map_set (param (ref any)) (param (ref eq)) (param (ref i31))))
   (import "io" "caml_really_putblock"
      (func $caml_really_putblock
         (param (ref eq)) (param (ref $string)) (param i32) (param i32)))
   (import "io" "caml_really_getblock"
      (func $caml_really_getblock
         (param (ref eq)) (param (ref $string)) (param i32) (param i32)
         (result i32)))
   (import "io" "caml_flush_if_unbuffered"
      (func $caml_flush_if_unbuffered (param (ref eq))))
   (import "custom" "caml_init_custom_operations"
      (func $caml_init_custom_operations))
   (import "custom" "caml_find_custom_operations"
      (func $caml_find_custom_operations
         (param (ref $string)) (result (ref null $custom_operations))))

   (global $input_val_from_string (ref $string)
      (array.new_fixed $string 21
         (i32.const 105) (i32.const 110) (i32.const 112) (i32.const 117)
         (i32.const 116) (i32.const 95) (i32.const 118) (i32.const 97)
         (i32.const 108) (i32.const 95) (i32.const 102) (i32.const 114)
         (i32.const 111) (i32.const 109) (i32.const 95) (i32.const 115)
         (i32.const 116) (i32.const 114) (i32.const 105) (i32.const 110)
         (i32.const 103)))

   (export "caml_input_value_from_string" (func $caml_input_value_from_bytes))
   (func $caml_input_value_from_bytes (export "caml_input_value_from_bytes")
      (param $vstr (ref eq)) (param $vofs (ref eq)) (result (ref eq))
      (local $str (ref $string))
      (local $ofs i32)
      (local $s (ref $intern_state))
      (local $h (ref $marshal_header))
      (local.set $str (ref.cast (ref $string) (local.get $vstr)))
      (local.set $ofs (i31.get_u (ref.cast (ref i31) (local.get $vofs))))
      (local.set $s
         (call $get_intern_state (local.get $str) (local.get $ofs)))
      (local.set $h
         (call $parse_header (local.get $s) (global.get $input_val_from_string)))
      (if (i32.gt_s
             (i32.add (local.get $ofs)
                (i32.add (struct.get $marshal_header $data_len (local.get $h))
                   (i32.const 20)))
             (array.len (local.get $str)))
         (then
            (call $bad_length (global.get $input_val_from_string))))
      (return_call $intern_rec (local.get $s) (local.get $h)))

   (data $truncated_obj "input_value: truncated object")

   (global $input_value (ref $string)
      (array.new_fixed $string 11
         (i32.const 105) (i32.const 110) (i32.const 112) (i32.const 117)
         (i32.const 116) (i32.const 95) (i32.const 118) (i32.const 97)
         (i32.const 108) (i32.const 117) (i32.const 101)))

   (func (export "caml_input_value") (param $ch (ref eq)) (result (ref eq))
      ;; ZZZ check binary channel?
      (local $r i32) (local $len i32)
      (local $header (ref $string)) (local $buf (ref $string))
      (local $s (ref $intern_state)) (local $h (ref $marshal_header))
      (local.set $header (array.new $string (i32.const 0) (i32.const 20)))
      (local.set $r
         (call $caml_really_getblock
            (local.get $ch) (local.get $header) (i32.const 0) (i32.const 20)))
      (if (i32.eqz (local.get $r))
         (then (call $caml_raise_end_of_file)))
      (if (i32.lt_u (local.get $r) (i32.const 20))
         (then
            (call $caml_failwith
               (array.new_data $string $truncated_obj
                  (i32.const 0) (i32.const 29)))))
      (local.set $s
         (call $get_intern_state (local.get $header) (i32.const 0)))
      (local.set $h
         (call $parse_header (local.get $s) (global.get $input_value)))
      (local.set $len (struct.get $marshal_header $data_len (local.get $h)))
      (local.set $buf (array.new $string (i32.const 0) (local.get $len)))
      (if (i32.lt_u
             (call $caml_really_getblock (local.get $ch)
                (local.get $buf) (i32.const 0) (local.get $len))
             (local.get $len))
         (then
            (call $caml_failwith
               (array.new_data $string $truncated_obj
                  (i32.const 0) (i32.const 29)))))
      (local.set $s (call $get_intern_state (local.get $buf) (i32.const 0)))
      (return_call $intern_rec (local.get $s) (local.get $h)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $js (struct (field anyref)))

   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field $id (ref $string))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))))
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (global $Intext_magic_number_small i32 (i32.const 0x8495A6BE))
   (global $Intext_magic_number_big i32 (i32.const 0x8495A6BF))

   (global $PREFIX_SMALL_BLOCK i32 (i32.const 0x80))
   (global $PREFIX_SMALL_INT i32 (i32.const 0x40))
   (global $PREFIX_SMALL_STRING i32 (i32.const 0x20))
   (global $CODE_INT8 i32 (i32.const 0x00))
   (global $CODE_INT16 i32 (i32.const 0x01))
   (global $CODE_INT32 i32 (i32.const 0x02))
   (global $CODE_INT64 i32 (i32.const 0x03))
   (global $CODE_SHARED8 i32 (i32.const 0x04))
   (global $CODE_SHARED16 i32 (i32.const 0x05))
   (global $CODE_SHARED32 i32 (i32.const 0x06))
   (global $CODE_BLOCK32 i32 (i32.const 0x08))
   (global $CODE_BLOCK64 i32 (i32.const 0x13))
   (global $CODE_STRING8 i32 (i32.const 0x09))
   (global $CODE_STRING32 i32 (i32.const 0x0A))
   (global $CODE_DOUBLE_BIG i32 (i32.const 0x0B))
   (global $CODE_DOUBLE_LITTLE i32 (i32.const 0x0C))
   (global $CODE_DOUBLE_ARRAY8_BIG i32 (i32.const 0x0D))
   (global $CODE_DOUBLE_ARRAY8_LITTLE i32 (i32.const 0x0E))
   (global $CODE_DOUBLE_ARRAY32_BIG i32 (i32.const 0x0F))
   (global $CODE_DOUBLE_ARRAY32_LITTLE i32 (i32.const 0x07))
   (global $CODE_CODEPOINTER i32 (i32.const 0x10))
   (global $CODE_INFIXPOINTER i32 (i32.const 0x11))
   (global $CODE_CUSTOM i32 (i32.const 0x12))
   (global $CODE_CUSTOM_LEN i32 (i32.const 0x18))
   (global $CODE_CUSTOM_FIXED i32 (i32.const 0x19))

   (type $intern_state
      (struct
         (field $src (ref $string))
         (field $pos (mut i32))
         (field $obj_table (mut (ref null $block)))
         (field $obj_counter (mut i32))))

   (func $get_intern_state
      (param $src (ref $string)) (param $pos i32) (result (ref $intern_state))
      (struct.new $intern_state
         (local.get $src) (local.get $pos) (ref.null $block) (i32.const 0)))

   (func $read8u (param $s (ref $intern_state)) (result i32)
      (local $pos i32) (local $res i32)
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (array.get_u $string
            (struct.get $intern_state $src (local.get $s))
            (local.get $pos)))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)))
      (local.get $res))

   (func $read8s (param $s (ref $intern_state)) (result i32)
      (local $pos i32) (local $res i32)
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (array.get_s $string
            (struct.get $intern_state $src (local.get $s))
            (local.get $pos)))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)))
      (local.get $res))

   (func $read16u (param $s (ref $intern_state)) (result i32)
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (i32.or
            (i32.shl
               (array.get_u $string (local.get $src) (local.get $pos))
               (i32.const 8))
            (array.get_u $string (local.get $src)
               (i32.add (local.get $pos) (i32.const 1)))))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 2)))
      (local.get $res))

   (func $read16s (param $s (ref $intern_state)) (result i32)
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (i32.or
            (i32.shl
               (array.get_s $string (local.get $src) (local.get $pos))
               (i32.const 8))
            (array.get_u $string (local.get $src)
               (i32.add (local.get $pos) (i32.const 1)))))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 2)))
      (local.get $res))

   (func $read32 (param $s (ref $intern_state)) (result i32)
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (i32.or
            (i32.or
               (i32.shl
                  (array.get_u $string (local.get $src) (local.get $pos))
                  (i32.const 24))
               (i32.shl
                  (array.get_u $string (local.get $src)
                     (i32.add (local.get $pos) (i32.const 1)))
                  (i32.const 16)))
            (i32.or
               (i32.shl
                  (array.get_u $string (local.get $src)
                     (i32.add (local.get $pos) (i32.const 2)))
                  (i32.const 8))
               (array.get_u $string (local.get $src)
                  (i32.add (local.get $pos) (i32.const 3))))))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 4)))
      (local.get $res))

   (func $readblock (param $s (ref $intern_state)) (param $str (ref $string))
      (local $len i32) (local $pos i32)
      (local.set $len (array.len (local.get $str)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (array.copy $string $string
         (local.get $str) (i32.const 0)
         (struct.get $intern_state $src (local.get $s)) (local.get $pos)
         (local.get $len))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (local.get $len))))

   (func $readstr (param $s (ref $intern_state)) (result (ref $string))
      (local $len i32) (local $pos i32) (local $res (ref $string))
      (local $src (ref $string))
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (loop $loop
         (if (array.get_u $string (local.get $src)
                (i32.add (local.get $pos) (local.get $len)))
            (then
               (local.set $len (i32.add (local.get $len) (i32.const 1)))
               (br $loop))))
      (local.set $res (array.new $string (i32.const 0) (local.get $len)))
      (array.copy $string $string
         (local.get $res) (i32.const 0)
         (local.get $src) (local.get $pos)
         (local.get $len))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.add (local.get $len) (i32.const 1))))
      (local.get $res))

   (func $readfloat
      (param $s (ref $intern_state)) (param $code i32) (result (ref eq))
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local $d i64)
      (local $i i32)
      (local $v (ref eq))
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 8)))
      (if (i32.eq (local.get $code) (global.get $CODE_DOUBLE_BIG))
         (then
            (loop $loop
               (local.set $d
                  (i64.or
                     (i64.shl (local.get $d) (i64.const 8))
                     (i64.extend_i32_u
                        (array.get_u $string (local.get $src)
                          (i32.add (local.get $pos) (local.get $i))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.lt_u (local.get $i) (i32.const 8)))))
         (else
            (loop $loop
               (local.set $d
                  (i64.rotr
                     (i64.or (local.get $d)
                        (i64.extend_i32_u
                           (array.get_u $string (local.get $src)
                             (i32.add (local.get $pos) (local.get $i)))))
                     (i64.const 8)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.lt_u (local.get $i) (i32.const 8))))))
      (struct.new $float (f64.reinterpret_i64 (local.get $d))))

   (func $readfloats
      (param $s (ref $intern_state)) (param $code i32) (param $len i32)
      (result (ref eq))
      ;; ZZZ float array
      (local $dest (ref $block))
      (local $i i32)
      (local.set $code
         (select (global.get $CODE_DOUBLE_BIG) (global.get $CODE_DOUBLE_LITTLE)
            (i32.or
               (i32.eq (local.get $code) (global.get $CODE_DOUBLE_ARRAY8_BIG))
               (i32.eq (local.get $code)
                 (global.get $CODE_DOUBLE_ARRAY32_BIG)))))
      (local.set $dest
         (array.new $block (i31.new (i32.const 0))
            (i32.add (local.get $len) (i32.const 1))))
      (array.set $block (local.get $dest) (i32.const 0)
         (i31.new (global.get $double_array_tag)))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.le_u (local.get $i) (local.get $len))
            (then
               (array.set $block (local.get $dest) (local.get $i)
                  (call $readfloat (local.get $s) (local.get $code)))
               (br $loop))))
      (local.get $dest))

   (func (export "caml_deserialize_uint_1") (param $s (ref eq)) (result i32)
      (return_call $read8u (ref.cast (ref $intern_state) (local.get $s))))

   (func (export "caml_deserialize_sint_1") (param $s (ref eq)) (result i32)
      (return_call $read8s (ref.cast (ref $intern_state) (local.get $s))))

   (func (export "caml_deserialize_uint_2") (param $s (ref eq)) (result i32)
      (return_call $read16u (ref.cast (ref $intern_state) (local.get $s))))

   (func (export "caml_deserialize_sint_2") (param $s (ref eq)) (result i32)
      (return_call $read16s (ref.cast (ref $intern_state) (local.get $s))))

   (func (export "caml_deserialize_int_4") (param $s (ref eq)) (result i32)
      (return_call $read32 (ref.cast (ref $intern_state) (local.get $s))))

   (func (export "caml_deserialize_int_8") (param $vs (ref eq)) (result i64)
      (local $s (ref $intern_state))
      (local.set $s (ref.cast (ref $intern_state) (local.get $vs)))
      (i64.or (i64.shl (i64.extend_i32_u (call $read32 (local.get $s)))
                 (i64.const 32))
         (i64.extend_i32_u (call $read32 (local.get $s)))))

   (func $register_object (param $s (ref $intern_state)) (param $v (ref eq))
      (local $obj_table (ref $block))
      (local $p i32)
      (block $exit
         (local.set $obj_table
            (br_on_null $exit
               (struct.get $intern_state $obj_table (local.get $s))))
         (local.set $p (struct.get $intern_state $obj_counter (local.get $s)))
         (array.set $block (local.get $obj_table) (local.get $p) (local.get $v))
         (struct.set $intern_state $obj_counter (local.get $s)
            (i32.add (local.get $p) (i32.const 1)))))

   (type $stack_item
      (struct
         (field $blk (ref $block))
         (field $pos (mut i32))
         (field $next (ref null $stack_item))))

   (data $integer_too_large "input_value: integer too large")
   (data $code_pointer "input_value: code pointer")
   (data $ill_formed "input_value: ill-formed message")

   (data $unknown_custom "input_value: unknown custom block identifier")
   (data $expected_size "input_value: expected a fixed-size custom block")
   (data $incorrect_size
      "input_value: incorrect length of serialized custom block")

   (func $intern_custom
      (param $s (ref $intern_state)) (param $code i32) (result (ref eq))
      (local $ops (ref $custom_operations))
      (local $expected_size i32)
      (local $r ((ref eq) i32))
      (block $unknown
         (local.set $ops
            (br_on_null $unknown
               (call
                  $caml_find_custom_operations
                  (call $readstr
                     (local.get $s)))))
         (block $no_length
            (if (i32.eq (local.get $code) (global.get $CODE_CUSTOM_FIXED))
               (then
                  (local.set $expected_size
                     (struct.get $fixed_length $bsize_32
                        (br_on_null $no_length
                           (struct.get $custom_operations $fixed_length
                              (local.get $ops))))))
            (else
               (if (i32.eq (local.get $code) (global.get $CODE_CUSTOM_LEN))
                  (then
                     (local.set $expected_size (call $read32 (local.get $s)))
                     (struct.set $intern_state $pos (local.get $s)
                        (i32.add (struct.get $intern_state $pos (local.get $s))
                           (i32.const 8)))))))
            (local.set $r
               (call_ref $deserialize (local.get $s)
                  (struct.get $custom_operations $deserialize (local.get $ops))))
            (if (i32.and
                  (i32.ne (tuple.extract 1 (local.get $r))
                     (local.get $expected_size))
                  (i32.ne (local.get $code) (global.get $CODE_CUSTOM)))
               (then
                  (call $caml_failwith
                     (array.new_data $string $incorrect_size
                        (i32.const 0) (i32.const 56)))))
            (return (tuple.extract 0 (local.get $r))))
         (call $caml_failwith
            (array.new_data $string $expected_size
               (i32.const 0) (i32.const 47))))
      (call $caml_failwith
         (array.new_data $string $unknown_custom
            (i32.const 0) (i32.const 44)))
      (i31.new (i32.const 0)))

   (func $intern_rec
      (param $s (ref $intern_state)) (param $h (ref $marshal_header))
      (result (ref eq))
      (local $res (ref $block)) (local $dest (ref $block))
      (local $sp (ref null $stack_item))
      (local $item (ref $stack_item))
      (local $code i32)
      (local $header i32) (local $tag i32) (local $size i32)
      (local $len i32) (local $pos i32) (local $pos' i32) (local $ofs i32)
      (local $b (ref $block))
      (local $str (ref $string))
      (local $v (ref eq))
      (call $caml_init_custom_operations)
      (local.set $res (array.new_fixed $block 1 (i31.new (i32.const 0))))
      (local.set $sp
         (struct.new $stack_item
            (local.get $res) (i32.const 0) (ref.null $stack_item)))
      (local.set $size (struct.get $marshal_header $num_objects (local.get $h)))
      (if (local.get $size)
         (then
            (struct.set $intern_state $obj_table (local.get $s)
               (array.new $block (i31.new (i32.const 0)) (local.get $size)))))
      (local.set $v (i31.new (i32.const 0))) ;; keep validator happy
      (block $exit
       (loop $loop
        (local.set $item (br_on_null $exit (local.get $sp)))
        (local.set $dest (struct.get $stack_item $blk (local.get $item)))
        (local.set $pos (struct.get $stack_item $pos (local.get $item)))
        (local.set $pos' (i32.add (local.get $pos) (i32.const 1)))
        (struct.set $stack_item $pos (local.get $item) (local.get $pos'))
        (if (i32.eq (local.get $pos') (array.len (local.get $dest)))
           (then
               (local.set $sp
                  (struct.get $stack_item $next (local.get $item)))))
        (block $done
         (block $read_block
          (block $read_string
           (block $read_double_array
            (block $read_shared
             (local.set $code (call $read8u (local.get $s)))
             (if (i32.ge_u (local.get $code) (global.get $PREFIX_SMALL_INT))
                (then
                   (if (i32.ge_u (local.get $code) (global.get $PREFIX_SMALL_BLOCK))
                      (then
                         ;; Small block
                         (local.set $tag
                            (i32.and (local.get $code) (i32.const 0xF)))
                         (local.set $size
                            (i32.and (i32.shr_u (local.get $code) (i32.const 4))
                               (i32.const 0x7)))
                         (br $read_block))
                      (else
                         ;; Small int
                         (local.set $v
                            (i31.new
                               (i32.and (local.get $code) (i32.const 0x3F))))
                         (br $done))))
                (else
                   (if (i32.ge_u (local.get $code)
                         (global.get $PREFIX_SMALL_STRING))
                      (then
                         (local.set $len
                            (i32.and (local.get $code) (i32.const 0x1F)))
                         (br $read_string))
                      (else
                         (block $INT8
                          (block $INT16
                           (block $INT32
                            (block $INT64
                             (block $SHARED8
                              (block $SHARED16
                               (block $SHARED32
                                (block $BLOCK32
                                 (block $STRING8
                                  (block $STRING32
                                   (block $DOUBLE
                                    (block $DOUBLE_ARRAY8
                                     (block $DOUBLE_ARRAY32
                                      (block $CODEPOINTER
                                       (block $CUSTOM
                                        (block $default
                                         (br_table $INT8 $INT16 $INT32 $INT64
                                            $SHARED8 $SHARED16 $SHARED32
                                            $DOUBLE_ARRAY32 $BLOCK32 $STRING8
                                            $STRING32 $DOUBLE $DOUBLE
                                            $DOUBLE_ARRAY8 $DOUBLE_ARRAY8
                                            $DOUBLE_ARRAY32 $CODEPOINTER
                                            $CODEPOINTER $CUSTOM $default
                                            $default $default $default $default
                                            $CUSTOM $CUSTOM $default
                                            (local.get $code)))
                                        ;; default
                                        (call $caml_failwith
                                           (array.new_data $string $ill_formed
                                              (i32.const 0) (i32.const 31)))
                                        (br $done))
                                       ;; CUSTOM
                                       (local.set $v
                                          (call $intern_custom (local.get $s)
                                             (local.get $code)))
                                       (call $register_object (local.get $s)
                                          (local.get $v))
                                       (br $done))
                                      ;; CODEPOINTER
                                      (call $caml_failwith
                                        (array.new_data $string $code_pointer
                                           (i32.const 0) (i32.const 25)))
                                      (br $done))
                                     ;; DOUBLE_ARRAY32
                                     (local.set $len
                                        (call $read32 (local.get $s)))
                                     (br $read_double_array))
                                    ;; DOUBLE_ARRAY8
                                    (local.set $len
                                       (call $read8u (local.get $s)))
                                    (br $read_double_array))
                                   ;; DOUBLE
                                   (local.set $v
                                      (call $readfloat
                                         (local.get $s) (local.get $code)))
                                   (call $register_object
                                      (local.get $s) (local.get $v))
                                   (br $done))
                                  ;; STRING32
                                  (local.set $len (call $read32 (local.get $s)))
                                  (br $read_string))
                                 ;; STRING8
                                 (local.set $len (call $read8u (local.get $s)))
                                 (br $read_string))
                                ;; BLOCK32
                                (local.set $header (call $read32 (local.get $s)))
                                (local.set $tag
                                   (i32.and (local.get $header)
                                      (i32.const 0xFF)))
                                (local.set $size
                                   (i32.shr_u (local.get $header)
                                      (i32.const 10)))
                                (br $read_block))
                               ;; SHARED32
                               (local.set $ofs (call $read32 (local.get $s)))
                               (br $read_shared))
                              ;; SHARED16
                              (local.set $ofs (call $read16u (local.get $s)))
                              (br $read_shared))
                             ;; SHARED8
                             (local.set $ofs (call $read8u (local.get $s)))
                             (br $read_shared))
                            ;; INT64
                            (call $caml_failwith
                               (array.new_data $string $integer_too_large
                                  (i32.const 0) (i32.const 30)))
                            (br $done))
                           ;; INT32
                           (local.set $v (i31.new (call $read32 (local.get $s))))
                           (br $done))
                          ;; INT16
                          (local.set $v (i31.new (call $read16s (local.get $s))))
                          (br $done))
                         ;; INT8
                         (local.set $v (i31.new (call $read8s (local.get $s))))
                         (br $done))
                        ))))
            ;; read_shared
            (local.set $ofs
               (i32.sub
                  (struct.get $intern_state $obj_counter (local.get $s))
                  (local.get $ofs)))
            (local.set $v
               (array.get $block
                  (ref.as_non_null
                     (struct.get $intern_state $obj_table
                        (local.get $s)))
                  (local.get $ofs)))
            (br $done))
           ;; read_double_array
           (local.set $v
              (call $readfloats
                 (local.get $s) (local.get $code) (local.get $len)))
           (call $register_object (local.get $s) (local.get $v))
           (br $done))
          ;; read_string
          (local.set $str (array.new $string (i32.const 0) (local.get $len)))
          (call $readblock (local.get $s) (local.get $str))
          (local.set $v (local.get $str))
          (call $register_object (local.get $s) (local.get $v))
          (br $done))
         ;; read_block
         (local.set $b
            (array.new $block (i31.new (i32.const 0))
               (i32.add (local.get $size) (i32.const 1))))
         (array.set $block (local.get $b) (i32.const 0)
            (i31.new (local.get $tag)))
         (if (local.get $size)
            (then
               (call $register_object (local.get $s) (local.get $b))
               (local.set $sp
                  (struct.new $stack_item
                     (local.get $b) (i32.const 1) (local.get $sp)))))
         (local.set $v (local.get $b))
         (br $done))
        ;; done
        (array.set $block (local.get $dest) (local.get $pos) (local.get $v))
        (br $loop)))
      (array.get $block (local.get $res) (i32.const 0)))

   (data $too_large ": object too large to be read back on a 32-bit platform")

   (func $too_large (param $prim (ref $string))
      (call $caml_failwith
         (call $caml_string_cat (local.get $prim)
            (array.new_data $string $too_large (i32.const 0) (i32.const 55)))))

   (data $bad_object ": bad object")

   (func $bad_object (param $prim (ref $string))
      (call $caml_failwith
         (call $caml_string_cat (local.get $prim)
            (array.new_data $string $bad_object (i32.const 0) (i32.const 12)))))

   (data $bad_length ": bad length")

   (func $bad_length (param $prim (ref $string))
      (call $caml_failwith
         (call $caml_string_cat (local.get $prim)
            (array.new_data $string $bad_length (i32.const 0) (i32.const 12)))))

   (type $marshal_header
      (struct
         (field $data_len i32)
         (field $num_objects i32)))

   (func $parse_header
      (param $s (ref $intern_state)) (param $prim (ref $string))
      (result (ref $marshal_header))
      (local $magic i32)
      (local $data_len i32) (local $num_objects i32) (local $whsize i32)
      (local.set $magic (call $read32 (local.get $s)))
      (if (i32.eq (local.get $magic) (global.get $Intext_magic_number_big))
         (then
            (call $too_large (local.get $prim))))
      (if (i32.ne (local.get $magic) (global.get $Intext_magic_number_small))
         (then
            (call $bad_object (local.get $prim))))
      (local.set $data_len (call $read32 (local.get $s)))
      (local.set $num_objects (call $read32 (local.get $s)))
      (drop (call $read32 (local.get $s)))
      (drop (call $read32 (local.get $s)))
      (struct.new $marshal_header
         (local.get $data_len)
         (local.get $num_objects)))

   (data $marshal_data_size "Marshal.data_size")

   (func (export "caml_marshal_data_size")
      (param $buf (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (local $s (ref $intern_state))
      (local $magic i32)
      (local.set $s
         (call $get_intern_state
            (ref.cast (ref $string) (local.get $buf))
            (i31.get_u (ref.cast (ref i31) (local.get $ofs)))))
      (local.set $magic (call $read32 (local.get $s)))
      (if (i32.eq (local.get $magic) (global.get $Intext_magic_number_big))
         (then
            (call $too_large
               (array.new_data $string $marshal_data_size
                  (i32.const 0) (i32.const 17)))))
      (if (i32.ne (local.get $magic) (global.get $Intext_magic_number_small))
         (then
            (call $bad_object
               (array.new_data $string $marshal_data_size
                  (i32.const 0) (i32.const 17)))))
      (i31.new (call $read32 (local.get $s))))

   (type $output_block
      (struct
         (field $next (mut (ref null $output_block)))
         (field $end (mut i32))
         (field $data (ref $string))))

   (type $extern_state
      (struct
         ;; Flags
         (field $no_sharing i32)
         (field $user_provided_output i32)
         ;; Header information
         (field $obj_counter (mut i32))
         (field $size_32 (mut i32))
         (field $size_64 (mut i32))
         ;; Position of already marshalled objects
         (field $pos_table (ref any))
         ;; Buffers
         (field $buf (mut (ref $string)))
         (field $pos (mut i32))
         (field $limit (mut i32))
         (field $output_first (ref $output_block))
         (field $output_last (mut (ref $output_block)))))

   (func $init_extern_state
      (param $flags (ref eq)) (param $output (ref $output_block))
      (param $pos i32) (param $user_provided_output i32)
      (result (ref $extern_state))
      (local $b (ref $block))
      (local $no_sharing i32)
      (loop $parse_flags
         (drop (block $done (result (ref eq))
            (local.set $b
               (br_on_cast_fail $done (ref eq) (ref $block) (local.get $flags)))
            (if (ref.eq (array.get $block (local.get $b) (i32.const 1))
                   (i31.new (i32.const 0)))
               (then (local.set $no_sharing (i32.const 1))))
            (local.set $flags (array.get $block (local.get $b) (i32.const 2)))
            (br $parse_flags))))
      (struct.new $extern_state
         (local.get $no_sharing)
         (local.get $user_provided_output)
         (i32.const 0)
         (i32.const 0)
         (i32.const 0)
         (call $weak_map_new)
         (struct.get $output_block $data (local.get $output))
         (local.get $pos)
         (struct.get $output_block $end (local.get $output))
         (local.get $output)
         (local.get $output)))

   (data $buffer_overflow "Marshal.to_buffer: buffer overflow")

   (global $SIZE_EXTERN_OUTPUT_BLOCK i32 (i32.const 8100))

   (func $reserve_extern_output
      (param $s (ref $extern_state)) (param $required i32) (result i32)
      (local $last (ref $output_block)) (local $blk (ref $output_block))
      (local $pos i32) (local $extra i32)
      (local $buf (ref $string))
      (local.set $pos (struct.get $extern_state $pos (local.get $s)))
      (if (i32.le_u (i32.add (local.get $pos) (local.get $required))
             (struct.get $extern_state $limit (local.get $s)))
         (then
            (struct.set $extern_state $pos (local.get $s)
               (i32.add (local.get $pos) (local.get $required)))
            (return (local.get $pos))))
      (if (struct.get $extern_state $user_provided_output (local.get $s))
         (then
            (call $caml_failwith
               (array.new_data $string $buffer_overflow
                  (i32.const 0) (i32.const 34)))))
      (local.set $last (struct.get $extern_state $output_last (local.get $s)))
      (struct.set $output_block $end (local.get $last)
         (struct.get $extern_state $pos (local.get $s)))
      (if (i32.gt_s (local.get $required)
             (i32.shr_u (global.get $SIZE_EXTERN_OUTPUT_BLOCK) (i32.const 1)))
         (then
            (local.set $extra (local.get $required))))
      (local.set $buf
         (array.new $string (i32.const 0)
            (i32.add (global.get $SIZE_EXTERN_OUTPUT_BLOCK) (local.get $extra))))
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (i32.const 0)
            (local.get $buf)))
      (struct.set $output_block $next (local.get $last) (local.get $blk))
      (struct.set $extern_state $output_last (local.get $s) (local.get $blk))
      (struct.set $extern_state $buf (local.get $s) (local.get $buf))
      (struct.set $extern_state $pos (local.get $s) (local.get $required))
      (struct.set $extern_state $limit (local.get $s)
         (array.len (local.get $buf)))
      (i32.const 0))

   (func $store16 (param $s (ref $string)) (param $pos i32) (param $n i32)
      (array.set $string (local.get $s) (local.get $pos)
         (i32.shr_u (local.get $n) (i32.const 8)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)) (local.get $n)))

   (func $store32 (param $s (ref $string)) (param $pos i32) (param $n i32)
      (array.set $string (local.get $s) (local.get $pos)
         (i32.shr_u (local.get $n) (i32.const 24)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 1))
         (i32.shr_u (local.get $n) (i32.const 16)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 2))
         (i32.shr_u (local.get $n) (i32.const 8)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 3)) (local.get $n)))

   (func $store64 (param $s (ref $string)) (param $pos i32) (param $n i64)
      (call $store32 (local.get $s) (local.get $pos)
         (i32.wrap_i64 (i64.shr_u (local.get $n) (i64.const 32))))
      (call $store32 (local.get $s) (i32.add (local.get $pos) (i32.const 4))
         (i32.wrap_i64 (local.get $n))))

   (func $write (param $s (ref $extern_state)) (param $c i32)
      (local $pos i32)
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 1)))
      (array.set $string (struct.get $extern_state $buf (local.get $s))
         (local.get $pos) (local.get $c)))

   (func $writecode8
      (param $s (ref $extern_state)) (param $c i32) (param $v i32)
      (local $pos i32) (local $buf (ref $string))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 2)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (array.set $string (local.get $buf) (local.get $pos) (local.get $c))
      (array.set $string (local.get $buf)
         (i32.add (local.get $pos) (i32.const 1)) (local.get $v)))

   (func $writecode16
      (param $s (ref $extern_state)) (param $c i32) (param $v i32)
      (local $pos i32) (local $buf (ref $string))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 3)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (array.set $string (local.get $buf) (local.get $pos) (local.get $c))
      (call $store16 (local.get $buf) (i32.add (local.get $pos) (i32.const 1))
         (local.get $v)))

   (func $writecode32
      (param $s (ref $extern_state)) (param $c i32) (param $v i32)
      (local $pos i32) (local $buf (ref $string))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 5)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (array.set $string (local.get $buf) (local.get $pos) (local.get $c))
      (call $store32 (local.get $buf) (i32.add (local.get $pos) (i32.const 1))
         (local.get $v)))

   (func $writeblock
      (param $s (ref $extern_state)) (param $str (ref $string))
      (local $len i32) (local $pos i32)
      (local.set $len (array.len (local.get $str)))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (local.get $len)))
      (array.copy $string $string
         (struct.get $extern_state $buf (local.get $s)) (local.get $pos)
         (local.get $str) (i32.const 0) (local.get $len)))

   (func $writefloat
      (param $s (ref $extern_state)) (param $f f64)
      (local $pos i32) (local $buf (ref $string)) (local $d i64) (local $i i32)
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 8)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (local.set $d (i64.reinterpret_f64 (local.get $f)))
      (loop $loop
         (array.set $string (local.get $buf)
            (i32.add (local.get $pos) (local.get $i))
            (i32.wrap_i64
               (i64.shr_u (local.get $d)
                  (i64.extend_i32_u (i32.shl (local.get $i) (i32.const 3))))))
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (br_if $loop (i32.lt_u (local.get $i) (i32.const 8)))))

   (func $writefloats
      (param $s (ref $extern_state)) (param $b (ref $block))
      (local $pos i32) (local $sz i32) (local $buf (ref $string)) (local $d i64)
      (local $i i32) (local $j i32)
      (local.set $sz (i32.sub (array.len (local.get $b)) (i32.const 1)))
      (local.set $pos
         (call $reserve_extern_output
            (local.get $s) (i32.shl (local.get $sz) (i32.const 3))))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (local.set $j (i32.const 1))
      (loop $loop2
         (if (i32.le_u (local.get $j) (local.get $sz))
            (then
               (local.set $d
                  (i64.reinterpret_f64
                     (struct.get $float 0
                        (ref.cast (ref $float)
                           (array.get $block (local.get $b) (local.get $j))))))
               (local.set $i (i32.const 0))
               (loop $loop
                  (array.set $string (local.get $buf)
                     (i32.add (local.get $pos) (local.get $i))
                     (i32.wrap_i64
                        (i64.shr_u (local.get $d)
                           (i64.extend_i32_u
                              (i32.shl (local.get $i) (i32.const 3))))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br_if $loop (i32.lt_u (local.get $i) (i32.const 8))))
               (local.set $pos (i32.add (local.get $pos) (i32.const 8)))
               (local.set $j (i32.add (local.get $j) (i32.const 1)))
               (br $loop2)))))

   (func $extern_lookup_position
      (param $s (ref $extern_state)) (param $obj (ref eq)) (result i32)
      (block $not_found
         (br_if $not_found (struct.get $extern_state $no_sharing (local.get $s)))
         (return
            (i31.get_s
               (br_on_null $not_found
                  (call $weak_map_get
                     (struct.get $extern_state $pos_table (local.get $s))
                     (local.get $obj))))))
      (i32.const -1))

   (func $extern_record_location
      (param $s (ref $extern_state)) (param $obj (ref eq))
      (local $pos i32)
      (if (struct.get $extern_state $no_sharing (local.get $s))
         (then (return)))
      (local.set $pos (struct.get $extern_state $obj_counter (local.get $s)))
      (struct.set $extern_state $obj_counter (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)))
      (call $weak_map_set
         (struct.get $extern_state $pos_table (local.get $s))
         (local.get $obj) (i31.new (local.get $pos))))

   (func $extern_size
      (param $s (ref $extern_state)) (param $s32 i32) (param $s64 i32)
      (struct.set $extern_state $size_32 (local.get $s)
         (i32.add (struct.get $extern_state $size_32 (local.get $s))
            (i32.add (local.get $s32) (i32.const 1))))
      (struct.set $extern_state $size_64 (local.get $s)
         (i32.add (struct.get $extern_state $size_64 (local.get $s))
            (i32.add (local.get $s64) (i32.const 1)))))

   (func $extern_int (param $s (ref $extern_state)) (param $n i32)
      (if (i32.and (i32.ge_s (local.get $n) (i32.const 0))
             (i32.lt_s (local.get $n) (i32.const 0x40)))
         (then
            (call $write (local.get $s)
               (i32.add (global.get $PREFIX_SMALL_INT) (local.get $n))))
      (else (if (i32.and (i32.ge_s (local.get $n) (i32.const -128))
                   (i32.lt_s (local.get $n) (i32.const 128)))
         (then
            (call $writecode8 (local.get $s) (global.get $CODE_INT8)
               (local.get $n)))
      (else (if (i32.and (i32.ge_s (local.get $n) (i32.const -32768))
                   (i32.lt_s (local.get $n) (i32.const 32768)))
         (then
            (call $writecode16 (local.get $s) (global.get $CODE_INT16)
               (local.get $n)))
      (else
         (call $writecode32 (local.get $s) (global.get $CODE_INT32)
            (local.get $n)))))))))

   (func $extern_shared_reference (param $s (ref $extern_state)) (param $d i32)
      (if (i32.lt_u (local.get $d) (i32.const 0x100))
         (then
            (call $writecode8 (local.get $s) (global.get $CODE_SHARED8)
               (local.get $d)))
      (else (if (i32.lt_u (local.get $d) (i32.const 0x10000))
         (then
            (call $writecode16 (local.get $s) (global.get $CODE_SHARED16)
               (local.get $d)))
      (else
         (call $writecode32 (local.get $s) (global.get $CODE_SHARED32)
            (local.get $d)))))))

   (func $extern_header
      (param $s (ref $extern_state)) (param $sz (i32)) (param $tag i32)
      (if (i32.and (i32.lt_u (local.get $tag) (i32.const 16))
             (i32.lt_u (local.get $sz) (i32.const 8)))
         (then
             (call $write (local.get $s)
                (i32.add (global.get $PREFIX_SMALL_BLOCK)
                   (i32.or (local.get $tag)
                      (i32.shl (local.get $sz) (i32.const 4))))))
         (else
            (call $writecode32 (local.get $s) (global.get $CODE_BLOCK32)
               (i32.or (local.get $tag)
                  (i32.shl (local.get $sz) (i32.const 10)))))))

   (func $extern_string (param $s (ref $extern_state)) (param $v (ref $string))
      (local $len i32)
      (local.set $len (array.len (local.get $v)))
      (if (i32.lt_u (local.get $len) (i32.const 0x20))
         (then
            (call $write (local.get $s)
               (i32.add (global.get $PREFIX_SMALL_STRING) (local.get $len))))
      (else (if (i32.lt_u (local.get $len) (i32.const 0x100))
         (then
            (call $writecode8 (local.get $s) (global.get $CODE_STRING8)
               (local.get $len)))
      (else
         (call $writecode32 (local.get $s) (global.get $CODE_STRING32)
            (local.get $len))))))
      (call $writeblock (local.get $s) (local.get $v)))

   (func $extern_float (param $s (ref $extern_state)) (param $v f64)
      (call $write (local.get $s) (global.get $CODE_DOUBLE_LITTLE))
      (call $writefloat (local.get $s) (local.get $v)))

   (func $extern_float_array
      (param $s (ref $extern_state)) (param $v (ref $block))
      (local $nfloats i32)
      (local.set $nfloats (array.len (local.get $v)))
      (if (i32.lt_u (local.get $nfloats) (i32.const 0x100))
         (then
            (call $writecode8 (local.get $s)
               (global.get $CODE_DOUBLE_ARRAY8_LITTLE) (local.get $nfloats)))
         (else
            (call $writecode32 (local.get $s)
               (global.get $CODE_DOUBLE_ARRAY32_LITTLE) (local.get $nfloats))))
      (call $writefloats (local.get $s) (local.get $v)))

   (data $incorrect_sizes "output_value: incorrect fixed sizes specified by ")

   (func $extern_custom
      (param $s (ref $extern_state)) (param $v (ref $custom)) (result i32 i32)
      (local $ops (ref $custom_operations))
      (local $serialize (ref $serialize))
      (local $fixed_length (ref $fixed_length))
      (local $pos i32) (local $buf (ref $string))
      (local $r (i32 i32))
      (local.set $ops (struct.get $custom 0 (local.get $v)))
      (block $abstract
         (local.set $serialize
            (br_on_null $abstract
               (struct.get $custom_operations $serialize (local.get $ops))))
         (block $variable_length
            (local.set $fixed_length
               (br_on_null $variable_length
                  (struct.get $custom_operations $fixed_length
                     (local.get $ops))))
            (call $write (local.get $s) (global.get $CODE_CUSTOM_FIXED))
            (call $writeblock (local.get $s)
               (struct.get $custom_operations $id (local.get $ops)))
            (call $write (local.get $s) (i32.const 0))
            (local.set $r
               (call_ref $serialize
                  (local.get $s) (local.get $v) (local.get $serialize)))
            (if (i32.or
                   (i32.ne (tuple.extract 0 (local.get $r))
                      (struct.get $fixed_length $bsize_32
                        (local.get $fixed_length)))
                   (i32.ne (tuple.extract 1 (local.get $r))
                      (struct.get $fixed_length $bsize_64
                        (local.get $fixed_length))))
                (then
                   (call $caml_failwith
                      (call $caml_string_cat
                         (array.new_data $string $incorrect_sizes
                            (i32.const 0) (i32.const 49))
                         (struct.get $custom_operations $id
                            (local.get $ops))))))
            (return (local.get $r)))
         ;; variable length
         (call $write (local.get $s) (global.get $CODE_CUSTOM_LEN))
         (call $writeblock (local.get $s)
            (struct.get $custom_operations $id (local.get $ops)))
         (call $write (local.get $s) (i32.const 0))
         (local.set $pos
            (call $reserve_extern_output (local.get $s) (i32.const 12)))
         (local.set $buf (struct.get $extern_state $buf (local.get $s)))
         (local.set $r
            (call_ref $serialize
               (local.get $s) (local.get $v) (local.get $serialize)))
         (call $store32 (local.get $buf) (local.get $pos)
            (tuple.extract 0 (local.get $r)))
         (call $store32 (local.get $buf) (i32.add (local.get $pos) (i32.const 8))
            (tuple.extract 1 (local.get $r)))
         (return (local.get $r)))
      (call $caml_invalid_argument
         (array.new_data $string $cust_value (i32.const 0) (i32.const 37)))
      (return (tuple.make (i32.const 0) (i32.const 0))))

   (data $func_value "output_value: functional value")
   (data $cont_value "output_value: continuation value")
   (data $js_value "output_value: abstract value (JavaScript value)")
   (data $abstract_value "output_value: abstract value")
   (data $cust_value "output_value: abstract value (Custom)")

   (func $extern_rec (param $s (ref $extern_state)) (param $v (ref eq))
      (local $sp (ref null $stack_item))
      (local $item (ref $stack_item))
      (local $b (ref $block)) (local $str (ref $string))
      (local $hd i32) (local $tag i32) (local $sz i32)
      (local $pos i32)
      (local $r (i32 i32))
      (loop $loop
         (block $next_item
            (drop (block $not_int (result (ref eq))
               (call $extern_int (local.get $s)
                  (i31.get_s
                     (br_on_cast_fail $not_int (ref eq) (ref i31)
                        (local.get $v))))
               (br $next_item)))
            (drop (block $not_block (result (ref eq))
               (local.set $b
                  (br_on_cast_fail $not_block (ref eq) (ref $block)
                     (local.get $v)))
               (local.set $tag
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block (local.get $b) (i32.const 0)))))
               (local.set $sz (i32.sub (array.len (local.get $b)) (i32.const 1)))
               (if (i32.eqz (local.get $sz))
                  (then
                     (call $extern_header
                        (local.get $s) (i32.const 0) (local.get $tag))
                     (br $next_item)))
               (local.set $pos
                  (call $extern_lookup_position (local.get $s) (local.get $v)))
               (if (i32.ge_s (local.get $pos) (i32.const 0))
                  (then
                     (call $extern_shared_reference (local.get $s)
                        (i32.sub
                           (struct.get $extern_state $obj_counter (local.get $s))
                           (local.get $pos)))
                     (br $next_item)))
               (call $extern_record_location (local.get $s) (local.get $v))
               (if (i32.eq (local.get $tag) (global.get $double_array_tag))
                  (then
                     (call $extern_float_array (local.get $s) (local.get $b))
                     (call $extern_size (local.get $s)
                        (i32.mul (local.get $sz) (i32.const 2))
                        (local.get $sz))
                     (br $next_item)))
               (call $extern_header
                  (local.get $s) (local.get $sz) (local.get $tag))
               (call $extern_size
                  (local.get $s) (local.get $sz) (local.get $sz))
               (if (i32.gt_u (local.get $sz) (i32.const 1))
                  (then
                     (local.set $sp
                        (struct.new $stack_item
                           (local.get $b)
                           (i32.const 2)
                           (local.get $sp)))))
               (local.set $v (array.get $block (local.get $b) (i32.const 1)))
               (br $loop)))
            (local.set $pos
               (call $extern_lookup_position (local.get $s) (local.get $v)))
            (if (i32.ge_s (local.get $pos) (i32.const 0))
               (then
                  (call $extern_shared_reference (local.get $s)
                     (i32.sub
                        (struct.get $extern_state $obj_counter (local.get $s))
                        (local.get $pos)))
                  (br $next_item)))
            (call $extern_record_location (local.get $s) (local.get $v))
            (drop (block $not_string (result (ref eq))
               (local.set $str
                  (br_on_cast_fail $not_string (ref eq) (ref $string)
                     (local.get $v)))
               (call $extern_string (local.get $s) (local.get $str))
               (local.set $sz (array.len (local.get $str)))
               (call $extern_size (local.get $s)
                  (i32.add (i32.const 1)
                     (i32.shr_u (local.get $sz) (i32.const 2)))
                  (i32.add (i32.const 1)
                     (i32.shr_u (local.get $sz) (i32.const 3))))
               (br $next_item)))
            (drop (block $not_float (result (ref eq))
               (call $extern_float (local.get $s)
                  (struct.get $float 0
                     (br_on_cast_fail $not_float (ref eq) (ref $float)
                        (local.get $v))))
               (call $extern_size (local.get $s) (i32.const 2) (i32.const 1))
               (br $next_item)))
            (drop (block $not_custom (result (ref eq))
               (local.set $r
                  (call $extern_custom (local.get $s)
                     (br_on_cast_fail $not_custom (ref eq) (ref $custom)
                        (local.get $v))))
               (call $extern_size (local.get $s)
                  (i32.shr_u
                     (i32.add (tuple.extract 0 (local.get $r)) (i32.const 7))
                        (i32.const 2))
                  (i32.shr_u
                     (i32.add (tuple.extract 1 (local.get $r)) (i32.const 15))
                        (i32.const 3)))
               (br $next_item)))
            (if (call $caml_is_closure (local.get $v))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $func_value
                        (i32.const 0) (i32.const 30)))))
            (if (call $caml_is_continuation (local.get $v))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $cont_value
                        (i32.const 0) (i32.const 32)))))
            (if (ref.test (ref $js) (local.get $v))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $js_value
                        (i32.const 0) (i32.const 47)))))
            (call $caml_invalid_argument
               (array.new_data $string $abstract_value
                  (i32.const 0) (i32.const 28)))
         )
         ;; next_item
         (block $done
            (local.set $item (br_on_null $done (local.get $sp)))
            (local.set $b (struct.get $stack_item $blk (local.get $item)))
            (local.set $pos (struct.get $stack_item $pos (local.get $item)))
            (local.set $v (array.get $block (local.get $b) (local.get $pos)))
            (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
            (struct.set $stack_item $pos (local.get $item) (local.get $pos))
            (if (i32.eq (local.get $pos) (array.len (local.get $b)))
               (then
                  (local.set $sp
                     (struct.get $stack_item $next (local.get $item)))))
            (br $loop))))

   (func $extern_output_length
      (param $s (ref $extern_state)) (param $pos i32) (result i32)
      (local $len i32)
      (local $output_block (ref $output_block))
      (if (struct.get $extern_state $user_provided_output (local.get $s))
         (then
            (return
               (i32.sub (struct.get $extern_state $pos (local.get $s))
                  (local.get $pos))))
         (else
            (struct.set $output_block $end
               (struct.get $extern_state $output_last (local.get $s))
               (struct.get $extern_state $pos (local.get $s)))
            (local.set $output_block
               (struct.get $extern_state $output_first (local.get $s)))
            (loop $loop
               (block $done
                  (local.set $len
                     (i32.add (local.get $len)
                        (struct.get $output_block $end
                           (local.get $output_block))))
                  (local.set $output_block
                     (br_on_null $done
                        (struct.get $output_block $next
                           (local.get $output_block))))
                  (br $loop)))
            (return (local.get $len)))))

   (func $extern_value
      (param $flags (ref eq)) (param $output (ref $output_block))
      (param $pos i32) (param $user_provided_output i32) (param $v (ref eq))
      (result i32 (ref $string) (ref $extern_state))
      (local $s (ref $extern_state)) (local $len i32)
      (local $header (ref $string))
      (local.set $s
         (call $init_extern_state
            (local.get $flags) (local.get $output) (local.get $pos)
            (local.get $user_provided_output)))
      (call $extern_rec (local.get $s) (local.get $v))
      (local.set $len
         (call $extern_output_length (local.get $s) (local.get $pos)))
      (local.set $header (array.new $string (i32.const 0) (i32.const 20)))
      (call $store32 (local.get $header) (i32.const 0)
         (global.get $Intext_magic_number_small))
      (call $store32 (local.get $header) (i32.const 4) (local.get $len))
      (call $store32 (local.get $header) (i32.const 8)
         (struct.get $extern_state $obj_counter (local.get $s)))
      (call $store32 (local.get $header) (i32.const 12)
         (struct.get $extern_state $size_32 (local.get $s)))
      (call $store32 (local.get $header) (i32.const 16)
         (struct.get $extern_state $size_64 (local.get $s)))
      (tuple.make (local.get $len) (local.get $header) (local.get $s)))

   (func (export "caml_output_value_to_string")
      (param $v (ref eq)) (param $flags (ref eq)) (result (ref eq))
      (local $r (i32 (ref $string) (ref $extern_state)))
      (local $blk (ref $output_block)) (local $pos i32) (local $len i32)
      (local $res (ref $string))
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (global.get $SIZE_EXTERN_OUTPUT_BLOCK)
            (array.new $string (i32.const 0)
               (global.get $SIZE_EXTERN_OUTPUT_BLOCK))))
      (local.set $r
         (call $extern_value
            (local.get $flags) (local.get $blk)
            (i32.const 0) (i32.const 0) (local.get $v)))
      (local.set $res
         (array.new $string (i32.const 0)
            (i32.add (tuple.extract 0 (local.get $r)) (i32.const 20))))
      (array.copy $string $string
         (local.get $res) (i32.const 0)
         (tuple.extract 1 (local.get $r)) (i32.const 0) (i32.const 20))
      (local.set $pos (i32.const 20))
      (loop $loop
         (block $done
            (local.set $len (struct.get $output_block $end (local.get $blk)))
            (array.copy $string $string
               (local.get $res) (local.get $pos)
               (struct.get $output_block $data (local.get $blk)) (i32.const 0)
               (local.get $len))
            (local.set $pos (i32.add (local.get $pos) (local.get $len)))
            (local.set $blk
               (br_on_null $done
                  (struct.get $output_block $next (local.get $blk))))
            (br $loop)))
      (local.get $res))

   (func (export "caml_output_value_to_buffer")
      (param $vbuf (ref eq)) (param $vpos (ref eq)) (param $vlen (ref eq))
      (param $v (ref eq)) (param $flags (ref eq)) (result (ref eq))
      (local $buf (ref $string)) (local $pos i32) (local $len i32)
      (local $r (i32 (ref $string) (ref $extern_state)))
      (local $blk (ref $output_block))
      (local.set $buf (ref.cast (ref $string) (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (i32.add (local.get $pos) (local.get $len))
            (local.get $buf)))
      (local.set $r
         (call $extern_value
            (local.get $flags)
            (local.get $blk)
            (i32.add (local.get $pos) (i32.const 20))
            (i32.const 1)
            (local.get $v)))
      (array.copy $string $string
         (local.get $buf) (local.get $pos)
         (tuple.extract 1 (local.get $r)) (i32.const 0) (i32.const 20))
      (i31.new (i32.const 0)))

   (func (export "caml_output_value")
      (param $ch (ref eq)) (param $v (ref eq)) (param $flags (ref eq))
      (result (ref eq))
      (local $r (i32 (ref $string) (ref $extern_state)))
      (local $blk (ref $output_block)) (local $len i32)
      (local $res (ref $string))
      ;; ZZZ check if binary channel?
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (global.get $SIZE_EXTERN_OUTPUT_BLOCK)
            (array.new $string (i32.const 0)
               (global.get $SIZE_EXTERN_OUTPUT_BLOCK))))
      (local.set $r
         (call $extern_value
            (local.get $flags) (local.get $blk)
            (i32.const 0) (i32.const 0) (local.get $v)))
      (call $caml_really_putblock (local.get $ch)
         (tuple.extract 1 (local.get $r)) (i32.const 0) (i32.const 20))
      (loop $loop
         (block $done
            (local.set $len (struct.get $output_block $end (local.get $blk)))
            (call $caml_really_putblock (local.get $ch)
               (struct.get $output_block $data (local.get $blk))
               (i32.const 0)
               (struct.get $output_block $end (local.get $blk)))
            (local.set $blk
               (br_on_null $done
                  (struct.get $output_block $next (local.get $blk))))
            (br $loop)))
      (call $caml_flush_if_unbuffered (local.get $ch))
      (i31.new (i32.const 0)))

   (func (export "caml_serialize_int_1") (param $vs (ref eq)) (param $i i32)
      (local $s (ref $extern_state))
      (local $pos i32)
      (local.set $s (ref.cast (ref $extern_state) (local.get $vs)))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 1)))
      (array.set $string (struct.get $extern_state $buf (local.get $s))
         (local.get $pos) (local.get $i)))

   (func (export "caml_serialize_int_2") (param $vs (ref eq)) (param $i i32)
      (local $s (ref $extern_state))
      (local $pos i32)
      (local.set $s (ref.cast (ref $extern_state) (local.get $vs)))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 2)))
      (call $store16 (struct.get $extern_state $buf (local.get $s))
         (local.get $pos) (local.get $i)))

   (func (export "caml_serialize_int_4") (param $vs (ref eq)) (param $i i32)
      (local $s (ref $extern_state))
      (local $pos i32)
      (local.set $s (ref.cast (ref $extern_state) (local.get $vs)))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 4)))
      (call $store32 (struct.get $extern_state $buf (local.get $s))
         (local.get $pos) (local.get $i)))

   (func (export "caml_serialize_int_8") (param $vs (ref eq)) (param $i i64)
      (local $s (ref $extern_state))
      (local $pos i32)
      (local.set $s (ref.cast (ref $extern_state) (local.get $vs)))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 8)))
      (call $store64 (struct.get $extern_state $buf (local.get $s))
         (local.get $pos) (local.get $i)))
)
