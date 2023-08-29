(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_to_typed_array"
     (func $caml_ba_to_typed_array (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_from_typed_array"
      (func $caml_ba_from_typed_array (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_sub"
      (func $caml_ba_sub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_fill"
      (func $caml_ba_fill (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bindings" "ta_create"
      (func $ta_create (param i32) (param anyref) (result anyref)))
   (import "bindings" "ta_get_ui8"
      (func $ta_get_ui8 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_set_ui8"
      (func $ta_set_ui8 (param (ref extern)) (param i32) (param (ref i31))))
   (import "bindings" "ta_subarray"
      (func $ta_subarray
         (param (ref extern)) (param i32) (param i32) (result (ref extern))))
   (import "bindings" "ta_set"
      (func $ta_set (param (ref extern)) (param (ref extern)) (param i32)))
   (import "bindings" "ta_len"
      (func $ta_len (param (ref extern)) (result i32)))
   (import "bindings" "ta_bytes"
      (func $ta_bytes (param anyref) (result anyref)))
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))

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
   (type $int_array (array (mut i32)))
   (type $bigarray
      (sub final $custom
         (struct
            (field $ba_ops (ref $custom_operations))
            (field $ba_data (mut (ref extern))) ;; data
            (field $ba_dim (ref $int_array)) ;; size in each dimension
            (field $ba_num_dims i8) ;; number of dimensions
            (field $ba_kind i8) ;; kind
            (field $ba_layout i8)))) ;; layout

   (func (export "caml_hash_mix_bigstring")
      (param $h i32) (param $vb (ref eq)) (result i32)
      (local $b (ref $bigarray))
      (local $data (ref extern))
      (local $len i32) (local $i i32) (local $w i32)
      (local.set $b (ref.cast (ref $bigarray) (local.get $vb)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $b)))
      (local.set $len (call $ta_len (local.get $data)))
      (loop $loop
         (if (i32.le_u (i32.add (local.get $i) (i32.const 4)) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_int
                     (local.get $h)
                     (i32.or
                        (i32.or
                           (call $ta_get_ui8 (local.get $data) (local.get $i))
                           (i32.shl (call $ta_get_ui8 (local.get $data)
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (i32.const 8)))
                        (i32.or
                           (i32.shl (call $ta_get_ui8 (local.get $data)
                                       (i32.add (local.get $i) (i32.const 2)))
                                    (i32.const 16))
                           (i32.shl (call $ta_get_ui8 (local.get $data)
                                       (i32.add (local.get $i) (i32.const 3)))
                                    (i32.const 24))))))
               (local.set $i (i32.add (local.get $i) (i32.const 4)))
               (br $loop))))
      (local.set $w (i32.const 0))
      (block $0_bytes
         (block $1_byte
            (block $2_bytes
               (block $3_bytes
                  (br_table $0_bytes $1_byte $2_bytes $3_bytes
                     (i32.and (local.get $len) (i32.const 3))))
               (local.set $w
                  (i32.shl (call $ta_get_ui8 (local.get $data)
                              (i32.add (local.get $i) (i32.const 2)))
                           (i32.const 16))))
            (local.set $w
               (i32.or (local.get $w)
                  (i32.shl (call $ta_get_ui8 (local.get $data)
                              (i32.add (local.get $i) (i32.const 1)))
                           (i32.const 8)))))
         (local.set $w
            (i32.or (local.get $w)
               (call $ta_get_ui8 (local.get $data) (local.get $i))))
         (local.set $h (call $caml_hash_mix_int (local.get $h) (local.get $w))))
      (i32.xor (local.get $h) (local.get $len)))

   (func (export "bigstring_to_array_buffer")
      (param $bs (ref eq)) (result (ref eq))
      (return_call $caml_js_get
         (call $caml_ba_to_typed_array (local.get $bs))
         (call $wrap (string.const "buffer"))))

   (export "bigstring_to_typed_array" (func $caml_ba_to_typed_array))

   (func (export "bigstring_of_array_buffer") (param (ref eq)) (result (ref eq))
       (return_call $caml_ba_from_typed_array
          (call $wrap
             (call $ta_create (i32.const 12) (call $unwrap (local.get $0))))))

   (func (export "bigstring_of_typed_array") (param (ref eq)) (result (ref eq))
       (return_call $caml_ba_from_typed_array
          (call $wrap (call $ta_bytes (call $unwrap (local.get $0))))))

   (func (export "caml_bigstring_memset")
      (param $s (ref eq)) (param $pos (ref eq)) (param $len (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (return_call $caml_ba_fill
         (call $caml_ba_sub (local.get $s) (local.get $pos) (local.get $len))
         (local.get $v)))

   (func (export "caml_bigstring_memcmp")
      (param $s1 (ref eq)) (param $vpos1 (ref eq))
      (param $s2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $c1 i32) (local $c2 i32)
      (local $d1 (ref extern))
      (local $d2 (ref extern))
      (local.set $d1
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $s1))))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $d2
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $s2))))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (call $ta_get_ui8 (local.get $d1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $c2
                  (call $ta_get_ui8 (local.get $d2)
                     (i32.add (local.get $pos2) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.eq (local.get $c1) (local.get $c2)))
               (return
                  (select (i31.new (i32.const -1)) (i31.new (i32.const 1))
                     (i32.lt_u (local.get $c1) (local.get $c2)))))))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_memcmp_string")
      (param $s1 (ref eq)) (param $vpos1 (ref eq))
      (param $vs2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $c1 i32) (local $c2 i32)
      (local $d1 (ref extern))
      (local $s2 (ref $string))
      (local.set $d1
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $s1))))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $s2 (ref.cast (ref $string) (local.get $vs2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (call $ta_get_ui8 (local.get $d1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $c2
                  (array.get $string (local.get $s2)
                     (i32.add (local.get $pos2) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.eq (local.get $c1) (local.get $c2)))
               (return
                  (select (i31.new (i32.const -1)) (i31.new (i32.const 1))
                     (i32.lt_u (local.get $c1) (local.get $c2)))))))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_memchr")
      (param $s (ref eq)) (param $vc (ref eq))
      (param $vpos (ref eq)) (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32) (local $c i32)
      (local $d (ref extern))
      (local.set $c (i31.get_s (ref.cast (ref i31) (local.get $vc))))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (local.set $d
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $s))))
      (loop $loop
         (if (i32.gt_s (local.get $len) (i32.const 0))
            (then
               (if (i32.eq (local.get $c)
                      (call $ta_get_ui8 (local.get $d) (local.get $pos)))
                  (then
                     (return (i31.new (local.get $pos)))))
               (local.set $len (i32.sub (local.get $len) (i32.const 1)))
               (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
               (br $loop))))
     (i31.new (i32.const -1)))

   (export "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_bytes_to_ba))
   (func $caml_bigstring_blit_bytes_to_ba
      (export "caml_bigstring_blit_bytes_to_ba")
      (param $str1 (ref eq)) (param $vpos1 (ref eq))
      (param $ba2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $s1 (ref $string))
      (local $d2 (ref extern))
      (local.set $s1 (ref.cast (ref $string) (local.get $str1)))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $d2
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $ba2))))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (call $ta_set_ui8 (local.get $d2)
                  (i32.add (local.get $pos2) (local.get $i))
                  (i31.new
                     (array.get_u $string (local.get $s1)
                        (i32.add (local.get $pos1) (local.get $i)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_bytes")
      (param $ba1 (ref eq)) (param $vpos1 (ref eq))
      (param $str2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $d1 (ref extern))
      (local $s2 (ref $string))
      (local.set $d1
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $ba1))))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $s2 (ref.cast (ref $string) (local.get $str2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s2)
                  (i32.add (local.get $pos2) (local.get $i))
                  (call $ta_get_ui8 (local.get $d1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_ba")
      (param $ba1 (ref eq)) (param $vpos1 (ref eq))
      (param $ba2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $d1 (ref extern))
      (local $d2 (ref extern))
      (local.set $d1
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $ba1))))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $d2
         (struct.get $bigarray $ba_data
            (ref.cast (ref $bigarray) (local.get $ba2))))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (call $ta_set (local.get $d2)
         (call $ta_subarray (local.get $d1)
            (local.get $pos1) (i32.add (local.get $pos1) (local.get $len)))
         (local.get $pos2))
      (i31.new (i32.const 0)))
)
