(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $string (array (mut i8)))
   (type $int_array (array (mut i32)))

   (type $context
      (struct
         (field (ref $int_array)) ;; w
         (field (mut i64))        ;; len
         (field (ref $int_array)) ;; buffer
         (field (ref $string))))  ;; intermediate buffer

   (func (export "caml_md5_string")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $ctx (ref $context))
      (local.set $ctx (call $MD5Init))
      (call $MD5Update (local.get $ctx) (ref.cast (ref $string) (local.get 0))
         (i31.get_u (ref.cast (ref i31) (local.get 1)))
         (i31.get_u (ref.cast (ref i31) (local.get 2))))
      (return_call $MD5Final (local.get $ctx)))

   (func (export "caml_md5_chan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_chan"))
      (array.new $string (i32.const 0) (i32.const 16)))

   (func $xx
      (param $q i32) (param $a i32) (param $b i32) (param $x i32) (param $s i32)
      (param $t i32) (result i32)
      (i32.add
         (i32.rotl
            (i32.add
               (i32.add (local.get $a) (local.get $q))
               (i32.add (local.get $x) (local.get $t)))
            (local.get $s))
         (local.get $b)))

   (func $ff
      (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $x i32)
      (param $s i32) (param $t i32) (result i32)
      (call $xx
         (i32.xor (local.get $d)
            (i32.and (local.get $b) (i32.xor (local.get $c) (local.get $d))))
         (local.get $a) (local.get $b)
         (local.get $x) (local.get $s) (local.get $t)))

   (func $gg
      (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $x i32)
      (param $s i32) (param $t i32) (result i32)
      (call $xx
         (i32.xor (local.get $c)
            (i32.and (local.get $d) (i32.xor (local.get $b) (local.get $c))))
         (local.get $a) (local.get $b)
         (local.get $x) (local.get $s) (local.get $t)))

   (func $hh
      (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $x i32)
      (param $s i32) (param $t i32) (result i32)
      (call $xx
         (i32.xor (local.get $b) (i32.xor (local.get $c) (local.get $d)))
         (local.get $a) (local.get $b)
         (local.get $x) (local.get $s) (local.get $t)))

   (func $ii
      (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $x i32)
      (param $s i32) (param $t i32) (result i32)
      (call $xx
         (i32.xor (local.get $c)
            (i32.or (local.get $b) (i32.xor (local.get $d) (i32.const -1))))
         (local.get $a) (local.get $b)
         (local.get $x) (local.get $s) (local.get $t)))

   (func $get_32 (param $s (ref $string)) (param $p i32) (result i32)
       (i32.or
          (i32.or
             (array.get_u $string (local.get $s) (local.get $p))
             (i32.shl (array.get_u $string (local.get $s)
                         (i32.add (local.get $p) (i32.const 1)))
                      (i32.const 8)))
          (i32.or
             (i32.shl (array.get_u $string (local.get $s)
                         (i32.add (local.get $p) (i32.const 2)))
                      (i32.const 16))
             (i32.shl (array.get_u $string (local.get $s)
                         (i32.add (local.get $p) (i32.const 3)))
                        (i32.const 24)))))

   (func $MD5Transform
      (param $w (ref $int_array)) (param $buffer (ref $int_array))
      (param $buffer' (ref $string)) (param $p i32)
      (local $i i32)
      (local $a i32) (local $b i32) (local $c i32) (local $d i32)
      (loop $loop
         (array.set $int_array (local.get $buffer) (local.get $i)
            (call $get_32 (local.get $buffer') (local.get $p)))
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (local.set $p (i32.add (local.get $p) (i32.const 4)))
         (br_if $loop (i32.lt_u (local.get $i) (i32.const 16))))
      (local.set $a (array.get $int_array (local.get $w) (i32.const 0)))
      (local.set $b (array.get $int_array (local.get $w) (i32.const 1)))
      (local.set $c (array.get $int_array (local.get $w) (i32.const 2)))
      (local.set $d (array.get $int_array (local.get $w) (i32.const 3)))
      (local.set $a
         (call $ff (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  0))
            (i32.const 7) (i32.const 0xD76AA478)))
      (local.set $d
         (call $ff (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  1))
            (i32.const 12) (i32.const 0xE8C7B756)))
      (local.set $c
         (call $ff (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  2))
            (i32.const 17) (i32.const 0x242070DB)))
      (local.set $b
         (call $ff (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  3))
            (i32.const 22) (i32.const 0xC1BDCEEE)))
      (local.set $a
         (call $ff (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  4))
            (i32.const 7) (i32.const 0xF57C0FAF)))
      (local.set $d
         (call $ff (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  5))
            (i32.const 12) (i32.const 0x4787C62A)))
      (local.set $c
         (call $ff (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  6))
            (i32.const 17) (i32.const 0xA8304613)))
      (local.set $b
         (call $ff (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  7))
            (i32.const 22) (i32.const 0xFD469501)))
      (local.set $a
         (call $ff (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  8))
            (i32.const 7) (i32.const 0x698098D8)))
      (local.set $d
         (call $ff (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  9))
            (i32.const 12) (i32.const 0x8B44F7AF)))
      (local.set $c
         (call $ff (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 10))
            (i32.const 17) (i32.const 0xFFFF5BB1)))
      (local.set $b
         (call $ff (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const 11))
            (i32.const 22) (i32.const 0x895CD7BE)))
      (local.set $a
         (call $ff (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const 12))
            (i32.const 7) (i32.const 0x6B901122)))
      (local.set $d
         (call $ff (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const 13))
            (i32.const 12) (i32.const 0xFD987193)))
      (local.set $c
         (call $ff (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 14))
            (i32.const 17) (i32.const 0xA679438E)))
      (local.set $b
         (call $ff (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const 15))
            (i32.const 22) (i32.const 0x49B40821)))
      (local.set $a
         (call $gg (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  1))
            (i32.const 5) (i32.const 0xF61E2562)))
      (local.set $d
         (call $gg (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  6))
            (i32.const 9) (i32.const 0xC040B340)))
      (local.set $c
         (call $gg (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 11))
            (i32.const 14) (i32.const 0x265E5A51)))
      (local.set $b
         (call $gg (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  0))
            (i32.const 20) (i32.const 0xE9B6C7AA)))
      (local.set $a
         (call $gg (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  5))
            (i32.const 5) (i32.const 0xD62F105D)))
      (local.set $d
         (call $gg (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const 10))
            (i32.const 9) (i32.const 0x02441453)))
      (local.set $c
         (call $gg (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 15))
            (i32.const 14) (i32.const 0xD8A1E681)))
      (local.set $b
         (call $gg (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  4))
            (i32.const 20) (i32.const 0xE7D3FBC8)))
      (local.set $a
         (call $gg (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  9))
            (i32.const 5) (i32.const 0x21E1CDE6)))
      (local.set $d
         (call $gg (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const 14))
            (i32.const 9) (i32.const 0xC33707D6)))
      (local.set $c
         (call $gg (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  3))
            (i32.const 14) (i32.const 0xF4D50D87)))
      (local.set $b
         (call $gg (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  8))
            (i32.const 20) (i32.const 0x455A14ED)))
      (local.set $a
         (call $gg (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const 13))
            (i32.const 5) (i32.const 0xA9E3E905)))
      (local.set $d
         (call $gg (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  2))
            (i32.const 9) (i32.const 0xFCEFA3F8)))
      (local.set $c
         (call $gg (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  7))
            (i32.const 14) (i32.const 0x676F02D9)))
      (local.set $b
         (call $gg (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const 12))
            (i32.const 20) (i32.const 0x8D2A4C8A)))
      (local.set $a
         (call $hh (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  5))
            (i32.const 4) (i32.const 0xFFFA3942)))
      (local.set $d
         (call $hh (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  8))
            (i32.const 11) (i32.const 0x8771F681)))
      (local.set $c
         (call $hh (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 11))
            (i32.const 16) (i32.const 0x6D9D6122)))
      (local.set $b
         (call $hh (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const 14))
            (i32.const 23) (i32.const 0xFDE5380C)))
      (local.set $a
         (call $hh (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  1))
            (i32.const 4) (i32.const 0xA4BEEA44)))
      (local.set $d
         (call $hh (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  4))
            (i32.const 11) (i32.const 0x4BDECFA9)))
      (local.set $c
         (call $hh (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  7))
            (i32.const 16) (i32.const 0xF6BB4B60)))
      (local.set $b
         (call $hh (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const 10))
            (i32.const 23) (i32.const 0xBEBFBC70)))
      (local.set $a
         (call $hh (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const 13))
            (i32.const 4) (i32.const 0x289B7EC6)))
      (local.set $d
         (call $hh (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  0))
            (i32.const 11) (i32.const 0xEAA127FA)))
      (local.set $c
         (call $hh (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  3))
            (i32.const 16) (i32.const 0xD4EF3085)))
      (local.set $b
         (call $hh (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  6))
            (i32.const 23) (i32.const 0x04881D05)))
      (local.set $a
         (call $hh (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  9))
            (i32.const 4) (i32.const 0xD9D4D039)))
      (local.set $d
         (call $hh (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const 12))
            (i32.const 11) (i32.const 0xE6DB99E5)))
      (local.set $c
         (call $hh (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 15))
            (i32.const 16) (i32.const 0x1FA27CF8)))
      (local.set $b
         (call $hh (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  2))
            (i32.const 23) (i32.const 0xC4AC5665)))
      (local.set $a
         (call $ii (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  0))
            (i32.const 6) (i32.const 0xF4292244)))
      (local.set $d
         (call $ii (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  7))
            (i32.const 10) (i32.const 0x432AFF97)))
      (local.set $c
         (call $ii (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 14))
            (i32.const 15) (i32.const 0xAB9423A7)))
      (local.set $b
         (call $ii (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  5))
            (i32.const 21) (i32.const 0xFC93A039)))
      (local.set $a
         (call $ii (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const 12))
            (i32.const 6) (i32.const 0x655B59C3)))
      (local.set $d
         (call $ii (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const  3))
            (i32.const 10) (i32.const 0x8F0CCC92)))
      (local.set $c
         (call $ii (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const 10))
            (i32.const 15) (i32.const 0xFFEFF47D)))
      (local.set $b
         (call $ii (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  1))
            (i32.const 21) (i32.const 0x85845DD1)))
      (local.set $a
         (call $ii (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  8))
            (i32.const 6) (i32.const 0x6FA87E4F)))
      (local.set $d
         (call $ii (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const 15))
            (i32.const 10) (i32.const 0xFE2CE6E0)))
      (local.set $c
         (call $ii (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  6))
            (i32.const 15) (i32.const 0xA3014314)))
      (local.set $b
         (call $ii (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const 13))
            (i32.const 21) (i32.const 0x4E0811A1)))
      (local.set $a
         (call $ii (local.get $a) (local.get $b) (local.get $c) (local.get $d)
            (array.get $int_array (local.get $buffer) (i32.const  4))
            (i32.const 6) (i32.const 0xF7537E82)))
      (local.set $d
         (call $ii (local.get $d) (local.get $a) (local.get $b) (local.get $c)
            (array.get $int_array (local.get $buffer) (i32.const 11))
            (i32.const 10) (i32.const 0xBD3AF235)))
      (local.set $c
         (call $ii (local.get $c) (local.get $d) (local.get $a) (local.get $b)
            (array.get $int_array (local.get $buffer) (i32.const  2))
            (i32.const 15) (i32.const 0x2AD7D2BB)))
      (local.set $b
         (call $ii (local.get $b) (local.get $c) (local.get $d) (local.get $a)
            (array.get $int_array (local.get $buffer) (i32.const  9))
            (i32.const 21) (i32.const 0xEB86D391)))
      (array.set $int_array (local.get $w) (i32.const 0)
         (i32.add (array.get $int_array (local.get $w) (i32.const 0))
                  (local.get $a)))
      (array.set $int_array (local.get $w) (i32.const 1)
         (i32.add (array.get $int_array (local.get $w) (i32.const 1))
                  (local.get $b)))
      (array.set $int_array (local.get $w) (i32.const 2)
         (i32.add (array.get $int_array (local.get $w) (i32.const 2))
                  (local.get $c)))
      (array.set $int_array (local.get $w) (i32.const 3)
         (i32.add (array.get $int_array (local.get $w) (i32.const 3))
                  (local.get $d))))

   (func $MD5Init (result (ref $context))
       (struct.new $context
          (array.new_fixed $int_array 4
             (i32.const 0x67452301) (i32.const 0xEFCDAB89)
             (i32.const 0x98BADCFE) (i32.const 0x10325476))
          (i64.const 0)
          (array.new $int_array (i32.const 0) (i32.const 16))
          (array.new $string (i32.const 0) (i32.const 64))))

   (func $MD5Update
      (param $ctx (ref $context)) (param $input (ref $string))
      (param $input_pos i32) (param $input_len i32)
      (local $in_buf i32) (local $len i64)
      (local $missing i32)
      (local.set $len (struct.get $context 1 (local.get $ctx)))
      (local.set $in_buf
         (i32.and (i32.wrap_i64 (local.get $len)) (i32.const 0x3f)))
      (struct.set $context 1 (local.get $ctx)
         (i64.add (local.get $len) (i64.extend_i32_u (local.get $input_len))))
      (if (local.get $in_buf)
         (then
            (local.set $missing (i32.sub (i32.const 64) (local.get $in_buf)))
            (if (i32.lt_u (local.get $input_len) (local.get $missing))
               (then
                  (array.copy $string $string
                     (struct.get $context 3 (local.get $ctx))
                     (local.get $missing)
                     (local.get $input) (local.get $input_pos)
                     (local.get $input_len))
                  (return)))
            (array.copy $string $string
               (struct.get $context 3 (local.get $ctx))
               (local.get $missing)
               (local.get $input) (local.get $input_pos) (local.get $missing))
            (call $MD5Transform (struct.get $context 0 (local.get $ctx))
               (struct.get $context 2 (local.get $ctx))
               (struct.get $context 3 (local.get $ctx))
               (i32.const 0))
            (local.set $input_pos
               (i32.add (local.get $input_pos) (local.get $missing)))
            (local.set $input_len
               (i32.sub (local.get $input_len) (local.get $missing)))))
      (loop $loop
         (if (i32.ge_u (local.get $input_len) (i32.const 64))
            (then
               (call $MD5Transform (struct.get $context 0 (local.get $ctx))
                  (struct.get $context 2 (local.get $ctx))
                  (local.get $input)
                  (local.get $input_pos))
                (local.set $input_pos
                   (i32.add (local.get $input_pos) (i32.const 64)))
                (local.set $input_len
                   (i32.sub (local.get $input_len) (i32.const 64)))
                (br $loop))))
      (if (local.get $input_len)
         (then
            (array.copy $string $string
               (struct.get $context 3 (local.get $ctx)) (i32.const 0)
               (local.get $input) (local.get $input_pos)
               (local.get $input_len)))))

   (func $MD5Final (param $ctx (ref $context)) (result (ref $string))
      (local $in_buf i32) (local $i i32) (local $len i64)
      (local $w (ref $int_array))
      (local $buffer (ref $string)) (local $res (ref $string))
      (local.set $len (struct.get $context 1 (local.get $ctx)))
      (local.set $in_buf
         (i32.and (i32.wrap_i64 (local.get $len)) (i32.const 0x3f)))
      (local.set $buffer (struct.get $context 3 (local.get $ctx)))
      (array.set $string (local.get $buffer) (local.get $in_buf)
         (i32.const 0x80))
      (local.set $in_buf (i32.add (local.get $in_buf) (i32.const 1)))
      (if (i32.gt_u (local.get $in_buf) (i32.const 56))
         (then
            (local.set $i (local.get $in_buf))
            (loop $loop
               (if (i32.lt_u (local.get $i) (i32.const 64))
                  (then
                     (array.set $string
                        (local.get $buffer) (local.get $i) (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (call $MD5Transform (struct.get $context 0 (local.get $ctx))
               (struct.get $context 2 (local.get $ctx))
               (local.get $buffer)
               (i32.const 0))
            (local.set $in_buf (i32.const 0))))
      (local.set $i (local.get $in_buf))
      (loop $loop
         (array.set $string (local.get $buffer) (local.get $i) (i32.const 0))
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (br_if $loop (i32.lt_u (local.get $i) (i32.const 56))))
      (local.set $len (i64.shl (local.get $len) (i64.const 3)))
      (array.set $string (local.get $buffer) (i32.const 56)
         (i32.wrap_i64 (local.get $len)))
      (array.set $string (local.get $buffer) (i32.const 57)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 8))))
      (array.set $string (local.get $buffer) (i32.const 58)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 16))))
      (array.set $string (local.get $buffer) (i32.const 59)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 24))))
      (array.set $string (local.get $buffer) (i32.const 60)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 32))))
      (array.set $string (local.get $buffer) (i32.const 61)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 40))))
      (array.set $string (local.get $buffer) (i32.const 62)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 48))))
      (array.set $string (local.get $buffer) (i32.const 63)
         (i32.wrap_i64 (i64.shr_u (local.get $len) (i64.const 56))))
      (call $MD5Transform (struct.get $context 0 (local.get $ctx))
         (struct.get $context 2 (local.get $ctx))
         (local.get $buffer)
         (i32.const 0))
      (local.set $res (array.new $string (i32.const 0) (i32.const 16)))
      (local.set $i (i32.const 0))
      (local.set $w (struct.get $context 0 (local.get $ctx)))
      (loop $loop
         (array.set $string (local.get $res) (local.get $i)
            (i32.shr_u
               (array.get $int_array (local.get $w)
                  (i32.shr_u (local.get $i) (i32.const 2)))
               (i32.shl (local.get $i) (i32.const 3))))
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (br_if $loop (i32.lt_u (local.get $i) (i32.const 16))))
      (local.get $res))
)
