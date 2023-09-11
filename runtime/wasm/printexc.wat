(module
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "fail" "caml_is_special_exception"
      (func $caml_is_special_exception (param (ref eq)) (result i32)))
   (import "ints" "caml_format_int"
      (func $caml_format_int
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (type $buffer
      (struct
         (field (mut i32))
         (field (ref $string))))

   (func $add_char (param $buf (ref $buffer)) (param $c i32)
      (local $pos i32)
      (local $data (ref $string))
      (local.set $pos (struct.get $buffer 0 (local.get $buf)))
      (local.set $data (struct.get $buffer 1 (local.get $buf)))
      (if (i32.lt_u (local.get $pos) (array.len (local.get $data)))
         (then
            (array.set $string (local.get $data) (local.get $pos) (local.get $c))
            (struct.set $buffer 0 (local.get $buf)
               (i32.add (local.get $pos) (i32.const 1))))))

   (func $add_string (param $buf (ref $buffer)) (param $v (ref eq))
      (local $pos i32) (local $len i32)
      (local $data (ref $string))
      (local $s (ref $string))
      (local.set $pos (struct.get $buffer 0 (local.get $buf)))
      (local.set $data (struct.get $buffer 1 (local.get $buf)))
      (local.set $s (ref.cast (ref $string) (local.get $v)))
      (local.set $len (array.len (local.get $s)))
      (if (i32.gt_u (i32.add (local.get $pos) (local.get $len))
                    (array.len (local.get $data)))
         (then
            (local.set $len
                (i32.sub (array.len (local.get $data)) (local.get $pos)))))
      (array.copy $string $string
         (local.get $data) (local.get $pos)
         (local.get $s) (i32.const 0)
         (local.get $len))
      (struct.set $buffer 0 (local.get $buf)
         (i32.add (local.get $pos) (local.get $len))))

   (func (export "caml_format_exception") (param (ref eq)) (result anyref)
      (local $exn (ref $block))
      (local $buf (ref $buffer))
      (local $v (ref eq))
      (local $bucket (ref $block))
      (local $i i32) (local $len i32)
      (local.set $exn (ref.cast (ref $block) (local.get 0)))
      (if (result anyref)
          (ref.eq (array.get $block (local.get $exn) (i32.const 0))
                  (ref.i31 (i32.const 0)))
         (then
            (local.set $buf
               (struct.new $buffer
                  (i32.const 0)
                  (array.new $string (i32.const 0) (i32.const 256))))
            (call $add_string
               (local.get $buf)
               (array.get $block
                  (ref.cast (ref $block)
                     (array.get $block (local.get $exn) (i32.const 1)))
                  (i32.const 1)))
            (local.set $bucket
               (block $continue (result (ref $block))
                  (block $default
                     (br_if $default
                        (i32.ne (array.len (local.get $exn)) (i32.const 3)))
                     (br_if $default
                        (i32.eqz
                           (call $caml_is_special_exception
                              (array.get $block (local.get $exn) (i32.const 1)))))
                     (local.set $v
                        (array.get $block (local.get $exn) (i32.const 2)))
                     (br_if $default
                        (i32.eqz (ref.test (ref $block) (local.get $v))))
                     (local.set $bucket (ref.cast (ref $block) (local.get $v)))
                     (br_if $default
                        (i32.eqz
                           (ref.eq
                              (array.get $block (local.get $bucket) (i32.const 0))
                              (ref.i31 (i32.const 0)))))
                    (local.set $i (i32.const 1))
                    (br $continue (local.get $bucket)))
                 (local.set $i (i32.const 2))
                 (local.get $exn)))
           (local.set $len (array.len (local.get $bucket)))
           (if (i32.lt_u (local.get $i) (local.get $len))
              (then
                 (call $add_char (local.get $buf) (i32.const 40)) ;; '\('
                 (loop $loop
                    (local.set $v
                       (array.get $block (local.get $bucket) (local.get $i)))
                    (if (ref.test (ref i31) (local.get $v))
                       (then
                           (call $add_string (local.get $buf)
                              (call $caml_format_int
                                  (array.new_fixed $string 2
                                     (i32.const 37) (i32.const 100)) ;; %d
                                  (ref.cast (ref i31) (local.get $v)))))
                    (else (if (ref.test (ref $string) (local.get $v))
                       (then
                          (call $add_char (local.get $buf)
                             (i32.const 34)) ;; '\"'
                          (call $add_string (local.get $buf) (local.get $v))
                          (call $add_char (local.get $buf)
                             (i32.const 34))) ;; '\"'
                    (else
                       (call $add_char (local.get $buf)
                          (i32.const 95)))))) ;; '_'
                    (local.set $i (i32.add (local.get $i) (i32.const 1)))
                    (if (i32.lt_u (local.get $i) (local.get $len))
                       (then
                          (call $add_char (local.get $buf)
                             (i32.const 44)) ;; ','
                          (br $loop))))
                 (call $add_char (local.get $buf) (i32.const 41)))) ;; '\)'
           (string.new_lossy_utf8_array
              (struct.get $buffer 1 (local.get $buf)) (i32.const 0)
              (struct.get $buffer 0 (local.get $buf))))
         (else
            (call $unwrap
               (call $caml_jsstring_of_string
                  (array.get $block (local.get $exn) (i32.const 1)))))))
)
