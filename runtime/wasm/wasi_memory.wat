(module
(@if wasi
(@then
   (import "libc" "memory" (memory 2))
   (import "libc" "malloc" (func $malloc (param i32) (result i32)))
   (import "libc" "free" (func $free (param i32)))
   (import "io" "IO_BUFFER_SIZE" (global $IO_BUFFER_SIZE i32))
   (import "fail" "caml_raise_out_of_memory" (func $caml_raise_out_of_memory))

   (type $bytes (array (mut i8)))

   (func (export "checked_malloc") (param $size i32) (result i32)
      (local $p i32)
      (local.set $p (call $malloc (local.get $size)))
      (if (i32.eqz (local.get $p))
         (then (call $caml_raise_out_of_memory)))
      (local.get $p))

   (func (export "blit_substring_to_memory")
      (param $buf i32) (param $s (ref $bytes)) (param $ofs i32) (param $len i32)
      (local $i i32)
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (i32.store8 (i32.add (local.get $buf) (local.get $i))
                  (array.get $bytes (local.get $s)
                     (i32.add (local.get $ofs) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))

   (func $blit_string_to_memory (export "blit_string_to_memory")
      (param $buf i32) (param $s (ref $bytes))
      (local $i i32) (local $len i32)
      (local.set $len (array.len (local.get $s)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (i32.store8 (i32.add (local.get $buf) (local.get $i))
                  (array.get $bytes (local.get $s) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))

   (func (export "blit_memory_to_substring")
      (param $buf i32) (param $s (ref $bytes)) (param $ofs i32) (param $len i32)
      (local $i i32)
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $bytes (local.get $s)
                  (i32.add (local.get $ofs) (local.get $i))
                  (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))

   (func $blit_memory_to_string (export "blit_memory_to_string")
      (param $buf i32) (param $len i32) (result (ref $bytes))
      (local $s (ref $bytes))
      (local $i i32)
      (local.set $s (array.new $bytes (i32.const 0) (local.get $len)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $bytes (local.get $s) (local.get $i)
                  (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $s))

   (func (export "write_string_to_memory")
      (param $buf i32) (param $avail i32) (param $v (ref eq))
      (result i32)
      (local $s (ref $bytes)) (local $i i32) (local $len i32)
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $len (array.len (local.get $s)))
      (if (i32.lt_u (local.get $avail) (i32.add (local.get $len) (i32.const 1)))
         (then
            (local.set $buf
               (call $checked_malloc (i32.add (local.get $len) (i32.const 1))))))
      (call $blit_string_to_memory (local.get $buf) (local.get $s))
      (i32.store8 (i32.add (local.get $buf) (local.get $len)) (i32.const 0))
      (local.get $buf))

   (func (export "release_memory") (param $initial_buffer i32) (param $buf i32)
      (if (i32.ne (local.get $initial_buffer) (local.get $buf))
         (then
            (call $free (local.get $buf)))))

   (global $buffer (mut i32) (i32.const 0))

   (func $get_buffer (export "get_buffer") (result i32)
      (if (i32.eqz (global.get $buffer))
         (then
            (global.set $buffer
               (call $checked_malloc
                  (i32.add (global.get $IO_BUFFER_SIZE) (i32.const 12))))))
      (global.get $buffer))
))
)
