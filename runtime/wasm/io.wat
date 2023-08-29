(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "fail" "caml_raise_end_of_file" (func $caml_raise_end_of_file))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_list_of_js_array"
      (func $caml_list_of_js_array (param (ref eq)) (result (ref eq))))
   (import "bindings" "open"
      (func $open (param anyref) (param i32) (param i32) (result i32)))
   (import "bindings" "close" (func $close (param i32)))
   (import "bindings" "write"
      (func $write
         (param i32) (param (ref extern)) (param i32) (param i32) (param i64)
         (result i32)))
   (import "bindings" "write"
      (func $write'
         (param i32) (param (ref extern)) (param i32) (param i32)
         (param nullexternref) (result i32)))
   (import "bindings" "read"
      (func $read
         (param i32) (param (ref extern)) (param i32) (param i32) (param i64)
         (result i32)))
   (import "bindings" "read"
      (func $read'
         (param i32) (param (ref extern)) (param i32) (param i32)
         (param nullexternref) (result i32)))
   (import "bindings" "file_size" (func $file_size (param i32) (result i64)))
   (import "bindings" "register_channel"
      (func $register_channel (param (ref eq))))
   (import "bindings" "unregister_channel"
      (func $unregister_channel (param (ref eq))))
   (import "bindings" "channel_list" (func $channel_list (result anyref)))
   (import "bindings" "ta_new" (func $ta_new (param i32) (result (ref extern))))
   (import "bindings" "ta_copy"
      (func $ta_copy (param (ref extern)) (param i32) (param i32) (param i32)))
   (import "bindings" "ta_set_ui8"
      (func $ta_set_ui8 (param (ref extern)) (param i32) (param i32))) ;; ZZZ ??
   (import "bindings" "ta_get_ui8"
      (func $ta_get_ui8 (param (ref extern)) (param i32) (result i32)))
   (import "custom" "custom_compare_id"
      (func $custom_compare_id
        (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (import "custom" "custom_hash_id"
      (func $custom_hash_id (param (ref eq)) (result i32)))
   (import "custom" "custom_next_id" (func $custom_next_id (result i64)))
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "int64" "Int64_val"
      (func $Int64_val (param (ref eq)) (result i64)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $offset_array (array (mut i64)))

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
   (type $custom_with_id
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $id i64))))

   (global $channel_ops (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 5 ;; "_chan"
            (i32.const 95) (i32.const 99) (i32.const 104) (i32.const 97)
            (i32.const 110))
         (ref.func $custom_compare_id)
         (ref.null $compare)
         (ref.func $custom_hash_id)
         (ref.null $fixed_length)
         (ref.null $serialize)
         (ref.null $deserialize)))

   (type $channel
      (sub final $custom_with_id
         (struct
            (field (ref $custom_operations))
            (field i64)
            (field $fd (mut i32))
            (field $buffer (mut (ref extern)))
            (field $curr (mut i32))
            (field $max (mut i32))
            (field $size (mut i32))
            (field $unbuffered (mut i32)))))

   (global $fd_offsets (export "fd_offsets") (mut (ref $offset_array))
      (array.new $offset_array (i64.const 0) (i32.const 3)))
   (global $fd_seeked (mut (ref $string))
      (array.new $string (i32.const 0) (i32.const 3)))

   (func $initialize_fd_offset (param $fd i32) (param $offset i64)
      (local $len i32)
      (local $a (ref $offset_array))
      (local $b (ref $string))
      (local.set $len (array.len (global.get $fd_offsets)))
      (if (i32.ge_u (local.get $fd) (local.get $len))
         (then
            (loop $loop
               (local.set $len (i32.shl (local.get $len) (i32.const 1)))
               (br_if $loop (i32.ge_u (local.get $fd) (local.get $len))))
            (local.set $a
               (array.new $offset_array (i64.const 0) (local.get $len)))
            (array.copy $offset_array $offset_array
               (local.get $a) (i32.const 0)
               (global.get $fd_offsets) (i32.const 0)
               (array.len (global.get $fd_offsets)))
            (global.set $fd_offsets (local.get $a))
            (local.set $b
               (array.new $string (i32.const 0) (local.get $len)))
            (array.copy $string $string
               (local.get $b) (i32.const 0)
               (global.get $fd_seeked) (i32.const 0)
               (array.len (global.get $fd_seeked)))
            (global.set $fd_seeked (local.get $b))))
      (array.set $offset_array (global.get $fd_offsets) (local.get $fd)
        (local.get $offset))
      (array.set $string (global.get $fd_seeked) (local.get $fd) (i32.const 0)))

   (global $IO_BUFFER_SIZE i32 (i32.const 65536))

   (type $open_flags (array i8))
   ;;  1 O_RDONLY
   ;;  2 O_WRONLY
   ;;  4 O_APPEND
   ;;  8 O_CREAT
   ;; 16 O_TRUNC
   ;; 32 O_EXCL
   ;; 64 O_NONBLOCK
   (global $sys_open_flags (ref $open_flags)
      (array.new_fixed $open_flags 9
         (i32.const 1) (i32.const 2) (i32.const 6) (i32.const 8) (i32.const 16)
         (i32.const 32) (i32.const 0) (i32.const 0) (i32.const 64)))

   (func $convert_flag_list (param $vflags (ref eq)) (result i32)
      (local $flags i32)
      (local $cons (ref $block))
      (loop $loop
         (drop (block $done (result (ref eq))
            (local.set $cons
               (br_on_cast_fail $done (ref eq) (ref $block) (local.get $vflags)))
            (local.set $flags
               (i32.or (local.get $flags)
                  (array.get_u $open_flags (global.get $sys_open_flags)
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block
                              (local.get $cons) (i32.const 1)))))))
            (local.set $vflags
               (array.get $block (local.get $cons) (i32.const 2)))
            (br $loop))))
      (local.get $flags))

   (func (export "caml_sys_open")
      (param $path (ref eq)) (param $vflags (ref eq)) (param $perm (ref eq))
      (result (ref eq))
      (local $fd i32) (local $flags i32) (local $offset i64)
      (local.set $flags (call $convert_flag_list (local.get $vflags)))
      (local.set $fd
         (call $open
            (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
            (local.get $flags)
            (i31.get_u (ref.cast (ref i31) (local.get $perm)))))
      (if (i32.and (local.get $flags) (i32.const 4)) ;; O_APPEND
         (then (local.set $offset (call $file_size (local.get $fd)))))
      (call $initialize_fd_offset (local.get $fd) (local.get $offset))
      (i31.new (local.get $fd)))

   (func (export "caml_sys_close") (param (ref eq)) (result (ref eq))
      (call $close (i31.get_u (ref.cast (ref i31) (local.get 0))))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_set_channel_name")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_out_channels_list")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_list_of_js_array (call $wrap (call $channel_list))))

   (func (export "caml_ml_open_descriptor_in")
      (param $fd (ref eq)) (result (ref eq))
      (struct.new $channel
         (global.get $channel_ops)
         (call $custom_next_id)
         (i31.get_u (ref.cast (ref i31) (local.get $fd)))
         (call $ta_new (global.get $IO_BUFFER_SIZE))
         (i32.const 0)
         (i32.const 0)
         (global.get $IO_BUFFER_SIZE)
         (i32.const 0)))

   (global $caml_stderr (export "caml_stderr")
      (mut (ref eq)) (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_out")
      (param $fd (ref eq)) (result (ref eq))
      (local $res (ref eq))
      (local.set $res
         (struct.new $channel
            (global.get $channel_ops)
            (call $custom_next_id)
            (i31.get_u (ref.cast (ref i31) (local.get $fd)))
            (call $ta_new (global.get $IO_BUFFER_SIZE))
            (i32.const 0)
            (i32.const -1)
            (global.get $IO_BUFFER_SIZE)
            (i32.const 0)))
      (call $register_channel (local.get $res))
      (if (ref.eq (local.get $fd) (i31.new (i32.const 2)))
         (then
            (global.set $caml_stderr (local.get $res))))
      (local.get $res))

   (func (export "caml_ml_close_channel")
      (param (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local $fd i32)
      (local.set $ch (ref.cast (ref $channel) (local.get 0)))
      (struct.set $channel $curr (local.get $ch) (i32.const 0))
      (struct.set $channel $max (local.get $ch) (i32.const 0))
      (struct.set $channel $size (local.get $ch) (i32.const 0))
      (local.set $fd (struct.get $channel $fd (local.get $ch)))
      (if (i32.ne (local.get $fd) (i32.const -1))
         (then
            (struct.set $channel $fd (local.get $ch) (i32.const -1))
            (call $unregister_channel (local.get $ch))
            (call $close (local.get $fd))))
      (i31.new (i32.const 0)))

   (func $caml_do_read
      (param $ch (ref $channel)) (param $pos i32) (param $len i32) (result i32)
      (local $fd i32)
      (local $offset i64)
      (local $n i32)
      (local.set $fd (struct.get $channel $fd (local.get $ch)))
      (local.set $offset
         (array.get $offset_array (global.get $fd_offsets) (local.get $fd)))
      (local.set $n
         (if (result i32)
             (array.get_u $string (global.get $fd_seeked) (local.get $fd))
            (then
               (call $read
                  (local.get $fd)
                  (struct.get $channel $buffer (local.get $ch))
                  (local.get $pos)
                  (local.get $len)
                  (local.get $offset)))
            (else
               (call $read'
                  (local.get $fd)
                  (struct.get $channel $buffer (local.get $ch))
                  (local.get $pos)
                  (local.get $len)
                  (ref.null noextern)))))
      (array.set $offset_array
         (global.get $fd_offsets) (local.get $fd)
         (i64.add (local.get $offset) (i64.extend_i32_u (local.get $n))))
      (local.get $n))

   (func $copy_from_buffer
      (param $buf (ref extern)) (param $curr i32)
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (local $i i32)
      (loop $loop
        (if (i32.lt_u (local.get $i) (local.get $len))
           (then
              (array.set $string (local.get $s)
                 (i32.add (local.get $pos) (local.get $i))
                 (call $ta_get_ui8 (local.get $buf)
                    (i32.add (local.get $curr) (local.get $i))))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $loop)))))

   (func $caml_refill (param $ch (ref $channel)) (result i32)
      (local $n i32)
      (local $buf (ref extern))
      (local.set $buf (struct.get $channel $buffer (local.get $ch)))
      (local.set $n
         (call $caml_do_read (local.get $ch)
            (i32.const 0) (struct.get $channel $size (local.get $ch))))
      (if (i32.eqz (local.get $n))
         (then (call $caml_raise_end_of_file)))
      (struct.set $channel $max (local.get $ch) (local.get $n))
      (struct.set $channel $curr (local.get $ch) (i32.const 1))
      (return (call $ta_get_ui8 (local.get $buf) (i32.const 0))))

   (func $caml_getblock (export "caml_getblock")
      (param $vch (ref eq)) (param $s (ref $string))
      (param $pos i32) (param $len i32)
      (result i32)
      (local $ch (ref $channel))
      (local $avail i32)
      (local $nread i32)
      (if (i32.eqz (local.get $len))
         (then (return (i32.const 0))))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (local.set $avail
         (i32.sub (struct.get $channel $max (local.get $ch))
            (struct.get $channel $curr (local.get $ch))))
      (if (local.get $avail)
         (then
            (if (i32.gt_u (local.get $len) (local.get $avail))
               (then (local.set $len (local.get $avail))))
            (call $copy_from_buffer
               (struct.get $channel $buffer (local.get $ch))
               (struct.get $channel $curr (local.get $ch))
               (local.get $s) (local.get $pos)
               (local.get $len))
            (struct.set $channel $curr (local.get $ch)
               (i32.add (struct.get $channel $curr (local.get $ch))
                  (local.get $len)))
            (return (local.get $len))))
      (local.set $nread
         (call $caml_do_read (local.get $ch)
            (i32.const 0) (struct.get $channel $size (local.get $ch))))
      (struct.set $channel $max (local.get $ch) (local.get $nread))
      (if (i32.gt_u (local.get $len) (local.get $nread))
         (then (local.set $len (local.get $nread))))
      (call $copy_from_buffer
         (struct.get $channel $buffer (local.get $ch))
         (i32.const 0)
         (local.get $s) (local.get $pos)
         (local.get $len))
      (struct.set $channel $curr (local.get $ch) (local.get $len))
      (local.get $len))

   (func (export "caml_really_getblock")
      (param $ch (ref eq)) (param $s (ref $string))
      (param $pos i32) (param $len i32)
      (result i32)
      (local $read i32) (local $n i32)
      (local.set $n (local.get $len))
      (loop $loop
         (if (local.get $n)
            (then
               (local.set $read
                  (call $caml_getblock(local.get $ch)
                     (local.get $s) (local.get $pos) (local.get $n)))
               (if (i32.eqz (local.get $read))
                  (then (return (i32.sub (local.get $len) (local.get $n)))))
               (local.set $pos (i32.add (local.get $pos) (local.get $read)))
               (local.set $n (i32.sub (local.get $n) (local.get $read)))
               (br $loop))))
      (local.get $len))

   (func (export "caml_ml_input")
      (param $vch (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $ch (ref $channel)) (local $s (ref $string))
      (local $pos i32) (local $len i32) (local $curr i32)
      (local $i i32) (local $avail i32) (local $nread $i32)
      (local $buf (ref extern))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (local.set $s (ref.cast (ref $string) (local.get $vs)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $buf (struct.get $channel $buffer (local.get $ch)))
      (local.set $curr (struct.get $channel $curr (local.get $ch)))
      (local.set $avail
         (i32.sub (struct.get $channel $max (local.get $ch)) (local.get $curr)))
      (if (i32.gt_u (local.get $len) (local.get $avail))
         (then
            (if (i32.gt_u (local.get $avail) (i32.const 0))
               (then
                  (local.set $len (local.get $avail)))
               (else
                  (local.set $nread
                     (call $caml_do_read (local.get $ch)
                        (i32.const 0)
                        (struct.get $channel $size (local.get $ch))))
                  (struct.set $channel $max (local.get $ch) (local.get $nread))
                  (local.set $curr (i32.const 0))
                  (if (i32.gt_u (local.get $len) (local.get $nread))
                     (then (local.set $len (local.get $nread))))))))
      (call $copy_from_buffer
         (local.get $buf) (local.get $curr)
         (local.get $s) (local.get $pos) (local.get $len))
      (struct.set $channel $curr (local.get $ch)
         (i32.add (local.get $curr) (local.get $len)))
      (i31.new (local.get $len)))

   (func $caml_getch (param $ch (ref $channel)) (result i32)
      (local $curr i32)
      (local.set $curr (struct.get $channel $curr (local.get $ch)))
      (if (i32.ge_u (local.get $curr) (struct.get $channel $max (local.get $ch)))
         (then (return_call $caml_refill (local.get $ch))))
      (struct.set $channel $curr (local.get $ch)
         (i32.add (local.get $curr) (i32.const 1)))
      (return_call $ta_get_ui8
         (struct.get $channel $buffer (local.get $ch))
         (local.get $curr)))

   (func (export "caml_ml_input_char")
      (param $ch (ref eq)) (result (ref eq))
      (i31.new (call $caml_getch (ref.cast (ref $channel) (local.get $ch)))))

   (func (export "caml_ml_input_int")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel)) (local $res i32)
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (local.set $res
         (i32.shl (call $caml_getch (local.get $ch)) (i32.const 24)))
      (local.set $res
         (i32.or (local.get $res)
            (i32.shl (call $caml_getch (local.get $ch)) (i32.const 16))))
      (local.set $res
         (i32.or (local.get $res)
            (i32.shl (call $caml_getch (local.get $ch)) (i32.const 8))))
      (return
         (i31.new (i32.or (local.get $res) (call $caml_getch (local.get $ch))))))

   (func (export "caml_ml_pos_in")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (i31.new
         (i32.sub
            (i32.wrap_i64
               (array.get $offset_array
                  (global.get $fd_offsets)
                  (struct.get $channel $fd (local.get $ch))))
            (i32.sub
              (struct.get $channel $max (local.get $ch))
              (struct.get $channel $curr (local.get $ch))))))

   (func (export "caml_ml_pos_in_64")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (call $caml_copy_int64
         (i64.sub
            (array.get $offset_array
               (global.get $fd_offsets)
               (struct.get $channel $fd (local.get $ch)))
            (i64.extend_i32_s
               (i32.sub
                  (struct.get $channel $max (local.get $ch))
                  (struct.get $channel $curr (local.get $ch)))))))

   (func (export "caml_ml_pos_out")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (i31.new
         (i32.add
            (i32.wrap_i64
               (array.get $offset_array
                  (global.get $fd_offsets)
                  (struct.get $channel $fd (local.get $ch))))
            (struct.get $channel $curr (local.get $ch)))))

   (func (export "caml_ml_pos_out_64")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (call $caml_copy_int64
         (i64.add
            (array.get $offset_array
               (global.get $fd_offsets)
               (struct.get $channel $fd (local.get $ch)))
            (i64.extend_i32_s (struct.get $channel $curr (local.get $ch))))))

   (func $caml_seek_in
      (param $ch (ref $channel)) (param $dest i64) (result (ref eq))
      (local $fd i32) (local $offset i64)
      (local.set $fd (struct.get $channel $fd (local.get $ch)))
      (local.set $offset
         (array.get $offset_array (global.get $fd_offsets) (local.get $fd)))
      (if (i32.and
             (i64.ge_s
                (local.get $dest)
                (i64.sub
                   (local.get $offset)
                   (i64.extend_i32_s
                      (struct.get $channel $max (local.get $ch)))))
             (i64.le_s (local.get $dest) (local.get $offset)))
         (then
            (struct.set $channel $curr (local.get $ch)
               (i32.sub
                  (struct.get $channel $max (local.get $ch))
                  (i32.wrap_i64
                     (i64.sub (local.get $offset) (local.get $dest))))))
         (else
            ;; ZZZ Check for error
            (array.set $offset_array (global.get $fd_offsets) (local.get $fd)
               (local.get $dest))
            (array.set $string (global.get $fd_seeked) (local.get $fd)
               (i32.const 1))
            (struct.set $channel $curr (local.get $ch) (i32.const 0))
            (struct.set $channel $max (local.get $ch) (i32.const 0))))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in")
      (param $ch (ref eq)) (param $dest (ref eq)) (result (ref eq))
      (return_call $caml_seek_in (ref.cast (ref $channel) (local.get $ch))
         (i64.extend_i32_s
            (i31.get_s (ref.cast (ref i31) (local.get $dest))))))

   (func (export "caml_ml_seek_in_64")
      (param $ch (ref eq)) (param $dest (ref eq)) (result (ref eq))
      (return_call $caml_seek_in (ref.cast (ref $channel) (local.get $ch))
         (call $Int64_val (local.get $dest))))

   (func (export "caml_ml_seek_out")
      (param $vch (ref eq)) (param $voffset (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (call $caml_flush (local.get $ch))
      ;; ZZZ Check for error
      (array.set $offset_array
         (global.get $fd_offsets)
         (struct.get $channel $fd (local.get $ch))
         (i64.extend_i32_s
            (i31.get_s (ref.cast (ref i31) (local.get $voffset)))))
      (array.set $string (global.get $fd_seeked)
         (struct.get $channel $fd (local.get $ch)) (i32.const 1))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_out_64")
      (param $vch (ref eq)) (param $voffset (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (call $caml_flush (local.get $ch))
      ;; ZZZ Check for error
      (array.set $offset_array
         (global.get $fd_offsets)
         (struct.get $channel $fd (local.get $ch))
         (call $Int64_val (local.get $voffset)))
      (array.set $string (global.get $fd_seeked)
         (struct.get $channel $fd (local.get $ch)) (i32.const 1))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_scan_line")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel)) (local $p i32) (local $n i32)
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (local.set $p (struct.get $channel $curr (local.get $ch)))
      (loop $loop
         (if (i32.ge_u (local.get $p) (struct.get $channel $max (local.get $ch)))
            (then
               (if (struct.get $channel $curr (local.get $ch))
                  (then
                     (local.set $n (struct.get $channel $curr (local.get $ch)))
                     (call $ta_copy
                        (struct.get $channel $buffer (local.get $ch))
                        (i32.const 0) (local.get $n)
                        (i32.sub (struct.get $channel $max (local.get $ch))
                           (struct.get $channel $curr (local.get $ch))))
                     (struct.set $channel $curr (local.get $ch) (i32.const 0))
                     (struct.set $channel $max (local.get $ch)
                        (i32.sub (struct.get $channel $max (local.get $ch))
                           (local.get $n)))
                     (local.set $p (i32.sub (local.get $p) (local.get $n)))))
               (if (i32.ge_u (struct.get $channel $max (local.get $ch))
                      (struct.get $channel $size (local.get $ch)))
                  (then
                     (return
                        (i31.new
                           (i32.sub (struct.get $channel $curr (local.get $ch))
                              (struct.get $channel $size (local.get $ch)))))))
               (local.set $n
                  (call $caml_do_read
                     (local.get $ch)
                     (struct.get $channel $max (local.get $ch))
                     (i32.sub
                        (struct.get $channel $size (local.get $ch))
                        (struct.get $channel $max (local.get $ch)))))
               (if (i32.eqz (local.get $n))
                  (then
                     (return
                        (i31.new
                           (i32.sub (struct.get $channel $curr (local.get $ch))
                              (struct.get $channel $max (local.get $ch)))))))
               (struct.set $channel $max (local.get $ch)
                   (i32.add (struct.get $channel $max (local.get $ch))
                      (local.get $n)))))
         (if (i32.eq (i32.const 10) ;; '\n'
               (call $ta_get_ui8 (struct.get $channel $buffer (local.get $ch))
                  (local.get $p)))
            (then
               (return
                  (i31.new
                     (i32.add (i32.const 1)
                        (i32.sub (local.get $p)
                           (struct.get $channel $curr (local.get $ch))))))))
         (local.set $p (i32.add (local.get $p) (i32.const 1)))
         (br $loop)))

   (func $caml_flush (param $ch (ref $channel))
      (loop $loop
         (br_if $loop (i32.eqz (call $caml_flush_partial (local.get $ch))))))

   (func $caml_flush_if_unbuffered (export "caml_flush_if_unbuffered")
      (param $vch (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (if (struct.get $channel $unbuffered (local.get $ch))
         (then (call $caml_flush (local.get $ch)))))

   (func $caml_ml_flush (export "caml_ml_flush")
      (param $vch (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (if (i32.ne (struct.get $channel $fd (local.get $ch)) (i32.const -1))
         (then (call $caml_flush (local.get $ch))))
      (i31.new (i32.const 0)))

   (func $caml_flush_partial (param $ch (ref $channel)) (result i32)
      (local $towrite i32) (local $written i32) (local $fd i32)
      (local $offset i64) (local $buf (ref extern))
      (local.set $towrite (struct.get $channel $curr (local.get $ch)))
      (if (i32.gt_u (local.get $towrite) (i32.const 0))
         (then
            (local.set $buf (struct.get $channel $buffer (local.get $ch)))
            (local.set $fd (struct.get $channel $fd (local.get $ch)))
            (local.set $offset
               (array.get $offset_array
                  (global.get $fd_offsets) (local.get $fd)))
            (local.set $written
               (if (result i32)
                   (array.get_u $string (global.get $fd_seeked) (local.get $fd))
                  (then
                     (call $write
                        (local.get $fd)
                        (local.get $buf)
                        (i32.const 0)
                        (local.get $towrite)
                        (local.get $offset)))
                  (else
                     (call $write'
                        (local.get $fd)
                        (local.get $buf)
                        (i32.const 0)
                        (local.get $towrite)
                        (ref.null noextern)))))
            (array.set $offset_array
               (global.get $fd_offsets) (local.get $fd)
               (i64.add
                  (local.get $offset)
                  (i64.extend_i32_u (local.get $written))))
            (local.set $towrite
               (i32.sub (local.get $towrite) (local.get $written)))
            (if (i32.gt_u (local.get $towrite) (i32.const 0))
               (then
                  (call $ta_copy (local.get $buf)
                     (i32.const 0) (local.get $written) (local.get $towrite))))
            (struct.set $channel $curr (local.get $ch) (local.get $towrite))))
      (i32.eqz (local.get $towrite)))

   (func $caml_putblock
      (param $ch (ref $channel)) (param $s (ref $string)) (param $pos i32)
      (param $len i32) (result i32)
      (local $free i32) (local $curr i32) (local $i i32)
      (local $buf (ref extern))
      (local.set $curr (struct.get $channel $curr (local.get $ch)))
      (local.set $free
         (i32.sub (struct.get $channel $size (local.get $ch)) (local.get $curr)))
      (if (i32.ge_u (local.get $len) (local.get $free))
         (then (local.set $len (local.get $free))))
      (local.set $buf (struct.get $channel $buffer (local.get $ch)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (call $ta_set_ui8 (local.get $buf)
                  (i32.add (local.get $curr) (local.get $i))
                  (array.get_u $string (local.get $s)
                     (i32.add (local.get $pos) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.set $channel $curr (local.get $ch)
         (i32.add (local.get $curr) (local.get $len)))
      (if (i32.ge_u (local.get $len) (local.get $free))
         (then (drop (call $caml_flush_partial (local.get $ch)))))
      (local.get $len))

   (func (export "caml_really_putblock")
      (param $ch (ref eq)) (param $s (ref $string))
      (param $pos i32) (param $len i32)
      (local $written i32)
      (loop $loop
         (if (local.get $len)
            (then
               (local.set $written
                  (call $caml_putblock (ref.cast (ref $channel) (local.get $ch))
                     (local.get $s) (local.get $pos) (local.get $len)))
               (local.set $pos (i32.add (local.get $pos) (local.get $written)))
               (local.set $len (i32.sub (local.get $len) (local.get $written)))
               (br $loop)))))

   (export "caml_ml_output_bytes" (func $caml_ml_output))
   (func $caml_ml_output (export "caml_ml_output")
      (param $ch (ref eq)) (param $s (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32) (local $written i32)
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.gt_u (local.get $len) (i32.const 0))
            (then
               (local.set $written
                  (call $caml_putblock (ref.cast (ref $channel) (local.get $ch))
                     (ref.cast (ref $string) (local.get $s))
                     (local.get $pos) (local.get $len)))
               (local.set $pos (i32.add (local.get $pos) (local.get $written)))
               (local.set $len (i32.sub (local.get $len) (local.get $written)))
               (br $loop))))
      (call $caml_flush_if_unbuffered (local.get $ch))
      (i31.new (i32.const 0)))

   (func $caml_putch (param $ch (ref $channel)) (param $c $i32)
      (local $curr i32)
      (if (i32.ge_u (struct.get $channel $curr (local.get $ch))
             (struct.get $channel $size (local.get $ch)))
         (then
            (drop (call $caml_flush_partial (local.get $ch)))))
     (local.set $curr (struct.get $channel $curr (local.get $ch)))
     (call $ta_set_ui8 (struct.get $channel $buffer (local.get $ch))
        (local.get $curr) (local.get $c))
     (struct.set $channel $curr (local.get $ch)
        (i32.add (local.get $curr) (i32.const 1))))

   (func (export "caml_ml_output_char")
      (param $ch (ref eq)) (param $c (ref eq)) (result (ref eq))
      (call $caml_putch (ref.cast (ref $channel) (local.get $ch))
         (i31.get_u (ref.cast (ref i31) (local.get 1))))
      (call $caml_flush_if_unbuffered (local.get $ch))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_int")
      (param $vch (ref eq)) (param $vn (ref eq)) (result (ref eq))
      (local $ch (ref $channel)) (local $n i32)
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (local.set $n (i31.get_u (ref.cast (ref i31) (local.get 1))))
      (call $caml_putch (local.get $ch)
         (i32.shr_u (local.get $n) (i32.const 24)))
      (call $caml_putch (local.get $ch)
         (i32.shr_u (local.get $n) (i32.const 16)))
      (call $caml_putch (local.get $ch)
         (i32.shr_u (local.get $n) (i32.const 8)))
      (call $caml_putch (local.get $ch) (local.get $n))
      (call $caml_flush_if_unbuffered (local.get $ch))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_is_buffered") (param $ch (ref eq)) (result (ref eq))
      (i31.new
         (i32.eqz
            (struct.get $channel $unbuffered
               (ref.cast (ref $channel) (local.get $ch))))))

   (func (export "caml_ml_set_buffered")
      (param $vch (ref eq)) (param $mode (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      (if (i31.get_s (ref.cast (ref i31) (local.get $mode)))
         (then
            (struct.set $channel $unbuffered (local.get $ch) (i32.const 0)))
         (else
            (struct.set $channel $unbuffered (local.get $ch) (i32.const 1))
            (if (i32.ne (struct.get $channel $fd (local.get $ch)) (i32.const -1))
               (then (call $caml_flush (local.get $ch))))))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_set_channel_refill")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_set_channel_refill"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_channel_size") (param (ref eq)) (result (ref eq))
      ;; ZZZ check for overflow
      (i31.new
         (i32.wrap_i64
            (call $file_size (call $caml_ml_get_channel_fd (local.get 0))))))

   (func (export "caml_ml_channel_size_64") (param (ref eq)) (result (ref eq))
      (call $caml_copy_int64
         (call $file_size (call $caml_ml_get_channel_fd (local.get 0)))))

   (func $caml_ml_get_channel_fd (export "caml_ml_get_channel_fd")
      (param (ref eq)) (result i32)
      (struct.get $channel $fd (ref.cast (ref $channel) (local.get 0))))

   (func (export "caml_ml_set_channel_fd") (param (ref eq)) (param i32)
      (struct.set $channel $fd
         (ref.cast (ref $channel) (local.get 0)) (local.get 1)))

   (func (export "caml_ml_get_channel_offset") (param (ref eq)) (result i64)
      (array.get $offset_array (global.get $fd_offsets)
         (struct.get $channel $fd (ref.cast (ref $channel) (local.get 0)))))
)
