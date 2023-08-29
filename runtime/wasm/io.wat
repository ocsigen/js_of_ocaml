(module
   (import "bindings" "log" (func $log_js (param anyref)))
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
         (param i32) (param (ref extern)) (param i32) (param i32) (result i32)))
   (import "bindings" "read"
      (func $read
         (param i32) (param (ref extern)) (param i32) (param i32) (param i64)
         (result i32)))
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

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $offset_array (array (mut i64)))

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

   (global $channel_ops (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 5 ;; "_chan"
            (i32.const 95) (i32.const 99) (i32.const 104) (i32.const 97)
            (i32.const 110))
         (ref.func $custom_compare_id)
         (ref.null $value->value->int->int)
         (ref.func $custom_hash_id)))

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
            (field $flags (mut i32))))) ;; flags

   (global $fd_offsets (export "fd_offsets") (mut (ref $offset_array))
      (array.new $offset_array (i64.const 0) (i32.const 3)))

   (func $initialize_fd_offset (param $fd i32) (param $offset i64)
      (local $len i32)
      (local $a (ref $offset_array))
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
             (global.set $fd_offsets (local.get $a))))
      (array.set $offset_array (global.get $fd_offsets) (local.get $fd)
        (local.get $offset)))

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
      (param $path (ref eq)) (param $flags (ref eq)) (param $perm (ref eq))
      (result (ref eq))
      (local $fd i32)
      (local.set $fd
         (call $open
            (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
            (call $convert_flag_list (local.get $flags))
            (i31.get_u (ref.cast (ref i31) (local.get $perm)))))
      ;; ZZZ initial offset is file size when appending
      (call $initialize_fd_offset (local.get $fd) (i64.const 0))
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

   (func (export "caml_ml_input")
      (param $vch (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $ch (ref $channel)) (local $s (ref $string))
      (local $pos i32) (local $len i32) (local $curr i32)
      (local $i i32) (local $avail i32) (local $nread $i32)
      (local $fd i32)
      (local $buf (ref extern))
      (local $offset i64)
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
                  (local.set $fd (struct.get $channel $fd (local.get $ch)))
                  (local.set $offset
                     (array.get $offset_array (global.get $fd_offsets)
                        (local.get $fd)))
                  (local.set $nread
                     (call $read
                        (local.get $fd)
                        (local.get $buf)
                        (i32.const 0)
                        (struct.get $channel $size (local.get $ch))
                        (local.get $offset)))
                  (array.set $offset_array
                     (global.get $fd_offsets) (local.get $fd)
                     (i64.add (local.get $offset)
                        (i64.extend_i32_u (local.get $nread))))
                  (struct.set $channel $max (local.get $ch) (local.get $nread))
                  (local.set $curr (i32.const 0))
                  (if (i32.gt_u (local.get $len) (local.get $nread))
                     (then (local.set $len (local.get $nread))))))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s)
                  (i32.add (local.get $pos) (local.get $i))
                  (call $ta_get_ui8 (local.get $buf)
                     (i32.add (local.get $curr) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.set $channel $curr (local.get $ch)
         (i32.add (local.get $curr) (local.get $len)))
      (i31.new (local.get $len)))

   (func (export "caml_input_value") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_char")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_char"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_int")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_int"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in")
      (param $vch (ref eq)) (param $voffset (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local.set $ch (ref.cast (ref $channel) (local.get $vch)))
      ;; ZZZ Check for error
      (array.set $offset_array
         (global.get $fd_offsets)
         (struct.get $channel $fd (local.get $ch))
         (i64.extend_i32_s
            (i31.get_s (ref.cast (ref i31) (local.get $voffset)))))
      (struct.set $channel $curr (local.get $ch) (i32.const 0))
      (struct.set $channel $max (local.get $ch) (i32.const 0))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in_64")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in_64"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_out")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_scan_line")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_scan_line"))
      (i31.new (i32.const 0)))

   (func $caml_ml_flush (export "caml_ml_flush")
      (param $ch (ref eq)) (result (ref eq))
      (loop $loop
         (br_if $loop
            (i32.eqz
               (call $caml_flush_partial
                  (ref.cast (ref $channel) (local.get $ch))))))
      (i31.new (i32.const 0)))

   (func $caml_flush_partial (param $ch (ref $channel)) (result i32)
      (local $towrite i32) (local $written i32) (local $fd i32)
      (local $buf (ref extern))
      (local.set $towrite (struct.get $channel $curr (local.get $ch)))
      (if (i32.gt_u (local.get $towrite) (i32.const 0))
         (then
            (local.set $buf (struct.get $channel $buffer (local.get $ch)))
            (local.set $fd (struct.get $channel $fd (local.get $ch)))
            (local.set $written
               (call $write
                  (local.get $fd)
                  (local.get $buf)
                  (i32.const 0)
                  (local.get $towrite)))
            (array.set $offset_array
               (global.get $fd_offsets) (local.get $fd)
               (i64.add
                  (array.get $offset_array
                     (global.get $fd_offsets) (local.get $fd))
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
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_char")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;;(call $log_js (string.const "caml_ml_output_char"))
      (return_call $caml_ml_output (local.get 0)
         (array.new $string
            (i31.get_u (ref.cast (ref i31) (local.get 1))) (i32.const 1))
         (i31.new (i32.const 0)) (i31.new (i32.const 1))))

   (func (export "caml_output_value")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_int"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_is_buffered") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_is_buffered"))
      (i31.new (i32.const 1)))

   (func (export "caml_ml_set_buffered")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_set_buffered"))
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
