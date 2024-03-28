(module
   (import "wasm:js-string" "compare"
      (func $compare_strings (param externref externref) (result i32)))
   (import "wasm:js-string" "test"
      (func $is_string (param externref) (result i32)))
   (import "wasm:js-string" "hash"
      (func $hash_string (param i32) (param anyref) (result i32)))

   (import "wasm:text-decoder" "decodeStringFromUTF8Array"
      (func $decodeStringFromUTF8Array
         (param (ref null $string)) (param i32) (param i32)
         (result (ref extern))))
   (import "wasm:text-encoder" "encodeStringToUTF8Array"
      (func $encodeStringToUTF8Array
         (param externref) (result (ref $string))))

   (import "bindings" "read_string"
      (func $read_string (param i32) (result anyref)))
   (import "bindings" "read_string_stream"
      (func $read_string_stream (param i32) (param i32) (result anyref)))
   (import "bindings" "write_string"
      (func $write_string (param anyref) (result i32)))
   (import "bindings" "append_string"
      (func $append_string (param anyref) (param anyref) (result anyref)))

   (type $string (array (mut i8)))

   (global $builtins_available (mut i32) (i32.const 0))

   (start $init)

   (func $init
      ;; Our dummy implementation of string conversion always returns
      ;; the empty string.
      (global.set $builtins_available
         (i32.ne
            (i32.const 0)
            (call $compare_strings
               (call $decodeStringFromUTF8Array
                  (array.new_fixed $string 1 (i32.const 0))
                  (i32.const 0) (i32.const 1))
               (call $decodeStringFromUTF8Array
                  (array.new_fixed $string 1 (i32.const 1))
                   (i32.const 0) (i32.const 1))))))

   (func (export "jsstring_compare")
      (param $s anyref) (param $s' anyref) (result i32)
      (return_call $compare_strings
         (extern.externalize (local.get $s))
         (extern.externalize (local.get $s'))))

   (func (export "jsstring_test") (param $s anyref) (result i32)
      (return_call $is_string (extern.externalize (local.get $s))))

   (export "jsstring_hash" (func $hash_string))

   ;; Used by package zarith_stubs_js
   (func $jsstring_of_substring (export "jsstring_of_substring")
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (result anyref)
      (if (global.get $builtins_available)
         (then
            (return
               (extern.internalize
                  (call $decodeStringFromUTF8Array (local.get $s)
                     (local.get $pos)
                     (i32.add (local.get $pos) (local.get $len)))))))
      (return_call $jsstring_of_substring_fallback
         (local.get $s) (local.get $pos) (local.get $len)))

   (func (export "jsstring_of_string") (param $s (ref $string)) (result anyref)
      (return_call $jsstring_of_substring
         (local.get $s) (i32.const 0) (array.len (local.get $s))))

   (func (export "string_of_jsstring") (param $s anyref) (result (ref $string))
      (if (global.get $builtins_available)
         (then
            (return_call $encodeStringToUTF8Array
               (extern.externalize (local.get $s)))))
      (return_call $string_of_jsstring_fallback (local.get $s)))

   ;; Fallback implementation of string conversion functions

   (memory (export "caml_buffer") 1)

   (global $buffer_size i32 (i32.const 65536))

   (func $write_to_buffer
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (local $i i32)
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (i32.store8 (local.get $i)
                  (array.get_u $string (local.get $s)
                     (i32.add (local.get $pos) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))

   (func $jsstring_of_substring_fallback
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (result anyref)
      (local $s' anyref)
      (local $continued i32)
      (if (i32.le_u (local.get $len) (global.get $buffer_size))
         (then
            (call $write_to_buffer
               (local.get $s) (local.get $pos) (local.get $len))
            (return_call $read_string (local.get $len))))
      (call $write_to_buffer
         (local.get $s) (local.get $pos) (global.get $buffer_size))
      (local.set $s'
         (call $read_string_stream (global.get $buffer_size) (i32.const 1)))
      (loop $loop
         (local.set $len (i32.sub (local.get $len) (global.get $buffer_size)))
         (local.set $pos (i32.add (local.get $pos) (global.get $buffer_size)))
         (local.set $continued
            (i32.gt_u (local.get $len) (global.get $buffer_size)))
         (call $write_to_buffer
            (local.get $s) (local.get $pos)
            (select (global.get $buffer_size) (local.get $len)
               (local.get $continued)))
         (local.set $s'
            (call $append_string (local.get $s')
               (call $read_string_stream
                  (select (global.get $buffer_size) (local.get $len)
                     (local.get $continued))
                  (local.get $continued))))
         (br_if $loop (local.get $continued)))
      (local.get $s'))

   (func $read_from_buffer
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (local $i i32)
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s)
                  (i32.add (local.get $pos) (local.get $i))
                  (i32.load8_u (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))

   (type $stack
      (struct (field $s (ref $string)) (field $next (ref null $stack))))
   (global $stack (mut (ref null $stack)) (ref.null $stack))

   (func $string_of_jsstring_fallback (param $s anyref) (result (ref $string))
      (local $ofs i32) (local $len i32)
      (local $s' (ref $string)) (local $s'' (ref $string))
      (local $item (ref $stack))
      (local.set $len (call $write_string (local.get $s)))
      (if (ref.is_null (global.get $stack))
         (then
            (local.set $s'
               (array.new $string (i32.const 0) (local.get $len)))
            (call $read_from_buffer
               (local.get $s') (i32.const 0) (local.get $len))
            (return (local.get $s'))))
      (block $done
         (local.set $item (br_on_null $done (global.get $stack)))
         (loop $loop
            (local.set $ofs
               (i32.add (local.get $ofs)
                  (array.len (struct.get $stack $s (local.get $item)))))
            (local.set $item
               (br_on_null $done (struct.get $stack $next (local.get $item))))
            (br $loop)))
      (local.set $s'
         (array.new $string (i32.const 0)
            (i32.add (local.get $len) (local.get $ofs))))
      (call $read_from_buffer
         (local.get $s') (local.get $ofs) (local.get $len))
      (block $done
         (local.set $item (br_on_null $done (global.get $stack)))
         (global.set $stack (ref.null $stack))
         (loop $loop
            (local.set $s'' (struct.get $stack $s (local.get $item)))
            (local.set $len (array.len (local.get $s'')))
            (local.set $ofs (i32.sub (local.get $ofs) (local.get $len)))
            (array.copy $string $string
               (local.get $s') (local.get $ofs)
               (local.get $s'') (i32.const 0)
               (local.get $len))
            (local.set $item
               (br_on_null $done (struct.get $stack $next (local.get $item))))
            (br $loop)))
      (local.get $s'))

   (func (export "caml_extract_string") (param $len i32)
      (local $s (ref $string))
      (local.set $s (array.new $string (i32.const 0) (local.get $len)))
      (call $read_from_buffer (local.get $s) (i32.const 0) (local.get $len))
      (global.set $stack (struct.new $stack (local.get $s) (global.get $stack))))
)
