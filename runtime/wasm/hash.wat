;; Wasm_of_ocaml runtime support
;; http://www.ocsigen.org/js_of_ocaml/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, with linking exception;
;; either version 2.1 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(module
   (import "obj" "object_tag" (global $object_tag i32))
   (import "obj" "forward_tag" (global $forward_tag i32))
   (import "obj" "abstract_tag" (global $abstract_tag i32))
   (import "jsstring" "jsstring_test"
      (func $jsstring_test (param anyref) (result i32)))
   (import "jsstring" "jsstring_hash"
      (func $jsstring_hash (param i32) (param anyref) (result i32)))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $float (struct (field $f f64)))
   (type $js (struct (field $js anyref)))

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
   (type $custom (sub (struct (field $ops (ref $custom_operations)))))

   (func $caml_hash_mix_int (export "caml_hash_mix_int")
      (param $h i32) (param $d i32) (result i32)
      (i32.add
         (i32.mul
            (i32.rotl
               (i32.xor
                  (i32.mul
                     (i32.rotl
                        (i32.mul (local.get $d) (i32.const 0xcc9e2d51))
                        (i32.const 15))
                     (i32.const 0x1b873593))
                  (local.get $h))
               (i32.const 13))
            (i32.const 5))
         (i32.const 0xe6546b64)))

   (func $caml_hash_mix_final (export "caml_hash_mix_final")
      (param $h i32) (result i32)
      (local.set $h
         (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 16))))
      (local.set $h (i32.mul (local.get $h) (i32.const 0x85ebca6b)))
      (local.set $h
         (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 13))))
      (local.set $h (i32.mul (local.get $h) (i32.const 0xc2b2ae35)))
      (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 16))))

   (func $caml_hash_mix_int64 (export "caml_hash_mix_int64")
      (param $h i32) (param $d i64) (result i32)
      (return_call $caml_hash_mix_int
         (call $caml_hash_mix_int (local.get $h) (i32.wrap_i64 (local.get $d)))
         (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 32)))))

   (func $caml_hash_mix_double (export "caml_hash_mix_double")
      (param $h i32) (param $d f64) (result i32)
      (local $i i64)
      (local.set $i (i64.reinterpret_f64 (local.get $d)))
      (if (i64.eq (i64.and (local.get $i) (i64.const 0x7FF0000000000000))
                  (i64.const 0x7ff0000000000000))
         (then
            (if (i64.ne (i64.and (local.get $i) (i64.const 0xFFFFFFFFFFFFF))
                        (i64.const 0))
               (then (local.set $i (i64.const 0x7ff0000000000001))))))
      (if (i64.eq (local.get $i) (i64.const 0x8000000000000000))
         (then (local.set $i (i64.const 0))))
      (return_call $caml_hash_mix_int64 (local.get $h) (local.get $i)))

   (func $caml_hash_mix_float (export "caml_hash_mix_float")
      (param $h i32) (param $d f32) (result i32)
      (local $i i32)
      (local.set $i (i32.reinterpret_f32 (local.get $d)))
      (if (i32.eq (i32.and (local.get $i) (i32.const 0x7F800000))
                  (i32.const 0x7F800000))
         (then
            (if (i32.ne (i32.and (local.get $i) (i32.const 0x7FFFFF))
                        (i32.const 0))
               (then (local.set $i (i32.const 0x7F800001))))))
      (if (i32.eq (local.get $i) (i32.const 0x80000000))
         (then (local.set $i (i32.const 0))))
      (return_call $caml_hash_mix_int (local.get $h) (local.get $i)))

   (func (export "caml_hash_mix_float16")
      (param $h i32) (param $i i32) (result i32)
      (if (i32.eq (i32.and (local.get $i) (i32.const 0x7c00))
                  (i32.const 0x7c00))
         (then
            (if (i32.and (local.get $i) (i32.const 0x03ff))
               (then (local.set $i (i32.const 0x7C01))))))
      (if (i32.eq (local.get $i) (i32.const 0x8000))
         (then (local.set $i (i32.const 0))))
      (return_call $caml_hash_mix_int (local.get $h) (local.get $i)))

   (func $caml_hash_mix_string (export "caml_hash_mix_string")
      (param $h i32) (param $s (ref $bytes)) (result i32)
      (local $i i32) (local $len i32) (local $w i32)
      (local.set $len (array.len (local.get $s)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.le_u (i32.add (local.get $i) (i32.const 4)) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_int
                     (local.get $h)
                     (i32.or
                        (i32.or
                           (array.get_u $bytes (local.get $s) (local.get $i))
                           (i32.shl (array.get_u $bytes (local.get $s)
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (i32.const 8)))
                        (i32.or
                           (i32.shl (array.get_u $bytes (local.get $s)
                                       (i32.add (local.get $i) (i32.const 2)))
                                    (i32.const 16))
                           (i32.shl (array.get_u $bytes (local.get $s)
                                       (i32.add (local.get $i) (i32.const 3)))
                                    (i32.const 24))))))
               (local.set $i (i32.add (local.get $i) (i32.const 4)))
               (br $loop))))
      (local.set $w (i32.const 0))
      (block $zero_bytes
         (block $one_byte
            (block $two_bytes
               (block $three_bytes
                  (br_table $zero_bytes $one_byte $two_bytes $three_bytes
                     (i32.and (local.get $len) (i32.const 3))))
               (local.set $w
                  (i32.shl (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $i) (i32.const 2)))
                           (i32.const 16))))
            (local.set $w
               (i32.or (local.get $w)
                  (i32.shl (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $i) (i32.const 1)))
                           (i32.const 8)))))
         (local.set $w
            (i32.or (local.get $w)
               (array.get_u $bytes (local.get $s) (local.get $i))))
         (local.set $h (call $caml_hash_mix_int (local.get $h) (local.get $w))))
      (i32.xor (local.get $h) (local.get $len)))

   (global $HASH_QUEUE_SIZE i32 (i32.const 256))
   (global $MAX_FORWARD_DEREFERENCE i32 (i32.const 1000))

   ;; The values are visited in breadth-first order, like in the C
   ;; runtime: first the root, then the fields of each block, in the
   ;; order the blocks are visited. Rather than copying these fields into
   ;; a queue, we keep a queue of the blocks whose fields remain to be
   ;; visited, and we read the fields directly from the blocks. The block
   ;; whose fields are being visited is kept in a local variable, so that
   ;; the queue is only needed when a block is found while the fields of
   ;; another block are still pending. The queue is then allocated, and
   ;; grown as needed, by the current call rather than being a global:
   ;; stores into a young object are cheap (no generational write barrier
   ;; slow path), and a dead queue cannot cause a memory leak, so it does
   ;; not need to be cleared.
   (type $block_queue (array (mut (ref null $block))))

   (global $empty_queue (ref $block_queue) (array.new_fixed $block_queue 0))
   (global $empty_block (ref $block) (array.new_fixed $block 0))

   (type $float_array (array (mut f64)))

   (func (export "caml_hash") (export "caml_hash_exn")
      (param $count (ref eq)) (param $limit (ref eq)) (param $seed (ref eq))
      (param $obj (ref eq)) (result (ref eq))
      (local $sz i32) (local $num i32) (local $h i32)
      ;; number of values visited / scheduled for visit so far
      (local $rd i32) (local $wr i32)
      ;; read and write positions in the queue of blocks
      (local $queue (ref $block_queue)) (local $new_queue (ref $block_queue))
      (local $qrd i32) (local $qwr i32)
      ;; block whose fields are being visited, next field, field count
      (local $cur (ref $block)) (local $cur_pos i32) (local $cur_len i32)
      (local $v (ref eq))
      (local $bv (ref $block))
      (local $fa (ref $float_array))
      (local $iv (ref i31)) (local $sv (ref $bytes))
      (local $flv (ref $float)) (local $cv (ref $custom))
      (local $i i32)
      (local $len i32)
      (local $tg i32)
      (local $str anyref)
      (local.set $sz (i31.get_u (ref.cast (ref i31) (local.get $limit))))
      (if (i32.gt_u (local.get $sz) (global.get $HASH_QUEUE_SIZE))
         (then (local.set $sz (global.get $HASH_QUEUE_SIZE))))
      (local.set $num (i31.get_s (ref.cast (ref i31) (local.get $count))))
      (local.set $h (i31.get_s (ref.cast (ref i31) (local.get $seed))))
      (local.set $v (local.get $obj))
      (local.set $rd (i32.const 1))
      (local.set $wr (i32.const 1))
      ;; no current block ($cur_pos = $cur_len = 0)
      (local.set $cur (global.get $empty_block))
      (local.set $queue (global.get $empty_queue))
      (block $done
         (br_if $done (i32.le_s (local.get $num) (i32.const 0)))
         (loop $loop
            (block $next
               (drop (block $not_int (result (ref eq))
                  (local.set $iv
                     (br_on_cast_fail $not_int (ref eq) (ref i31)
                        (local.get $v)))
                  (local.set $h
                     (call $caml_hash_mix_int (local.get $h)
                        (i32.add
                           (i32.shl
                              (i31.get_s (local.get $iv))
                              (i32.const 1))
                           (i32.const 1))))
                  (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                  (br $next)))
               (drop (block $not_string (result (ref eq))
                  (local.set $sv
                     (br_on_cast_fail $not_string (ref eq) (ref $bytes)
                        (local.get $v)))
                  (local.set $h
                     (call $caml_hash_mix_string (local.get $h)
                        (local.get $sv)))
                  (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                  (br $next)))
               (drop (block $not_block (result (ref eq))
                  (local.set $bv
                     (br_on_cast_fail $not_block (ref eq) (ref $block)
                        (local.get $v)))
                  (local.set $tg
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $bv) (i32.const 0)))))
                  (if (i32.eq (local.get $tg) (global.get $forward_tag))
                     (then
                        (local.set $i (i32.const 0))
                        (loop $forward
                           (local.set $v
                              (array.get $block
                                 (local.get $bv) (i32.const 1)))
                           (drop (block $not_block' (result (ref eq))
                              (local.set $bv
                                 (br_on_cast_fail
                                    $not_block' (ref eq) (ref $block)
                                    (local.get $v)))
                              (br_if $loop
                                 (i32.eqz
                                    (ref.eq
                                       (array.get $block (local.get $bv)
                                          (i32.const 0))
                                       (ref.i31 (global.get $forward_tag)))))
                              (local.set $i
                                 (i32.add (local.get $i) (i32.const 1)))
                              ;; give up on this object
                              (br_if $next
                                 (i32.eq
                                    (local.get $i)
                                    (global.get $MAX_FORWARD_DEREFERENCE)))
                              (br $forward)))
                           ;; hash the value found
                           (br $loop))))
                  (if (i32.eq (local.get $tg) (global.get $object_tag))
                     (then
                        (local.set $h
                           (call $caml_hash_mix_int (local.get $h)
                              (i31.get_s
                                 (ref.cast (ref i31)
                                    (array.get $block
                                       (local.get $bv) (i32.const 2))))))
                        (local.set $num
                           (i32.sub (local.get $num) (i32.const 1)))
                        (br $next)))
                  ;; abstract tag: block contents unknown, do nothing
                  (br_if $next
                     (i32.eq (local.get $tg) (global.get $abstract_tag)))
                  (local.set $len (array.len (local.get $bv)))
                  (local.set $h
                     (call $caml_hash_mix_int (local.get $h)
                        (i32.or
                           (i32.shl (i32.sub (local.get $len) (i32.const 1))
                              (i32.const 10))
                           (local.get $tg))))
                  ;; schedule the fields for visit, not exceeding the
                  ;; total size $sz
                  (br_if $next
                     (i32.or
                        (i32.le_u (local.get $len) (i32.const 1))
                        (i32.ge_s (local.get $wr) (local.get $sz))))
                  (local.set $wr
                     (i32.add (local.get $wr)
                        (i32.sub (local.get $len) (i32.const 1))))
                  (if (i32.gt_s (local.get $wr) (local.get $sz))
                     (then (local.set $wr (local.get $sz))))
                  ;; Only the last scheduled block can have been
                  ;; truncated. The check $rd < $wr ensures that we do not
                  ;; visit its fields beyond the limit.
                  (if (i32.and
                         (i32.ge_u (local.get $cur_pos) (local.get $cur_len))
                         (i32.eq (local.get $qrd) (local.get $qwr)))
                     (then
                        ;; nothing else pending: visit the fields next
                        (local.set $cur (local.get $bv))
                        (local.set $cur_pos (i32.const 1))
                        (local.set $cur_len (local.get $len)))
                     (else
                        (if (i32.eq (local.get $qwr)
                               (array.len (local.get $queue)))
                           (then
                              (if (local.get $qwr)
                                 (then
                                    ;; the queue is full: double its size
                                    (local.set $new_queue
                                       (array.new_default $block_queue
                                          (i32.shl (local.get $qwr)
                                             (i32.const 1))))
                                    (array.copy $block_queue $block_queue
                                       (local.get $new_queue) (i32.const 0)
                                       (local.get $queue) (i32.const 0)
                                       (local.get $qwr))
                                    (local.set $queue (local.get $new_queue)))
                                 (else
                                    ;; first use: allocate the queue
                                    (local.set $queue
                                       (array.new_default $block_queue
                                          (i32.const 2)))))))
                        (array.set $block_queue (local.get $queue)
                           (local.get $qwr) (local.get $bv))
                        (local.set $qwr
                           (i32.add (local.get $qwr) (i32.const 1)))))
                  (br $next)))
               (drop (block $not_float (result (ref eq))
                  (local.set $flv
                     (br_on_cast_fail $not_float (ref eq) (ref $float)
                        (local.get $v)))
                  (local.set $h
                     (call $caml_hash_mix_double (local.get $h)
                        (struct.get $float 0 (local.get $flv))))
                  (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                  (br $next)))
               (drop (block $not_float_array (result (ref eq))
                  (local.set $fa
                     (br_on_cast_fail $not_float_array (ref eq)
                        (ref $float_array) (local.get $v)))
                  ;; mix the elements directly, no header
                  (local.set $len (array.len (local.get $fa)))
                  (local.set $i (i32.const 0))
                  (loop $fa_iter
                     (if (i32.lt_u (local.get $i) (local.get $len))
                        (then
                           (local.set $h
                              (call $caml_hash_mix_double (local.get $h)
                                 (array.get $float_array (local.get $fa)
                                    (local.get $i))))
                           (local.set $num
                              (i32.sub (local.get $num) (i32.const 1)))
                           (local.set $i
                              (i32.add (local.get $i) (i32.const 1)))
                           (br_if $fa_iter
                              (i32.gt_s (local.get $num) (i32.const 0))))))
                  (br $next)))
               (drop (block $not_custom (result (ref eq))
                  (local.set $cv
                     (br_on_cast_fail $not_custom (ref eq) (ref $custom)
                        (local.get $v)))
                  (local.set $h
                     (call $caml_hash_mix_int (local.get $h)
                        (call_ref $hash
                           (local.get $v)
                           (br_on_null $next
                              (struct.get $custom_operations $hash
                                 (struct.get $custom 0
                                    (local.get $cv)))))))
                  (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                  (br $next)))
(@if (not $wasi)
(@then
               (drop (block $not_jsstring (result anyref)
                  (local.set $str
                     (struct.get $js 0
                        (br_on_cast_fail $not_jsstring (ref eq) (ref $js)
                           (local.get $v))))
                  (drop (br_if $not_jsstring
                     (ref.i31 (i32.const 0))
                     (i32.eqz (call $jsstring_test (local.get $str)))))
                  (local.set $h
                     (call $jsstring_hash (local.get $h) (local.get $str)))
                  ;; count the string against the budget, like every other
                  ;; hashed leaf (and like the JS runtime)
                  (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                  (ref.i31 (i32.const 0))))
))
               ;; closures and continuations and other js values are ignored
            )
            ;; move to the next value
            (br_if $done
               (i32.or (i32.ge_u (local.get $rd) (local.get $wr))
                       (i32.le_s (local.get $num) (i32.const 0))))
            (if (i32.ge_u (local.get $cur_pos) (local.get $cur_len))
               (then
                  ;; the queue cannot be empty since $rd < $wr
                  (local.set $cur
                     (ref.as_non_null
                        (array.get $block_queue (local.get $queue)
                           (local.get $qrd))))
                  (local.set $qrd (i32.add (local.get $qrd) (i32.const 1)))
                  (local.set $cur_pos (i32.const 1))
                  (local.set $cur_len (array.len (local.get $cur)))))
            (local.set $v
               (array.get $block (local.get $cur) (local.get $cur_pos)))
            (local.set $cur_pos (i32.add (local.get $cur_pos) (i32.const 1)))
            (local.set $rd (i32.add (local.get $rd) (i32.const 1)))
            (br $loop)))
      (ref.i31 (i32.and (call $caml_hash_mix_final (local.get $h))
                        (i32.const 0x3FFFFFFF))))

   (func (export "caml_string_hash")
      (param $vh (ref eq)) (param $vs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.and
            (call $caml_hash_mix_final
               (call $caml_hash_mix_string
                  (i31.get_s (ref.cast (ref i31) (local.get $vh)))
                  (ref.cast (ref $bytes) (local.get $vs))))
            (i32.const 0x3FFFFFFF))))
)
