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
   (import "bindings" "equals"
      (func $equals (param anyref) (param anyref) (result i32)))
   (import "obj" "forward_tag" (global $forward_tag i32))
   (import "obj" "object_tag" (global $object_tag i32))
   (import "obj" "caml_obj_tag"
      (func $caml_obj_tag (param (ref eq)) (result (ref eq))))
   (import "obj" "caml_is_closure"
      (func $caml_is_closure (param (ref eq)) (result i32)))
   (import "obj" "null" (global $null_value (ref eq)))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "effect" "caml_is_continuation"
      (func $caml_is_continuation (param (ref eq)) (result i32)))
   (import "string" "caml_string_compare"
      (func $caml_string_compare
        (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jsstring" "jsstring_test"
      (func $jsstring_test (param anyref) (result i32)))
   (import "jsstring" "jsstring_compare"
      (func $jsstring_compare (param anyref) (param anyref) (result i32)))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $float (struct (field $f f64)))
   (type $float_array (array (mut f64)))
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

   ;; Compare stack: pairs of blocks whose fields remain to be compared.
   ;; The top of the stack is kept in local variables, so that comparing
   ;; flat blocks (tuples, records, list cells) does not need any memory.
   ;; The other entries are stored as triples (first block, second block,
   ;; index of the next field) in a single array. This array is allocated
   ;; by each comparison, the first time an entry has to be stored, rather
   ;; than being a global: stores into a young object are cheap (no
   ;; generational write barrier slow path), and a dead stack cannot cause
   ;; a memory leak, so it does not need to be cleared. This also makes
   ;; comparison reentrant (a custom comparison function can call compare).
   (global $empty_stack (ref $block) (array.new_fixed $block 0))

   (global $unordered (export "unordered") i32 (i32.const 0x80000000))

   (func $compare_bytes
      (param $s1 (ref $bytes)) (param $s2 (ref $bytes)) (result i32)
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $s1) (local.get $s2))
         (then (return (i32.const 0))))
      (local.set $l1 (array.len (local.get $s1)))
      (local.set $l2 (array.len (local.get $s2)))
      (local.set $len (select (local.get $l1) (local.get $l2)
                         (i32.le_u (local.get $l1) (local.get $l2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (array.get_u $bytes (local.get $s1) (local.get $i)))
               (local.set $c2
                  (array.get_u $bytes (local.get $s2) (local.get $i)))
               (if (i32.ne (local.get $c1) (local.get $c2))
                  (then
                     (if (i32.le_u (local.get $c1) (local.get $c2))
                        (then (return (i32.const -1)))
                        (else (return (i32.const 1))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.sub (local.get $l1) (local.get $l2)))

   (func $compare_val
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32)
      (result i32)
      (call $do_compare_val (local.get $v1) (local.get $v2) (local.get $total)))

   (@string $abstract_value "compare: abstract value")
   (@string $functional_value "compare: functional value")
   (@string $continuation_value "compare: continuation value")

   (func $do_compare_val
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32) (result i32)
      ;; the top of the stack is kept in local variables: the two blocks,
      ;; the index of the next field and the block size (0 if no top)
      (local $cur1 (ref $block)) (local $cur2 (ref $block))
      (local $cur_pos i32) (local $cur_len i32)
      ;; the rest of the stack, and its size
      (local $stack (ref $block)) (local $new_stack (ref $block))
      (local $sp i32)
      (local $i i32) (local $i1 (ref i31)) (local $i2 (ref i31))
      (local $b1 (ref $block)) (local $b2 (ref $block))
      (local $t1 i32) (local $t2 i32)
      (local $s1 i32) (local $s2 i32)
      (local $f1 f64) (local $f2 f64) (local $fl1 (ref $float))
      (local $jsv (ref $js))
      (local $fa1 (ref $float_array)) (local $fa2 (ref $float_array))
      (local $str1 (ref $bytes)) (local $str2 (ref $bytes))
      (local $c1 (ref $custom)) (local $c2 (ref $custom))
      (local $js1 anyref) (local $js2 anyref)
      (local $res i32)
      (local.set $cur1 (global.get $empty_stack))
      (local.set $cur2 (global.get $empty_stack))
      (local.set $cur_len (i32.const 0))
      (local.set $stack (global.get $empty_stack))
      (local.set $sp (i32.const 0))
      (loop $loop
         (block $next_item
            (if (local.get $total)
               (then
                  (br_if $next_item (ref.eq (local.get $v1) (local.get $v2)))))
            (if (ref.eq (local.get $v1) (global.get $null_value))
               (then
                  (if (ref.eq (local.get $v2) (global.get $null_value))
                     (then (return (i32.const 0)))
                     (else (return (i32.const -1))))))
            (if (ref.eq (local.get $v2) (global.get $null_value))
               (then (return (i32.const 1))))
            (drop (block $v1_is_not_int (result (ref eq))
               (local.set $i1
                  (br_on_cast_fail $v1_is_not_int (ref eq) (ref i31)
                    (local.get $v1)))
               (br_if $next_item (ref.eq (local.get $v1) (local.get $v2)))
               (drop (block $v2_is_not_int (result (ref eq))
                  (local.set $i2
                     (br_on_cast_fail $v2_is_not_int (ref eq) (ref i31)
                        (local.get $v2)))
                  ;; v1 and v2 are both integers
                  (return (i32.sub (i31.get_s (local.get $i1))
                                   (i31.get_s (local.get $i2))))))
               ;; check for forward tag
               (drop (block $v2_not_forward (result (ref eq))
                  (local.set $b2
                     (br_on_cast_fail $v2_not_forward (ref eq) (ref $block)
                        (local.get $v2)))
                  (local.set $t2
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $b2)
                              (i32.const 0)))))
                  (if (i32.eq (local.get $t2) (global.get $forward_tag))
                     (then
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  (ref.i31 (i32.const 1))))
               (block $v2_not_comparable
                  (drop (block $v2_not_custom (result (ref eq))
                     (local.set $c2
                         (br_on_cast_fail $v2_not_custom (ref eq) (ref $custom)
                            (local.get $v2)))
                     (local.set $res
                        (call_ref $compare
                           (local.get $v1) (local.get $v2) (local.get $total)
                           (br_on_null $v2_not_comparable
                              (struct.get $custom_operations $compare_ext
                                 (struct.get $custom 0 (local.get $c2))))))
                     (br_if $next_item (i32.eqz (local.get $res)))
                     (return (local.get $res)))))
               ;; v1 long < v2 block
               (return (i32.const -1))))
            (if (ref.test (ref i31) (local.get $v2))
               (then
                  ;; check for forward tag
                  (drop (block $v1_not_forward (result (ref eq))
                     (local.set $b1
                        (br_on_cast_fail $v1_not_forward (ref eq) (ref $block)
                           (local.get $v1)))
                     (local.set $t1
                        (i31.get_u (ref.cast (ref i31)
                                      (array.get $block (local.get $b1)
                                         (i32.const 0)))))
                     (if (i32.eq (local.get $t1) (global.get $forward_tag))
                        (then
                           (local.set $v1
                              (array.get $block (local.get $b1) (i32.const 1)))
                           (br $loop)))
                     (ref.i31 (i32.const 1))))
                  (block $v1_not_comparable
                     (drop (block $v1_not_custom (result (ref eq))
                        (local.set $c1
                            (br_on_cast_fail
                               $v1_not_custom (ref eq) (ref $custom)
                               (local.get $v1)))
                        (local.set $res
                           (call_ref $compare
                              (local.get $v1) (local.get $v2) (local.get $total)
                              (br_on_null $v1_not_comparable
                                 (struct.get $custom_operations $compare_ext
                                    (struct.get $custom 0 (local.get $c1))))))
                        (br_if $next_item (i32.eqz (local.get $res)))
                        (return (local.get $res)))))
                  ;; v1 block > v1 long
                  (return (i32.const 1))))
            (drop (block $heterogeneous (result (ref eq))
               (drop (block $v1_not_block (result (ref eq))
                  (local.set $b1
                     (br_on_cast_fail $v1_not_block (ref eq) (ref $block)
                        (local.get $v1)))
                  (local.set $t1
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $b1) (i32.const 0)))))
                  (local.set $b2
                     (br_on_cast_fail $heterogeneous (ref eq) (ref $block)
                        (local.get $v2)))
                  (local.set $t2
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $b2) (i32.const 0)))))
                  (drop (br_if $heterogeneous (ref.i31 (i32.const 0))
                     (i32.ne (local.get $t1) (local.get $t2))))
                  ;; forward tag
                  (if (i32.eq (local.get $t1) (global.get $forward_tag))
                     (then
                        (local.set $v1
                           (array.get $block (local.get $b1) (i32.const 1)))
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  (if (i32.eq (local.get $t1) (global.get $object_tag))
                     (then
                        (local.set $v1
                           (array.get $block (local.get $b1) (i32.const 2)))
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 2)))
                        (br_if $next_item
                           (ref.eq (local.get $v1) (local.get $v2)))
                        (return
                           (i32.sub
                              (i31.get_s (ref.cast (ref i31) (local.get $v1)))
                              (i31.get_s
                                 (ref.cast (ref i31) (local.get $v2)))))))
                  (local.set $s1 (array.len (local.get $b1)))
                  (local.set $s2 (array.len (local.get $b2)))
                  ;; compare size first
                  (if (i32.ne (local.get $s1) (local.get $s2))
                     (then
                        (return (i32.sub (local.get $s1) (local.get $s2)))))
                  (br_if $next_item (i32.eq (local.get $s1) (i32.const 1)))
                  (if (i32.gt_u (local.get $s1) (i32.const 2))
                     (then
                        ;; push the blocks, to compare the remaining fields
                        (if (local.get $cur_len)
                           (then
                              ;; spill the current top of the stack
                              (if (i32.eq (local.get $sp)
                                     (array.len (local.get $stack)))
                                 (then
                                    (if (local.get $sp)
                                       (then
                                          ;; the stack is full: double its
                                          ;; size
                                          (local.set $new_stack
                                             (array.new $block
                                                (ref.i31 (i32.const 0))
                                                (i32.shl (local.get $sp)
                                                   (i32.const 1))))
                                          (array.copy $block $block
                                             (local.get $new_stack)
                                             (i32.const 0)
                                             (local.get $stack) (i32.const 0)
                                             (local.get $sp))
                                          (local.set $stack
                                             (local.get $new_stack)))
                                       (else
                                          ;; first use: allocate the stack
                                          ;; (room for two entries)
                                          (local.set $stack
                                             (array.new $block
                                                (ref.i31 (i32.const 0))
                                                (i32.const 6)))))))
                              (array.set $block (local.get $stack)
                                 (local.get $sp) (local.get $cur1))
                              (array.set $block (local.get $stack)
                                 (i32.add (local.get $sp) (i32.const 1))
                                 (local.get $cur2))
                              (array.set $block (local.get $stack)
                                 (i32.add (local.get $sp) (i32.const 2))
                                 (ref.i31 (local.get $cur_pos)))
                              (local.set $sp
                                 (i32.add (local.get $sp) (i32.const 3)))))
                        (local.set $cur1 (local.get $b1))
                        (local.set $cur2 (local.get $b2))
                        (local.set $cur_pos (i32.const 2))
                        (local.set $cur_len (local.get $s1))))
                  (local.set $v1
                     (array.get $block (local.get $b1) (i32.const 1)))
                  (local.set $v2
                     (array.get $block (local.get $b2) (i32.const 1)))
                  (br $loop)))
               (drop (block $v1_not_float (result (ref eq))
                  (local.set $fl1
                     (br_on_cast_fail $v1_not_float (ref eq) (ref $float)
                        (local.get $v1)))
                  (local.set $f1 (struct.get $float 0 (local.get $fl1)))
                  (local.set $f2
                     (struct.get $float 0
                        (br_on_cast_fail $heterogeneous (ref eq) (ref $float)
                           (local.get $v2))))
                  (if (f64.lt (local.get $f1) (local.get $f2))
                     (then (return (i32.const -1))))
                  (if (f64.gt (local.get $f1) (local.get $f2))
                     (then (return (i32.const 1))))
                  (if (f64.ne (local.get $f1) (local.get $f2))
                     (then
                        (if (i32.eqz (local.get $total))
                           (then (return (global.get $unordered))))
                        (if (f64.eq (local.get $f1) (local.get $f1))
                           (then (return (i32.const 1))))
                        (if (f64.eq (local.get $f2) (local.get $f2))
                           (then (return (i32.const -1))))))
                  (br $next_item)))
               (drop (block $v1_not_bytes (result (ref eq))
                  (local.set $str1
                     (br_on_cast_fail $v1_not_bytes (ref eq) (ref $bytes)
                        (local.get $v1)))
                  (local.set $str2
                      (br_on_cast_fail $heterogeneous (ref eq) (ref $bytes)
                         (local.get $v2)))
                  (local.set $res
                     (call $compare_bytes (local.get $str1) (local.get $str2)))
                  (br_if $next_item (i32.eqz (local.get $res)))
                  (return (local.get $res))))
               (drop (block $v1_not_float_array (result (ref eq))
                  (local.set $fa1
                     (br_on_cast_fail $v1_not_float_array
                        (ref eq) (ref $float_array)
                        (local.get $v1)))
                  (local.set $fa2
                      (br_on_cast_fail $heterogeneous
                         (ref eq) (ref $float_array)
                         (local.get $v2)))
                  (local.set $s1 (array.len (local.get $fa1)))
                  (local.set $s2 (array.len (local.get $fa2)))
                  (if (i32.ne (local.get $s1) (local.get $s2))
                     (then
                        (return (i32.sub (local.get $s1) (local.get $s2)))))
                  (local.set $i (i32.const 0))
                  (loop $float_array
                     (if (i32.lt_s (local.get $i) (local.get $s1))
                        (then
                           (local.set $f1
                              (array.get $float_array (local.get $fa1)
                                 (local.get $i)))
                           (local.set $f2
                              (array.get $float_array (local.get $fa2)
                                 (local.get $i)))
                           (if (f64.lt (local.get $f1) (local.get $f2))
                              (then (return (i32.const -1))))
                           (if (f64.gt (local.get $f1) (local.get $f2))
                              (then (return (i32.const 1))))
                           (if (f64.ne (local.get $f1) (local.get $f2))
                              (then
                                 (if (i32.eqz (local.get $total))
                                    (then
                                       (return (global.get $unordered))))
                                 (if (f64.eq (local.get $f1)
                                             (local.get $f1))
                                    (then (return (i32.const 1))))
                                 (if (f64.eq (local.get $f2)
                                             (local.get $f2))
                                    (then (return (i32.const -1))))))
                           (local.set $i
                              (i32.add (local.get $i) (i32.const 1)))
                           (br $float_array))))
                  (br $next_item)))
               (drop (block $v1_not_custom (result (ref eq))
                  (local.set $c1
                     (br_on_cast_fail $v1_not_custom (ref eq) (ref $custom)
                        (local.get $v1)))
                  (local.set $c2
                      (br_on_cast_fail $heterogeneous (ref eq) (ref $custom)
                         (local.get $v2)))
                  (if (i32.eqz
                         (ref.eq (struct.get $custom 0 (local.get $c1))
                                 (struct.get $custom 0 (local.get $c2))))
                     (then
                        (return
                           (i31.get_s
                              (ref.cast (ref i31)
                                 (call $caml_string_compare
                                    (struct.get $custom_operations $id
                                       (struct.get $custom 0
                                          (local.get $c1)))
                                    (struct.get $custom_operations $id
                                       (struct.get $custom 0
                                          (local.get $c2)))))))))
                  (block $not_comparable
                     (local.set $res
                        (call_ref $compare
                           (local.get $v1) (local.get $v2) (local.get $total)
                           (br_on_null $not_comparable
                              (struct.get $custom_operations $compare
                                 (struct.get $custom 0 (local.get $c1))))))
                     (br_if $next_item (i32.eqz (local.get $res)))
                     (return (local.get $res)))
                  (call $caml_invalid_argument (global.get $abstract_value))
                  (ref.i31 (i32.const 0))))
(@if (not $wasi)
(@then
               (drop (block $v1_not_js (result (ref eq))
                  (local.set $jsv
                     (br_on_cast_fail $v1_not_js (ref eq) (ref $js)
                        (local.get $v1)))
                  (local.set $js1 (struct.get $js 0 (local.get $jsv)))
                  (local.set $js2
                     (struct.get $js 0
                        (br_on_cast_fail $heterogeneous (ref eq) (ref $js)
                           (local.get $v2))))
                  (block $not_jsstring
                     (br_if $not_jsstring
                        (i32.eqz (call $jsstring_test (local.get $js1))))
                     (br_if $not_jsstring
                         (i32.eqz (call $jsstring_test (local.get $js2))))
                     (local.set $res
                        (call $jsstring_compare
                           (local.get $js1) (local.get $js2)))
                     (br_if $next_item (i32.eqz (local.get $res)))
                     (return (local.get $res)))
                  ;; We cannot order two JavaScript objects,
                  ;; but we can tell whether they are equal or not
                  (if (i32.eqz (local.get $total))
                     (then
                        (br_if $next_item
                           (call $equals (local.get $js1) (local.get $js2)))
                        (return (global.get $unordered))))
                  (br $heterogeneous (ref.i31 (i32.const 0)))))
))
               (if (call $caml_is_closure (local.get $v1))
                  (then
                     (drop (br_if $heterogeneous (ref.i31 (i32.const 0))
                              (i32.eqz (call $caml_is_closure (local.get $v2)))))
                        (call $caml_invalid_argument
                        (global.get $functional_value))))
               (if (call $caml_is_continuation (local.get $v1))
                  (then
                     (drop (br_if $heterogeneous(ref.i31 (i32.const 0))
                              (i32.eqz
                                 (call $caml_is_continuation (local.get $v2)))))
                        (call $caml_invalid_argument
                        (global.get $continuation_value))))
               (ref.i31 (i32.const 0)))) ;; fall through
            ;; heterogeneous comparison
            (local.set $t1
               (i31.get_u
                  (ref.cast (ref i31) (call $caml_obj_tag (local.get $v1)))))
            (local.set $t2
               (i31.get_u
                  (ref.cast (ref i31) (call $caml_obj_tag (local.get $v2)))))
            (if (i32.eq (local.get $t1) (global.get $forward_tag))
               (then
                  (local.set $v1
                     (array.get $block (ref.cast (ref $block) (local.get $v1))
                        (i32.const 1)))
                  (br $loop)))
            (if (i32.eq (local.get $t2) (global.get $forward_tag))
               (then
                  (local.set $v2
                     (array.get $block (ref.cast (ref $block) (local.get $v2))
                        (i32.const 1)))
                  (br $loop)))
            (local.set $res (i32.sub (local.get $t1) (local.get $t2)))
            (if (i32.eqz (local.get $res))
               (then
                  (call $caml_invalid_argument (global.get $abstract_value))))
            (return (local.get $res)))
         ;; compare the next fields of the blocks at the top of the stack
         (if (i32.eqz (local.get $cur_len))
            (then
               (if (i32.eqz (local.get $sp))
                  (then (return (i32.const 0))))
               (local.set $sp (i32.sub (local.get $sp) (i32.const 3)))
               (local.set $cur1
                  (ref.cast (ref $block)
                     (array.get $block (local.get $stack) (local.get $sp))))
               (local.set $cur2
                  (ref.cast (ref $block)
                     (array.get $block (local.get $stack)
                        (i32.add (local.get $sp) (i32.const 1)))))
               (local.set $cur_pos
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block (local.get $stack)
                           (i32.add (local.get $sp) (i32.const 2))))))
               (local.set $cur_len (array.len (local.get $cur1)))))
         (local.set $v1
            (array.get $block (local.get $cur1) (local.get $cur_pos)))
         (local.set $v2
            (array.get $block (local.get $cur2) (local.get $cur_pos)))
         (local.set $cur_pos (i32.add (local.get $cur_pos) (i32.const 1)))
         (if (i32.eq (local.get $cur_pos) (local.get $cur_len))
            (then (local.set $cur_len (i32.const 0))))
         (br $loop))
     (unreachable))

   (func (export "caml_compare")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 1)))
      (if (i32.lt_s (local.get $res) (i32.const 0))
         (then (return (i32.const -1))))
      (if (i32.gt_s (local.get $res) (i32.const 0))
         (then (return (i32.const 1))))
      (i32.const 0))

   (func (export "caml_equal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (i32.eqz
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0))))

   (func (export "caml_notequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (i32.ne (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0))))

   (func (export "caml_lessthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))
      (i32.and (i32.lt_s (local.get $res) (i32.const 0))
               (i32.ne (local.get $res) (global.get $unordered))))

   (func (export "caml_lessequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))
      (i32.and (i32.le_s (local.get $res) (i32.const 0))
               (i32.ne (local.get $res) (global.get $unordered))))

   (func (export "caml_greaterthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (i32.lt_s (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0))))

   (func (export "caml_greaterequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (i32.le_s (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0))))
)
