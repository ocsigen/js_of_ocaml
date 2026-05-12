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
   (import "fail" "caml_invalid_argument" (func $caml_invalid_argument (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))

   (data $invalid_get_idx
      "caml_get_idx_bytecode: attempted to read from an invalid index")
   (data $invalid_set_idx
      "caml_set_idx_bytecode: attempted to write to an invalid index")

   ;; caml_get_idx_bytecode : base -> idx -> result
   ;; idx is a block (tag 0) of integer field positions.
   ;; Iterates through the positions, indexing into nested blocks.
   ;; Special case: if base is a float array (all-float record), box the result.
   ;; Raises if [idx] has a non-zero tag (an "invalid" index).
   (func (export "caml_get_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (result (ref eq))
      (local $idx_block (ref $block))
      (local $depth i32)
      (local $i i32)
      (local $pos i32)
      (local $res (ref eq))
      (local $fa (ref $float_array))
      (local.set $idx_block (ref.cast (ref $block) (local.get $idx)))
      (local.set $depth
         (i32.sub (array.len (local.get $idx_block)) (i32.const 1)))
      ;; Invalid (non-zero-tag) index: raise.
      (if (i32.ne
              (i31.get_s
                 (ref.cast (ref i31)
                    (array.get $block (local.get $idx_block) (i32.const 0))))
              (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $invalid_get_idx
                  (i32.const 0) (i32.const 62)))))
      ;; Float array case: base is $float_array, depth must be 1
      (drop (block $not_float_array (result (ref eq))
         (local.set $fa
            (br_on_cast_fail $not_float_array (ref eq) (ref $float_array)
               (local.get $base)))
         (local.set $pos
            (i31.get_s
               (ref.cast (ref i31)
                  (array.get $block (local.get $idx_block) (i32.const 1)))))
         (return
            (struct.new $float
               (array.get $float_array (local.get $fa) (local.get $pos))))))
      ;; General case: iterate through block fields
      (local.set $res (local.get $base))
      (local.set $i (i32.const 1))
      (loop $loop
         (if (i32.le_u (local.get $i) (local.get $depth))
            (then
               (local.set $pos
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $idx_block)
                           (local.get $i)))))
               (local.set $res
                  (array.get $block
                     (ref.cast (ref $block) (local.get $res))
                     (i32.add (local.get $pos) (i32.const 1))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $res))

   ;; caml_set_idx_bytecode : base -> idx -> value -> unit
   ;; idx is a block (tag 0) of integer field positions.
   ;; Traverses to the parent block, then sets the final field.
   ;; Special case: if base is a float array (all-float record), unbox the value.
   ;; Raises if [idx] has a non-zero tag (an "invalid" index).
   (func (export "caml_set_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $idx_block (ref $block))
      (local $depth i32)
      (local $i i32)
      (local $pos i32)
      (local $dst (ref eq))
      (local $fa (ref $float_array))
      (local.set $idx_block (ref.cast (ref $block) (local.get $idx)))
      (local.set $depth
         (i32.sub (array.len (local.get $idx_block)) (i32.const 1)))
      ;; Invalid (non-zero-tag) index: raise.
      (if (i32.ne
              (i31.get_s
                 (ref.cast (ref i31)
                    (array.get $block (local.get $idx_block) (i32.const 0))))
              (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $invalid_set_idx
                  (i32.const 0) (i32.const 61)))))
      ;; Float array case: base is $float_array, depth must be 1
      (drop (block $not_float_array (result (ref eq))
         (local.set $fa
            (br_on_cast_fail $not_float_array (ref eq) (ref $float_array)
               (local.get $base)))
         (local.set $pos
            (i31.get_s
               (ref.cast (ref i31)
                  (array.get $block (local.get $idx_block) (i32.const 1)))))
         (array.set $float_array (local.get $fa) (local.get $pos)
            (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))
         (return (ref.i31 (i32.const 0)))))
      ;; General case
      (local.set $dst (local.get $base))
      (local.set $i (i32.const 1))
      ;; Traverse to the parent (all but the last position)
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $depth))
            (then
               (local.set $pos
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $idx_block)
                           (local.get $i)))))
               (local.set $dst
                  (array.get $block
                     (ref.cast (ref $block) (local.get $dst))
                     (i32.add (local.get $pos) (i32.const 1))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      ;; Set the final field
      (local.set $pos
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $idx_block)
                  (local.get $depth)))))
      (array.set $block
         (ref.cast (ref $block) (local.get $dst))
         (i32.add (local.get $pos) (i32.const 1))
         (local.get $v))
      (ref.i31 (i32.const 0)))

   ;; caml_deepen_idx_bytecode : idx_prefix -> idx_suffix -> idx
   ;; Concatenates two block indices into a new one.
   (func (export "caml_deepen_idx_bytecode")
      (param $idx_prefix (ref eq)) (param $idx_suffix (ref eq))
      (result (ref eq))
      (local $prefix (ref $block))
      (local $suffix (ref $block))
      (local $prefix_depth i32)
      (local $suffix_depth i32)
      (local $result (ref $block))
      (local.set $prefix (ref.cast (ref $block) (local.get $idx_prefix)))
      (local.set $suffix (ref.cast (ref $block) (local.get $idx_suffix)))
      (local.set $prefix_depth
         (i32.sub (array.len (local.get $prefix)) (i32.const 1)))
      (local.set $suffix_depth
         (i32.sub (array.len (local.get $suffix)) (i32.const 1)))
      ;; Allocate result block: tag 0, size = prefix_depth + suffix_depth
      (local.set $result
         (array.new $block (ref.i31 (i32.const 0))
            (i32.add
               (i32.add (local.get $prefix_depth) (local.get $suffix_depth))
               (i32.const 1))))
      ;; Copy prefix fields
      (array.copy $block $block
         (local.get $result) (i32.const 1)
         (local.get $prefix) (i32.const 1)
         (local.get $prefix_depth))
      ;; Copy suffix fields
      (array.copy $block $block
         (local.get $result)
         (i32.add (local.get $prefix_depth) (i32.const 1))
         (local.get $suffix) (i32.const 1)
         (local.get $suffix_depth))
      (local.get $result))
)
