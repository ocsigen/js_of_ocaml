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
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "obj" "null" (global $null_value (ref eq)))
   (import "bigarray" "caml_ba_uint8_geti8"
      (func $caml_ba_uint8_geti8 (param (ref eq)) (param i32) (result i32)))
   (import "string" "caml_bytes_geti8"
      (func $caml_bytes_geti8 (param (ref eq)) (param i32) (result i32)))
   (import "bigarray" "caml_ba_uint8_set8"
      (func $caml_ba_uint8_set8
         (param (ref eq)) (param i32) (param i32) (result (ref eq))))
   (import "string" "caml_bytes_set8"
      (func $caml_bytes_set8
         (param (ref eq)) (param i32) (param i32) (result (ref eq))))

   (import "domain" "caml_atomic_load_field"
      (func $caml_atomic_load_field
         (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_set_field"
      (func $caml_atomic_set_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_exchange_field"
      (func $caml_atomic_exchange_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_compare_exchange_field"
      (func $caml_atomic_compare_exchange_field
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_cas_field"
      (func $caml_atomic_cas_field
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_fetch_add_field"
      (func $caml_atomic_fetch_add_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_add_field"
      (func $caml_atomic_add_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_sub_field"
      (func $caml_atomic_sub_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_land_field"
      (func $caml_atomic_land_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_lor_field"
      (func $caml_atomic_lor_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "domain" "caml_atomic_lxor_field"
      (func $caml_atomic_lxor_field
         (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))

   (data $invalid_get_idx
      "caml_get_idx_bytecode: attempted to read from an invalid index")
   (data $invalid_set_idx
      "caml_set_idx_bytecode: attempted to write to an invalid index")
   (data $unimplemented_ext_ptr
      "External ptrs are unimplemented on bytecode")
   (data $unimplemented_ext_ptr_primitives
      "External ptr primitives are unimplemented on bytecode")
   (data $invalid_atomic_idx
      "caml_check_atomic_idx: attempted to access an invalid index")

   ;; Indices with a non-zero tag are special --- the tag tells us how to
   ;; perform the indexing:
   ;;
   ;; - A tag-1 block whose only field is the number of times the pointer has
   ;;   been advanced represents a known-invalid index. Reads/writes from/to
   ;;   such an index raise.
   ;; - A tag-2 (resp. tag-3, tag-4) block [tag; offset] represents an index
   ;;   to position [offset] of a string (resp. bytes, bigarray). The tag
   ;;   tells us how to perform the indexing into the base value.

   ;; string_like_get : tag -> base -> idx -> result
   ;; Reads through a string-like (tag-2, tag-3, or tag-4) index
   ;; [tag; offset].
   (func $string_like_get
      (param $kind i32) (param $base (ref eq)) (param $idx_block (ref $block))
      (result (ref eq))
      (local $i i32)
      (local.set $i
         ;; Offset field of a runtime-built string-like index; always an
         ;; [i31].
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $idx_block) (i32.const 1)))))
      (if (i32.eq (local.get $kind) (i32.const 4))
         (then
            (return
               (ref.i31
                  (call $caml_ba_uint8_geti8
                     (local.get $base) (local.get $i))))))
      ;; Strings and bytes have the same representation, so
      ;; [caml_bytes_geti8] (also exported as [caml_string_geti8]) handles
      ;; both. Like [caml_ba_uint8_geti8], it bounds-checks.
      (ref.i31
         (call $caml_bytes_geti8 (local.get $base) (local.get $i))))

   ;; caml_get_idx_bytecode : base -> idx -> result
   ;; idx is a block (tag 0) of integer field positions.
   ;; Iterates through the positions, indexing into nested blocks.
   ;; Special case: if base is a float array (all-float record), box the result.
   ;; The tag of a string-like (tag-2, tag-3, tag-4) index tells us how to
   ;; index into [base].
   ;; Raises if [idx] has any other non-zero tag (an "invalid" index).
   (func $caml_get_idx_bytecode (export "caml_get_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (result (ref eq))
      (local $idx_block (ref $block))
      (local $kind i32)
      (local $depth i32)
      (local $i i32)
      (local $pos i32)
      (local $res (ref eq))
      (local $fa (ref $float_array))
      (local.set $idx_block (ref.cast (ref $block) (local.get $idx)))
      (local.set $depth
         (i32.sub (array.len (local.get $idx_block)) (i32.const 1)))
      (local.set $kind
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $idx_block) (i32.const 0)))))
      (if (i32.ne (local.get $kind) (i32.const 0))
         (then
            ;; String-like (tag-2, tag-3, tag-4) index: read from [base] as
            ;; directed by the tag.
            (if (i32.and (i32.ge_s (local.get $kind) (i32.const 2))
                   (i32.le_s (local.get $kind) (i32.const 4)))
               (then
                  (return_call $string_like_get
                     (local.get $kind) (local.get $base) (local.get $idx_block))))
            ;; Invalid index: raise.
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

   ;; string_like_set : tag -> base -> idx -> value -> unit
   ;; Writes through a string-like (tag-3 or tag-4) index [tag; offset].
   (func $string_like_set
      (param $kind i32) (param $base (ref eq)) (param $idx_block (ref $block))
      (param $v (ref eq))
      (result (ref eq))
      (local $i i32)
      (local $b i32)
      ;; The offset field of a runtime-built string-like index is always an
      ;; [i31], and [$v] is a [bits8] value, so it is an [i31] too.
      (local.set $i
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $idx_block) (i32.const 1)))))
      (local.set $b (i31.get_s (ref.cast (ref i31) (local.get $v))))
      (if (i32.eq (local.get $kind) (i32.const 4))
         (then
            (return_call $caml_ba_uint8_set8
               (local.get $base) (local.get $i) (local.get $b))))
      ;; Strings and bytes have the same representation. Like
      ;; [caml_ba_uint8_set8], [caml_bytes_set8] bounds-checks and keeps only
      ;; the low byte.
      (return_call $caml_bytes_set8
         (local.get $base) (local.get $i) (local.get $b)))

   ;; caml_set_idx_bytecode : base -> idx -> value -> unit
   ;; idx is a block (tag 0) of integer field positions.
   ;; Traverses to the parent block, then sets the final field.
   ;; Special case: if base is a float array (all-float record), unbox the value.
   ;; The tag of a string-like (tag-3, tag-4) index tells us how to index
   ;; into [base]. Writing through a tag-2 (string) index is invalid: strings
   ;; are immutable.
   ;; Raises if [idx] has any other non-zero tag (an "invalid" index).
   (func $caml_set_idx_bytecode (export "caml_set_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $idx_block (ref $block))
      (local $kind i32)
      (local $depth i32)
      (local $i i32)
      (local $pos i32)
      (local $dst (ref eq))
      (local $fa (ref $float_array))
      (local.set $idx_block (ref.cast (ref $block) (local.get $idx)))
      (local.set $depth
         (i32.sub (array.len (local.get $idx_block)) (i32.const 1)))
      (local.set $kind
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $idx_block) (i32.const 0)))))
      (if (i32.ne (local.get $kind) (i32.const 0))
         (then
            ;; Writable string-like (tag-3, tag-4) index: write to [base] as
            ;; directed by the tag.
            (if (i32.and (i32.ge_s (local.get $kind) (i32.const 3))
                   (i32.le_s (local.get $kind) (i32.const 4)))
               (then
                  (return_call $string_like_set
                     (local.get $kind) (local.get $base) (local.get $idx_block)
                     (local.get $v))))
            ;; Invalid index: raise.
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

   ;; We do not support nested atomic indices, so a valid index to an atomic
   ;; field is a tag-0 block with exactly one position.

   ;; check_atomic_idx : base -> idx -> unit
   ;; Raises [Failure] if [base] is [Null]: [idx] is then a raw address (an
   ;; external ptr), which cannot be dereferenced here.
   ;; Raises [Invalid_argument] if [idx] is not a tag-0 block with exactly one
   ;; position.
   (func $check_atomic_idx (param $base (ref eq)) (param $idx (ref eq))
      (local $idx_block (ref $block))
      (call $check_ptr_base (local.get $base))
      (local.set $idx_block (ref.cast (ref $block) (local.get $idx)))
      (if (i32.or
             (i32.ne
                ;; Tag of a runtime-built block index; always an [i31].
                (i31.get_s
                   (ref.cast (ref i31)
                      (array.get $block (local.get $idx_block) (i32.const 0))))
                (i32.const 0))
             (i32.ne (array.len (local.get $idx_block)) (i32.const 2)))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $invalid_atomic_idx
                  (i32.const 0) (i32.const 59))))))

   ;; atomic_idx_position : idx -> position
   ;; The single field position held by a checked atomic index.
   (func $atomic_idx_position (param $idx (ref eq)) (result (ref eq))
      (array.get $block (ref.cast (ref $block) (local.get $idx)) (i32.const 1)))

   (func (export "caml_atomic_load_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_load_field
         (local.get $base) (call $atomic_idx_position (local.get $idx))))

   (func (export "caml_atomic_set_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_set_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $v)))

   (func (export "caml_atomic_exchange_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_exchange_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $v)))

   (func (export "caml_atomic_compare_exchange_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_compare_exchange_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $o) (local.get $n)))

   (func (export "caml_atomic_cas_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_cas_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $o) (local.get $n)))

   (func (export "caml_atomic_fetch_add_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_fetch_add_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $i)))

   (func (export "caml_atomic_add_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_add_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $i)))

   (func (export "caml_atomic_sub_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_sub_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $i)))

   (func (export "caml_atomic_land_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_land_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $i)))

   (func (export "caml_atomic_lor_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_lor_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $i)))

   (func (export "caml_atomic_lxor_idx_bytecode")
      (param $base (ref eq)) (param $idx (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (call $check_atomic_idx (local.get $base) (local.get $idx))
      (return_call $caml_atomic_lxor_field
         (local.get $base) (call $atomic_idx_position (local.get $idx)) (local.get $i)))

   ;; In bytecode, a pointer is an unboxed pair of a base value and a block
   ;; index. Unboxed products are represented as blocks in bytecode, so a
   ;; pointer arrives as a single tag-0 block [0; base; idx], and
   ;; reading/writing through it is exactly reading/writing at the block
   ;; index.
   ;;
   ;; External pointers (and pointers with a [Null] base) are unsupported in
   ;; bytecode and fail with an 'unimplemented' message. The messages are
   ;; those of the OxCaml bytecode runtime.

   ;; check_ptr_base : base -> unit
   ;; Raises [Failure] if [base] is [Null].
   (func $check_ptr_base (param $base (ref eq))
      (if (ref.eq (local.get $base) (global.get $null_value))
         (then
            (call $caml_failwith
               (array.new_data $string $unimplemented_ext_ptr
                  (i32.const 0) (i32.const 43))))))

   ;; caml_get_ptr_bytecode : ptr -> result
   (func (export "caml_get_ptr_bytecode")
      (param $ptr (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $base (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ptr)))
      (local.set $base (array.get $block (local.get $b) (i32.const 1)))
      (call $check_ptr_base (local.get $base))
      (return_call $caml_get_idx_bytecode
         (local.get $base)
         (array.get $block (local.get $b) (i32.const 2))))

   ;; caml_set_ptr_bytecode : ptr -> value -> unit
   (func (export "caml_set_ptr_bytecode")
      (param $ptr (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $base (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ptr)))
      (local.set $base (array.get $block (local.get $b) (i32.const 1)))
      (call $check_ptr_base (local.get $base))
      (return_call $caml_set_idx_bytecode
         (local.get $base)
         (array.get $block (local.get $b) (i32.const 2))
         (local.get $v)))

   ;; caml_get_ext_ptr_bytecode : idx -> result
   (func (export "caml_get_ext_ptr_bytecode")
      (param $idx (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $unimplemented_ext_ptr_primitives
            (i32.const 0) (i32.const 53)))
      (ref.i31 (i32.const 0)))

   ;; caml_set_ext_ptr_bytecode : idx -> value -> unit
   (func (export "caml_set_ext_ptr_bytecode")
      (param $idx (ref eq)) (param $v (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $unimplemented_ext_ptr_primitives
            (i32.const 0) (i32.const 53)))
      (ref.i31 (i32.const 0)))

   ;; caml_deepen_idx_bytecode : idx_prefix -> idx_suffix -> idx
   ;; Concatenates two block indices into a new one.
   ;; Deepening a non-block (invalid or string-like) prefix is only meaningful
   ;; if the suffix is the identity (empty) index; otherwise, produces an
   ;; invalid index.
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
      (if (i32.ne
              (i31.get_s
                 (ref.cast (ref i31)
                    (array.get $block (local.get $prefix) (i32.const 0))))
              (i32.const 0))
         (then
            (if (i32.eqz (local.get $suffix_depth))
               (then (return (local.get $idx_prefix))))
            ;; Invalid index [1; 0]
            (return
               (array.new_fixed $block 2
                  (ref.i31 (i32.const 1))
                  (ref.i31 (i32.const 0))))))
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
