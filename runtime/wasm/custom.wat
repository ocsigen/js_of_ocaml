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
   (import "int32" "int32_ops" (global $int32_ops (ref $custom_operations)))
   (import "int32" "nativeint_ops"
      (global $nativeint_ops (ref $custom_operations)))
   (import "int64" "int64_ops" (global $int64_ops (ref $custom_operations)))
   (import "bigarray" "bigarray_ops"
      (global $bigarray_ops (ref $custom_operations)))
   (import "string" "caml_bytes_equal"
      (func $caml_bytes_equal
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $bytes (array (mut i8)))
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
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (type $custom_with_id
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $id i64))))

   (func (export "caml_is_custom") (param (ref eq)) (result i32)
      (ref.test (ref $custom) (local.get 0)))

   (func (export "caml_dup_custom") (param $v (ref eq)) (result (ref eq))
      (call_ref $dup (local.get $v)
         (ref.as_non_null
            (struct.get $custom_operations $dup
               (struct.get $custom 0
                  (block $custom (result (ref $custom))
                     (drop (br_on_cast $custom (ref eq) (ref $custom)
                        (local.get $v)))
                     (unreachable)))))))

   (func (export "custom_compare_id")
      (param (ref eq)) (param (ref eq)) (param i32) (result i32)
      (local $i1 i64) (local $i2 i64)
      (local.set $i1
         (struct.get $custom_with_id $id
            (ref.cast (ref $custom_with_id) (local.get 0))))
      (local.set $i2
         (struct.get $custom_with_id $id
            (ref.cast (ref $custom_with_id) (local.get 1))))
      (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
               (i64.lt_s (local.get $i1) (local.get $i2))))

   (func (export "custom_hash_id") (param (ref eq)) (result i32)
      (i32.wrap_i64
         (struct.get $custom_with_id $id
           (ref.cast (ref $custom_with_id) (local.get 0)))))

   (global $next_id (mut i64) (i64.const 0))

   (func (export "custom_next_id") (result i64)
      (local $id i64)
      (local.set $id (global.get $next_id))
      (global.set $next_id (i64.add (local.get $id) (i64.const 1)))
      (local.get $id))

   (type $custom_operations_list
      (struct
         (field $ops (ref $custom_operations))
         (field $next (ref null $custom_operations_list))))

   (global $custom_operations
      (mut (ref null $custom_operations_list))
      (ref.null $custom_operations_list))

   (func $caml_register_custom_operations
      (export "caml_register_custom_operations")
      (param $ops (ref $custom_operations))
      (global.set $custom_operations
         (struct.new $custom_operations_list
            (local.get $ops) (global.get $custom_operations))))

   (func (export "caml_find_custom_operations")
      (param $id (ref $bytes)) (result (ref null $custom_operations))
      (local $l (ref null $custom_operations_list))
      (block $not_found
         (local.set $l (br_on_null $not_found (global.get $custom_operations)))
         (loop $loop
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_bytes_equal (local.get $id)
                         (struct.get $custom_operations $id
                            (struct.get $custom_operations_list $ops
                               (local.get $l))))))
               (then
                  (return
                     (struct.get $custom_operations_list $ops (local.get $l)))))
            (local.set $l
               (br_on_null $not_found
                  (struct.get $custom_operations_list $next (local.get $l))))
            (br $loop)))
      (ref.null $custom_operations))

   (global $initialized (mut i32) (i32.const 0))

   (func (export "caml_init_custom_operations")
      (if (global.get $initialized) (then (return)))
      (call $caml_register_custom_operations (global.get $int32_ops))
      (call $caml_register_custom_operations (global.get $nativeint_ops))
      (call $caml_register_custom_operations (global.get $int64_ops))
      (call $caml_register_custom_operations (global.get $bigarray_ops))
      (global.set $initialized (i32.const 1)))

  (func (export "caml_custom_identifier") (param $v (ref eq)) (result (ref eq))
     (struct.get $custom_operations $id
        (struct.get $custom 0 (ref.cast (ref $custom) (local.get $v)))))
)
