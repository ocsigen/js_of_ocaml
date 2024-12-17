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
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "custom" "custom_compare_id"
      (func $custom_compare_id
        (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (import "custom" "custom_hash_id"
      (func $custom_hash_id (param (ref eq)) (result i32)))
   (import "custom" "custom_next_id" (func $custom_next_id (result i64)))

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

   (global $mutex_ops (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_mutex")
         (ref.func $custom_compare_id)
         (ref.null $compare)
         (ref.func $custom_hash_id)
         (ref.null $fixed_length)
         (ref.null $serialize)
         (ref.null $deserialize)
         (ref.null $dup)))

   (type $mutex
      (sub final $custom_with_id
         (struct
            (field (ref $custom_operations))
            (field i64)
            (field $state (mut i32)))))

   (func (export "caml_ml_mutex_new") (param (ref eq)) (result (ref eq))
      (struct.new $mutex
         (global.get $mutex_ops) (call $custom_next_id) (i32.const 0)))

   (@string $lock_failure "Mutex.lock: mutex already locked. Cannot wait.")

   (func (export "caml_ml_mutex_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast (ref $mutex) (local.get 0)))
      (if (struct.get $mutex $state (local.get $t))
         (then (call $caml_failwith (global.get $lock_failure))))
      (struct.set $mutex $state (local.get $t) (i32.const 1))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_mutex_try_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast (ref $mutex) (local.get 0)))
      (if (result (ref eq)) (struct.get $mutex $state (local.get $t))
         (then
            (ref.i31 (i32.const 0)))
         (else
            (struct.set $mutex $state (local.get $t) (i32.const 1))
            (ref.i31 (i32.const 1)))))

   (func (export "caml_ml_mutex_unlock") (param (ref eq)) (result (ref eq))
      (struct.set $mutex $state
         (ref.cast (ref $mutex) (local.get 0)) (i32.const 0))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_condition_new") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (@string $condition_failure "Condition.wait: cannot wait")

   (func (export "caml_ml_condition_wait")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $condition_failure))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_condition_signal") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_condition_broadcast")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
