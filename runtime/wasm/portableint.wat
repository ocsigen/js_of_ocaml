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
   (@if $portable-int
   (@then
      (type $ocaml_large_int (struct (field i64)))

      (func $is_ocaml_portable_int (export "is_ocaml_portable_int")
         (param $v (ref eq)) (result i32)
         (i32.or (ref.test (ref i31) (local.get $v))
                 (ref.test (ref $ocaml_large_int) (local.get $v))))

      (func $portable_int_val (export "portable_int_val")
         (param $v (ref eq)) (result i64)
         (if (result i64) (ref.test (ref i31) (local.get $v))
            (then
               (i64.extend_i32_s
                  (i31.get_s (ref.cast (ref i31) (local.get $v)))))
            (else
               (struct.get $ocaml_large_int 0
                  (ref.cast (ref $ocaml_large_int) (local.get $v))))))

      (func $val_portable_int (export "val_portable_int")
         (param $l i64) (result (ref eq))
         (local.set $l
            (i64.shr_s (i64.shl (local.get $l) (i64.const 1)) (i64.const 1)))
         (if (result (ref eq))
             (i64.lt_u (i64.add (local.get $l) (i64.const 0x40000000))
                (i64.const 0x80000000))
            (then (ref.i31 (i32.wrap_i64 (local.get $l))))
            (else (struct.new $ocaml_large_int (local.get $l)))))

      (func $portable_int_val_32 (export "portable_int_val_32")
         (param $v (ref eq)) (result i32)
         (if (ref.test (ref i31) (local.get $v))
            (then (return (i31.get_s (ref.cast (ref i31) (local.get $v))))))
         (i32.wrap_i64
            (struct.get $ocaml_large_int 0
               (ref.cast (ref $ocaml_large_int) (local.get $v)))))

      (func $checked_portable_int_val_32 (export "checked_portable_int_val_32")
         (param $v (ref eq)) (result i32)
         (local $l i64)
         (if (ref.test (ref i31) (local.get $v))
            (then (return (i31.get_s (ref.cast (ref i31) (local.get $v))))))
         (local.set $l
            (struct.get $ocaml_large_int 0
               (ref.cast (ref $ocaml_large_int) (local.get $v))))
         (if (result i32)
            (i64.eq (local.get $l)
               (i64.extend_i32_s (i32.wrap_i64 (local.get $l))))
            (then (i32.wrap_i64 (local.get $l)))
            (else (i32.const -1))))

      (func $val_int_32 (export "val_int_32")
         (param $i i32) (result (ref eq))
         (if (result (ref eq))
             (i32.eq (local.get $i)
                (i32.shr_s (i32.shl (local.get $i) (i32.const 1))
                   (i32.const 1)))
            (then (ref.i31 (local.get $i)))
            (else
               (struct.new $ocaml_large_int
                  (i64.extend_i32_s (local.get $i))))))
   )
   (@else
      (func (export "val_int_32") (param $i i32) (result (ref eq))
         (ref.i31 (local.get $i)))
   ))
)
