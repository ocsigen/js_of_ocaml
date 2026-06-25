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
   (import "fail" "caml_is_special_exception"
      (func $caml_is_special_exception (param (ref eq)) (result i32)))
   (import "ints" "caml_format_int"
      (func $caml_format_int
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (type $buffer
      (struct
         (field $pos (mut i32))
         (field $dat (mut (ref $bytes)))))

   ;; Ensure the buffer has room for [$extra] more bytes, growing (and
   ;; copying) its backing array if needed -- so the formatted message is not
   ;; truncated at a fixed size, matching the unbounded JS runtime.
   (func $ensure_capacity (param $buf (ref $buffer)) (param $extra i32)
      (local $pos i32) (local $cap i32) (local $need i32) (local $newcap i32)
      (local $dat (ref $bytes)) (local $new (ref $bytes))
      (local.set $pos (struct.get $buffer 0 (local.get $buf)))
      (local.set $dat (struct.get $buffer 1 (local.get $buf)))
      (local.set $cap (array.len (local.get $dat)))
      (local.set $need (i32.add (local.get $pos) (local.get $extra)))
      (if (i32.gt_u (local.get $need) (local.get $cap))
         (then
            (local.set $newcap (i32.shl (local.get $cap) (i32.const 1)))
            (if (i32.lt_u (local.get $newcap) (local.get $need))
               (then (local.set $newcap (local.get $need))))
            (local.set $new
               (array.new $bytes (i32.const 0) (local.get $newcap)))
            (array.copy $bytes $bytes
               (local.get $new) (i32.const 0)
               (local.get $dat) (i32.const 0)
               (local.get $pos))
            (struct.set $buffer 1 (local.get $buf) (local.get $new)))))

   (func $add_char (param $buf (ref $buffer)) (param $c i32)
      (local $pos i32)
      (call $ensure_capacity (local.get $buf) (i32.const 1))
      (local.set $pos (struct.get $buffer 0 (local.get $buf)))
      (array.set $bytes (struct.get $buffer 1 (local.get $buf))
         (local.get $pos) (local.get $c))
      (struct.set $buffer 0 (local.get $buf)
         (i32.add (local.get $pos) (i32.const 1))))

   (func $add_string (param $buf (ref $buffer)) (param $v (ref eq))
      (local $pos i32) (local $len i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $len (array.len (local.get $s)))
      (call $ensure_capacity (local.get $buf) (local.get $len))
      (local.set $pos (struct.get $buffer 0 (local.get $buf)))
      (array.copy $bytes $bytes
         (struct.get $buffer 1 (local.get $buf)) (local.get $pos)
         (local.get $s) (i32.const 0)
         (local.get $len))
      (struct.set $buffer 0 (local.get $buf)
         (i32.add (local.get $pos) (local.get $len))))

   (func (export "caml_format_exception") (param $vexn (ref eq)) (result (ref eq))
      (local $exn (ref $block))
      (local $buf (ref $buffer))
      (local $v (ref eq))
      (local $bucket (ref $block))
      (local $i i32) (local $len i32)
      (local $s (ref $bytes))
      (local.set $exn (ref.cast (ref $block) (local.get $vexn)))
      (if (result (ref eq))
          (ref.eq (array.get $block (local.get $exn) (i32.const 0))
                  (ref.i31 (i32.const 0)))
         (then
            (local.set $buf
               (struct.new $buffer
                  (i32.const 0)
                  (array.new $bytes (i32.const 0) (i32.const 256))))
            (call $add_string
               (local.get $buf)
               (array.get $block
                  (ref.cast (ref $block)
                     (array.get $block (local.get $exn) (i32.const 1)))
                  (i32.const 1)))
            (local.set $bucket
               (block $continue (result (ref $block))
                  (block $default
                     (br_if $default
                        (i32.ne (array.len (local.get $exn)) (i32.const 3)))
                     (br_if $default
                        (i32.eqz
                           (call $caml_is_special_exception
                              (array.get $block (local.get $exn) (i32.const 1)))))
                     (local.set $v
                        (array.get $block (local.get $exn) (i32.const 2)))
                     (br_if $default
                        (i32.eqz (ref.test (ref $block) (local.get $v))))
                     (local.set $bucket (ref.cast (ref $block) (local.get $v)))
                     (br_if $default
                        (i32.eqz
                           (ref.eq
                              (array.get $block (local.get $bucket) (i32.const 0))
                              (ref.i31 (i32.const 0)))))
                    (local.set $i (i32.const 1))
                    (br $continue (local.get $bucket)))
                 (local.set $i (i32.const 2))
                 (local.get $exn)))
           (local.set $len (array.len (local.get $bucket)))
           (if (i32.lt_u (local.get $i) (local.get $len))
              (then
                 (call $add_char (local.get $buf) (@char "("))
                 (loop $loop
                    (local.set $v
                       (array.get $block (local.get $bucket) (local.get $i)))
                    (if (ref.test (ref i31) (local.get $v))
                       (then
                           (call $add_string (local.get $buf)
                              (call $caml_format_int
                                  (@string "%d")
                                  (ref.cast (ref i31) (local.get $v)))))
                    (else (if (ref.test (ref $bytes) (local.get $v))
                       (then
                          (call $add_char (local.get $buf) (@char "\""))
                          (call $add_string (local.get $buf) (local.get $v))
                          (call $add_char (local.get $buf) (@char "\"")))
                    (else
                       (call $add_char (local.get $buf) (@char "_"))))))
                    (local.set $i (i32.add (local.get $i) (i32.const 1)))
                    (if (i32.lt_u (local.get $i) (local.get $len))
                       (then
                          (call $add_char (local.get $buf) (@char ","))
                          (call $add_char (local.get $buf) (@char " "))
                          (br $loop))))
                 (call $add_char (local.get $buf) (@char ")"))))
            (local.set $s
               (array.new $bytes (i32.const 0)
                  (struct.get $buffer 0 (local.get $buf))))
            (array.copy $bytes $bytes
               (local.get $s) (i32.const 0)
               (struct.get $buffer 1 (local.get $buf)) (i32.const 0)
               (struct.get $buffer 0 (local.get $buf)))
            (local.get $s))
         (else
            (array.get $block (local.get $exn) (i32.const 1)))))
)
