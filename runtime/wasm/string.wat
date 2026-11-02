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
   (@string_imports "str")
   (@string_type $string)
   (import "fail" "caml_bound_error" (func $caml_bound_error))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param $arg (ref eq))))
(@if $use_js_string
(@then
   (import "wasm:js-string" "equals"
      (func $string_equals (param externref externref) (result i32)))
   (import "wasm:js-string" "compare"
      (func $string_compare (param externref externref) (result i32)))
   (import "wasm:js-string" "length"
      (func $js_length (param externref) (result i32)))
   (import "wasm:js-string" "charCodeAt"
      (func $js_char_code_at (param externref i32) (result i32)))
   (import "wasm:js-string" "concat"
      (func $string_concat (param externref externref) (result (ref extern))))
   (import "jsstring" "jsbytes_of_bytes"
      (func $jsbytes_of_bytes (param (ref $bytes)) (result externref)))
))

   (type $bytes (array (mut i8)))
   ;; An OCaml string: with [use_js_string], a JavaScript string whose
   ;; characters are the bytes of the string; otherwise, a byte array (the
   ;; same type as [$bytes]).
(@if $use_js_string
(@then
   (type $string (struct (field externref))))
(@else
   (type $string (array (mut i8)))))

   ;; Accessors to OCaml strings, for the other modules of the runtime. They
   ;; are written once against these, whatever the representation of strings.
(@if $use_js_string
(@then
   ;; The length of a string
   (func (export "string_length") (param $s (ref $string)) (result i32)
      (call $js_length (struct.get $string 0 (local.get $s))))

   ;; The byte at position [$i] of a string (unchecked)
   (func (export "string_get") (param $s (ref $string)) (param $i i32)
      (result i32)
      (call $js_char_code_at (struct.get $string 0 (local.get $s)) (local.get $i)))

   ;; Copy [$len] bytes of a string at [$pos] to a byte array at [$dst_pos]
   (func $string_blit (export "string_blit")
      (param $s (ref $string)) (param $pos i32)
      (param $dst (ref $bytes)) (param $dst_pos i32) (param $len i32)
      (local $js externref) (local $i i32)
      (local.set $js (struct.get $string 0 (local.get $s)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $bytes (local.get $dst)
                  (i32.add (local.get $dst_pos) (local.get $i))
                  (call $js_char_code_at (local.get $js)
                     (i32.add (local.get $pos) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))

   ;; The contents of a string as a byte array, which must not be modified
   ;; (it is the string itself when strings are byte arrays)
   (func $string_to_bytes (export "string_to_bytes")
      (param $s (ref $string)) (result (ref $bytes))
      (local $b (ref $bytes)) (local $l i32)
      (local.set $l
         (call $js_length (struct.get $string 0 (local.get $s))))
      (local.set $b (array.new $bytes (i32.const 0) (local.get $l)))
      (call $string_blit
         (local.get $s) (i32.const 0) (local.get $b) (i32.const 0)
         (local.get $l))
      (local.get $b))

   ;; The bytes [$pos, $pos + $len) of a string, as a byte array, which must
   ;; not be modified, and the position of the first of them in it (the
   ;; string itself and [$pos] when strings are byte arrays). Only these bytes
   ;; are copied otherwise.
   (func (export "string_slice")
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (result (ref $bytes) i32)
      (local $b (ref $bytes))
      (local.set $b (array.new $bytes (i32.const 0) (local.get $len)))
      (call $string_blit
         (local.get $s) (local.get $pos) (local.get $b) (i32.const 0)
         (local.get $len))
      (local.get $b) (i32.const 0))

   ;; A string with the contents of a byte array, which must not be modified
   ;; afterwards (it is the string itself when strings are byte arrays)
   (func $string_of_bytes (export "string_of_bytes")
      (param $b (ref $bytes)) (result (ref $string))
      (struct.new $string (call $jsbytes_of_bytes (local.get $b))))
)
(@else
   (func (export "string_length") (param $s (ref $string)) (result i32)
      (array.len (local.get $s)))

   (func (export "string_get") (param $s (ref $string)) (param $i i32)
      (result i32)
      (array.get_u $string (local.get $s) (local.get $i)))

   (func (export "string_blit")
      (param $s (ref $string)) (param $pos i32)
      (param $dst (ref $bytes)) (param $dst_pos i32) (param $len i32)
      (array.copy $bytes $string
         (local.get $dst) (local.get $dst_pos)
         (local.get $s) (local.get $pos) (local.get $len)))

   (func (export "string_to_bytes")
      (param $s (ref $string)) (result (ref $bytes))
      (local.get $s))

   (func (export "string_slice")
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (result (ref $bytes) i32)
      (local.get $s) (local.get $pos))

   (func (export "string_of_bytes")
      (param $b (ref $bytes)) (result (ref $string))
      (local.get $b))
))

(@if $use_js_string
(@then
   ;; The JavaScript string of an OCaml string
   (func $jsstring (param $s (ref eq)) (result externref)
      (struct.get $string 0 (ref.cast (ref $string) (local.get $s))))

   (func (export "caml_string_length") (param $s (ref eq)) (result i32)
      (call $js_length (call $jsstring (local.get $s))))

   (func $caml_string_equal (export "caml_string_equal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (ref.i31
         (call $string_equals
            (call $jsstring (local.get $p1)) (call $jsstring (local.get $p2)))))

   (func (export "caml_string_notequal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (ref.i31
         (i32.eqz
            (call $string_equals
               (call $jsstring (local.get $p1))
               (call $jsstring (local.get $p2))))))

   (func $js_string_compare (param $p1 (ref eq)) (param $p2 (ref eq)) (result i32)
      (call $string_compare
         (call $jsstring (local.get $p1)) (call $jsstring (local.get $p2))))

   (func (export "caml_string_compare")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31 (call $js_string_compare (local.get $v1) (local.get $v2))))

   (func (export "caml_string_lessequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31
         (i32.le_s (call $js_string_compare (local.get $v1) (local.get $v2))
            (i32.const 0))))

   (func (export "caml_string_lessthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31
         (i32.lt_s (call $js_string_compare (local.get $v1) (local.get $v2))
            (i32.const 0))))

   (func (export "caml_string_greaterequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31
         (i32.ge_s (call $js_string_compare (local.get $v1) (local.get $v2))
            (i32.const 0))))

   (func (export "caml_string_greaterthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31
         (i32.gt_s (call $js_string_compare (local.get $v1) (local.get $v2))
            (i32.const 0))))

   (func (export "caml_bytes_of_string") (param $v (ref eq)) (result (ref eq))
      (call $string_to_bytes (ref.cast (ref $string) (local.get $v))))

   (func $caml_string_of_bytes (export "caml_string_of_bytes")
      (param $b (ref eq)) (result (ref eq))
      (call $string_of_bytes (ref.cast (ref $bytes) (local.get $b))))

   (func (export "caml_blit_string")
      (param $v1 (ref eq)) (param $vi1 (ref eq))
      (param $v2 (ref eq)) (param $vi2 (ref eq))
      (param $vn (ref eq)) (result (ref eq))
      (call $string_blit
         (ref.cast (ref $string) (local.get $v1))
         (i31.get_s (ref.cast (ref i31) (local.get $vi1)))
         (ref.cast (ref $bytes) (local.get $v2))
         (i31.get_s (ref.cast (ref i31) (local.get $vi2)))
         (i31.get_s (ref.cast (ref i31) (local.get $vn))))
      (ref.i31 (i32.const 0)))

   ;; The [$n] bytes of the string [$v] at position [$p], as a little-endian
   ;; integer, raising [Invalid_argument "index out of bounds"] if [$check] is
   ;; set and they are not all in the string.
   (func $string_load
      (param $v (ref eq)) (param $p i32) (param $n i32) (param $check i32)
      (result i64)
      (local $s externref) (local $r i64) (local $i i32)
      (local.set $s (call $jsstring (local.get $v)))
      ;; Computed on 64 bits, so that p + n cannot overflow; negative
      ;; positions zero-extend to at least 2^31 and fail the test too.
      (if (i32.and (local.get $check)
             (i64.gt_u
                (i64.add (i64.extend_i32_u (local.get $p))
                   (i64.extend_i32_u (local.get $n)))
                (i64.extend_i32_u (call $js_length (local.get $s)))))
         (then (call $caml_bound_error) (unreachable)))
      (local.set $i (local.get $n))
      (loop $loop
         (if (local.get $i)
            (then
               (local.set $i (i32.sub (local.get $i) (i32.const 1)))
               (local.set $r
                  (i64.or (i64.shl (local.get $r) (i64.const 8))
                     (i64.extend_i32_u
                        (call $js_char_code_at (local.get $s)
                           (i32.add (local.get $p) (local.get $i))))))
               (br $loop))))
      (local.get $r))

   (func (export "caml_string_geti8")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.extend8_s
         (i32.wrap_i64
            (call $string_load
               (local.get $v) (local.get $p) (i32.const 1) (i32.const 1)))))

   (func (export "caml_string_geti16")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.extend16_s
         (i32.wrap_i64
            (call $string_load
               (local.get $v) (local.get $p) (i32.const 2) (i32.const 1)))))

   (func (export "caml_string_get16")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.wrap_i64
         (call $string_load
            (local.get $v) (local.get $p) (i32.const 2) (i32.const 1))))

   (func (export "caml_string_get16u")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.wrap_i64
         (call $string_load
            (local.get $v) (local.get $p) (i32.const 2) (i32.const 0))))

   (func (export "caml_string_get32")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.wrap_i64
         (call $string_load
            (local.get $v) (local.get $p) (i32.const 4) (i32.const 1))))

   (func (export "caml_string_get32u")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.wrap_i64
         (call $string_load
            (local.get $v) (local.get $p) (i32.const 4) (i32.const 0))))

   (func (export "caml_string_get64")
      (param $v (ref eq)) (param $p i32) (result i64)
      (call $string_load
         (local.get $v) (local.get $p) (i32.const 8) (i32.const 1)))

   (func (export "caml_string_get64u")
      (param $v (ref eq)) (param $p i32) (result i64)
      (call $string_load
         (local.get $v) (local.get $p) (i32.const 8) (i32.const 0)))

   ;; The (at most [$vn]) bytes of the string [$v] starting at position
   ;; [$vi], which must be a valid position. The standard library's
   ;; [String.get_utf_8_uchar], [String.get_int32_le]... are compiled to read
   ;; such a window rather than a copy of the whole string.
   (func (export "caml_string_window")
      (param $v (ref eq)) (param $vi (ref eq)) (param $vn (ref eq))
      (result (ref eq))
      (local $s (ref $string)) (local $b (ref $bytes))
      (local $i i32) (local $n i32) (local $len i32)
      (local.set $s (ref.cast (ref $string) (local.get $v)))
      (local.set $i (i31.get_s (ref.cast (ref i31) (local.get $vi))))
      (local.set $len (call $js_length (struct.get $string 0 (local.get $s))))
      (if (i32.ge_u (local.get $i) (local.get $len))
         (then (call $caml_bound_error) (unreachable)))
      (local.set $n (i31.get_s (ref.cast (ref i31) (local.get $vn))))
      (if (i32.gt_u (local.get $n) (i32.sub (local.get $len) (local.get $i)))
         (then (local.set $n (i32.sub (local.get $len) (local.get $i)))))
      (local.set $b (array.new $bytes (i32.const 0) (local.get $n)))
      (call $string_blit
         (local.get $s) (local.get $i) (local.get $b) (i32.const 0)
         (local.get $n))
      (local.get $b))

   (@string $String_sub "String.sub / Bytes.sub")

   ;; A copy of the bytes of [$v] from [$vofs] of length [$vlen]: the
   ;; standard library's [String.sub] is compiled to this rather than to
   ;; [Bytes.sub] applied to a copy of the whole string.
   (func (export "caml_string_sub_bytes")
      (param $v (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $s (ref $string)) (local $b (ref $bytes))
      (local $ofs i32) (local $len i32)
      (local.set $s (ref.cast (ref $string) (local.get $v)))
      (local.set $ofs (i31.get_s (ref.cast (ref i31) (local.get $vofs))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.or (i32.or (i32.lt_s (local.get $ofs) (i32.const 0))
                          (i32.lt_s (local.get $len) (i32.const 0)))
             (i32.gt_s (local.get $ofs)
                (i32.sub
                   (call $js_length (struct.get $string 0 (local.get $s)))
                   (local.get $len))))
         (then
            (call $caml_invalid_argument (global.get $String_sub))
            (unreachable)))
      (local.set $b (array.new $bytes (i32.const 0) (local.get $len)))
      (call $string_blit
         (local.get $s) (local.get $ofs) (local.get $b) (i32.const 0)
         (local.get $len))
      (local.get $b))

   (func (export "caml_string_concat")
      (param $vs1 (ref eq)) (param $vs2 (ref eq)) (result (ref eq))
      (struct.new $string
         (call $string_concat
            (call $jsstring (local.get $vs1)) (call $jsstring (local.get $vs2)))))
)
(@else
   (export "caml_string_equal" (func $caml_bytes_equal))
   (export "caml_string_notequal" (func $caml_bytes_notequal))
   (export "caml_string_compare" (func $caml_bytes_compare))
   (export "caml_string_lessequal" (func $caml_bytes_lessequal))
   (export "caml_string_lessthan" (func $caml_bytes_lessthan))
   (export "caml_string_greaterequal" (func $caml_bytes_greaterequal))
   (export "caml_string_greaterthan" (func $caml_bytes_greaterthan))
   (export "caml_string_geti8" (func $caml_bytes_geti8))
   (export "caml_string_geti16" (func $caml_bytes_geti16))
   (export "caml_string_get16" (func $caml_bytes_get16))
   (export "caml_string_get16u" (func $caml_bytes_get16u))
   (export "caml_string_get32" (func $caml_bytes_get32))
   (export "caml_string_get32u" (func $caml_bytes_get32u))
   (export "caml_string_get64" (func $caml_bytes_get64))
   (export "caml_string_get64u" (func $caml_bytes_get64u))
   (export "caml_blit_string" (func $caml_blit_bytes))
   (export "caml_string_concat" (func $caml_bytes_concat))
   (func (export "caml_string_length") (param $s (ref eq)) (result i32)
      (array.len (ref.cast (ref $bytes) (local.get $s))))

   (export "caml_bytes_of_string" (func $caml_string_of_bytes))
   (func $caml_string_of_bytes (export "caml_string_of_bytes")
      (param $v (ref eq)) (result (ref eq))
      (local.get $v))
))

   (func $caml_bytes_equal (export "caml_bytes_equal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (local $s1 (ref $bytes)) (local $s2 (ref $bytes))
      (local $len i32) (local $i i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (ref.i31 (i32.const 1)))))
      (local.set $s1 (ref.cast (ref $bytes) (local.get $p1)))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $p2)))
      (local.set $len (array.len (local.get $s1)))
      (if (i32.ne (local.get $len) (array.len (local.get $s2)))
         (then (return (ref.i31 (i32.const 0)))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (if (i32.ne (array.get_u $bytes (local.get $s1) (local.get $i))
                           (array.get_u $bytes (local.get $s2) (local.get $i)))
                  (then (return (ref.i31 (i32.const 0)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 1)))

   (func $caml_bytes_notequal (export "caml_bytes_notequal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (return
         (ref.i31 (i32.eqz (i31.get_u (ref.cast (ref i31)
            (call $caml_bytes_equal (local.get $p1) (local.get $p2))))))))

   (func $bytes_compare
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result i32)
      (local $s1 (ref $bytes)) (local $s2 (ref $bytes))
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (i32.const 0))))
      (local.set $s1 (ref.cast (ref $bytes) (local.get $p1)))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $p2)))
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
               (if (i32.lt_u (local.get $c1) (local.get $c2))
                  (then (return (i32.const -1))))
               (if (i32.gt_u (local.get $c1) (local.get $c2))
                  (then (return (i32.const 1))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (if (i32.lt_u (local.get $l1) (local.get $l2))
         (then (return (i32.const -1))))
      (if (i32.gt_u (local.get $l1) (local.get $l2))
         (then (return (i32.const 1))))
      (i32.const 0))

   (func $caml_bytes_compare (export "caml_bytes_compare")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31 (call $bytes_compare (local.get $v1) (local.get $v2))))

   (func $caml_bytes_lessequal (export "caml_bytes_lessequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31 (i32.le_s (call $bytes_compare (local.get $v1) (local.get $v2))
                         (i32.const 0))))

   (func $caml_bytes_lessthan (export "caml_bytes_lessthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31 (i32.lt_s (call $bytes_compare (local.get $v1) (local.get $v2))
                         (i32.const 0))))

   (func $caml_bytes_greaterequal (export "caml_bytes_greaterequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31 (i32.ge_s (call $bytes_compare (local.get $v1) (local.get $v2))
                         (i32.const 0))))

   (func $caml_bytes_greaterthan (export "caml_bytes_greaterthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (ref.i31 (i32.gt_s (call $bytes_compare (local.get $v1) (local.get $v2))
                         (i32.const 0))))


   (@string $Bytes_create "Bytes.create")

   (func (export "caml_create_bytes") (export "caml_create_local_bytes")
      (param $len (ref eq)) (result (ref eq))
      (local $l i32)
      (local.set $l (i31.get_s (ref.cast (ref i31) (local.get $len))))
      (if (i32.lt_s (local.get $l) (i32.const 0))
         (then
            (call $caml_invalid_argument (global.get $Bytes_create))
            (unreachable)))
      (array.new $bytes (i32.const 0) (local.get $l)))

   (func $caml_blit_bytes (export "caml_blit_bytes")
      (param $v1 (ref eq)) (param $i1 (ref eq))
      (param $v2 (ref eq)) (param $i2 (ref eq))
      (param $n (ref eq)) (result (ref eq))
      (array.copy $bytes $bytes
         (ref.cast (ref $bytes) (local.get $v2))
         (i31.get_s (ref.cast (ref i31) (local.get $i2)))
         (ref.cast (ref $bytes) (local.get $v1))
         (i31.get_s (ref.cast (ref i31) (local.get $i1)))
         (i31.get_s (ref.cast (ref i31) (local.get $n))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_fill_bytes")
      (param $v (ref eq)) (param $offset (ref eq))
      (param $len (ref eq)) (param $init (ref eq))
      (result (ref eq))
      (array.fill $bytes (ref.cast (ref $bytes) (local.get $v))
         (i31.get_u (ref.cast (ref i31) (local.get $offset)))
         (i31.get_u (ref.cast (ref i31) (local.get $init)))
         (i31.get_u (ref.cast (ref i31) (local.get $len))))
      (ref.i31 (i32.const 0)))

   (func $caml_bytes_geti8 (export "caml_bytes_geti8")
      (param $v (ref eq)) (param $p i32) (result i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (if (i32.ge_u (local.get $p) (array.len (local.get $s)))
         (then (call $caml_bound_error) (unreachable)))
      (i32.extend8_s (array.get_u $bytes (local.get $s) (local.get $p))))

   (func $caml_bytes_geti16 (export "caml_bytes_geti16")
      (param $v (ref eq)) (param $p i32) (result i32)
      (i32.extend16_s (call $caml_bytes_get16 (local.get $v) (local.get $p))))

   (func $caml_bytes_get16 (export "caml_bytes_get16")
      (param $v (ref eq)) (param $p i32) (result i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      ;; Computed on 64 bits, so that p + 1 cannot overflow; negative
      ;; positions zero-extend to at least 2^31 and fail the test too.
      (if (i64.ge_u
             (i64.add (i64.extend_i32_u (local.get $p)) (i64.const 1))
             (i64.extend_i32_u (array.len (local.get $s))))
         (then (call $caml_bound_error) (unreachable)))
      (i32.or
         (array.get_u $bytes (local.get $s) (local.get $p))
         (i32.shl (array.get_u $bytes (local.get $s)
                     (i32.add (local.get $p) (i32.const 1)))
                  (i32.const 8))))

   (func $caml_bytes_get16u (export "caml_bytes_get16u")
      (param $v (ref eq)) (param $p i32) (result i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (i32.or
         (array.get_u $bytes (local.get $s) (local.get $p))
         (i32.shl (array.get_u $bytes (local.get $s)
                     (i32.add (local.get $p) (i32.const 1)))
                  (i32.const 8))))

   (func $caml_bytes_get32 (export "caml_bytes_get32")
      (param $v (ref eq)) (param $p i32) (result i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (if (i64.ge_u
             (i64.add (i64.extend_i32_u (local.get $p)) (i64.const 3))
             (i64.extend_i32_u (array.len (local.get $s))))
         (then (call $caml_bound_error) (unreachable)))
      (i32.or
         (i32.or
            (array.get_u $bytes (local.get $s) (local.get $p))
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 1)))
                     (i32.const 8)))
         (i32.or
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 2)))
                     (i32.const 16))
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 3)))
                     (i32.const 24)))))

   (func $caml_bytes_get32u (export "caml_bytes_get32u")
      (param $v (ref eq)) (param $p i32) (result i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (i32.or
         (i32.or
            (array.get_u $bytes (local.get $s) (local.get $p))
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 1)))
                     (i32.const 8)))
         (i32.or
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 2)))
                     (i32.const 16))
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 3)))
                     (i32.const 24)))))

   (func $caml_bytes_get64 (export "caml_bytes_get64")
      (param $v (ref eq)) (param $p i32) (result i64)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (if (i64.ge_u
             (i64.add (i64.extend_i32_u (local.get $p)) (i64.const 7))
             (i64.extend_i32_u (array.len (local.get $s))))
         (then (call $caml_bound_error) (unreachable)))
      (i64.or
         (i64.or
            (i64.or
               (i64.extend_i32_u
                  (array.get_u $bytes (local.get $s) (local.get $p)))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 1))))
                        (i64.const 8)))
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 2))))
                        (i64.const 16))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 3))))
                        (i64.const 24))))
         (i64.or
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 4))))
                        (i64.const 32))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 5))))
                        (i64.const 40)))
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 6))))
                        (i64.const 48))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 7))))
                        (i64.const 56))))))

   (func $caml_bytes_get64u (export "caml_bytes_get64u")
      (param $v (ref eq)) (param $p i32) (result i64)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (i64.or
         (i64.or
            (i64.or
               (i64.extend_i32_u
                  (array.get_u $bytes (local.get $s) (local.get $p)))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 1))))
                        (i64.const 8)))
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 2))))
                        (i64.const 16))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 3))))
                        (i64.const 24))))
         (i64.or
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 4))))
                        (i64.const 32))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 5))))
                        (i64.const 40)))
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 6))))
                        (i64.const 48))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 7))))
                        (i64.const 56))))))

   (func (export "caml_bytes_set8")
      (param $vs (ref eq)) (param $p i32) (param $v i32) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (if (i32.ge_u (local.get $p) (array.len (local.get $s)))
         (then (call $caml_bound_error) (unreachable)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set16")
      (param $vs (ref eq)) (param $p i32) (param $v i32) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (if (i64.ge_u
             (i64.add (i64.extend_i32_u (local.get $p)) (i64.const 1))
             (i64.extend_i32_u (array.len (local.get $s))))
         (then (call $caml_bound_error) (unreachable)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set16u")
      (param $vs (ref eq)) (param $p i32) (param $v i32) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set32")
      (param $vs (ref eq)) (param $p i32) (param $v i32) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (if (i64.ge_u
             (i64.add (i64.extend_i32_u (local.get $p)) (i64.const 3))
             (i64.extend_i32_u (array.len (local.get $s))))
         (then (call $caml_bound_error) (unreachable)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.shr_u (local.get $v) (i32.const 16)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.shr_u (local.get $v) (i32.const 24)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set32u")
      (param $vs (ref eq)) (param $p i32) (param $v i32) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.shr_u (local.get $v) (i32.const 16)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.shr_u (local.get $v) (i32.const 24)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set64")
      (param $vs (ref eq)) (param $p i32) (param $v i64) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (if (i64.ge_u
             (i64.add (i64.extend_i32_u (local.get $p)) (i64.const 7))
             (i64.extend_i32_u (array.len (local.get $s))))
         (then (call $caml_bound_error) (unreachable)))
      (array.set $bytes (local.get $s) (local.get $p)
         (i32.wrap_i64 (local.get $v)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 8))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 16))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 24))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 4))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 32))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 5))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 40))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 6))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 48))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 7))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 56))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set64u")
      (param $vs (ref eq)) (param $p i32) (param $v i64) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $vs)))
      (array.set $bytes (local.get $s) (local.get $p)
         (i32.wrap_i64 (local.get $v)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 8))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 16))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 24))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 4))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 32))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 5))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 40))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 6))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 48))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 7))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 56))))
      (ref.i31 (i32.const 0)))

   (func $caml_bytes_concat
      (param $vs1 (ref eq)) (param $vs2 (ref eq)) (result (ref eq))
      (local $s1 (ref $bytes)) (local $s2 (ref $bytes))
      (local $s (ref $bytes))
      (local $l1 i32) (local $l2 i32)
      (local.set $s1 (ref.cast (ref $bytes) (local.get $vs1)))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $vs2)))
      (local.set $l1 (array.len (local.get $s1)))
      (local.set $l2 (array.len (local.get $s2)))
      (local.set $s
         (array.new $bytes (i32.const 0)
            (i32.add (local.get $l1) (local.get $l2))))
      (array.copy $bytes $bytes
         (local.get $s) (i32.const 0) (local.get $s1) (i32.const 0)
         (local.get $l1))
      (array.copy $bytes $bytes
         (local.get $s) (local.get $l1) (local.get $s2) (i32.const 0)
         (local.get $l2))
      (local.get $s))
)
