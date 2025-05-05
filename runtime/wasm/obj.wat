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
   (import "custom" "caml_is_custom"
      (func $caml_is_custom (param (ref eq)) (result i32)))
   (import "custom" "caml_dup_custom"
      (func $caml_dup_custom (param (ref eq)) (result (ref eq))))
   (import "effect" "caml_is_continuation"
      (func $caml_is_continuation (param (ref eq)) (result i32)))
(@if (= effects "cps")
(@then
   (import "effect" "caml_cps_trampoline"
      (func $caml_cps_trampoline (param (ref eq) (ref eq)) (result (ref eq))))
))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field (ref $function_1)))))
   (type $closure_last_arg
      (sub $closure (struct (;(field i32);) (field (ref $function_1)))))
   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $cps_closure (sub (struct (field (ref $function_2)))))
   (type $cps_closure_last_arg
      (sub $cps_closure (struct (field (ref $function_2)))))

   (type $int_array (array (mut i32)))

   (type $dummy_closure_1
      (sub final $closure_last_arg
         (struct (field (ref $function_1)) (field (mut (ref null $closure))))))

   (type $closure_2
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_2)))))

   (type $dummy_closure_2
      (sub final $closure_2
         (struct (field (ref $function_1)) (field (ref $function_2))
            (field (mut (ref null $closure_2))))))

   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   (type $dummy_closure_3
      (sub final $closure_3
         (struct (field (ref $function_1)) (field (ref $function_3))
            (field (mut (ref null $closure_3))))))

   (type $function_4
      (func (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq))
         (result (ref eq))))

   (type $closure_4
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_4)))))

   (type $dummy_closure_4
      (sub final $closure_4
         (struct (field (ref $function_1)) (field (ref $function_4))
            (field (mut (ref null $closure_4))))))

   (type $cps_dummy_closure
      (sub final $cps_closure_last_arg
         (struct
            (field (ref $function_2))
            (field (mut (ref null $cps_closure))))))

   (global $forcing_tag i32 (i32.const 244))
   (global $cont_tag (export "cont_tag") i32 (i32.const 245))
   (global $lazy_tag (export "lazy_tag") i32 (i32.const 246))
   (global $closure_tag i32 (i32.const 247))
   (global $object_tag (export "object_tag") i32 (i32.const 248))
   (global $forward_tag (export "forward_tag") i32 (i32.const 250))
   (global $abstract_tag (export "abstract_tag") i32 (i32.const 251))
   (global $string_tag i32 (i32.const 252))
   (global $float_tag i32 (i32.const 253))
   (global $double_array_tag (export "double_array_tag") i32 (i32.const 254))
   (global $custom_tag i32 (i32.const 255))

   (func $caml_is_closure (export "caml_is_closure")
      (param $v (ref eq)) (result i32)
      (i32.or (ref.test (ref $closure) (local.get $v))
              (ref.test (ref $cps_closure) (local.get $v))))

   (func (export "caml_is_last_arg")
      (param $v (ref eq)) (result i32)
      (i32.or (ref.test (ref $closure_last_arg) (local.get $v))
              (ref.test (ref $cps_closure_last_arg) (local.get $v))))

   (func (export "caml_alloc_dummy") (param $size (ref eq)) (result (ref eq))
      (array.new $block (ref.i31 (i32.const 0))
                 (i32.add (i31.get_u (ref.cast (ref i31) (local.get $size)))
                          (i32.const 1))))

   (func (export "caml_alloc_dummy_float")
      (param $size (ref eq)) (result (ref eq))
      (array.new $float_array (f64.const 0)
         (i31.get_u (ref.cast (ref i31) (local.get $size)))))

   (func (export "caml_update_dummy")
      (param $dummy (ref eq)) (param $newval (ref eq)) (result (ref eq))
      (local $i i32)
      (local $dst (ref $block)) (local $fdst (ref $float_array))
      (local $src (ref $block))
      (drop (block $not_block (result (ref eq))
         (local.set $dst
            (br_on_cast_fail $not_block (ref eq) (ref $block)
               (local.get $dummy)))
         (local.set $src (ref.cast (ref $block) (local.get $newval)))
         (array.copy $block $block
            (local.get $dst) (i32.const 0) (local.get $src) (i32.const 0)
            (array.len (local.get $dst)))
         (return (ref.i31 (i32.const 0)))))
      (drop (block $not_float_array (result (ref eq))
         (local.set $fdst
            (br_on_cast_fail $not_float_array (ref eq) (ref $float_array)
               (local.get $dummy)))
         (array.copy $float_array $float_array
            (local.get $fdst) (i32.const 0)
            (ref.cast (ref $float_array) (local.get $newval)) (i32.const 0)
            (array.len (local.get $fdst)))
         (return (ref.i31 (i32.const 0)))))
      (drop (block $not_closure_1 (result (ref eq))
         (struct.set $dummy_closure_1 1
            (br_on_cast_fail $not_closure_1 (ref eq) (ref $dummy_closure_1)
               (local.get $dummy))
            (ref.cast (ref $closure) (local.get $newval)))
         (return (ref.i31 (i32.const 0)))))
      (drop (block $not_closure_2 (result (ref eq))
         (struct.set $dummy_closure_2 2
            (br_on_cast_fail $not_closure_2 (ref eq) (ref $dummy_closure_2)
               (local.get $dummy))
            (ref.cast (ref $closure_2) (local.get $newval)))
         (return (ref.i31 (i32.const 0)))))
      (drop (block $not_closure_3 (result (ref eq))
         (struct.set $dummy_closure_3 2
            (br_on_cast_fail $not_closure_3 (ref eq) (ref $dummy_closure_3)
               (local.get $dummy))
            (ref.cast (ref $closure_3) (local.get $newval)))
         (return (ref.i31 (i32.const 0)))))
      (drop (block $not_closure_4 (result (ref eq))
         (struct.set $dummy_closure_4 2
            (br_on_cast_fail $not_closure_4 (ref eq) (ref $dummy_closure_4)
               (local.get $dummy))
            (ref.cast (ref $closure_4) (local.get $newval)))
         (return (ref.i31 (i32.const 0)))))
      (drop (block $not_cps_closure (result (ref eq))
         (struct.set $cps_dummy_closure 1
            (br_on_cast_fail $not_cps_closure (ref eq) (ref $cps_dummy_closure)
               (local.get $dummy))
            (ref.cast (ref $cps_closure) (local.get $newval)))
         (return (ref.i31 (i32.const 0)))))
      (unreachable))

   (func $caml_obj_dup (export "caml_obj_dup")
      (param (ref eq)) (result (ref eq))
      (local $orig (ref $block)) (local $res (ref $block))
      (local $forig (ref $float_array)) (local $fres (ref $float_array))
      (local $s (ref $bytes)) (local $s' (ref $bytes))
      (local $len i32)
      (drop (block $not_block (result (ref eq))
         (local.set $orig (br_on_cast_fail $not_block (ref eq) (ref $block)
            (local.get 0)))
         (local.set $len (array.len (local.get $orig)))
         (local.set $res
            (array.new $block (array.get $block (local.get $orig) (i32.const 0))
               (local.get $len)))
         (array.copy $block $block
            (local.get $res) (i32.const 1) (local.get $orig) (i32.const 1)
            (i32.sub (local.get $len) (i32.const 1)))
         (return (local.get $res))))
      (drop (block $not_float_array (result (ref eq))
         (local.set $forig
            (br_on_cast_fail $not_float_array (ref eq) (ref $float_array)
               (local.get 0)))
         (local.set $len (array.len (local.get $forig)))
         (local.set $fres
            (array.new $float_array (f64.const 0) (local.get $len)))
         (array.copy $float_array $float_array
            (local.get $fres) (i32.const 0) (local.get $forig) (i32.const 0)
            (local.get $len))
         (return (local.get $fres))))
      (drop (block $not_string (result (ref eq))
         (local.set $s (br_on_cast_fail $not_string (ref eq) (ref $bytes)
            (local.get 0)))
         (local.set $len (array.len (local.get $s)))
         (local.set $s' (array.new $bytes (i32.const 0) (local.get $len)))
         (array.copy $bytes $bytes
            (local.get $s') (i32.const 0) (local.get $s) (i32.const 0)
            (local.get $len))
         (return (local.get $s'))))
      (drop (block $not_float (result (ref eq))
         (return
            (struct.new $float
               (struct.get $float 0
                  (br_on_cast_fail $not_float (ref eq) (ref $float)
                     (local.get 0)))))))
      (call $caml_dup_custom (local.get 0)))

   (func (export "caml_obj_with_tag")
      (param $tag (ref eq)) (param (ref eq)) (result (ref eq))
      (local $res (ref eq))
      (local.set $res (call $caml_obj_dup (local.get 1)))
      (array.set $block (ref.cast (ref $block) (local.get $res)) (i32.const 0)
         (local.get $tag))
      (local.get $res))

   (func (export "caml_obj_block")
      (param $tag (ref eq)) (param $size (ref eq)) (result (ref eq))
      (local $res (ref $block))
      ;; ZZZ float array / specific types?
      ;; TODO: fail for value that are not represented as an array
      (local.set $res
         (array.new $block
            (ref.i31 (i32.const 0))
            (i32.add (i31.get_s (ref.cast (ref i31) (local.get $size)))
                     (i32.const 1))))
      (array.set $block (local.get $res) (i32.const 0) (local.get $tag))
      (local.get $res))

   (func (export "caml_obj_tag") (param $v (ref eq)) (result (ref eq))
      (if (ref.test (ref i31) (local.get $v))
         (then (return (ref.i31 (i32.const 1000)))))
      (drop (block $not_block (result (ref eq))
         (return
            (array.get $block
              (br_on_cast_fail $not_block (ref eq) (ref $block) (local.get $v))
              (i32.const 0)))))
      (if (ref.test (ref $bytes) (local.get $v))
         (then (return (ref.i31 (global.get $string_tag)))))
      (if (ref.test (ref $float) (local.get $v))
         (then (return (ref.i31 (global.get $float_tag)))))
      (if (ref.test (ref $float_array) (local.get $v))
         (then (return (ref.i31 (global.get $double_array_tag)))))
      (if (call $caml_is_custom (local.get $v))
         (then (return (ref.i31 (global.get $custom_tag)))))
      (if (call $caml_is_closure (local.get $v))
         (then (return (ref.i31 (global.get $closure_tag)))))
      (if (call $caml_is_continuation (local.get $v))
         (then (return (ref.i31 (global.get $cont_tag)))))
      (ref.i31 (global.get $abstract_tag)))

   (func (export "caml_obj_make_forward")
      (param $b (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $block (ref $block))
      (local.set $block (ref.cast (ref $block) (local.get $b)))
      (array.set $block (local.get $block)
         (i32.const 0) (ref.i31 (global.get $forward_tag)))
      (array.set $block (local.get $block) (i32.const 1) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_lazy_make_forward")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 2 (ref.i31 (global.get $forward_tag))
         (local.get 0)))

   (func $obj_update_tag
      (param (ref eq)) (param $o i32) (param $n i32) (result i32)
      (local $b (ref $block))
      (local.set $b (ref.cast (ref $block) (local.get 0)))
      (if (result i32) (ref.eq (array.get $block (local.get $b) (i32.const 0))
                               (ref.i31 (local.get $o)))
         (then
            (array.set $block (local.get $b) (i32.const 0)
               (ref.i31 (local.get $n)))
            (i32.const 1))
         (else
            (i32.const 0))))

   (func (export "caml_lazy_reset_to_lazy") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $lazy_tag)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_lazy_update_to_forward") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $forward_tag)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_lazy_update_to_forcing")
      (param (ref eq)) (result (ref eq))
      (if (ref.test (ref $block) (local.get 0))
         (then
            (if (call $obj_update_tag (local.get 0)
                   (global.get $lazy_tag) (global.get $forcing_tag))
               (then (return (ref.i31 (i32.const 0)))))))
      (ref.i31 (i32.const 1)))

   (func (export "caml_obj_compare_and_swap")
      (param (ref eq)) (param (ref eq))
      (param $old (ref eq)) (param $new (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $i i32)
      (local.set $b (ref.cast (ref $block) (local.get 0)))
      (local.set $i
         (i32.add (i31.get_u (ref.cast (ref i31) (local.get 1))) (i32.const 1)))
      (if (result (ref eq))
          (ref.eq
            (array.get $block (local.get $b) (local.get $i)) (local.get $old))
         (then
            (array.set $block (local.get $b) (local.get $i) (local.get $new))
            (ref.i31 (i32.const 1)))
         (else
            (ref.i31 (i32.const 0)))))

   (func (export "caml_obj_is_shared") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))

   (func (export "caml_obj_raw_field")
      (param $o (ref eq)) (param $i (ref eq)) (result (ref eq))
      (array.get $block (ref.cast (ref $block) (local.get $o))
         (i32.add
            (i31.get_u (ref.cast (ref i31) (local.get $i))) (i32.const 1))))

   (func (export "caml_obj_set_raw_field")
      (param $o (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (array.set $block (ref.cast (ref $block) (local.get $o))
         (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i))) (i32.const 1))
         (local.get $v))
      (ref.i31 (i32.const 0)))

   (@string $not_implemented "Obj.add_offset is not supported")

   (func (export "caml_obj_add_offset")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $not_implemented))
      (ref.i31 (i32.const 0)))

   (@string $truncate_not_implemented "Obj.truncate is not supported")

   (func (export "caml_obj_truncate")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $truncate_not_implemented))
      (ref.i31 (i32.const 0)))

   (global $method_cache (mut (ref $int_array))
      (array.new $int_array (i32.const 0) (i32.const 8)))

   (func (export "caml_get_public_method")
      (param $obj (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $meths (ref $block))
      (local $tag i32) (local $cacheid i32) (local $ofs i32)
      (local $li i32) (local $mi i32) (local $hi i32)
      (local $a (ref $int_array)) (local $len i32)
      (local.set $meths
         (ref.cast (ref $block)
            (array.get $block
               (ref.cast (ref $block) (local.get $obj)) (i32.const 1))))
      (local.set $tag (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (local.set $cacheid (i31.get_u (ref.cast (ref i31) (local.get 2))))
      (local.set $len (array.len (global.get $method_cache)))
      (if (i32.ge_s (local.get $cacheid) (local.get $len))
         (then
            (loop $size
               (local.set $len (i32.shl (local.get $len) (i32.const 1)))
               (br_if $size (i32.ge_s (local.get $cacheid) (local.get $len))))
            (local.set $a (array.new $int_array (i32.const 0) (local.get $len)))
            (array.copy $int_array $int_array
               (local.get $a) (i32.const 0)
               (global.get $method_cache) (i32.const 0)
               (array.len (global.get $method_cache)))
            (global.set $method_cache (local.get $a))))
      (local.set $ofs
         (array.get $int_array (global.get $method_cache) (local.get $cacheid)))
      (if (i32.eq (local.get $tag)
             (i31.get_s
                (ref.cast (ref i31)
                   (array.get $block (local.get $meths) (local.get $ofs)))))
         (then
            (return
               (array.get $block
                  (local.get $meths) (i32.sub (local.get $ofs) (i32.const 1))))))
      (local.set $li (i32.const 3))
      (local.set $hi
         (i32.add
            (i32.shl
               (i31.get_u
                  (ref.cast (ref i31)
                     (array.get $block (local.get $meths) (i32.const 1))))
               (i32.const 1))
            (i32.const 1)))
      (loop $loop
         (if (i32.lt_u (local.get $li) (local.get $hi))
            (then
               (local.set $mi
                  (i32.or (i32.shr_u (i32.add (local.get $li) (local.get $hi))
                                     (i32.const 1))
                          (i32.const 1)))
               (if (i32.lt_s
                      (local.get $tag)
                      (i31.get_s
                         (ref.cast (ref i31)
                            (array.get $block
                               (local.get $meths)
                               (i32.add (local.get $mi) (i32.const 1))))))
                  (then
                     (local.set $hi (i32.sub (local.get $mi) (i32.const 2))))
                  (else
                     (local.set $li (local.get $mi))))
               (br $loop))))
      (array.set $int_array (global.get $method_cache) (local.get $cacheid)
         (i32.add (local.get $li) (i32.const 1)))
      (if (result (ref eq))
          (i32.eq (local.get $tag)
             (i31.get_s
                (ref.cast (ref i31)
                   (array.get $block (local.get $meths)
                      (i32.add (local.get $li) (i32.const 1))))))
         (then
            (array.get $block (local.get $meths) (local.get $li)))
         (else
            (ref.i31 (i32.const 0)))))

   (global $caml_oo_last_id (mut i32) (i32.const 0))

   (func (export "caml_set_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (array.set $block (ref.cast (ref $block) (local.get 0)) (i32.const 2)
         (ref.i31 (local.get $id)))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (local.get 0))

   (func (export "caml_fresh_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (ref.i31 (local.get $id)))

   (func (export "caml_obj_reachable_words") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

(@if (= effects "cps")
(@then
   (func $caml_callback_1 (export "caml_callback_1")
      (param $f (ref eq)) (param $x (ref eq)) (result (ref eq))
      (return_call $caml_cps_trampoline
         (local.get $f)
         (array.new_fixed $block 2 (ref.i31 (i32.const 0)) (local.get $x))))

   (func (export "caml_callback_2")
      (param $f (ref eq)) (param $x (ref eq)) (param $y (ref eq))
      (result (ref eq))
      (return_call $caml_cps_trampoline
         (local.get $f)
         (array.new_fixed $block 3 (ref.i31 (i32.const 0))
           (local.get $x) (local.get $y))))
)
(@else
   (func $caml_callback_1 (export "caml_callback_1")
      (param $f (ref eq)) (param $x (ref eq)) (result (ref eq))
      (return_call_ref $function_1 (local.get $x)
         (local.get $f)
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   (func (export "caml_callback_2")
      (param $f (ref eq)) (param $x (ref eq)) (param $y (ref eq))
      (result (ref eq))
      (drop (block $not_direct (result (ref eq))
         (return_call_ref $function_2 (local.get $x) (local.get $y)
            (local.get $f)
            (struct.get $closure_2 1
               (br_on_cast_fail $not_direct (ref eq) (ref $closure_2)
                  (local.get $f))))))
      (return_call $caml_callback_1
         (call $caml_callback_1 (local.get $f) (local.get $x))
         (local.get $y)))
))
)
