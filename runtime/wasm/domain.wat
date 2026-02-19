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
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "sync" "caml_ml_mutex_unlock"
      (func $caml_ml_mutex_unlock (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))

   (func (export "caml_atomic_cas")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (if (result (ref eq))
         (ref.eq (array.get $block (local.get $b) (i32.const 1))
                 (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (ref.i31 (i32.const 1)))
         (else
            (ref.i31 (i32.const 0)))))

   (func (export "caml_atomic_cas_field")
      (param $ref (ref eq)) (param $i (ref eq)) (param $o (ref eq))
      (param $n (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $j i32)
      (local.set $j
         (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i))) (i32.const 1)))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (if (result (ref eq))
         (ref.eq (array.get $block (local.get $b) (local.get $j))
                 (local.get $o))
         (then
            (array.set $block (local.get $b) (local.get $j) (local.get $n))
            (ref.i31 (i32.const 1)))
         (else
            (ref.i31 (i32.const 0)))))

   (func (export "caml_atomic_compare_exchange")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (if (result (ref eq))
         (ref.eq (local.get $old) (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (local.get $old))
         (else
            (local.get $old))))

   (func (export "caml_atomic_load") (param (ref eq)) (result (ref eq))
      (array.get $block (ref.cast (ref $block) (local.get 0)) (i32.const 1)))

   (func (export "caml_atomic_load_field")
      (param $b (ref eq)) (param $i (ref eq)) (result (ref eq))
      (array.get $block (ref.cast (ref $block) (local.get $b))
        (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i))) (i32.const 1))))

   (func (export "caml_atomic_fetch_add")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (local.get $old))

   (func (export "caml_atomic_fetch_add_field")
      (param $ref (ref eq)) (param $i (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local $j i32)
      (local.set $j
         (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i))) (i32.const 1)))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (local.get $j)))
      (array.set $block (local.get $b) (local.get $j)
         (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $n))))))
      (local.get $old))

   (func (export "caml_atomic_add")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_sub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.sub (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_land")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.and (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lor")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.or (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lxor")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.xor (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_exchange")
      (param $ref (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $r (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $r (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1) (local.get $v))
      (local.get $r))

   (func (export "caml_atomic_exchange_field")
      (param $ref (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $r (ref eq))
      (local $j i32)
      (local.set $j
         (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i))) (i32.const 1)))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $r (array.get $block (local.get $b) (local.get $j)))
      (array.set $block (local.get $b) (local.get $j) (local.get $v))
      (local.get $r))

   (func (export "caml_atomic_set")
      (param $ref (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $r (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $r (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_make_contended")
      (param $v (ref eq)) (result (ref eq))
      (array.new_fixed $block 2 (ref.i31 (i32.const 0)) (local.get $v)))

   (global $caml_domain_dls (mut (ref eq))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_domain_dls_set") (param $a (ref eq)) (result (ref eq))
      (global.set $caml_domain_dls (local.get $a))
      (ref.i31 (i32.const 0)))

   (func (export "caml_domain_dls_compare_and_set") (param $old (ref eq)) (param $new (ref eq)) (result (ref eq))
      (if (result (ref eq))
         (ref.eq (global.get $caml_domain_dls) (local.get $old))
         (then
            (global.set $caml_domain_dls (local.get $new))
            (ref.i31 (i32.const 1)))
         (else
            (ref.i31 (i32.const 0)))))

   (func (export "caml_domain_dls_get") (param (ref eq)) (result (ref eq))
      (global.get $caml_domain_dls))

   (global $caml_domain_tls (mut (ref eq))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_domain_tls_set") (param $a (ref eq)) (result (ref eq))
      (global.set $caml_domain_tls (local.get $a))
      (ref.i31 (i32.const 0)))

   (func (export "caml_domain_tls_get") (param (ref eq)) (result (ref eq))
      (global.get $caml_domain_tls))

   (global $caml_ml_domain_unique_token (ref eq)
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_ml_domain_unique_token")
      (param (ref eq)) (result (ref eq))
      (global.get $caml_ml_domain_unique_token))

   (func (export "caml_ml_domain_set_name")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_recommended_domain_count")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))

   (global $caml_domain_id (export "caml_domain_id") (mut i32) (i32.const 0))
   (global $caml_domain_latest_id (export "caml_domain_latest_id") (mut i32)
      (i32.const 1))

(@if (>= ocaml_version (5 2 0))
(@then
   (func (export "caml_domain_spawn")
      (param $f (ref eq)) (param $term_sync_v (ref eq)) (result (ref eq))
      (local $id i32) (local $old i32) (local $ts (ref $block)) (local $res (ref eq))
      (local.set $id (global.get $caml_domain_latest_id))
      (global.set $caml_domain_latest_id
         (i32.add (local.get $id) (i32.const 1)))
      (local.set $old (global.get $caml_domain_id))
      (local.set $res
         (call $caml_callback_1 (local.get $f) (ref.i31 (i32.const 0))))
      (global.set $caml_domain_id (local.get $old))
      (local.set $ts (ref.cast (ref $block) (local.get $term_sync_v)))
      (drop (call $caml_ml_mutex_unlock (array.get $block (local.get $ts) (i32.const 2))))
      ;; TODO: fix exn case
      (array.set
         $block
         (local.get $ts)
         (i32.const 1)
         (array.new_fixed
            $block
            2
            (ref.i31 (i32.const 0))
            (array.new_fixed $block 2 (ref.i31 (i32.const 0)) (local.get $res))))
      (ref.i31 (local.get $id)))
)
(@else
   (func (export "caml_domain_spawn")
      (param $f (ref eq)) (param $mutex (ref eq)) (result (ref eq))
      (local $id i32) (local $old i32)
      (local.set $id (global.get $caml_domain_latest_id))
      (global.set $caml_domain_latest_id
         (i32.add (local.get $id) (i32.const 1)))
      (local.set $old (global.get $caml_domain_id))
      (drop (call $caml_callback_1 (local.get $f) (ref.i31 (i32.const 0))))
      (global.set $caml_domain_id (local.get $old))
      (drop (call $caml_ml_mutex_unlock (local.get $mutex)))
      (ref.i31 (local.get $id)))
))


   (func (export "caml_ml_domain_id") (export "caml_ml_domain_index")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (global.get $caml_domain_id)))

   (func (export "caml_ml_domain_cpu_relax") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_add_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (local.set $old (array.get $block (local.get $b) (local.get $idx)))
      (array.set $block (local.get $b) (local.get $idx)
         (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_sub_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (local.set $old (array.get $block (local.get $b) (local.get $idx)))
      (array.set $block (local.get $b) (local.get $idx)
         (ref.i31 (i32.sub (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_land_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (local.set $old (array.get $block (local.get $b) (local.get $idx)))
      (array.set $block (local.get $b) (local.get $idx)
         (ref.i31 (i32.and (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lor_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (local.set $old (array.get $block (local.get $b) (local.get $idx)))
      (array.set $block (local.get $b) (local.get $idx)
         (ref.i31 (i32.or (i31.get_s (ref.cast (ref i31) (local.get $old)))
                          (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lxor_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (local.set $old (array.get $block (local.get $b) (local.get $idx)))
      (array.set $block (local.get $b) (local.get $idx)
         (ref.i31 (i32.xor (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_compare_exchange_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $o (ref eq))
      (param $n (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (local.set $old (array.get $block (local.get $b) (local.get $idx)))
      (if (ref.eq (local.get $old) (local.get $o))
         (then
            (array.set $block (local.get $b) (local.get $idx) (local.get $n))))
      (local.get $old))

   (func (export "caml_atomic_set_field")
      (param $ref (ref eq)) (param $field (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $idx i32)
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $idx
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $field)))
            (i32.const 1)))
      (array.set $block (local.get $b) (local.get $idx) (local.get $v))
      (ref.i31 (i32.const 0)))
)
