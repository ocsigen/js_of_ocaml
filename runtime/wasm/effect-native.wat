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
(@if (= $effects "native")
(@then
   (import "fail" "caml_raise_constant"
      (func $caml_raise_constant (param (ref eq))))
   (import "fail" "caml_raise_with_arg"
      (func $caml_raise_with_arg (param $tg (ref eq)) (param $arg (ref eq))))
   (import "obj" "caml_fresh_oo_id"
     (func $caml_fresh_oo_id (param (ref eq)) (result (ref eq))))
   (import "obj" "cont_tag" (global $cont_tag i32))
   (import "obj" "object_tag" (global $object_tag i32))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "stdlib" "caml_main_wrapper"
      (global $caml_main_wrapper (mut (ref null $wrapper_func))))
   (import "effect" "effect_allowed" (global $effect_allowed (mut i32)))
(@if $wasi
(@then
   ;; Never actually called since there is no JavaScript exception
   (func $caml_wrap_exception (param externref) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
(@else
   (import "jslib" "caml_wrap_exception"
      (func $caml_wrap_exception (param externref) (result (ref eq))))
))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field $func (ref $function_1)))))
   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_3
      (sub $closure
         (struct (field $func (ref $function_1)) (field $direct (ref $function_3)))))

   ;; Effect types

   (tag $effect (param (ref eq)) (result (ref eq) (ref eq)))

   (type $cont_function (func (param (ref eq) (ref eq)) (result (ref eq))))

   (type $continuation (cont $cont_function))

   (type $generic_fiber
      (sub
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq))))))

   (type $fiber
      (sub final $generic_fiber
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq)))
            (field $continuation (mut (ref $continuation))))))

   ;; Unhandled effects

   (@string $effect_unhandled "Effect.Unhandled")

   (func $raise_unhandled
      (param $eff (ref eq)) (param (ref eq)) (result (ref eq))
      (block $null
         (call $caml_raise_with_arg
            (br_on_null $null
               (call $caml_named_value (global.get $effect_unhandled)))
            (local.get $eff)))
      (call $caml_raise_constant
         (array.new_fixed $block 3 (ref.i31 (global.get $object_tag))
            (global.get $effect_unhandled)
            (call $caml_fresh_oo_id (ref.i31 (i32.const 0)))))
      (ref.i31 (i32.const 0)))

   (global $raise_unhandled_closure (ref $closure)
      (struct.new $closure (ref.func $raise_unhandled)))

   (type $thunk (func (result (ref eq))))
   (type $wrapper_func (func (param (ref $thunk))))
   (type $func_closure (struct (field $func (ref $thunk))))

   (func $wrapper_cont
      (param $f (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call_ref $thunk
         (local.get $f)
         (struct.get $func_closure 0
            (ref.cast (ref $func_closure) (local.get $f)))))

   (func $unhandled_effect_wrapper (param $start (ref $thunk))
      (local $continuation (ref $continuation))
      (local $f (ref eq)) (local $v (ref eq))
      (local $resume_res_0 (ref eq)) (local $resume_res_1 (ref $continuation))
      (local.set $continuation (cont.new $continuation (ref.func $wrapper_cont)))
      (local.set $f (struct.new $func_closure (local.get $start)))
      (local.set $v (ref.i31 (i32.const 0)))
      (loop $loop
         (block $handle_effect (result (ref eq) (ref $continuation))
            (resume $continuation (on $effect $handle_effect)
               (local.get $f) (local.get $v) (local.get $continuation))
            (return))
         (local.set $resume_res_1)
         (local.set $resume_res_0)
         (local.set $continuation (local.get $resume_res_1))
         (local.set $v (local.get $resume_res_0))
         (local.set $f (global.get $raise_unhandled_closure))
         (br $loop)))

   (func $init
      (global.set $caml_main_wrapper (ref.func $unhandled_effect_wrapper)))

   (start $init)

   ;; Resume

   (@string $already_resumed "Effect.Continuation_already_resumed")

   (func $resume_fiber (export "%resume")
      (param $vfiber (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (param $tail (ref eq)) (result (ref eq))
      (local $fiber (ref $fiber))
      (local $res (ref eq))
      (local $exn (ref eq))
      (local $resume_res_0 (ref eq)) (local $resume_res_1 (ref $continuation))
      (if (ref.eq (local.get $vfiber) (ref.i31 (i32.const 0)))
         (then
            (call $caml_raise_constant
               (ref.as_non_null
                  (call $caml_named_value (global.get $already_resumed))))))
      (local.set $fiber (ref.cast (ref $fiber) (local.get $vfiber)))
      (local.set $exn
         (block $handle_exception (result (ref eq))
               (block $handle_effect (result (ref eq) (ref $continuation))
                  (local.set $res
                     (try (result (ref eq))
                        (do
                           (resume $continuation
                               (on $effect $handle_effect)
                               (local.get $f) (local.get $v)
                               (struct.get $fiber $continuation (local.get $fiber))))
                        (catch $javascript_exception
                           (br $handle_exception
                              (call $caml_wrap_exception)))
                        (catch $ocaml_exception
                           (br $handle_exception))))
                  ;; handle return
                  (return_call_ref $function_1 (local.get $res)
                     (local.tee $f
                        (struct.get $fiber $value (local.get $fiber)))
                     (struct.get $closure 0
                        (ref.cast (ref $closure) (local.get $f)))))
            (local.set $resume_res_1)
            (local.set $resume_res_0)
            ;; handle effect
            (struct.set $fiber $continuation (local.get $fiber)
               (local.get $resume_res_1))
            (return_call_ref $function_3
               (local.get $resume_res_0)
               (array.new_fixed $block 3 (ref.i31 (global.get $cont_tag))
                  (local.get $fiber)
                  (local.get $fiber))
               (if (result (ref eq))
                     (ref.eq (local.get $tail) (ref.i31 (i32.const 0)))
                  (then (local.get $fiber))
                  (else (local.get $tail)))
               (local.tee $f
                  (struct.get $fiber $effect (local.get $fiber)))
               (struct.get $closure_3 1
                  (ref.cast (ref $closure_3) (local.get $f))))))
      ;; handle exception
      (return_call_ref $function_1 (local.get $exn)
         (local.tee $f
            (struct.get $fiber $exn (local.get $fiber)))
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   ;; Perform

   (func (export "%reperform")
      (param $eff (ref eq)) (param $continuation (ref eq)) (param $tail (ref eq))
      (result (ref eq))
      (local $res_0 (ref eq)) (local $res_1 (ref eq))
      (suspend $effect (local.get $eff))
      (local.set $res_1)
      (local.set $res_0)
      (return_call $resume_fiber
         (ref.as_non_null
            (array.get $block
               (ref.cast (ref $block) (local.get $continuation))
               (i32.const 1)))
         (local.get $res_0)
         (local.get $res_1)
         (local.get $tail)))

   (func (export "%perform") (param $eff (ref eq)) (result (ref eq))
      (local $res_0 (ref eq)) (local $res_1 (ref eq))
      (if (i32.eqz (global.get $effect_allowed))
         (then
            (return_call $raise_unhandled
               (local.get $eff) (ref.i31 (i32.const 0)))))
      (suspend $effect (local.get $eff))
      (local.set $res_1)
      (local.set $res_0)
      (return_call_ref $function_1 (local.get $res_1)
         (local.get $res_0)
         (struct.get $closure 0
            (ref.cast (ref $closure) (local.get $res_0)))))

   ;; Allocate a stack

   (func $initial_cont
      (param $f (ref eq)) (param $x (ref eq)) (result (ref eq))
      (return_call_ref $function_1 (local.get $x)
         (local.get $f)
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   (func (export "caml_alloc_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (result (ref eq))
      (struct.new $fiber
         (local.get $hv) (local.get $hx) (local.get $hf)
         (cont.new $continuation (ref.func $initial_cont))))

   (func (export "%with_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (return_call $resume_fiber
         (struct.new $fiber
            (local.get $hv) (local.get $hx) (local.get $hf)
            (cont.new $continuation (ref.func $initial_cont)))
         (local.get $f) (local.get $v)
         (ref.i31 (i32.const 0))))

   (func (export "%with_stack_bind")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (param $dyn (ref eq)) (param $bind (ref eq))
      (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (return_call $resume_fiber
         (struct.new $fiber
            (local.get $hv) (local.get $hx) (local.get $hf)
            (cont.new $continuation (ref.func $initial_cont)))
         (local.get $f) (local.get $v)
         (ref.i31 (i32.const 0))))
))
)
