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
   (import "obj" "null" (global $null_value (ref eq)))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "effect" "caml_continuation_use_noexc"
      (func $caml_continuation_use_noexc (param (ref eq)) (result (ref eq))))
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
(@if (< $ocaml_version (5 6 0))
(@then
   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_3
      (sub $closure
         (struct (field $func (ref $function_1)) (field $direct (ref $function_3)))))
)
(@else
   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_2
      (sub $closure
         (struct (field $func (ref $function_1)) (field $direct (ref $function_2)))))
))

   ;; Effect types

   (tag $effect (param (ref eq)) (result (ref eq) (ref eq)))

   (type $cont_function (func (param (ref eq) (ref eq)) (result (ref eq))))

   (type $continuation (cont $cont_function))

   ;; Must remain identical to the type in effect.wat. The dynamic binding
   ;; fields are unused with this backend, which does not track the current
   ;; fiber (see effect.wat).
   (type $generic_fiber
      (sub
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq)))
            (field $dynamic (mut (ref eq)))
            (field $is_task (mut i32)))))

   (type $fiber
      (sub final $generic_fiber
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq)))
            (field $dynamic (mut (ref eq)))
            (field $is_task (mut i32))
            (field $continuation (mut (ref $continuation))))))

   ;; Unhandled effects

   (@string $effect_unhandled "Effect.Unhandled")

   (func $raise_unhandled
      (param $eff (ref eq)) (param (ref eq)) (result (ref eq))
      (block $null_value
         (call $caml_raise_with_arg
            (br_on_null $null_value
               (call $caml_named_value (global.get $effect_unhandled)))
            (local.get $eff)))
      (call $caml_raise_constant
         (array.new_fixed $block 3 (ref.i31 (global.get $object_tag))
            (global.get $effect_unhandled)
            (call $caml_fresh_oo_id (ref.i31 (i32.const 0)))))
      (ref.i31 (i32.const 0)))

   (global $raise_unhandled_closure (ref $closure)
      (struct.new $closure (ref.func $raise_unhandled)))

   ;; A suspend with no enclosing handler traps, so we cannot let
   ;; %perform suspend blindly. $effect_allowed tells whether a
   ;; $resume_fiber frame is directly above us with no JavaScript frame
   ;; in between: it is initially 0 (toplevel), set to 1 by
   ;; $resume_fiber while a fiber runs, and reset to 0 by the
   ;; JavaScript callback wrappers (runtime.js) around each call into
   ;; OCaml, since suspending across a JavaScript frame is not
   ;; possible. Effects performed while it is 0 raise Effect.Unhandled,
   ;; as in the native OCaml runtime when crossing C frames.
   ;; caml_assume_no_perform also sets it to 0.
   (global $effect_allowed (export "effect_allowed") (mut i32) (i32.const 0))

   (func (export "caml_assume_no_perform") (param $f (ref eq)) (result (ref eq))
      (local $saved_effect_allowed i32)
      (local $res (ref eq))
      (local $exn (ref eq))
      (local.set $saved_effect_allowed (global.get $effect_allowed))
      (global.set $effect_allowed (i32.const 0))
      (local.set $res
         (try (result (ref eq))
            (do
               (call $caml_callback_1 (local.get $f) (ref.i31 (i32.const 0))))
            (catch $ocaml_exception
               (local.set $exn)
               (global.set $effect_allowed (local.get $saved_effect_allowed))
               (throw $ocaml_exception (local.get $exn)))
            (catch $javascript_exception
               (local.set $exn (call $caml_wrap_exception))
               (global.set $effect_allowed (local.get $saved_effect_allowed))
               (throw $ocaml_exception (local.get $exn)))))
      (global.set $effect_allowed (local.get $saved_effect_allowed))
      (local.get $res))

   ;; Resume

   (@string $already_resumed "Effect.Continuation_already_resumed")

   (func $resume_fiber
      (param $vfiber (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $fiber (ref $fiber))
      (local $res (ref eq))
      (local $exn (ref eq))
      (local $val (ref eq)) (local $continuation (ref $continuation))
      (local $saved_effect_allowed i32)
      (if (ref.eq (local.get $vfiber) (ref.i31 (i32.const 0)))
         (then
            (call $caml_raise_constant
               (ref.as_non_null
                  (call $caml_named_value (global.get $already_resumed))))))
      (local.set $fiber (ref.cast (ref $fiber) (local.get $vfiber)))
      ;; Effects are handled while the fiber runs; the handlers below
      ;; run in the enclosing context, so restore the flag on each exit
      (local.set $saved_effect_allowed (global.get $effect_allowed))
      (global.set $effect_allowed (i32.const 1))
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
                  (global.set $effect_allowed (local.get $saved_effect_allowed))
                  (return_call_ref $function_1 (local.get $res)
                     (local.tee $f
                        (struct.get $fiber $value (local.get $fiber)))
                     (struct.get $closure 0
                        (ref.cast (ref $closure) (local.get $f)))))
            (local.set $continuation)
            (local.set $val)
            ;; handle effect
            (global.set $effect_allowed (local.get $saved_effect_allowed))
            (struct.set $fiber $continuation (local.get $fiber)
               (local.get $continuation))
(@if (< $ocaml_version (5 6 0))
(@then
            (return_call_ref $function_3
               (local.get $val)
               (array.new_fixed $block 3 (ref.i31 (global.get $cont_tag))
                  (local.get $fiber)
                  (local.get $fiber))
               ;; last_fiber: only ever handed back to %reperform, which
               ;; ignores it (no stack relinking is needed here)
               (local.get $fiber)
               (local.tee $f
                  (struct.get $fiber $effect (local.get $fiber)))
               (struct.get $closure_3 1
                  (ref.cast (ref $closure_3) (local.get $f))))
)
(@else
            (return_call_ref $function_2
               (local.get $val)
               (array.new_fixed $block 3 (ref.i31 (global.get $cont_tag))
                  (local.get $fiber)
                  (local.get $fiber))
               (local.tee $f
                  (struct.get $fiber $effect (local.get $fiber)))
               (struct.get $closure_2 1
                  (ref.cast (ref $closure_2) (local.get $f))))
))))
      ;; handle exception
      (global.set $effect_allowed (local.get $saved_effect_allowed))
      (return_call_ref $function_1 (local.get $exn)
         (local.tee $f
            (struct.get $fiber $exn (local.get $fiber)))
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   (func (export "%resume")
      (param $vfiber (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (param $_tail (ref eq)) (result (ref eq))
      (return_call $resume_fiber
         (local.get $vfiber) (local.get $f) (local.get $v)))

   (func $resume_identity
      (param $x (ref eq)) (param (ref eq)) (result (ref eq))
      (local.get $x))

   (global $resume_identity_closure (ref $closure)
      (struct.new $closure (ref.func $resume_identity)))

   (func $resume_raise
      (param $exn (ref eq)) (param (ref eq)) (result (ref eq))
      (throw $ocaml_exception (local.get $exn)))

   (global $resume_raise_closure (ref $closure)
      (struct.new $closure (ref.func $resume_raise)))

   ;; Resume the continuation, returning [$v] to the perform site.
   (func (export "%continue")
      (param $vfiber (ref eq)) (param $v (ref eq)) (param $_tail (ref eq))
      (result (ref eq))
      (return_call $resume_fiber
         (local.get $vfiber) (global.get $resume_identity_closure)
         (local.get $v)))

   ;; Resume the continuation, raising [$exn] at the perform site.
   (func (export "%discontinue")
      (param $vfiber (ref eq)) (param $exn (ref eq)) (param $_tail (ref eq))
      (result (ref eq))
      (return_call $resume_fiber
         (local.get $vfiber) (global.get $resume_raise_closure)
         (local.get $exn)))

   ;; As %discontinue; backtraces are not supported, so [$bt] is ignored.
   (func (export "%discontinue_with_backtrace")
      (param $vfiber (ref eq)) (param $exn (ref eq)) (param $bt (ref eq))
      (param $_tail (ref eq)) (result (ref eq))
      (return_call $resume_fiber
         (local.get $vfiber) (global.get $resume_raise_closure)
         (local.get $exn)))

   ;; Perform

   (func (export "%reperform")
      (param $eff (ref eq)) (param $continuation (ref eq)) (param $_tail (ref eq))
      (result (ref eq))
      (local $res_0 (ref eq)) (local $res_1 (ref eq))
      (if (i32.eqz (global.get $effect_allowed))
         (then
            ;; No enclosing handler: raise Effect.Unhandled inside the
            ;; inner fiber, at the point where the effect was performed
            (return_call $resume_fiber
               (call $caml_continuation_use_noexc (local.get $continuation))
               (global.get $raise_unhandled_closure)
               (local.get $eff))))
      (suspend $effect (local.get $eff))
      (local.set $res_1)
      (local.set $res_0)
      ;; Forward the resumption to the inner fiber
      (return_call $resume_fiber
         (ref.as_non_null
            (array.get $block
               (ref.cast (ref $block) (local.get $continuation))
               (i32.const 1)))
         (local.get $res_0)
         (local.get $res_1)))

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
      (param $value (ref eq)) (param $exn (ref eq)) (param $effect (ref eq))
      (result (ref eq))
      (struct.new $fiber
         (local.get $value) (local.get $exn) (local.get $effect)
         (global.get $null_value) (i32.const 0)
         (cont.new $continuation (ref.func $initial_cont))))

   (func (export "%with_stack")
      (param $value (ref eq)) (param $exn (ref eq)) (param $effect (ref eq))
      (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (return_call $resume_fiber
         (struct.new $fiber
            (local.get $value) (local.get $exn) (local.get $effect)
            (global.get $null_value) (i32.const 0)
            (cont.new $continuation (ref.func $initial_cont)))
         (local.get $f) (local.get $v)))

   ;; There is no tick source, so the fiber is never preempted: the tick
   ;; handler is dropped and this behaves like [%with_stack].
   (func (export "%with_stack_preemptible")
      (param $value (ref eq)) (param $exn (ref eq)) (param $effect (ref eq))
      (param $tick (ref eq))
      (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (return_call $resume_fiber
         (struct.new $fiber
            (local.get $value) (local.get $exn) (local.get $effect)
            (global.get $null_value) (i32.const 0)
            (cont.new $continuation (ref.func $initial_cont)))
         (local.get $f) (local.get $v)))
))
)
