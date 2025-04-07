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
   (import "fail" "caml_raise_constant"
      (func $caml_raise_constant (param (ref eq))))
   (import "fail" "caml_raise_with_arg"
      (func $caml_raise_with_arg (param $tag (ref eq)) (param $arg (ref eq))))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "obj" "caml_fresh_oo_id"
     (func $caml_fresh_oo_id (param (ref eq)) (result (ref eq))))
   (import "obj" "cont_tag" (global $cont_tag i32))
   (import "obj" "object_tag" (global $object_tag i32))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "jslib" "caml_wrap_exception"
      (func $caml_wrap_exception (param externref) (result (ref eq))))
   (import "bindings" "start_fiber" (func $start_fiber (param (ref eq))))
   (import "bindings" "suspend_fiber"
      (func $suspend_fiber
         (param $f funcref) (param $env eqref) (result anyref)))
   (import "bindings" "resume_fiber"
      (func $resume_fiber (param externref) (param (ref eq))))
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field (ref $function_1)))))
   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   ;; Generic fibers

   (type $generic_fiber
      (sub
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq))))))

   (@string $already_resumed "Effect.Continuation_already_resumed")

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

   (global $raise_unhandled (ref $closure)
      (struct.new $closure (ref.func $raise_unhandled)))

   (global $effect_allowed (mut i32) (i32.const 1))

   (func $caml_continuation_use_noexc (export "caml_continuation_use_noexc")
      (param (ref eq)) (result (ref eq))
      (local $cont (ref $block))
      (local $stack (ref eq))
      (drop (block $used (result (ref eq))
         (local.set $cont (ref.cast (ref $block) (local.get 0)))
         (local.set $stack
            (br_on_cast_fail $used (ref eq) (ref $generic_fiber)
               (array.get $block (local.get $cont) (i32.const 1))))
         (array.set $block (local.get $cont) (i32.const 1)
            (ref.i31 (i32.const 0)))
         (return (local.get $stack))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_continuation_use_and_update_handler_noexc")
      (param $cont (ref eq)) (param $hval (ref eq)) (param $hexn (ref eq))
      (param $heff (ref eq)) (result (ref eq))
      (local $stack (ref eq))
      (local $tail (ref $generic_fiber))
      (local.set $stack (call $caml_continuation_use_noexc (local.get $cont)))
      (if (ref.test (ref $generic_fiber) (local.get $stack))
         (then
            (local.set $tail
               (ref.cast (ref $generic_fiber)
                  (array.get $block
                     (ref.cast (ref $block) (local.get $cont))
                        (i32.const 2))))
            (struct.set $generic_fiber $value (local.get $tail)
               (local.get $hval))
            (struct.set $generic_fiber $exn (local.get $tail) (local.get $hexn))
            (struct.set $generic_fiber $effect (local.get $tail)
               (local.get $heff))))
      (local.get $stack))

   (func (export "caml_get_continuation_callstack")
      (param (ref eq) (ref eq)) (result (ref eq))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_is_continuation") (param (ref eq)) (result i32)
      (drop (block $not_continuation (result (ref eq))
         (return
            (ref.eq
               (array.get $block
                  (br_on_cast_fail $not_continuation (ref eq) (ref $block)
                     (local.get 0))
                  (i32.const 0))
               (ref.i31 (global.get $cont_tag))))))
      (i32.const 0))

(@if (= effects "jspi")
(@then
   ;; Apply a function f to a value v, both contained in a pair (f, v)

   (type $pair (struct (field (ref eq)) (field (ref eq))))

   (func $apply_pair (param $p (ref $pair)) (result (ref eq))
      (local $f (ref eq))
      (return_call_ref $function_1 (struct.get $pair 1 (local.get $p))
         (local.tee $f (struct.get $pair 0 (local.get $p)))
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   ;; Low-level primitives

   ;; Capturing the current continuation

   (type $cont_func (func (param (ref $pair)) (param (ref eq))))
   (type $cont (sub (struct (field $cont_func (ref $cont_func)))))

   (type $called_with_continuation
      (func (param (ref $cont)) (param (ref eq))))

   (type $thunk
      (struct (field (ref $called_with_continuation)) (field (ref eq))))

   (type $cont_resume
      (sub final $cont
         (struct
            (field $cont_func (ref $cont_func))
            (field $cont_resolver externref))))

   (func $invoke_promise_resolver (param $p (ref $pair)) (param (ref eq))
      (return_call $resume_fiber
         (struct.get $cont_resume $cont_resolver
            (ref.cast (ref $cont_resume) (local.get 1)))
         (local.get $p)))

   (func $apply_continuation (param $resolver (ref extern)) (param $v (ref eq))
      (local $t (ref $thunk))
      (local.set $t (ref.cast (ref $thunk) (local.get $v)))
      (return_call_ref $called_with_continuation
         (struct.new $cont_resume
            (ref.func $invoke_promise_resolver) (local.get $resolver))
         (struct.get $thunk 1 (local.get $t))
         (struct.get $thunk 0 (local.get $t))))

   (@string $unsupported
      "Effect handlers are not supported: the JavaScript Promise Integration API is not enabled")

   (func $capture_continuation
      (param $f (ref $called_with_continuation))
      (param $v (ref eq))
      (result (ref eq))
      (drop (block $unsupported (result anyref)
         (return_call $apply_pair
            (br_on_cast_fail $unsupported anyref (ref $pair)
               (call $suspend_fiber
                  (ref.func $apply_continuation)
                  (struct.new $thunk (local.get $f) (local.get $v)))))))
      (call $caml_failwith (global.get $unsupported))
      (ref.i31 (i32.const 0)))

   ;; Stack of fibers

   (type $fiber
      (sub final $generic_fiber
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq)))
            (field $cont (mut (ref $cont)))
            (field $next (mut (ref null $fiber))))))

   (func $initial_cont (param $p (ref $pair)) (param (ref eq))
      (return_call $start_fiber (local.get $p)))

   (global $initial_cont (ref $cont) (struct.new $cont (ref.func $initial_cont)))

   (global $stack (mut (ref $fiber))
      (struct.new $fiber
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (global.get $initial_cont)
         (ref.null $fiber)))

   ;; Utility functions moving fibers between a continuation and the
   ;; current stack of fibers

   (func $pop_fiber (result (ref $cont))
      (local $f (ref $fiber))
      (local.set $f
         (ref.as_non_null (struct.get $fiber $next (global.get $stack))))
      (struct.set $fiber $next (global.get $stack) (ref.null $fiber))
      (global.set $stack (local.get $f))
      (struct.get $fiber $cont (local.get $f)))

   (func $push_stack
      (param $head (ref $fiber)) (param $tail (ref $fiber))
      (param $k (ref $cont)) (result (ref $cont))
      (struct.set $fiber $cont (global.get $stack) (local.get $k))
      (struct.set $fiber $next (local.get $tail) (global.get $stack))
      (global.set $stack (local.get $head))
      (struct.get $fiber $cont (local.get $head)))

   ;; Resume

   (type $resume
      (struct
         (field $head (ref eq))
         (field $tail (ref eq))
         (field $data (ref $pair))))

   (func $do_resume (param $k (ref $cont)) (param $vp (ref eq))
      (local $p (ref $resume))
      (local $head (ref $fiber)) (local $tail (ref $fiber))
      (local.set $p (ref.cast (ref $resume) (local.get $vp)))
      (local.set $head
         (ref.cast (ref $fiber) (struct.get $resume $head (local.get $p))))
      (local.set $tail
         (block $available (result (ref $fiber))
            (drop (br_on_cast $available (ref eq) (ref $fiber)
               (struct.get $resume $tail (local.get $p))))
            (local.set $tail (local.get $head))
            ;; Pre OCaml 5.2, last was not populated.
            (block $done
               (loop $loop
                  (local.set $tail
                     (br_on_null $done
                        (struct.get $fiber $next (local.get $tail))))
                  (br $loop)))
            (local.get $tail)))
      (local.set $k
         (call $push_stack (local.get $head) (local.get $tail) (local.get $k)))
      (return_call_ref $cont_func
         (struct.get $resume $data (local.get $p))
         (local.get $k)
         (struct.get $cont $cont_func (local.get $k))))

   (func $resume (export "%resume")
      (param $stack_head (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (param $stack_tail (ref eq)) (result (ref eq))
      (local $k (ref $cont))
      (local $pair (ref $pair))
      (if (ref.eq (local.get $stack_head) (ref.i31 (i32.const 0)))
         (then
            (call $caml_raise_constant
               (ref.as_non_null
                  (call $caml_named_value (global.get $already_resumed))))))
      (return_call $capture_continuation
         (ref.func $do_resume)
         (struct.new $resume
            (local.get $stack_head) (local.get $stack_tail)
            (struct.new $pair (local.get $f) (local.get $v)))))

   ;; Perform

   (type $call_handler_env
      (sub final $closure
         (struct
            (field (ref $function_1))
            (field $handler (ref eq))
            (field $eff (ref eq))
            (field $cont (ref eq)))))

   (func $call_effect_handler
      (param $tail (ref eq)) (param $venv (ref eq)) (result (ref eq))
      (local $env (ref $call_handler_env))
      (local $handler (ref $closure_3))
      (local.set $env (ref.cast (ref $call_handler_env) (local.get $venv)))
      (return_call_ref $function_3
         (struct.get $call_handler_env $eff (local.get $env))
         (struct.get $call_handler_env $cont (local.get $env))
         (local.get $tail)
         (local.tee $handler
            (ref.cast (ref $closure_3)
              (struct.get $call_handler_env $handler (local.get $env))))
         (struct.get $closure_3 1 (local.get $handler))))

   (func $do_perform
      (param $k0 (ref $cont)) (param $eff (ref eq))
      (local $cont (ref $block))
      (local $handler (ref eq))
      (local $k1 (ref $cont))
      (local $last_fiber (ref $fiber))
      (local.set $handler (struct.get $fiber $effect (global.get $stack)))
      (local.set $last_fiber (global.get $stack))
      (struct.set $fiber $cont (local.get $last_fiber) (local.get $k0))
      (local.set $cont
         (array.new_fixed $block 3 (ref.i31 (global.get $cont_tag))
           (local.get $last_fiber) (local.get $last_fiber)))
      (local.set $k1 (call $pop_fiber))
      (return_call_ref $cont_func
         (struct.new $pair
            (struct.new $call_handler_env
               (ref.func $call_effect_handler)
               (local.get $handler)
               (local.get $eff)
               (local.get $cont))
            (local.get $last_fiber))
         (local.get $k1)
         (struct.get $cont $cont_func (local.get $k1))))

   (func (export "%perform") (param $eff (ref eq)) (result (ref eq))
      (if (i32.or (i32.eqz (global.get $effect_allowed))
             (ref.is_null (struct.get $fiber $next (global.get $stack))))
         (then
            (return_call $raise_unhandled
               (local.get $eff) (ref.i31 (i32.const 0)))))
      (return_call $capture_continuation
         (ref.func $do_perform) (local.get $eff)))

   (type $reperform
      (struct
         (field $eff (ref eq))
         (field $cont (ref eq))
         (field $tail (ref eq))))

   (func $do_reperform
      (param $k0 (ref $cont)) (param $vp (ref eq))
      (local $eff (ref eq)) (local $cont (ref $block)) (local $tail (ref $fiber))
      (local $handler (ref eq))
      (local $k1 (ref $cont))
      (local $p (ref $reperform))
      (local $last_fiber (ref $fiber))
      (local.set $p (ref.cast (ref $reperform) (local.get $vp)))
      (local.set $cont
         (ref.cast (ref $block) (struct.get $reperform $cont (local.get $p))))
      (local.set $tail
         (ref.cast (ref $fiber) (struct.get $reperform $tail (local.get $p))))
      (local.set $handler (struct.get $fiber $effect (global.get $stack)))
      (local.set $last_fiber (global.get $stack))
      (struct.set $fiber $cont (local.get $last_fiber) (local.get $k0))
      (struct.set $fiber $next (local.get $tail) (local.get $last_fiber))
      (array.set $block (local.get $cont) (i32.const 2) (local.get $last_fiber))
      (local.set $k1 (call $pop_fiber))
      (return_call_ref $cont_func
         (struct.new $pair
            (struct.new $call_handler_env
               (ref.func $call_effect_handler)
               (local.get $handler)
               (struct.get $reperform $eff (local.get $p))
               (local.get $cont))
            (local.get $last_fiber))
         (local.get $k1)
         (struct.get $cont $cont_func (local.get $k1))))

   (func $reperform (export "%reperform")
      (param $eff (ref eq)) (param $cont (ref eq)) (param $tail (ref eq))
      (result (ref eq))
      (if (ref.is_null (struct.get $fiber $next (global.get $stack)))
         (then
            (return_call $resume
               (call $caml_continuation_use_noexc (local.get $cont))
               (global.get $raise_unhandled)
               (local.get $eff)
               (local.get $tail))))
      (return_call $capture_continuation
         (ref.func $do_reperform)
         (struct.new $reperform
            (local.get $eff) (local.get $cont) (local.get $tail))))

   ;; Allocate a stack

   (func $call_handler (param $f (ref eq)) (param $x (ref eq))
      ;; Propagate a value or an exception to the parent fiber
      (local $cont (ref $cont))
      (return_call_ref $cont_func
         (struct.new $pair (local.get $f) (local.get $x))
         (local.tee $cont (call $pop_fiber))
         (struct.get $cont $cont_func (local.get $cont))))

   (func (export "caml_start_fiber") (param $p eqref)
      ;; Start executing some code in a new fiber
      (local $exn (ref eq))
      (local $res (ref eq))
      (local.set $res
         (try (result (ref eq))
            (do
               (try (result (ref eq))
                  (do
                     (call $apply_pair (ref.cast (ref $pair) (local.get $p))))
                  (catch $javascript_exception
                     (throw $ocaml_exception
                        (call $caml_wrap_exception (pop externref))))))
            (catch $ocaml_exception
               (local.set $exn (pop (ref eq)))
               (return_call $call_handler
                  (struct.get $fiber $exn (global.get $stack))
                  (local.get $exn)))))
      (return_call $call_handler
         (struct.get $fiber $value (global.get $stack))
         (local.get $res)))

   (func (export "caml_alloc_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (result (ref eq))
      (struct.new $fiber
         (local.get $hv) (local.get $hx) (local.get $hf)
         (global.get $initial_cont)
         (ref.null $fiber)))
))

(@if (= effects "cps")
(@then
   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $function_4
      (func (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq))
         (result (ref eq))))
   (type $cps_closure (sub (struct (field (ref $function_2)))))
   (type $cps_closure_0 (sub (struct (field (ref $function_1)))))
   (type $cps_closure_3
      (sub $cps_closure
         (struct (field (ref $function_2)) (field (ref $function_4)))))

   (type $iterator
     (sub final $closure
       (struct
          (field (ref $function_1))
          (field $i (mut i32))
          (field $args (ref $block)))))

   (type $exn_stack
      (struct (field $h (ref eq)) (field $next (ref null $exn_stack))))

   (type $cps_fiber
      (sub final $generic_fiber
         (struct
            (field $value (mut (ref eq)))
            (field $exn (mut (ref eq)))
            (field $effect (mut (ref eq)))
            (field $cont (mut (ref eq)))
            (field $exn_stack (mut (ref null $exn_stack)))
            (field $next (mut (ref null $cps_fiber))))))

   (global $cps_fiber_stack (mut (ref $cps_fiber))
      (struct.new $cps_fiber
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.null $exn_stack)
         (ref.null $cps_fiber)))

   (func (export "caml_push_trap") (param $h (ref eq)) (result (ref eq))
      (struct.set $cps_fiber $exn_stack
         (global.get $cps_fiber_stack)
         (struct.new $exn_stack (local.get $h)
            (struct.get $cps_fiber $exn_stack (global.get $cps_fiber_stack))))
      (ref.i31 (i32.const 0)))

   (func $raise_exception
      (param $exn (ref eq)) (param (ref eq)) (result (ref eq))
      (throw $ocaml_exception (local.get $exn)))

   (global $raise_exception (ref eq)
      (struct.new $closure (ref.func $raise_exception)))

   (func (export "caml_pop_trap") (result (ref eq))
      (local $top (ref $exn_stack))
      (block $empty
         (local.set $top
            (br_on_null $empty
               (struct.get $cps_fiber $exn_stack (global.get $cps_fiber_stack))))
         (struct.set $cps_fiber $exn_stack (global.get $cps_fiber_stack)
            (struct.get $exn_stack $next (local.get $top)))
         (return (struct.get $exn_stack $h (local.get $top))))
      (global.get $raise_exception))

   (func (export "caml_maybe_attach_backtrace")
      (param $exn (ref eq)) (param (ref eq)) (result (ref eq))
      (local.get $exn))

   (func $identity (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local.get 0))

   (global $identity (ref $closure) (struct.new $closure (ref.func $identity)))

   (func $trampoline_iterator
      (param $f (ref eq)) (param $venv (ref eq)) (result (ref eq))
      (local $env (ref $iterator))
      (local $i i32) (local $args (ref $block))
      (local.set $env (ref.cast (ref $iterator) (local.get $venv)))
      (local.set $i (struct.get $iterator $i (local.get $env)))
      (local.set $args (struct.get $iterator $args (local.get $env)))
      (struct.set $iterator $i (local.get $env)
         (i32.add (local.get $i) (i32.const 1)))
      (return_call_ref $function_2
         (array.get $block (local.get $args) (local.get $i))
         (if (result (ref eq))
             (i32.eq (i32.add (local.get $i) (i32.const 1))
                (array.len (local.get $args)))
            (then (global.get $identity))
            (else (local.get $env)))
         (local.get $f)
         (struct.get $cps_closure 0
            (ref.cast (ref $cps_closure) (local.get $f)))))

   (func $apply_iterator
      (param $f (ref eq)) (param $venv (ref eq)) (result (ref eq))
      (local $env (ref $iterator))
      (local $i i32) (local $args (ref $block))
      (local.set $env (ref.cast (ref $iterator) (local.get $venv)))
      (local.set $i (struct.get $iterator $i (local.get $env)))
      (local.set $args (struct.get $iterator $args (local.get $env)))
      (struct.set $iterator $i (local.get $env)
         (i32.add (local.get $i) (i32.const 1)))
      (return_call_ref $function_2
         (array.get $block (local.get $args) (local.get $i))
         (if (result (ref eq))
             (i32.eq (i32.add (local.get $i) (i32.const 2))
                (array.len (local.get $args)))
            (then
               (array.get $block (local.get $args)
                  (i32.add (local.get $i) (i32.const 1))))
            (else
               (local.get $env)))
         (local.get $f)
         (struct.get $cps_closure 0
            (ref.cast (ref $cps_closure) (local.get $f)))))

   (func (export "caml_apply_continuation")
      (param $args (ref eq)) (result (ref eq))
      (struct.new $iterator
         (ref.func $apply_iterator)
         (i32.const 1)
         (ref.cast (ref $block) (local.get $args))))

   (func $dummy_cps_fun
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (unreachable))

   (func $caml_trampoline (export "caml_cps_trampoline")
      (param $f (ref eq)) (param $vargs (ref eq)) (result (ref eq))
      (local $args (ref $block))
      (local $i i32) (local $res (ref eq))
      (local $exn (ref eq)) (local $top (ref $exn_stack))
      (local $saved_fiber_stack (ref $cps_fiber))
      (local.set $saved_fiber_stack (global.get $cps_fiber_stack))
      (global.set $cps_fiber_stack
         (struct.new $cps_fiber
            (ref.i31 (i32.const 0))
            (ref.i31 (i32.const 0))
            (ref.i31 (i32.const 0))
            (ref.i31 (i32.const 0))
            (ref.null $exn_stack)
            (ref.null $cps_fiber)))
      (local.set $args (ref.cast (ref $block) (local.get $vargs)))
      (local.set $exn
         (try (result (ref eq))
            (do
               (local.set $res
                  (if (result (ref eq))
                      (i32.eq (array.len (local.get $args)) (i32.const 1))
                     (then
                        (call_ref $function_1 (global.get $identity)
                           (local.get $f)
                           (struct.get $cps_closure_0 0
                              (ref.cast (ref $cps_closure_0) (local.get $f)))))
                     (else
                        (call_ref $function_2
                           (array.get $block (local.get $args) (i32.const 1))
                           (if (result (ref eq))
                               (i32.eq (i32.const 2)
                                       (array.len (local.get $args)))
                              (then (global.get $identity))
                              (else
                                 (struct.new $iterator
                                    (ref.func $trampoline_iterator)
                                    (i32.const 2)
                                    (local.get $args))))
                           (local.get $f)
                           (struct.get $cps_closure 0
                              (ref.cast (ref $cps_closure) (local.get $f)))))))
               (global.set $cps_fiber_stack (local.get $saved_fiber_stack))
               (return (local.get $res)))
            (catch $ocaml_exception
               (pop (ref eq)))
            (catch $javascript_exception
               (call $caml_wrap_exception (pop externref)))))
      (loop $loop
         (block $empty
            (local.set $top
               (br_on_null $empty
                  (struct.get $cps_fiber $exn_stack
                     (global.get $cps_fiber_stack))))
            (struct.set $cps_fiber $exn_stack
               (global.get $cps_fiber_stack)
               (struct.get $exn_stack $next (local.get $top)))
            (local.set $f (struct.get $exn_stack $h (local.get $top)))
            (try
               (do
                  (local.set $res
                     (call_ref $function_1
                        (local.get $exn)
                        (local.get $f)
                        (struct.get $closure 0
                           (ref.cast (ref $closure) (local.get $f)))))
                  (global.set $cps_fiber_stack (local.get $saved_fiber_stack))
                  (return (local.get $res)))
               (catch $ocaml_exception
                  (local.set $exn (pop (ref eq)))
                  (br $loop))
               (catch $javascript_exception
                  (local.set $exn (call $caml_wrap_exception (pop externref)))
                  (br $loop)))))
      (global.set $cps_fiber_stack (local.get $saved_fiber_stack))
      (throw $ocaml_exception (local.get $exn)))

   (func $caml_pop_fiber (result (ref eq))
      (local $f (ref $cps_fiber))
      (local.set $f
         (ref.as_non_null
            (struct.get $cps_fiber $next (global.get $cps_fiber_stack))))
      (struct.set $cps_fiber $next (global.get $cps_fiber_stack)
         (ref.null $cps_fiber))
      (global.set $cps_fiber_stack (local.get $f))
      (struct.get $cps_fiber $cont (local.get $f)))

   (func $caml_resume_stack (export "caml_resume_stack")
      (param $vhead (ref eq)) (param $vtail (ref eq)) (param $k (ref eq))
      (result (ref eq))
      (local $head (ref $cps_fiber)) (local $tail (ref $cps_fiber))
      (drop (block $already_resumed (result (ref eq))
         (local.set $head
            (br_on_cast_fail $already_resumed (ref eq) (ref $cps_fiber)
               (local.get $vhead)))
         (local.set $tail
            (block $available (result (ref $cps_fiber))
               (drop (br_on_cast $available (ref eq) (ref $cps_fiber)
                  (local.get $vtail)))
               (local.set $tail (local.get $head))
               ;; Pre OCaml 5.2, last was not populated.
               (block $done
                  (loop $loop
                     (local.set $tail
                        (br_on_null $done
                           (struct.get $cps_fiber $next (local.get $tail))))
                     (br $loop)))
               (local.get $tail)))
         (struct.set $cps_fiber $cont (global.get $cps_fiber_stack)
            (local.get $k))
         (struct.set $cps_fiber $next (local.get $tail)
            (global.get $cps_fiber_stack))
         (global.set $cps_fiber_stack (local.get $head))
         (return (struct.get $cps_fiber $cont (local.get $head)))))
      (call $caml_raise_constant
         (ref.as_non_null
            (call $caml_named_value (global.get $already_resumed))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_perform_effect")
      (param $eff (ref eq)) (param $k0 (ref eq)) (result (ref eq))
      (local $handler (ref eq)) (local $k1 (ref eq))
      (local $cont (ref $block))
      (local $last_fiber (ref $cps_fiber))
      (if (ref.is_null
             (struct.get $cps_fiber $next (global.get $cps_fiber_stack)))
         (then
            (return_call $raise_unhandled
               (local.get $eff) (ref.i31 (i32.const 0)))))
      (local.set $handler
         (struct.get $cps_fiber $effect (global.get $cps_fiber_stack)))
      (local.set $last_fiber (global.get $cps_fiber_stack))
      (struct.set $cps_fiber $cont (local.get $last_fiber) (local.get $k0))
      (local.set $cont
         (array.new_fixed $block 3 (ref.i31 (global.get $cont_tag))
            (local.get $last_fiber) (local.get $last_fiber)))
      (local.set $k1 (call $caml_pop_fiber))
      (return_call_ref $function_4
         (local.get $eff) (local.get $cont) (local.get $last_fiber)
         (local.get $k1) (local.get $handler)
         (struct.get $cps_closure_3 1
            (ref.cast (ref $cps_closure_3) (local.get $handler)))))

   (func (export "caml_reperform_effect")
      (param $eff (ref eq)) (param $vcont (ref eq)) (param $vtail (ref eq))
      (param $k0 (ref eq)) (result (ref eq))
      (local $handler (ref eq)) (local $k1 (ref eq))
      (local $cont (ref $block))
      (local $tail (ref $cps_fiber)) (local $last_fiber (ref $cps_fiber))
      (if (ref.is_null
             (struct.get $cps_fiber $next (global.get $cps_fiber_stack)))
         (then
            (drop
               (call $caml_resume_stack
                  (call $caml_continuation_use_noexc (local.get $vcont))
                  (local.get $vtail)
                  (local.get $k0)))
            (return_call $raise_unhandled
               (local.get $eff) (ref.i31 (i32.const 0)))))
      (local.set $cont (ref.cast (ref $block) (local.get $vcont)))
      (local.set $tail (ref.cast (ref $cps_fiber) (local.get $vtail)))
      (local.set $handler
         (struct.get $cps_fiber $effect (global.get $cps_fiber_stack)))
      (local.set $last_fiber (global.get $cps_fiber_stack))
      (struct.set $cps_fiber $cont (local.get $last_fiber) (local.get $k0))
      (struct.set $cps_fiber $next (local.get $tail) (local.get $last_fiber))
      (array.set $block (local.get $cont) (i32.const 2) (local.get $last_fiber))
      (local.set $k1 (call $caml_pop_fiber))
      (return_call_ref $function_4
         (local.get $eff) (local.get $cont) (local.get $last_fiber)
         (local.get $k1)
         (local.get $handler)
         (struct.get $cps_closure_3 1
            (ref.cast (ref $cps_closure_3) (local.get $handler)))))

   (func $cps_call_handler
      (param $handler (ref eq)) (param $x (ref eq)) (result (ref eq))
      (return_call_ref $function_2
         (local.get $x)
         (call $caml_pop_fiber)
         (local.get $handler)
         (struct.get $cps_closure 0
            (ref.cast (ref $cps_closure) (local.get $handler)))))

   (func $value_handler (param $x (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $cps_call_handler
          (struct.get $cps_fiber $value (global.get $cps_fiber_stack))
          (local.get $x)))

   (global $value_handler (ref $closure)
      (struct.new $closure (ref.func $value_handler)))

   (func $exn_handler (param $x (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $cps_call_handler
          (struct.get $cps_fiber $exn (global.get $cps_fiber_stack))
          (local.get $x)))

   (global $exn_handler (ref $closure)
      (struct.new $closure (ref.func $exn_handler)))

   (func (export "caml_alloc_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (result (ref eq))
      (struct.new $cps_fiber
         (local.get $hv) (local.get $hx) (local.get $hf)
         (global.get $value_handler)
         (struct.new $exn_stack (global.get $exn_handler) (ref.null $exn_stack))
         (ref.null $cps_fiber)))
))

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
               (local.set $exn (pop (ref eq)))
               (global.set $effect_allowed (local.get $saved_effect_allowed))
               (throw $ocaml_exception (local.get $exn)))
            (catch $javascript_exception
               (local.set $exn (call $caml_wrap_exception (pop externref)))
               (global.set $effect_allowed (local.get $saved_effect_allowed))
               (throw $ocaml_exception (local.get $exn)))))
      (global.set $effect_allowed (local.get $saved_effect_allowed))
      (local.get $res))
)
