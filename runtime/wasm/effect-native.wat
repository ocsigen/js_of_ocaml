(module
(@if (and wasi (<> effects "cps"))
(@then
   (import "fail" "caml_raise_constant"
      (func $caml_raise_constant (param (ref eq))))
   (import "fail" "caml_raise_with_arg"
      (func $caml_raise_with_arg (param $tag (ref eq)) (param $arg (ref eq))))
   (import "obj" "caml_fresh_oo_id"
     (func $caml_fresh_oo_id (param (ref eq)) (result (ref eq))))
   (import "obj" "cont_tag" (global $cont_tag i32))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "jslib" "caml_wrap_exception"
      (func $caml_wrap_exception (param externref) (result (ref eq))))
   (import "stdlib" "caml_main_wrapper"
      (global $caml_main_wrapper (mut (ref null $wrapper_func))))
   (import "effect" "effect_allowed" (global $effect_allowed (mut i32)))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field (ref $function_1)))))
   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   ;; Effect types

   (tag $effect (param (ref eq)) (result (ref eq) (ref eq)))

   (type $cont_function (func (param (ref eq) (ref eq)) (result (ref eq))))

   (type $cont (cont $cont_function))

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
            (field $cont (mut (ref $cont))))))

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
         (array.new_fixed $block 3 (ref.i31 (i32.const 248))
            (global.get $effect_unhandled)
            (call $caml_fresh_oo_id (ref.i31 (i32.const 0)))))
      (ref.i31 (i32.const 0)))

   (global $raise_unhandled (ref $closure)
      (struct.new $closure (ref.func $raise_unhandled)))

   (type $func (func (result (ref eq))))
   (type $wrapper_func (func (param (ref $func))))
   (type $func_closure (struct (field (ref $func))))

   (func $wrapper_cont
      (param $f (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call_ref $func
         (local.get $f)
         (struct.get $func_closure 0
            (ref.cast (ref $func_closure) (local.get $f)))))

   (func $unhandled_effect_wrapper (param $start (ref $func))
      (local $cont (ref $cont))
      (local $f (ref eq)) (local $v (ref eq))
      (local $resume_res (tuple (ref eq) (ref $cont)))
      (local.set $cont (cont.new $cont (ref.func $wrapper_cont)))
      (local.set $f (struct.new $func_closure (local.get $start)))
      (local.set $v (ref.i31 (i32.const 0)))
      (loop $loop
         (local.set $resume_res
            (block $handle_effect (result (ref eq) (ref $cont))
               (resume $cont (on $effect $handle_effect)
                  (local.get $f) (local.get $v) (local.get $cont))
               (return)))
         (local.set $cont (tuple.extract 2 1 (local.get $resume_res)))
         (local.set $v (tuple.extract 2 0 (local.get $resume_res)))
         (local.set $f (global.get $raise_unhandled))
         (br $loop)))

   (func $init
      (global.set $caml_main_wrapper (ref.func $unhandled_effect_wrapper)))

   (start $init)

   ;; Resume

   (@string $already_resumed "Effect.Continuation_already_resumed")

   (func $resume (export "%resume")
      (param $vfiber (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (param $tail (ref eq)) (result (ref eq))
      (local $fiber (ref $fiber))
      (local $res (ref eq))
      (local $exn (ref eq))
      (local $resume_res (tuple (ref eq) (ref $cont)))
      (if (ref.eq (local.get $vfiber) (ref.i31 (i32.const 0)))
         (then
            (call $caml_raise_constant
               (ref.as_non_null
                  (call $caml_named_value (global.get $already_resumed))))))
      (local.set $fiber (ref.cast (ref $fiber) (local.get $vfiber)))
      (local.set $exn
         (block $handle_exception (result (ref eq))
            (local.set $resume_res
               (block $handle_effect (result (ref eq) (ref $cont))
                  (local.set $res
                     (try (result (ref eq))
                        (do
                           (resume $cont
                               (on $effect $handle_effect)
                               (local.get $f) (local.get $v)
                               (struct.get $fiber $cont (local.get $fiber))))
(@if (not wasi)
(@then
                        (catch $javascript_exception
                           (br $handle_exception
                              (call $caml_wrap_exception (pop externref))))
))
                        (catch $ocaml_exception
                           (br $handle_exception (pop (ref eq))))))
                  ;; handle return
                  (return_call_ref $function_1 (local.get $res)
                     (local.tee $f
                        (struct.get $fiber $value (local.get $fiber)))
                     (struct.get $closure 0
                        (ref.cast (ref $closure) (local.get $f))))))
            ;; handle effect
            (struct.set $fiber $cont (local.get $fiber)
               (tuple.extract 2 1 (local.get $resume_res)))
            (return_call_ref $function_3
               (tuple.extract 2 0 (local.get $resume_res))
               (array.new_fixed $block 3 (ref.i31 (global.get $cont_tag))
                  (local.get $fiber)
                  (local.get $fiber))
               (local.get $tail)
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
      (param $eff (ref eq)) (param $cont (ref eq)) (param $tail (ref eq))
      (result (ref eq))
      (local $res (tuple (ref eq) (ref eq)))
      (local.set $res (suspend $effect (local.get $eff)))
      (return_call $resume
         (ref.as_non_null
            (array.get $block
               (ref.cast (ref $block) (local.get $cont))
               (i32.const 1)))
         (tuple.extract 2 0 (local.get $res))
         (tuple.extract 2 1 (local.get $res))
         (local.get $tail)))

   (func (export "%perform") (param $eff (ref eq)) (result (ref eq))
      (local $res (tuple (ref eq) (ref eq)))
      (if (i32.eqz (global.get $effect_allowed))
         (then
            (return_call $raise_unhandled
               (local.get $eff) (ref.i31 (i32.const 0)))))
      (local.set $res (suspend $effect (local.get $eff)))
      (return_call_ref $function_1 (tuple.extract 2 1 (local.get $res))
         (tuple.extract 2 0 (local.get $res))
         (struct.get $closure 0
            (ref.cast (ref $closure) (tuple.extract 2 0 (local.get $res))))))

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
         (cont.new $cont (ref.func $initial_cont))))
))
)
