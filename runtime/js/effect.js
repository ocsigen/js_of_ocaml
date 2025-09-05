/*
To deal with effects, the execution context is intuitively composed of
a stack of fibers. Each fiber has a current low-level continuation
(one-argument JavaScript function), a stack of exception handlers and
a triple of handlers, which are invoked when the fiber terminates
(either with a value or an exception) or when an effect is performed.
The low-level continuation of the topmost fiber (which is currently
executing) is passed from function to function as an additional
argument. Its stack of exception handlers is stored in
[caml_current_stack.x].
Exception handlers are pushed into this stack
when entering a [try ... with ...] and popped on exit.
Handlers are stored in [caml_current_stack.h]
and the remaining fibers are stored in [caml_current_stack.e].
To install an effect handler, we push a new fiber into the execution context.

We have basically the following type for reified continuations (type
[continuation] in module [Effect] of the standard library):

  type ('a, 'b) continuation = ('a, 'b) stack ref

  and (_, _) stack =
      Cons : ('b -> unit) *             (* low-level continuation *)
             (exn -> unit) list *       (* exception handlers *)
             ('b, 'c) handler *
             ('a, 'b) stack
             -> ('a, 'c) stack
    | Empty : ('a, 'a) stack

  and ('a,'b) handler =   (* As in module Effect from the standard library *)
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c Effect.t -> (('c,'b) continuation -> 'b) option }

Continuations are one-shot. A continuation [ref Empty] has already
been resumed.

A continuation is basically composed of a list of fibers, which each
has its low-level continuation, its stack of exception handlers and a
triple of handlers to deal with when the fiber terminates or an
effect is performed. When resuming a continuation, the innermost fiber
is resumed first.

The handlers are CPS-transformed functions: they actually take an
additional parameter which is the current low-level continuation.
*/

//Provides: caml_current_stack
//If: effects
// This has the shape {k, x, h, e} where
// - h is a triple of handlers (see effect.ml)
// - k is the low level continuation
// - x is the exception stack
// - e is the fiber stack of the parent fiber.
var caml_current_stack = { k: 0, x: 0, h: 0, e: 0 };

//Provides: caml_push_trap
//Requires: caml_current_stack
//If: effects
function caml_push_trap(handler) {
  caml_current_stack.x = { h: handler, t: caml_current_stack.x };
}

//Provides: caml_pop_trap
//Requires: caml_current_stack
//If: effects
function caml_pop_trap() {
  if (!caml_current_stack.x)
    return function (x) {
      throw x;
    };
  var h = caml_current_stack.x.h;
  caml_current_stack.x = caml_current_stack.x.t;
  return h;
}

//Provides: caml_raise_unhandled
//Requires: caml_make_unhandled_effect_exn
//If: effects
//Version: >= 5.0
function caml_raise_unhandled(eff) {
  var exn = caml_make_unhandled_effect_exn(eff);
  throw exn;
}

//Provides:caml_resume_stack
//Requires: caml_named_value, caml_raise_constant
//Requires: caml_pop_fiber, caml_current_stack
//If: effects
//Version: >= 5.0
function caml_resume_stack(stack, last, k) {
  if (!stack)
    caml_raise_constant(
      caml_named_value("Effect.Continuation_already_resumed"),
    );
  if (last === 0) {
    last = stack;
    // Pre OCaml 5.2, last was not populated.
    while (last.e !== 0) last = last.e;
  }
  caml_current_stack.k = k;
  last.e = caml_current_stack;
  caml_current_stack = stack;
  return stack.k;
}

//Provides: caml_pop_fiber
//Requires: caml_current_stack
//If: effects
//Version: >= 5.0
function caml_pop_fiber() {
  // Move to the parent fiber, returning the parent's low-level continuation
  var c = caml_current_stack.e;
  caml_current_stack.e = 0;
  caml_current_stack = c;
  return c.k;
}

//Provides: caml_make_unhandled_effect_exn
//Requires: caml_named_value, caml_string_of_jsbytes, caml_fresh_oo_id
//If: effects
//Version: >= 5.0
function caml_make_unhandled_effect_exn(eff) {
  var exn = caml_named_value("Effect.Unhandled");
  if (exn) exn = [0, exn, eff];
  else {
    exn = [
      248,
      caml_string_of_jsbytes("Effect.Unhandled"),
      caml_fresh_oo_id(0),
    ];
  }
  return exn;
}

//Provides: caml_perform_effect
//Requires: caml_pop_fiber, caml_stack_check_depth, caml_trampoline_return
//Requires: caml_make_unhandled_effect_exn, caml_current_stack
//Requires: caml_get_cps_fun
//If: effects
//Version: >= 5.0
function caml_perform_effect(eff, k0) {
  if (caml_current_stack.e === 0) {
    var exn = caml_make_unhandled_effect_exn(eff);
    throw exn;
  }
  // Get current effect handler
  var handler = caml_current_stack.h[3];
  var last_fiber = caml_current_stack;
  last_fiber.k = k0;
  var cont = [245 /*continuation*/, last_fiber, last_fiber];
  // Move to parent fiber and execute the effect handler there
  // The handler is defined in Stdlib.Effect, so we know that the arity matches
  var k1 = caml_pop_fiber();
  return caml_stack_check_depth()
    ? caml_get_cps_fun(handler)(eff, cont, last_fiber, k1)
    : caml_trampoline_return(handler, [eff, cont, last_fiber, k1]);
}

//Provides: caml_reperform_effect
//Requires: caml_pop_fiber, caml_stack_check_depth, caml_trampoline_return
//Requires: caml_make_unhandled_effect_exn, caml_current_stack
//Requires: caml_resume_stack, caml_continuation_use_noexc
//Requires: caml_get_cps_fun
//If: effects
//Version: >= 5.0
function caml_reperform_effect(eff, cont, last, k0) {
  if (caml_current_stack.e === 0) {
    var exn = caml_make_unhandled_effect_exn(eff);
    var stack = caml_continuation_use_noexc(cont);
    caml_resume_stack(stack, last, k0);
    throw exn;
  }
  // Get current effect handler
  var handler = caml_current_stack.h[3];
  var last_fiber = caml_current_stack;
  last_fiber.k = k0;
  last.e = last_fiber;
  cont[2] = last_fiber;
  // Move to parent fiber and execute the effect handler there
  // The handler is defined in Stdlib.Effect, so we know that the arity matches
  var k1 = caml_pop_fiber();
  return caml_stack_check_depth()
    ? caml_get_cps_fun(handler)(eff, cont, last_fiber, k1)
    : caml_trampoline_return(handler, [eff, cont, last_fiber, k1]);
}

//Provides: caml_get_cps_fun
//If: effects
//If: !doubletranslate
function caml_get_cps_fun(f) {
  return f;
}

//Provides: caml_get_cps_fun
//If: effects
//If: doubletranslate
function caml_get_cps_fun(f) {
  // This function is only used to get the effect handler. If the
  // effect handler has no CPS function, we know that we can directly
  // call the direct version instead.
  return f.cps ? f.cps : f;
}

//Provides: caml_alloc_stack
//Requires: caml_pop_fiber, caml_call_gen, caml_stack_check_depth, caml_trampoline_return
//Requires: caml_call_gen_cps, caml_current_stack
//If: effects
//Version: >= 5.0
function caml_alloc_stack_call(f, x) {
  var args = [x, caml_pop_fiber()];
  return caml_stack_check_depth()
    ? caml_call_gen_cps(f, args)
    : caml_trampoline_return(f, args, 0);
}
function caml_alloc_stack_hval(x) {
  // Call [hv] in the parent fiber
  var f = caml_current_stack.h[1];
  return caml_alloc_stack_call(f, x);
}
function caml_alloc_stack_hexn(e) {
  // Call [hx] in the parent fiber
  var f = caml_current_stack.h[2];
  return caml_alloc_stack_call(f, e);
}
function caml_alloc_stack(hv, hx, hf) {
  var handlers = [0, hv, hx, hf];
  return {
    k: caml_alloc_stack_hval,
    x: { h: caml_alloc_stack_hexn, t: 0 },
    h: handlers,
    e: 0,
  };
}

//Provides: caml_alloc_stack
//If: !effects
//Version: >= 5.0
function caml_alloc_stack(hv, hx, hf) {
  return 0;
}

//Provides: caml_continuation_use_noexc
//Version: >= 5.0
function caml_continuation_use_noexc(cont) {
  var stack = cont[1];
  cont[1] = 0;
  return stack;
}

//Provides: caml_continuation_use_and_update_handler_noexc
//Requires: caml_continuation_use_noexc
//Version: >= 5.0
function caml_continuation_use_and_update_handler_noexc(
  cont,
  hval,
  hexn,
  heff,
) {
  var stack = caml_continuation_use_noexc(cont);
  if (stack === 0) return stack;
  var last = cont[2];
  last.h[1] = hval;
  last.h[2] = hexn;
  last.h[3] = heff;
  return stack;
}

//Provides: caml_get_continuation_callstack
//Version: >= 5.0
function caml_get_continuation_callstack() {
  return [0];
}

//Provides: caml_ml_condition_new
//Version: >= 5.0
function caml_ml_condition_new(unit) {
  return { condition: 1 };
}

//Provides: caml_ml_condition_wait
//Version: >= 5.0
function caml_ml_condition_wait(t, mutext) {
  return 0;
}

//Provides: caml_ml_condition_broadcast
//Version: >= 5.0
function caml_ml_condition_broadcast(t) {
  return 0;
}

//Provides: caml_ml_condition_signal
//Version: >= 5.0
function caml_ml_condition_signal(t) {
  return 0;
}

//Provides: jsoo_effect_not_supported
//Requires: caml_failwith
//!If: effects
//Version: >= 5.0
function jsoo_effect_not_supported() {
  caml_failwith("Effect handlers are not supported");
}

//Provides: caml_resume
//Requires:caml_stack_depth, caml_call_gen_cps, caml_current_stack, caml_wrap_exception, caml_resume_stack
//If: effects
//If: doubletranslate
//Version: >= 5.0
function caml_resume(f, arg, stack, last) {
  var saved_stack_depth = caml_stack_depth;
  var saved_current_stack = caml_current_stack;
  try {
    caml_current_stack = { k: 0, x: 0, h: 0, e: 0 };
    var k = caml_resume_stack(stack, last, function (x) {
      return x;
    });
    /* Note: f is not an ordinary function but a (direct-style, CPS) closure pair */
    var res = { joo_tramp: f, joo_args: [arg, k], joo_direct: 0 };
    do {
      /* Avoids trampolining too often while still avoiding stack overflow. See
         [caml_callback]. */
      caml_stack_depth = 40;
      try {
        res = res.joo_direct
          ? res.joo_tramp.apply(null, res.joo_args)
          : caml_call_gen_cps(res.joo_tramp, res.joo_args);
      } catch (e) {
        /* Handle exception coming from JavaScript or from the runtime. */
        if (!caml_current_stack.x) throw e;
        var handler = caml_current_stack.x.h;
        caml_current_stack.x = caml_current_stack.x.t;
        res = {
          joo_tramp: handler,
          joo_args: [caml_wrap_exception(e)],
          joo_direct: 1,
        };
      }
    } while (res?.joo_args);
    return res;
  } finally {
    caml_stack_depth = saved_stack_depth;
    caml_current_stack = saved_current_stack;
  }
}

//Provides: caml_cps_closure
//If: effects
//If: doubletranslate
function caml_cps_closure(direct_f, cps_f) {
  direct_f.cps = cps_f;
  return direct_f;
}

//Provides: caml_assume_no_perform
//Requires: caml_callback
//If: effects
//If: !doubletranslate
function caml_assume_no_perform(f) {
  return caml_callback(f, [0]);
}
