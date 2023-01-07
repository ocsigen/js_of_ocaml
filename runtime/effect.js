/*
To deal with effects, the execution context is intuitively composed of
a stack of fibers. Each fiber has a current low-level continuation
(one-argument JavaScript function), a stack of exception handlers and
a triple of handlers, which are invoked when the fiber terminates
(either with a value or an exception) or when an effect is performed.
The low-level continuation of the topmost fiber (which is currently
executing) is passed from function to function as an additional
argument. Its stack of exception handlers is stored in
[caml_exn_stack]. Exception handlers are pushed into this stack when
entering a [try ... with ...] and popped on exit. Then, handlers and
the remaining fibers are stored in [caml_fiber_stack]. To install an
effect handler, we push a new fiber into the execution context.

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

//Provides: caml_exn_stack
//If: effects
// This is an OCaml list of exception handlers
var caml_exn_stack = 0;

//Provides: caml_push_trap
//Requires: caml_exn_stack
//If: effects
function caml_push_trap(handler) {
  caml_exn_stack=[0,handler,caml_exn_stack];
}

//Provides: caml_pop_trap
//Requires: caml_exn_stack
//If: effects
function caml_pop_trap() {
  if (!caml_exn_stack) return function(x){throw x;}
  var h = caml_exn_stack[1];
  caml_exn_stack=caml_exn_stack[2];
  return h
}

//Provides: caml_fiber_stack
//If: effects
// This has the shape {h, r:{k, x, e}} where h is a triple of handlers
// (see effect.js) and k, x and e are the saved continuation,
// exception stack and fiber stack of the parent fiber.
var caml_fiber_stack;

//Provides:caml_resume_stack
//Requires: caml_named_value, caml_raise_constant, caml_exn_stack, caml_fiber_stack
//If: effects
function caml_resume_stack(stack, k) {
  if (!stack) caml_raise_constant
                 (caml_named_value("Effect.Continuation_already_resumed"));
  // Update the execution context with the stack of fibers in [stack] in
  // order to resume the continuation
  do {
    caml_fiber_stack =
      {h:stack[3], r:{k:k, x:caml_exn_stack, e:caml_fiber_stack}};
    k = stack[1];
    caml_exn_stack = stack[2];
    stack = stack[4];
  } while (stack)
  return k;
}

//Provides: caml_pop_fiber
//Requires: caml_exn_stack, caml_fiber_stack
//If: effects
function caml_pop_fiber() {
  // Move to the parent fiber, returning the parent's low-level continuation
  var rem = caml_fiber_stack.r;
  caml_exn_stack = rem.x;
  caml_fiber_stack = rem.e;
  return rem.k;
}

//Provides: caml_perform_effect
//Requires: caml_pop_fiber, caml_stack_check_depth, caml_trampoline_return, caml_exn_stack, caml_fiber_stack
//If: effects
function caml_perform_effect(eff, cont, k0) {
  // Allocate a continuation if we don't already have one
  if (!cont) cont = [245 /*continuation*/, 0];
  // Get current effect handler
  var handler = caml_fiber_stack.h[3];
  // Cons the current fiber onto the continuation:
  //   cont := Cons (k, exn_stack, handlers, !cont)
  cont[1] = [0,k0,caml_exn_stack,caml_fiber_stack.h,cont[1]];
  // Move to parent fiber and execute the effect handler there
  // The handler is defined in Stdlib.Effect, so we know that the arity matches
  var k1 = caml_pop_fiber();
  return caml_stack_check_depth()?handler(eff,cont,k1,k1)
         :caml_trampoline_return(handler,[eff,cont,k1,k1]);
}

//Provides: caml_alloc_stack
//Requires: caml_pop_fiber, caml_fiber_stack, caml_call_gen, caml_stack_check_depth, caml_trampoline_return
//If: effects
function caml_alloc_stack(hv, hx, hf) {
  function call(i, x) {
    var f=caml_fiber_stack.h[i];
    var args = [x, caml_pop_fiber()];
    return caml_stack_check_depth()?caml_call_gen(f,args)
           :caml_trampoline_return(f,args);
  }
  function hval(x) {
    // Call [hv] in the parent fiber
    return call(1, x);
  }
  function hexn(e) {
    // Call [hx] in the parent fiber
    return call(2, e);
  }
  return [0, hval, [0, hexn, 0], [0, hv, hx, hf], 0];
}

//Provides: caml_alloc_stack
//If: !effects
function caml_alloc_stack(hv, hx, hf) {
  return 0;
}

//Provides: caml_continuation_use_noexc
function caml_continuation_use_noexc(cont) {
  var stack=cont[1];
  cont[1]=0;
  return stack;
}

//Provides: caml_continuation_use_and_update_handler_noexc
//Requires: caml_continuation_use_noexc
function caml_continuation_use_and_update_handler_noexc(cont, hval, hexn, heff) {
  var stack = caml_continuation_use_noexc(cont);
  stack[3] = [0, hval, hexn, heff];
  return stack;
}

//Provides: caml_get_continuation_callstack
function caml_get_continuation_callstack () { return [0]; }

//Provides: caml_ml_condition_new
function caml_ml_condition_new(unit){
    return {condition:1};
}

//Provides: caml_ml_condition_wait
function caml_ml_condition_wait(t,mutext){
    return 0;
}

//Provides: caml_ml_condition_broadcast
function caml_ml_condition_broadcast(t){
    return 0;
}

//Provides: caml_ml_condition_signal
function caml_ml_condition_signal(t){
    return 0;
}

//Provides: jsoo_effect_not_supported
//Requires: caml_failwith
//!If: effects
function jsoo_effect_not_supported(){
  caml_failwith("Effect handlers are not supported");
}
