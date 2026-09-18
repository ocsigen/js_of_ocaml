/* Dynamic bindings (OxCaml).

This mirrors runtime/dynamic.c of the OxCaml runtime.

Each fiber carries its local dynamic bindings in field [d] (see the
description of fibers in effect.js): either [null], or an immutable node
[0, dyn, val, next] holding a dynamic key, the bound value, and the nullable
parent node. That is the same layout as the native runtime, so a node (or
[null]) is what [caml_dynamic_freeze_scope] hands out as a scope.

When effects are disabled there is a single fiber, and all bindings live on it.
*/

//Provides: caml_dynamic_current_fiber
//Requires: caml_current_stack
//If: effects
//If: oxcaml
function caml_dynamic_current_fiber() {
  return caml_current_stack;
}

//Provides: caml_dynamic_current_fiber
//If: !effects
//If: oxcaml
var caml_dynamic_root_fiber = { e: 0, p: 0, d: null, t: false };
function caml_dynamic_current_fiber() {
  return caml_dynamic_root_fiber;
}

//Provides: caml_dynamic_parent_fiber
//If: oxcaml
function caml_dynamic_parent_fiber(fiber) {
  return fiber.e || fiber.p;
}

//Provides: caml_dynamic_make
//Requires: caml_fresh_oo_id
//If: oxcaml
function caml_dynamic_make(_unit) {
  return caml_fresh_oo_id(0);
}

//Provides: caml_dynamic_get
//Requires: caml_dynamic_current_fiber, caml_dynamic_parent_fiber
//If: oxcaml
function caml_dynamic_get(dyn) {
  for (
    var fiber = caml_dynamic_current_fiber();
    fiber;
    fiber = caml_dynamic_parent_fiber(fiber)
  ) {
    // Naively, this would traverse the entire binding chain from [fiber] to
    // the root task at every iteration, which is quadratic. Instead, we
    // eagerly advance to the parent fiber when the lexical chain agrees with it.
    var parent = caml_dynamic_parent_fiber(fiber);
    var shared = parent ? parent.d : null;
    for (var node = fiber.d; node !== null && node !== shared; node = node[3]) {
      if (node[1] === dyn) return node[2];
    }
  }
  return null;
}

//Provides: caml_dynamic_push
//Requires: caml_dynamic_current_fiber
//If: oxcaml
function caml_dynamic_push(dyn, val) {
  var fiber = caml_dynamic_current_fiber();
  fiber.d = [0, dyn, val, fiber.d];
  return 0;
}

//Provides: caml_dynamic_pop
//Requires: caml_dynamic_current_fiber
//If: oxcaml
function caml_dynamic_pop(_dyn) {
  // Pops the fiber's most recent binding, which must be for [dyn].
  var fiber = caml_dynamic_current_fiber();
  var head = fiber.d;
  if (head !== null) fiber.d = head[3];
  return 0;
}

//Provides: caml_dynamic_freeze_scope
//Requires: caml_dynamic_current_fiber, caml_dynamic_parent_fiber
//If: oxcaml
function caml_dynamic_freeze_scope(_unit) {
  var fiber = caml_dynamic_current_fiber();
  // Copy bindings from plain fibers on the path to the enclosing task
  var head = null;
  var last = null;
  var parent;
  while (!fiber.t && (parent = caml_dynamic_parent_fiber(fiber))) {
    for (var node = fiber.d; node !== null; node = node[3]) {
      var copy = [0, node[1], node[2], null];
      if (last === null) head = copy;
      else last[3] = copy;
      last = copy;
    }
    fiber = parent;
  }
  // If we reached a task, link it in by reference
  if (last === null) return fiber.d;
  last[3] = fiber.d;
  return head;
}

//Provides: caml_dynamic_use_scope
//Requires: caml_dynamic_current_fiber
//If: oxcaml
function caml_dynamic_use_scope(scope) {
  // Installs [scope] as the current fiber's bindings (it must have none yet)
  // and marks it as a task.
  var fiber = caml_dynamic_current_fiber();
  fiber.t = true;
  fiber.d = scope;
  return 0;
}
