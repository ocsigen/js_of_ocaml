// Js_of_ocaml library
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Jslib

//Provides: caml_js_pure_expr const
//Requires: caml_callback
function caml_js_pure_expr (f) { return caml_callback(f, [0]); }

//Provides: caml_js_set (mutable, const, mutable)
function caml_js_set(o,f,v) { o[f]=v;return 0}
//Provides: caml_js_get (mutable, const)
function caml_js_get(o,f) { return o[f]; }
//Provides: caml_js_delete (mutable, const)
function caml_js_delete(o,f) { delete o[f]; return 0}

//Provides: caml_js_instanceof (const, const)
function caml_js_instanceof(o,c) { return (o instanceof c) ? 1 : 0; }

//Provides: caml_js_typeof (const)
function caml_js_typeof(o) { return typeof o; }

//Provides:caml_trampoline
function caml_trampoline(res) {
  var c = 1;
  while(res && res.joo_tramp){
    res = res.joo_tramp.apply(null, res.joo_args);
    c++;
  }
  return res;
}

//Provides:caml_trampoline_return
function caml_trampoline_return(f,args) {
  return {joo_tramp:f,joo_args:args};
}

//Provides:caml_stack_depth
//If: effects
var caml_stack_depth = 0;

//Provides:caml_stack_check_depth
//If: effects
//Requires:caml_stack_depth
function caml_stack_check_depth() {
    return --caml_stack_depth > 0;
}

//Provides: caml_callback
//If: !effects
//Requires:caml_call_gen
var caml_callback = caml_call_gen;

//Provides: caml_callback
//If: effects
//Requires:caml_stack_depth, caml_call_gen, caml_exn_stack, caml_fiber_stack, caml_wrap_exception, caml_resume_stack, caml_fresh_oo_id, caml_named_value, caml_raise_with_arg, caml_string_of_jsbytes
//Requires: caml_raise_constant
function caml_callback(f,args) {
  function uncaught_effect_handler(eff,k,ms) {
    // Resumes the continuation k by raising exception Unhandled.
    caml_resume_stack(k[1],ms);
    var exn = caml_named_value("Effect.Unhandled");
    if(exn) caml_raise_with_arg(exn, eff);
    else {
      exn = [248,caml_string_of_jsbytes("Effect.Unhandled"), caml_fresh_oo_id(0)];
      caml_raise_constant(exn);
    }
  }
  var saved_stack_depth = caml_stack_depth;
  var saved_exn_stack = caml_exn_stack;
  var saved_fiber_stack = caml_fiber_stack;
  try {
    caml_exn_stack = 0;
    caml_fiber_stack =
      {h:[0, 0, 0, uncaught_effect_handler], r:{k:0, x:0, e:0}};
    var res = {joo_tramp: f,
               joo_args: args.concat(function (x){return x;})};
    do {
      caml_stack_depth = 40;
      try {
        res = caml_call_gen(res.joo_tramp, res.joo_args);
      } catch (e) {
        /* Handle exception coming from JavaScript or from the runtime. */
        if (!caml_exn_stack) throw e;
        var handler = caml_exn_stack[1];
        caml_exn_stack = caml_exn_stack[2];
        res = {joo_tramp: handler,
               joo_args: [caml_wrap_exception(e)]};
      }
    } while(res && res.joo_args)
  } finally {
    caml_stack_depth = saved_stack_depth;
    caml_exn_stack = saved_exn_stack;
    caml_fiber_stack = saved_fiber_stack;
  }
  return res;
}

//Provides: caml_is_js
function caml_is_js() {
  return 1;
}

//Provides: caml_jsoo_flags_use_js_string
function caml_jsoo_flags_use_js_string(unit){
  return FLAG("use-js-string")
}

//Provides: caml_jsoo_flags_effects
function caml_jsoo_flags_effects(unit){
  return FLAG("effects")
}

//Provides: caml_wrap_exception const (mutable)
//Requires: caml_global_data,caml_string_of_jsstring,caml_named_value
function caml_wrap_exception(e) {
  if (FLAG("excwrap")) {
    if(e instanceof Array) return e;
    var exn;
    //Stack_overflow: chrome, safari
    if(globalThis.RangeError
       && e instanceof globalThis.RangeError
       && e.message
       && e.message.match(/maximum call stack/i))
      exn = caml_global_data.Stack_overflow;
    //Stack_overflow: firefox
    else if(globalThis.InternalError
       && e instanceof globalThis.InternalError
       && e.message
       && e.message.match(/too much recursion/i))
      exn = caml_global_data.Stack_overflow;
    //Wrap Error in Js.Error exception
    else if(e instanceof globalThis.Error && caml_named_value("jsError"))
      exn = [0,caml_named_value("jsError"),e];
    else
      //fallback: wrapped in Failure
      exn = [0,caml_global_data.Failure,caml_string_of_jsstring (String(e))];
    // We already have an error at hand, let's use it.
    if (e instanceof globalThis.Error)
      exn.js_error = e;
    return exn;
  } else
    return e;
}

//Provides: caml_maybe_attach_backtrace
//Requires: caml_exn_with_js_backtrace
//Requires: caml_record_backtrace_flag
function caml_maybe_attach_backtrace(exn, force) {
  if(caml_record_backtrace_flag)
    return caml_exn_with_js_backtrace(exn, force);
  else return exn
}

// Experimental
//Provides: caml_exn_with_js_backtrace
//Requires: caml_global_data
function caml_exn_with_js_backtrace(exn, force) {
  //never reraise for constant exn
  if(!exn.js_error || force || exn[0] == 248) exn.js_error = new globalThis.Error("Js exception containing backtrace");
  return exn;
}


//Provides: caml_js_error_option_of_exception
function caml_js_error_option_of_exception(exn) {
  if(exn.js_error) { return [0, exn.js_error]; }
  return 0;
}



//Provides: caml_js_from_bool const (const)
function caml_js_from_bool(x) { return !!x; }
//Provides: caml_js_to_bool const (const)
function caml_js_to_bool(x) { return +x; }
//Provides: caml_js_from_float const (const)
function caml_js_from_float(x) { return x; }
//Provides: caml_js_to_float const (const)
function caml_js_to_float(x) { return x; }

//Provides: caml_js_from_array mutable (shallow)
function caml_js_from_array(a) {
  return a.slice(1);
}
//Provides: caml_js_to_array mutable (shallow)
function caml_js_to_array(a) {
  var len = a.length;
  var b = new Array(len+1);
  b[0] = 0;
  for(var i=0;i<len;i++) b[i+1] = a[i];
  return b;
}

//Provides: caml_list_of_js_array const (mutable)
function caml_list_of_js_array(a){
  var l = 0;
  for(var i=a.length - 1; i>=0; i--){
    var e = a[i];
    l = [0,e,l];
  }
  return l
}

//Provides: caml_list_to_js_array const (mutable)
function caml_list_to_js_array(l){
  var a = [];
  for(; l !== 0; l = l[2]) {
    a.push(l[1]);
  }
  return a;
}

//Provides: caml_js_var mutable
//Requires: caml_jsstring_of_string
function caml_js_var(x) {
  var x = caml_jsstring_of_string(x);
  //Checks that x has the form ident[.ident]*
  if(!x.match(/^[a-zA-Z_$][a-zA-Z_$0-9]*(\.[a-zA-Z_$][a-zA-Z_$0-9]*)*$/)){
    console.error("caml_js_var: \"" + x + "\" is not a valid JavaScript variable. continuing ..");
    //console.error("Js.Unsafe.eval_string")
  }
  return eval(x);
}
//Provides: caml_js_call (const, mutable, shallow)
//Requires: caml_js_from_array
function caml_js_call(f, o, args) { return f.apply(o, caml_js_from_array(args)); }
//Provides: caml_js_fun_call (const, shallow)
//Requires: caml_js_from_array
function caml_js_fun_call(f, a) {
  switch (a.length) {
  case 1: return f();
  case 2: return f (a[1]);
  case 3: return f (a[1],a[2]);
  case 4: return f (a[1],a[2],a[3]);
  case 5: return f (a[1],a[2],a[3],a[4]);
  case 6: return f (a[1],a[2],a[3],a[4],a[5]);
  case 7: return f (a[1],a[2],a[3],a[4],a[5],a[6]);
  case 8: return f (a[1],a[2],a[3],a[4],a[5],a[6],a[7]);
  }
  return f.apply(null, caml_js_from_array(a));
}
//Provides: caml_js_meth_call (mutable, const, shallow)
//Requires: caml_jsstring_of_string
//Requires: caml_js_from_array
function caml_js_meth_call(o, f, args) {
  return o[caml_jsstring_of_string(f)].apply(o, caml_js_from_array(args));
}
//Provides: caml_js_new (const, shallow)
//Requires: caml_js_from_array
function caml_js_new(c, a) {
  switch (a.length) {
  case 1: return new c;
  case 2: return new c (a[1]);
  case 3: return new c (a[1],a[2]);
  case 4: return new c (a[1],a[2],a[3]);
  case 5: return new c (a[1],a[2],a[3],a[4]);
  case 6: return new c (a[1],a[2],a[3],a[4],a[5]);
  case 7: return new c (a[1],a[2],a[3],a[4],a[5],a[6]);
  case 8: return new c (a[1],a[2],a[3],a[4],a[5],a[6],a[7]);
  }
  function F() { return c.apply(this, caml_js_from_array(a)); }
  F.prototype = c.prototype;
  return new F;
}
//Provides: caml_ojs_new_arr (const, shallow)
//Requires: caml_js_from_array
function caml_ojs_new_arr(c, a) {
  switch (a.length) {
  case 0: return new c;
  case 1: return new c (a[0]);
  case 2: return new c (a[0],a[1]);
  case 3: return new c (a[0],a[1],a[2]);
  case 4: return new c (a[0],a[1],a[2],a[3]);
  case 5: return new c (a[0],a[1],a[2],a[3],a[4]);
  case 6: return new c (a[0],a[1],a[2],a[3],a[4],a[5]);
  case 7: return new c (a[0],a[1],a[2],a[3],a[4],a[5],a[6]);
  }
  function F() { return c.apply(this, a); }
  F.prototype = c.prototype;
  return new F;
}
//Provides: caml_js_wrap_callback const (const)
//Requires: caml_callback
function caml_js_wrap_callback(f) {
  return function () {
    var len = arguments.length;
    if(len > 0){
      var args = new Array(len);
      for (var i = 0; i < len; i++) args[i] = arguments[i];
    } else {
      args = [undefined];
    }
    var res = caml_callback(f, args);
    return (res instanceof Function)?caml_js_wrap_callback(res):res;
  }
}

//Provides: caml_js_wrap_callback_arguments
//Requires: caml_callback
function caml_js_wrap_callback_arguments(f) {
  return function() {
    var len = arguments.length;
    var args = new Array(len);
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_callback(f, [args]);
  }
}
//Provides: caml_js_wrap_callback_strict const
//Requires: caml_callback
function caml_js_wrap_callback_strict(arity, f) {
  return function () {
    var n = arguments.length;
    var args = new Array(arity);
    var len = Math.min(arguments.length, arity)
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_callback(f, args);
  };
}
//Provides: caml_js_wrap_callback_unsafe const (const)
//Requires: caml_callback, caml_js_function_arity
function caml_js_wrap_callback_unsafe(f) {
  return function () {
    var len = caml_js_function_arity(f);
    var args = new Array(len);
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_callback(f, args); }
}
//Provides: caml_js_wrap_meth_callback const (const)
//Requires: caml_callback, caml_js_wrap_callback
function caml_js_wrap_meth_callback(f) {
  return function () {
    var len = arguments.length;
    var args = new Array(len + 1);
    args[0] = this;
    for (var i = 0; i < len; i++) args[i+1] = arguments[i];
    var res = caml_callback(f,args);
    return (res instanceof Function)?caml_js_wrap_callback(res):res;
  }
}
//Provides: caml_js_wrap_meth_callback_arguments const (const)
//Requires: caml_callback
function caml_js_wrap_meth_callback_arguments(f) {
  return function () {
    var len = arguments.length;
    var args = new Array(len);
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_callback(f,[this,args]);
  }
}
//Provides: caml_js_wrap_meth_callback_strict const
//Requires: caml_callback
function caml_js_wrap_meth_callback_strict(arity, f) {
  return function () {
    var args = new Array(arity + 1);
    var len = Math.min(arguments.length, arity)
    args[0] = this;
    for (var i = 0; i < len; i++) args[i+1] = arguments[i];
    return caml_callback(f, args);
  };
}
//Provides: caml_js_wrap_meth_callback_unsafe const (const)
//Requires: caml_callback, caml_js_function_arity
function caml_js_wrap_meth_callback_unsafe(f) {
  return function () {
    var len = caml_js_function_arity(f) - 1;
    var args = new Array(len + 1);
    args[0] = this;
    for (var i = 0; i < len; i++) args[i+1] = arguments[i];
    return caml_callback(f, args); }
}

//Provides: caml_js_function_arity
//If: !effects
function caml_js_function_arity(f) {
  return (f.l >= 0)?f.l:(f.l = f.length)
}

//Provides: caml_js_function_arity
//If: effects
function caml_js_function_arity(f) {
  // Functions have an additional continuation parameter. This should
  // not be visible when calling them from JavaScript
  return ((f.l >= 0)?f.l:(f.l = f.length)) - 1
}

//Provides: caml_js_equals mutable (const, const)
function caml_js_equals (x, y) { return +(x == y); }

//Provides: caml_js_strict_equals mutable (const, const)
function caml_js_strict_equals (x, y) { return +(x === y); }

//Provides: caml_js_eval_string (const)
//Requires: caml_jsstring_of_string
function caml_js_eval_string (s) {return eval(caml_jsstring_of_string(s));}

//Provides: caml_js_expr (const)
//Requires: caml_jsstring_of_string
function caml_js_expr(s) {
  console.error("caml_js_expr: fallback to runtime evaluation\n");
  return eval(caml_jsstring_of_string(s));}

//Provides: caml_pure_js_expr const (const)
//Requires: caml_jsstring_of_string
function caml_pure_js_expr (s){
  console.error("caml_pure_js_expr: fallback to runtime evaluation\n");
  return eval(caml_jsstring_of_string(s));}

//Provides: caml_js_object (object_literal)
//Requires: caml_jsstring_of_string
function caml_js_object (a) {
  var o = {};
  for (var i = 1; i < a.length; i++) {
    var p = a[i];
    o[caml_jsstring_of_string(p[1])] = p[2];
  }
  return o;
}
