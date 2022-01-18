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
function caml_js_pure_expr (f) { return f(); }

//Provides: caml_js_set (mutable, const, const)
function caml_js_set(o,f,v) { o[f]=v;return 0}
//Provides: caml_js_get mutable (const, const)
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

//Provides: js_print_stdout (const)
//Requires: caml_utf16_of_utf8
function js_print_stdout(s) {
  var s = caml_utf16_of_utf8(s);
  var g = globalThis;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    g.process.stdout.write(s)
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if(s.charCodeAt(s.length - 1) == 10)
      s = s.substr(0,s.length - 1 );
    var v = g.console;
    v  && v.log && v.log(s);
  }
}
//Provides: js_print_stderr (const)
//Requires: caml_utf16_of_utf8
function js_print_stderr(s) {
  var s = caml_utf16_of_utf8(s);
  var g = globalThis;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    g.process.stderr.write(s)
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if(s.charCodeAt(s.length - 1) == 10)
      s = s.substr(0,s.length - 1 );
    var v = g.console;
    v && v.error && v.error(s);
  }
}

//Provides: caml_is_js
function caml_is_js() {
  return 1;
}

//Provides: caml_wrap_exception const (const)
//Requires: caml_global_data,caml_string_of_jsstring,caml_named_value
//Requires: caml_return_exn_constant
function caml_wrap_exception(e) {
  if(e instanceof Array) return e;
  //Stack_overflow: chrome, safari
  if(globalThis.RangeError
     && e instanceof globalThis.RangeError
     && e.message
     && e.message.match(/maximum call stack/i))
    return caml_return_exn_constant(caml_global_data.Stack_overflow);
  //Stack_overflow: firefox
  if(globalThis.InternalError
     && e instanceof globalThis.InternalError
     && e.message
     && e.message.match(/too much recursion/i))
    return caml_return_exn_constant(caml_global_data.Stack_overflow);
  //Wrap Error in Js.Error exception
  if(e instanceof globalThis.Error && caml_named_value("jsError"))
    return [0,caml_named_value("jsError"),e];
  //fallback: wrapped in Failure
  return [0,caml_global_data.Failure,caml_string_of_jsstring (String(e))];
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

//Provides: caml_list_of_js_array const (const)
function caml_list_of_js_array(a){
  var l = 0;
  for(var i=a.length - 1; i>=0; i--){
    var e = a[i];
    l = [0,e,l];
  }
  return l
}

//Provides: caml_list_to_js_array const (const)
function caml_list_to_js_array(l){
  var a = [];
  for(; l !== 0; l = l[2]) {
    a.push(l[1]);
  }
  return a;
}

//Provides: caml_js_var mutable (const)
//Requires: js_print_stderr
//Requires: caml_jsstring_of_string
function caml_js_var(x) {
  var x = caml_jsstring_of_string(x);
  //Checks that x has the form ident[.ident]*
  if(!x.match(/^[a-zA-Z_$][a-zA-Z_$0-9]*(\.[a-zA-Z_$][a-zA-Z_$0-9]*)*$/)){
    js_print_stderr("caml_js_var: \"" + x + "\" is not a valid JavaScript variable. continuing ..");
    //globalThis.console.error("Js.Unsafe.eval_string")
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
//Requires: caml_call_gen
function caml_js_wrap_callback(f) {
  return function () {
    var len = arguments.length;
    if(len > 0){
      var args = new Array(len);
      for (var i = 0; i < len; i++) args[i] = arguments[i];
      return caml_call_gen(f, args);
    } else {
      return caml_call_gen(f, [undefined]);
    }
  }
}

//Provides: caml_js_wrap_callback_arguments
//Requires: caml_call_gen
function caml_js_wrap_callback_arguments(f) {
  return function() {
    var len = arguments.length;
    var args = new Array(len);
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_call_gen(f, [args]);
  }
}
//Provides: caml_js_wrap_callback_strict const
//Requires: caml_call_gen
function caml_js_wrap_callback_strict(arity, f) {
  return function () {
    var n = arguments.length;
    if(n == arity && f.length == arity) return f.apply(null, arguments);
    var args = new Array(arity);
    var len = Math.min(arguments.length, arity)
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_call_gen(f, args);
  };
}
//Provides: caml_js_wrap_meth_callback const (const)
//Requires: caml_call_gen
function caml_js_wrap_meth_callback(f) {
  return function () {
    var len = arguments.length;
    var args = new Array(len + 1);
    args[0] = this;
    for (var i = 0; i < len; i++) args[i+1] = arguments[i];
    return caml_call_gen(f,args);
  }
}
//Provides: caml_js_wrap_meth_callback_arguments const (const)
//Requires: caml_call_gen
function caml_js_wrap_meth_callback_arguments(f) {
  return function () {
    var len = arguments.length;
    var args = new Array(len);
    for (var i = 0; i < len; i++) args[i] = arguments[i];
    return caml_call_gen(f,[this,args]);
  }
}
//Provides: caml_js_wrap_meth_callback_strict const
//Requires: caml_call_gen
function caml_js_wrap_meth_callback_strict(arity, f) {
  return function () {
    var args = new Array(arity + 1);
    var len = Math.min(arguments.length, arity)
    args[0] = this;
    for (var i = 0; i < len; i++) args[i+1] = arguments[i];
    return caml_call_gen(f, args);
  };
}
//Provides: caml_js_wrap_meth_callback_unsafe const (const)
//Requires: caml_call_gen
function caml_js_wrap_meth_callback_unsafe(f) {
  return function () {
    var len = arguments.length;
    var args = new Array(len + 1);
    args[0] = this;
    for (var i = 0; i < len; i++) args[i+1] = arguments[i];
    return f.apply(null, args); }
}
//Provides: caml_js_equals mutable (const, const)
function caml_js_equals (x, y) { return +(x == y); }

//Provides: caml_js_eval_string (const)
//Requires: caml_jsstring_of_string
function caml_js_eval_string (s) {return eval(caml_jsstring_of_string(s));}

//Provides: caml_js_expr (const)
//Requires: js_print_stderr
//Requires: caml_jsstring_of_string
function caml_js_expr(s) {
  js_print_stderr("caml_js_expr: fallback to runtime evaluation\n");
  return eval(caml_jsstring_of_string(s));}

//Provides: caml_pure_js_expr const (const)
//Requires: js_print_stderr
//Requires: caml_jsstring_of_string
function caml_pure_js_expr (s){
  js_print_stderr("caml_pure_js_expr: fallback to runtime evaluation\n");
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
