///////////// Jslib

//Provides: caml_js_from_bool const
function caml_js_from_bool(x) { return !!x; }
//Provides: caml_js_to_bool const
function caml_js_to_bool(x) { return +x; }
//Provides: caml_js_from_float const
function caml_js_from_float(x) { return x; }
//Provides: caml_js_to_float const
function caml_js_to_float(x) { return x; }
//Provides: caml_js_from_string mutable
function caml_js_from_string(s) { return s.toString(); }
//Provides: caml_js_to_string const
//Requires: MlString
function caml_js_to_string(s) { return new MlWrappedString(s); }
//Provides: caml_js_from_array mutable
function caml_js_from_array(a) { return a.slice(1); }
//Provides: caml_js_to_array mutable
function caml_js_to_array(a) { return [0].concat(a); }

//Provides: caml_js_set
function caml_js_set(o,f,v) { o[f]=v; }
//Provides: caml_js_get mutable
function caml_js_get(o,f) { return o[f]; }

//Provides: caml_js_var mutable
function caml_js_var(x) { eval(x); }
//Provides: caml_js_const const
function caml_js_const(x) {
  switch (caml_string_to_js(x)) {
  case "null": return null;
  case "true": return true;
  case "false": return false;
  // case "undefined: return undefined;
  }
}
//Provides: caml_js_meth_call
function caml_js_meth_call(o, f, args) { return f.apply(o, args.slice(1)); }
//Provides: caml_js_new
function caml_js_new(c, a) {
  switch (a.length) {
  case 1: return new c;
  case 2: return new c (a[1]);
  case 3: return new c (a[1],a[2]);
  case 4: return new c (a[1],a[2],a[3]);
  case 5: return new c (a[1],a[2],a[3],a[4]);
  case 6: return new c (a[1],a[2],a[3],a[4],a[5]);
  case 7: return new c (a[1],a[2],a[3],a[4],a[5],a[6]);
  case 8: return new c (a[1],a[2],a[3],a[4],a[5],a[6], a[7]);
  }
  function F() { return c.apply(this, args.slice(1)); }
  F.prototype = c.prototype;
  return new F;
}
//Provides: caml_js_wrap_callback const
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () { return caml_call_gen(f, toArray.call (arguments)); }
}
//Provides: caml_js_wrap_meth_callback const
function caml_js_wrap_meth_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = toArray.call(arguments);
    args.unshift (this);
    return caml_call_gen(f, args);
  }
}
