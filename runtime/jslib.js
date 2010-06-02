///////////// Jslib
//FIX: caml_js_new

function caml_js_from_bool(x) { return !!x; }
function caml_js_to_bool(x) { return +x; }
function caml_js_from_float(x) { return x; }
function caml_js_to_float(x) { return x; }
function caml_js_from_string(s) { return s.toString(); }
function caml_js_to_string(s) { return new MlString(s); }
function caml_js_from_array(a) { return a.slice(1); }
function caml_js_to_array(a) { return [0].concat(a); }

function caml_js_set(o,f,v) { o[f]=v; }
function caml_js_get(o,f) { return o[f]; }

function caml_js_var(x) { return window[x]; }
function caml_js_const(x) {
    switch (caml_string_to_js(x)) {
    case "null": return null;
    case "true": return true;
    case "false": return false;
    }
    // return undefined;
}
function caml_js_meth_call(o, f, args) { return f.apply(o, args); }
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () { return caml_call_gen(f, toArray.call (arguments)); }
}
function caml_js_wrap_meth_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = toArray.call(arguments);
    args.unshift (this);
    return caml_call_gen(f, args);
  }
}

//////////// XMLHttpRequest

//function createXMLHTTPObject() {
//  var XMLHttpFactories =
//    [ function () {return new XMLHttpRequest()},
//      function () {return new ActiveXObject("Msxml2.XMLHTTP")},
//      function () {return new ActiveXObject("Msxml3.XMLHTTP")},
//      function () {return new ActiveXObject("Microsoft.XMLHTTP")} ];
//
//  for (var i = 0; i < XMLHttpFactories.length; i++) {
//    try {
//      var xmlhttp = XMLHttpFactories[i]();
//      break;
//    } catch (e) { }
//  }
//  return xmlhttp;
//}
