///////////// Jslib
//FIX: other functions from jslib...
//js_meth_call, js_new, caml_js_wrap_callback, caml_js_wrap_meth_callback

function caml_bool_to_js(x) { return !!x; }
function caml_bool_from_js(x) { return +x; }
function caml_float_to_js(x) { return x; }
function caml_float_from_js(x) { return x; }
function caml_string_to_js(s) { return s.toString(); }
function caml_string_from_js(s) { return new MlString(s); }
function caml_array_to_js(a) { return a.slice(1); }
function caml_array_from_js(a) { return [0].concat(a); }

function caml_js_set(o,f,v) { o[f]=v; }
function caml_js_get(o,f) { return o[f]; }

function caml_js_variable(x} { return window[x]; }

//////////// XMLHttpRequest

// FIX: should be on the caml side (using code for invoking new)
function createXMLHTTPObject() {
  var XMLHttpFactories =
    [ function () {return new XMLHttpRequest()},
      function () {return new ActiveXObject("Msxml2.XMLHTTP")},
      function () {return new ActiveXObject("Msxml3.XMLHTTP")},
      function () {return new ActiveXObject("Microsoft.XMLHTTP")} ];

  for (var i = 0; i < XMLHttpFactories.length; i++) {
    try {
      var xmlhttp = XMLHttpFactories[i]();
      break;
    } catch (e) { }
  }
  return xmlhttp;
}
