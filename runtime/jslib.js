///////////// Jslib
//FIX: other functions from jslib...
function caml_string_to_js(s) { return s.toString(); }
function caml_string_from_js(s) { return new MlString(s); }

function caml_js_set(o,f,v) { o[f]=v; return 0; }
function caml_js_get(o,f) { return o[f]; }
function caml_js_constant(x} {
  switch (x) {
    case "null":  return null;
    case "true":  return true;
    case "false": return false;
 // default:      return 'undefined'
  }
}
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
