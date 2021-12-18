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
function caml_js_instanceof(o,c) { return o instanceof c; }

//Provides: caml_js_typeof (const)
function caml_js_typeof(o) { return typeof o; }

//Provides: caml_js_on_ie const
function caml_js_on_ie () {
  var ua =
      globalThis.navigator?globalThis.navigator.userAgent:"";
  return ua.indexOf("MSIE") != -1 && ua.indexOf("Opera") != 0;
}

//Provides: caml_js_html_escape const (const)
var caml_js_regexps = { amp:/&/g, lt:/</g, quot:/\"/g, all:/[&<\"]/ };
function caml_js_html_escape (s) {
  if (!caml_js_regexps.all.test(s)) return s;
  return s.replace(caml_js_regexps.amp, "&amp;")
    .replace(caml_js_regexps.lt, "&lt;")
    .replace(caml_js_regexps.quot, "&quot;");
}

//Provides: caml_js_html_entities
//Requires: caml_failwith
function caml_js_html_entities(s) {
  var entity = /^&#?[0-9a-zA-Z]+;$/
  if(s.match(entity))
  {
    var str, temp = document.createElement('p');
    temp.innerHTML= s;
    str= temp.textContent || temp.innerText;
    temp=null;
    return str;
  }
  else {
    caml_failwith("Invalid entity " + s);
  }
}

/////////// Debugging console
//Provides: caml_js_get_console const
function caml_js_get_console () {
  var c = globalThis.console?globalThis.console:{};
  var m = ["log", "debug", "info", "warn", "error", "assert", "dir", "dirxml",
           "trace", "group", "groupCollapsed", "groupEnd", "time", "timeEnd"];
  function f () {}
  for (var i = 0; i < m.length; i++) if (!c[m[i]]) c[m[i]]=f;
  return c;
}

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

//Provides: caml_js_error_of_exception
function caml_js_error_of_exception(exn) {
  if(exn.js_error) { return exn.js_error; }
  return null;
}
