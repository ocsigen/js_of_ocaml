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

///////////// Jslib: code specific to Js_of_ocaml

//Provides: caml_js_on_ie const
function caml_js_on_ie () {
  var ua =
      (globalThis.navigator&&globalThis.navigator.userAgent)
      ?globalThis.navigator.userAgent:"";
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
    return null;
  }
}

//Provides: caml_js_get_console const
function caml_js_get_console () {
  var c = console;
  var m = ["log", "debug", "info", "warn", "error", "assert", "dir", "dirxml",
           "trace", "group", "groupCollapsed", "groupEnd", "time", "timeEnd"];
  function f () {}
  for (var i = 0; i < m.length; i++) if (!c[m[i]]) c[m[i]]=f;
  return c;
}

//Provides: caml_xmlhttprequest_create
//Requires: caml_failwith
//Weakdef
function caml_xmlhttprequest_create(unit){
  if(typeof globalThis.XMLHttpRequest !== 'undefined') {
    try { return new globalThis.XMLHttpRequest } catch (e) { };
  }
  if(typeof globalThis.activeXObject !== 'undefined') {
    try { return new globalThis.activeXObject("Msxml2.XMLHTTP") } catch(e){ };
    try { return new globalThis.activeXObject("Msxml3.XMLHTTP") } catch(e){ };
    try { return new globalThis.activeXObject("Microsoft.XMLHTTP") } catch(e){ };
  }
  caml_failwith("Cannot create a XMLHttpRequest");
}

//Provides: caml_js_error_of_exception
function caml_js_error_of_exception(exn) {
  if(exn.js_error) { return exn.js_error; }
  return null;
}
