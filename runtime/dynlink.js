// Js_of_ocaml toplevel runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2015 Hugo Heuzard
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

//Provides: current_libs
var current_libs = [0, globalThis]

//Provides: caml_dynlink_open_lib
//Requires: current_libs, caml_failwith
//Requires: caml_jsstring_of_string
function caml_dynlink_open_lib (_mode,file) {
  var name = caml_jsstring_of_string(file);
  globalThis.console.log("Dynlink: try to open ", name);
  //caml_failwith("file not found: "+name)
  current_libs.push({});
  return current_libs.length;
}

//Provides: caml_dynlink_close_lib
//Requires: current_libs
function caml_dynlink_close_lib (idx) {
  current_libs[idx]=null;
  return 0;
}

//Provides: caml_dynlink_lookup_symbol
//Requires: current_libs
//Requires: caml_jsstring_of_string
function caml_dynlink_lookup_symbol (idx, fun_name) {
  var name = caml_jsstring_of_string(fun_name);
  globalThis.console.log("Dynlink: look for symbol ", name);
  if(current_libs[idx] && current_libs[idx][name])
    return current_libs[idx][name];
  return 0;
}

//Provides: caml_dynlink_add_primitive
//Requires: caml_global_data
function caml_dynlink_add_primitive (dll_addr) {
  return caml_global_data.prim_count++;
}

//Provides: caml_dynlink_get_current_libs
//Requires: current_libs
function caml_dynlink_get_current_libs () {
  var len = current_libs.length;
  var a = new Array(len);
  for(var i=0; i < len; i++)
    a[i]=i;
  return a;
}

//Provides: caml_register_code_fragment
function caml_register_code_fragment(code, codesize, digest){
  return 0
}

//Provides: caml_add_debug_info
function caml_add_debug_info(code, size, events){
  return 0
}

//Provides: caml_remove_debug_info
function caml_remove_debug_info(code){
  return 0
}
