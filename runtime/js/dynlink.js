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

//Provides: get_current_libs
var current_libs;
function get_current_libs() {
  if (!current_libs) current_libs = [0, globalThis, globalThis.jsoo_runtime];
  return current_libs;
}

//Provides: caml_dynlink_open_lib
//Requires: get_current_libs, caml_failwith
//Requires: caml_jsstring_of_string
function caml_dynlink_open_lib(_mode, file) {
  var name = caml_jsstring_of_string(file);
  console.log("Dynlink: try to open ", name);
  //caml_failwith("file not found: "+name)
  var current_libs = get_current_libs();
  current_libs.push({});
  return current_libs.length;
}

//Provides: caml_dynlink_close_lib
//Requires: get_current_libs
function caml_dynlink_close_lib(idx) {
  var current_libs = get_current_libs();
  current_libs[idx] = null;
  return 0;
}

//Provides: caml_dynlink_lookup_symbol
//Requires: get_current_libs
//Requires: caml_jsstring_of_string
function caml_dynlink_lookup_symbol(idx, fun_name) {
  var name = caml_jsstring_of_string(fun_name);
  console.log("Dynlink: looking for symbol", name);
  var current_libs = get_current_libs();
  if (current_libs[idx]?.[name])
    return { name: name, symbol: current_libs[idx][name] };
  return 0;
}

//Provides: caml_dynlink_add_primitive
//Requires: caml_global_data
function caml_dynlink_add_primitive(dll_addr) {
  globalThis.jsoo_runtime[dll_addr.name] = dll_addr.symbol;
  return caml_global_data.prim_count++;
}

//Provides: caml_dynlink_get_current_libs
//Requires: get_current_libs
function caml_dynlink_get_current_libs() {
  var current_libs = get_current_libs();
  var len = current_libs.length;
  var a = new Array(len);
  for (var i = 0; i < len; i++) a[i] = i;
  return a;
}
