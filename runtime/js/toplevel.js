// Js_of_ocaml toplevel runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2011 Jérôme Vouillon
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

//Provides: caml_terminfo_rows
function caml_terminfo_rows() {
  return 0;
}
//Provides: caml_invoke_traced_function
//Requires: caml_invalid_argument
function caml_invoke_traced_function() {
  caml_invalid_argument("Meta.invoke_traced_function");
}
//Provides: caml_get_current_environment
//Requires: caml_failwith
function caml_get_current_environment() {
  caml_failwith("caml_get_current_environment not Implemented");
}
//////////////////////////////////////////////////////////////////////

//Provides: caml_get_section_table
//Requires: caml_global_data, caml_failwith
//Requires: caml_string_of_jsbytes, caml_jsbytes_of_string
//Requires: caml_list_of_js_array
//Version: < 5.3
function caml_get_section_table() {
  if (!caml_global_data.sections) {
    caml_failwith("Program not compiled with --toplevel");
  }
  var symb = caml_global_data.sections[1];
  var crcs = caml_global_data.sections[2];
  var prim = caml_global_data.sections[3];
  var dlpt = caml_global_data.sections[4];
  function sl(l) {
    var x = "";
    while (l) {
      x += caml_jsbytes_of_string(l[1]);
      x += "\0";
      l = l[2];
    }
    return caml_string_of_jsbytes(x);
  }
  var res = caml_list_of_js_array([
    [0, caml_string_of_jsbytes("SYMB"), symb],
    [0, caml_string_of_jsbytes("CRCS"), crcs],
    [0, caml_string_of_jsbytes("PRIM"), sl(prim)],
    [0, caml_string_of_jsbytes("DLPT"), sl(dlpt)],
  ]);
  return res;
}

//Provides: caml_dynlink_get_bytecode_sections
//Requires: caml_global_data, caml_failwith
//Alias: jsoo_get_bytecode_sections
function caml_dynlink_get_bytecode_sections() {
  if (!caml_global_data.sections) {
    caml_failwith("Program not compiled with --toplevel");
  }
  return caml_global_data.sections;
}

//Provides: jsoo_get_runtime_aliases
//Requires: caml_global_data, caml_failwith
function jsoo_get_runtime_aliases() {
  if (caml_global_data.aliases === undefined) {
    caml_failwith("Program not compiled with --toplevel");
  }
  return caml_global_data.aliases;
}

//Provides: jsoo_toplevel_compile
//Requires: caml_failwith
var jsoo_toplevel_compile = undefined;

//Provides: jsoo_toplevel_init_compile
//Requires: jsoo_toplevel_compile
function jsoo_toplevel_init_compile(f) {
  jsoo_toplevel_compile = f;
}

//Provides: jsoo_toplevel_init_reloc
//Requires: jsoo_toplevel_reloc
function jsoo_toplevel_init_reloc(f) {
  jsoo_toplevel_reloc = f;
}

//Provides: caml_reify_bytecode
//Requires: caml_callback
//Requires: caml_string_of_uint8_array, caml_ba_to_typed_array
//Requires: jsoo_toplevel_compile, caml_failwith
//Version: >= 5.2
function caml_reify_bytecode(code, debug, _digest) {
  if (!jsoo_toplevel_compile) {
    caml_failwith("Toplevel not initialized (jsoo_toplevel_compile)");
  }
  code = caml_string_of_uint8_array(caml_ba_to_typed_array(code));
  return [0, 0, caml_callback(jsoo_toplevel_compile, [code, debug])];
}

//Provides: caml_reify_bytecode
//Requires: caml_callback
//Requires: caml_string_of_uint8_array, caml_uint8_array_of_bytes
//Requires: jsoo_toplevel_compile, caml_failwith
//Version: < 5.2
function caml_reify_bytecode(code, debug, _digest) {
  if (!jsoo_toplevel_compile) {
    caml_failwith("Toplevel not initialized (jsoo_toplevel_compile)");
  }
  var len = 0;
  var all = [];
  for (var i = 1; i < code.length; i++) {
    var a = caml_uint8_array_of_bytes(code[i]);
    all.push(a);
    len += a.length;
  }
  code = new Uint8Array(len);
  for (var i = 0, len = 0; i < all.length; i++) {
    code.set(all[i], len);
    len += all[i].length;
  }
  code = caml_string_of_uint8_array(code);
  return [0, 0, caml_callback(jsoo_toplevel_compile, [code, debug])];
}

//Provides: caml_static_release_bytecode
function caml_static_release_bytecode() {
  return 0;
}

//Provides: caml_realloc_global
//Requires: caml_global_data
function caml_realloc_global(len) {
  if (len + 1 > caml_global_data.length) caml_global_data.length = len + 1;
  return 0;
}
