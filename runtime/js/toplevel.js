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

import { caml_ba_to_typed_array } from './bigarray.js';
import { caml_failwith, caml_invalid_argument } from './fail.js';
import { caml_callback, caml_list_of_js_array } from './jslib.js';
import { caml_jsbytes_of_string, caml_string_of_jsbytes, caml_string_of_uint8_array, caml_uint8_array_of_bytes } from './mlBytes.js';
import { caml_global_data, jsoo_toplevel_reloc } from './stdlib.js';

//Provides: caml_terminfo_rows
export function caml_terminfo_rows() {
  return 0;
}
//Provides: caml_invoke_traced_function
export function caml_invoke_traced_function() {
  caml_invalid_argument("Meta.invoke_traced_function");
}
//Provides: caml_get_current_environment
export function caml_get_current_environment() {
  caml_failwith("caml_get_current_environment not Implemented");
}
//////////////////////////////////////////////////////////////////////

//Provides: caml_get_section_table
//Version: < 5.3
export function caml_get_section_table() {
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
//Alias: jsoo_get_bytecode_sections
export function caml_dynlink_get_bytecode_sections() {
  if (!caml_global_data.sections) {
    caml_failwith("Program not compiled with --toplevel");
  }
  return caml_global_data.sections;
}

//Provides: jsoo_get_runtime_aliases
export function jsoo_get_runtime_aliases() {
  if (caml_global_data.aliases === undefined) {
    caml_failwith("Program not compiled with --toplevel");
  }
  return caml_global_data.aliases;
}

//Provides: jsoo_toplevel_compile
export var jsoo_toplevel_compile = undefined;

//Provides: jsoo_toplevel_init_compile
export function jsoo_toplevel_init_compile(f) {
  jsoo_toplevel_compile = f;
}

//Provides: jsoo_toplevel_init_reloc
export function jsoo_toplevel_init_reloc(f) {
  jsoo_toplevel_reloc = f;
}

//Provides: caml_reify_bytecode
//Version: >= 5.2
export function caml_reify_bytecode(code, debug, _digest) {
  if (!jsoo_toplevel_compile) {
    caml_failwith("Toplevel not initialized (jsoo_toplevel_compile)");
  }
  code = caml_string_of_uint8_array(caml_ba_to_typed_array(code));
  return [0, 0, caml_callback(jsoo_toplevel_compile, [code, debug])];
}

//Provides: caml_reify_bytecode
//Version: < 5.2
export function caml_reify_bytecode(code, debug, _digest) {
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
export function caml_static_release_bytecode() {
  return 0;
}

//Provides: caml_realloc_global
export function caml_realloc_global(len) {
  if (len + 1 > caml_global_data.length) caml_global_data.length = len + 1;
  return 0;
}
