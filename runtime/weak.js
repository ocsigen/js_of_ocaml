// Js_of_ocaml runtime support
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

// Weak API, but without the weak semantics

//Provides: caml_ephe_key_offset
//Version: < 4.03
var caml_ephe_key_offset = 2

//Provides: caml_ephe_key_offset
//Version: >= 4.03
var caml_ephe_key_offset = 3

//Provides: caml_ephe_data_offset
//Version: >= 4.03
var caml_ephe_data_offset = 2

//Provides: caml_weak_create
//Requires: caml_ephe_key_offset
function caml_weak_create (n) {
  var x = [251,"caml_ephe_list_head"];
  x.length = caml_ephe_key_offset + n;
  return x;
}
//Provides: caml_weak_set
//Requires: caml_ephe_key_offset
function caml_weak_set(x, i, v) {
    x[caml_ephe_key_offset + i] = v;
    return 0;
}
//Provides: caml_weak_get mutable
//Requires: caml_ephe_key_offset
function caml_weak_get(x, i) {
    return (x[caml_ephe_key_offset + i ]===undefined)?0:x[caml_ephe_key_offset + i];
}
//Provides: caml_weak_get_copy mutable
//Requires: caml_weak_get
//Requires: caml_obj_dup
function caml_weak_get_copy(x, i) {
  var y = caml_weak_get(x, i);
  if (y === 0) return y;
  var z = y[1];
  if (z instanceof Array) return [0, caml_obj_dup(z)];
  return y;
}
//Provides: caml_weak_check mutable
//Requires: caml_ephe_key_offset
function caml_weak_check(x, i) {
  if(x[caml_ephe_key_offset + i]!==undefined && x[caml_ephe_key_offset + i] !==0)
    return 1;
  else
    return 0;
}

//Provides: caml_weak_blit
//Requires: caml_array_blit
//Requires: caml_ephe_key_offset
function caml_weak_blit(a1, i1, a2, i2, len) {
  // minus one because caml_array_blit works on ocaml array  
  caml_array_blit(a1, caml_ephe_key_offset + i1 - 1,
                  a2, caml_ephe_key_offset + i2 - 1,
                  len);
  return 0;
}

//Provides: caml_ephe_create
//Requires: caml_weak_create
var caml_ephe_create = caml_weak_create

//Provides: caml_ephe_blit_key
//Requires: caml_weak_blit
var caml_ephe_blit_key = caml_weak_blit

//Provides: caml_ephe_get_key
//Requires: caml_weak_get
var caml_ephe_get_key = caml_weak_get

//Provides: caml_ephe_get_key_copy
//Requires: caml_weak_get_copy
var caml_ephe_get_key_copy = caml_weak_get_copy

//Provides: caml_ephe_check_key
//Requires: caml_weak_check
var caml_ephe_check_key = caml_weak_check

//Provides: caml_ephe_set_key
//Requires: caml_weak_set
function caml_ephe_set_key(x, i, v) {
  return caml_weak_set(x, i, [0, v])
}

//Provides: caml_ephe_unset_key
//Requires: caml_weak_set
function caml_ephe_unset_key(x, i) {
  return caml_weak_set(x, i, 0)
}

//Provides: caml_ephe_blit_data
//Requires: caml_ephe_data_offset
//Version: >= 4.03
function caml_ephe_blit_data(src, dst){
  dst[caml_ephe_data_offset] = src[caml_ephe_data_offset];
  return 0;
}

//Provides: caml_ephe_get_data
//Requires: caml_ephe_data_offset
//Version: >= 4.03
function caml_ephe_get_data(x){
  if(x[caml_ephe_data_offset] === undefined)
    return 0;
  else
    return [0, x[caml_ephe_data_offset]];
}

//Provides: caml_ephe_get_data_copy
//Requires: caml_ephe_data_offset
//Requires: caml_obj_dup
//Version: >= 4.03
function caml_ephe_get_data_copy(x){
  if(x[caml_ephe_data_offset] === undefined)
    return 0;
  else
    return [0, caml_obj_dup(x[caml_ephe_data_offset])];
}

//Provides: caml_ephe_set_data
//Requires: caml_ephe_data_offset
//Version: >= 4.03
function caml_ephe_set_data(x, data){
  x[caml_ephe_data_offset] = data;
  return 0;
}

//Provides: caml_ephe_unset_data
//Requires: caml_ephe_data_offset
//Version: >= 4.03
function caml_ephe_unset_data(x, data){
  x[caml_ephe_data_offset] = undefined;
  return 0;
}

//Provides: caml_ephe_check_data
//Requires: caml_ephe_data_offset
//Version: >= 4.03
function caml_ephe_check_data(x){
  if(x[caml_ephe_data_offset] === undefined)
    return 0;
  else
    return 1;
}
