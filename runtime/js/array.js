// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
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

///////////// Array

//Provides: caml_array_sub mutable
function caml_array_sub(a, i, len) {
  var a2 = new Array(len + 1);
  a2[0] = 0;
  for (var i2 = 1, i1 = i + 1; i2 <= len; i2++, i1++) {
    a2[i2] = a[i1];
  }
  return a2;
}

//Provides: caml_floatarray_sub mutable
//Requires: caml_array_sub
//Version: >= 5.3
function caml_floatarray_sub(a, i, len) {
  return caml_array_sub(a, i, len);
}

//Provides: caml_uniform_array_sub mutable
//Requires: caml_array_sub
//Version: >= 5.3
function caml_uniform_array_sub(a, i, len) {
  return caml_array_sub(a, i, len);
}

//Provides: caml_array_append mutable
function caml_array_append(a1, a2) {
  var l1 = a1.length,
    l2 = a2.length;
  var l = l1 + l2 - 1;
  var a = new Array(l);
  a[0] = 0;
  var i = 1,
    j = 1;
  for (; i < l1; i++) a[i] = a1[i];
  for (; i < l; i++, j++) a[i] = a2[j];
  return a;
}

//Provides: caml_floatarray_append mutable
//Requires: caml_array_append
//Version: >= 5.3
function caml_floatarray_append(a1, a2) {
  return caml_array_append(a1, a2);
}

//Provides: caml_uniform_array_append mutable
//Requires: caml_array_append
//Version: >= 5.3
function caml_uniform_array_append(a1, a2) {
  return caml_array_append(a1, a2);
}

//Provides: caml_array_concat mutable
function caml_array_concat(l) {
  var a = [0];
  while (l !== 0) {
    var b = l[1];
    for (var i = 1; i < b.length; i++) a.push(b[i]);
    l = l[2];
  }
  return a;
}

//Provides: caml_array_blit
function caml_array_blit(a1, i1, a2, i2, len) {
  if (i2 <= i1) {
    for (var j = 1; j <= len; j++) a2[i2 + j] = a1[i1 + j];
  } else {
    for (var j = len; j >= 1; j--) a2[i2 + j] = a1[i1 + j];
  }
  return 0;
}

//Provides: caml_floatarray_blit
//Requires: caml_array_blit
function caml_floatarray_blit(a1, i1, a2, i2, len) {
  return caml_array_blit(a1, i1, a2, i2, len);
}

//Provides: caml_uniform_array_blit
//Requires: caml_array_blit
//Version: >= 5.3
function caml_uniform_array_blit(a1, i1, a2, i2, len) {
  return caml_array_blit(a1, i1, a2, i2, len);
}

///////////// Pervasive
//Provides: caml_array_set (mutable, const, mutable)
//Requires: caml_array_bound_error
//Alias: caml_array_set_float
//Alias: caml_floatarray_set
//Alias: caml_array_set_addr
function caml_array_set(array, index, newval) {
  if (index < 0 || index >= array.length - 1) caml_array_bound_error();
  array[index + 1] = newval;
  return 0;
}

//Provides: caml_array_get mutable (mutable, const)
//Requires: caml_array_bound_error
//Alias: caml_array_get_float
//Alias: caml_floatarray_get
//Alias: caml_array_get_addr
function caml_array_get(array, index) {
  if (index < 0 || index >= array.length - 1) caml_array_bound_error();
  return array[index + 1];
}

//Provides: caml_array_fill
function caml_array_fill(array, ofs, len, v) {
  for (var i = 0; i < len; i++) {
    array[ofs + i + 1] = v;
  }
  return 0;
}

//Provides: caml_floatarray_fill
//Requires: caml_array_fill
//Version: >= 5.3
function caml_floatarray_fill(array, ofs, len, v) {
  return caml_array_fill(array, ofs, len, v);
}

//Provides: caml_floatarray_fill_unboxed
//Requires: caml_array_fill
//Version: >= 5.3
function caml_floatarray_fill_unboxed(array, ofs, len, v) {
  return caml_array_fill(array, ofs, len, v);
}

//Provides: caml_uniform_array_fill
//Requires: caml_array_fill
//Version: >= 5.3
function caml_uniform_array_fill(array, ofs, len, v) {
  return caml_array_fill(array, ofs, len, v);
}

//Provides: caml_check_bound (mutable, const)
//Requires: caml_array_bound_error
//Alias: caml_check_bound_gen
//Alias: caml_check_bound_float
function caml_check_bound(array, index) {
  if (index >>> 0 >= array.length - 1) caml_array_bound_error();
  return array;
}

//Provides: caml_array_make const (const, mutable)
//Requires: caml_array_bound_error
function caml_array_make(len, init) {
  if (len >>> 0 >= ((0x7fffffff / 4) | 0)) caml_array_bound_error();
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 0;
  for (var i = 1; i < len; i++) b[i] = init;
  return b;
}

//Provides: caml_make_vect const (const, mutable)
//Requires: caml_array_make
function caml_make_vect(len, init) {
  return caml_array_make(len, init);
}

//Provides: caml_make_float_vect const (const)
//Requires: caml_array_bound_error
function caml_make_float_vect(len) {
  if (len >>> 0 >= ((0x7fffffff / 8) | 0)) caml_array_bound_error();
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b;
}

//Provides: caml_array_create_float const (const)
//Requires: caml_array_bound_error
//Version: >= 5.3
function caml_array_create_float(len) {
  if (len >>> 0 >= ((0x7fffffff / 8) | 0)) caml_array_bound_error();
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b;
}
//Provides: caml_floatarray_create const (const)
//Requires: caml_array_bound_error
function caml_floatarray_create(len) {
  if (len >>> 0 >= ((0x7fffffff / 8) | 0)) caml_array_bound_error();
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b;
}

//Provides: caml_floatarray_make const (const)
//Requires: caml_array_bound_error
//Version: >= 5.3
function caml_floatarray_make(len, init) {
  if (len >>> 0 >= ((0x7fffffff / 8) | 0)) caml_array_bound_error();
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 254;
  for (var i = 1; i < len; i++) b[i] = init;
  return b;
}

//Provides: caml_floatarray_make_unboxed const (const)
//Requires: caml_floatarray_make
//Version: >= 5.3
function caml_floatarray_make_unboxed(len, init) {
  return caml_floatarray_make(len, init);
}

//Provides: caml_uniform_array_make const (const)
//Requires: caml_array_make
//Version: >= 5.3
function caml_uniform_array_make(len, init) {
  return caml_array_make(len, init);
}
