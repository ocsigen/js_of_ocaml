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

// Weak API

//Provides: caml_ephe_key_offset
var caml_ephe_key_offset = 3;

//Provides: caml_ephe_data_offset
var caml_ephe_data_offset = 2;

//Provides: caml_ephe_none
var caml_ephe_none = { caml_ephe_none: 0 };

//Provides: caml_ephe_set_key
//Requires: caml_ephe_key_offset
//Requires: caml_ephe_get_data
//Requires: caml_ephe_set_data_opt
function caml_ephe_set_key(x, i, v) {
  var old = caml_ephe_get_data(x);
  if (globalThis.WeakRef && v instanceof Object) v = new globalThis.WeakRef(v);
  x[caml_ephe_key_offset + i] = v;
  caml_ephe_set_data_opt(x, old);
  return 0;
}

//Provides: caml_ephe_unset_key
//Requires: caml_ephe_key_offset
//Requires: caml_ephe_get_data
//Requires: caml_ephe_set_data_opt
//Requires: caml_ephe_none
function caml_ephe_unset_key(x, i) {
  var old = caml_ephe_get_data(x);
  x[caml_ephe_key_offset + i] = caml_ephe_none;
  caml_ephe_set_data_opt(x, old);
  return 0;
}

//Provides: caml_ephe_create
//Requires: caml_weak_create
function caml_ephe_create(n) {
  return caml_weak_create(n);
}

//Provides: caml_weak_create
//Requires: caml_ephe_key_offset
//Requires: caml_ephe_none
function caml_weak_create(n) {
  var alen = caml_ephe_key_offset + n;
  var x = new Array(alen);
  x[0] = 251;
  x[1] = "caml_ephe_list_head";
  for (var i = 2; i < alen; i++) {
    x[i] = caml_ephe_none;
  }
  return x;
}

//Provides: caml_weak_set
//Requires: caml_ephe_set_key, caml_ephe_unset_key
function caml_weak_set(x, i, v) {
  if (v === 0) caml_ephe_unset_key(x, i);
  else caml_ephe_set_key(x, i, v[1]);
  return 0;
}
//Provides: caml_ephe_get_key
//Requires: caml_ephe_key_offset, caml_ephe_data_offset
//Requires: caml_ephe_none
//Alias: caml_weak_get

function caml_ephe_get_key(x, i) {
  var weak = x[caml_ephe_key_offset + i];
  if (weak === caml_ephe_none) return 0;
  if (globalThis.WeakRef && weak instanceof globalThis.WeakRef) {
    weak = weak.deref();
    if (weak === undefined) {
      x[caml_ephe_key_offset + i] = caml_ephe_none;
      x[caml_ephe_data_offset] = caml_ephe_none;
      return 0;
    }
  }
  return [0, weak];
}
//Provides: caml_ephe_get_key_copy
//Requires: caml_ephe_get_key,caml_ephe_key_offset
//Requires: caml_obj_dup
//Alias: caml_weak_get_copy
function caml_ephe_get_key_copy(x, i) {
  var y = caml_ephe_get_key(x, i);
  if (y === 0) return y;
  var z = y[1];
  if (Array.isArray(z)) return [0, caml_obj_dup(z)];
  return y;
}

//Provides: caml_ephe_check_key mutable
//Requires: caml_ephe_key_offset, caml_ephe_data_offset
//Requires: caml_ephe_none
//Alias: caml_weak_check
function caml_ephe_check_key(x, i) {
  var weak = x[caml_ephe_key_offset + i];
  if (weak === caml_ephe_none) return 0;
  if (globalThis.WeakRef && weak instanceof globalThis.WeakRef) {
    weak = weak.deref();
    if (weak === undefined) {
      x[caml_ephe_key_offset + i] = caml_ephe_none;
      x[caml_ephe_data_offset] = caml_ephe_none;
      return 0;
    }
  }
  return 1;
}

//Provides: caml_ephe_blit_key
//Requires: caml_array_blit
//Requires: caml_ephe_key_offset
//Requires: caml_ephe_get_data
//Requires: caml_ephe_set_data_opt
//Alias: caml_weak_blit
function caml_ephe_blit_key(a1, i1, a2, i2, len) {
  var old = caml_ephe_get_data(a1);
  // minus one because caml_array_blit works on ocaml array
  caml_array_blit(
    a1,
    caml_ephe_key_offset + i1 - 1,
    a2,
    caml_ephe_key_offset + i2 - 1,
    len,
  );
  caml_ephe_set_data_opt(a2, old);
  return 0;
}

//Provides: caml_ephe_blit_data
//Requires: caml_ephe_get_data, caml_ephe_set_data_opt
function caml_ephe_blit_data(src, dst) {
  var old = caml_ephe_get_data(src);
  caml_ephe_set_data_opt(dst, old);
  return 0;
}

//Provides: caml_ephe_get_data
//Requires: caml_ephe_data_offset, caml_ephe_key_offset
//Requires: caml_ephe_none
function caml_ephe_get_data(x) {
  var data = x[caml_ephe_data_offset];
  if (data === caml_ephe_none) return 0;
  for (var i = caml_ephe_key_offset; i < x.length; i++) {
    var k = x[i];
    if (globalThis.WeakRef && k instanceof globalThis.WeakRef) {
      var d = k.deref();
      if (d === undefined) {
        x[i] = caml_ephe_none;
        x[caml_ephe_data_offset] = caml_ephe_none;
        return 0;
      }
      if (globalThis.WeakMap) {
        data = data.get(k);
        if (data === undefined) {
          x[caml_ephe_data_offset] = caml_ephe_none;
          return 0;
        }
      }
    }
  }
  return [0, data];
}

//Provides: caml_ephe_get_data_copy
//Requires: caml_ephe_get_data
//Requires: caml_obj_dup
function caml_ephe_get_data_copy(x) {
  var r = caml_ephe_get_data(x);
  if (r === 0) return 0;
  var z = r[1];
  if (Array.isArray(z)) return [0, caml_obj_dup(z)];
  return r;
}

//Provides: caml_ephe_set_data
//Requires: caml_ephe_data_offset, caml_ephe_key_offset
//Requires: caml_ephe_none
function caml_ephe_set_data(x, data) {
  for (var i = x.length - 1; i >= caml_ephe_key_offset; i--) {
    var k = x[i];
    if (globalThis.WeakRef && k instanceof globalThis.WeakRef) {
      var d = k.deref();
      if (d === undefined) {
        x[i] = caml_ephe_none;
        continue;
      }
      if (globalThis.WeakMap) {
        data = new globalThis.WeakMap().set(k, data);
      }
    }
  }
  x[caml_ephe_data_offset] = data;
  return 0;
}

//Provides: caml_ephe_set_data_opt
//Requires: caml_ephe_set_data
//Requires: caml_ephe_unset_data
function caml_ephe_set_data_opt(x, data_opt) {
  if (data_opt === 0) caml_ephe_unset_data(x);
  else caml_ephe_set_data(x, data_opt[1]);
  return 0;
}

//Provides: caml_ephe_unset_data
//Requires: caml_ephe_data_offset
//Requires: caml_ephe_none
function caml_ephe_unset_data(x) {
  x[caml_ephe_data_offset] = caml_ephe_none;
  return 0;
}

//Provides: caml_ephe_check_data
//Requires: caml_ephe_get_data
function caml_ephe_check_data(x) {
  var data = caml_ephe_get_data(x);
  if (data === 0) return 0;
  else return 1;
}
