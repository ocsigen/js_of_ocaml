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
var caml_ephe_key_offset = 3

//Provides: caml_ephe_data_offset
var caml_ephe_data_offset = 2

//Provides: caml_ephe_set_key
//Requires: caml_invalid_argument, caml_ephe_key_offset
function caml_ephe_set_key(x, i, v) {
  if(i < 0 || caml_ephe_key_offset + i >= x.length)
    caml_invalid_argument ("Weak.set");
  if (v instanceof Object && globalThis.WeakRef) {
    if(x[1].register) x[1].register(v, undefined, v);
    x[caml_ephe_key_offset + i] = new globalThis.WeakRef(v);
  }
  else x[caml_ephe_key_offset + i] = v;
  return 0
}

//Provides: caml_ephe_unset_key
//Requires: caml_invalid_argument, caml_ephe_key_offset
function caml_ephe_unset_key(x, i) {
  if(i < 0 || caml_ephe_key_offset + i >= x.length)
    caml_invalid_argument ("Weak.set");
  if(globalThis.WeakRef && x[caml_ephe_key_offset + i] instanceof globalThis.WeakRef && x[1].unregister) {
    var old = x[caml_ephe_key_offset + i].deref();
    if(old !== undefined) {
      var count = 0
      for(var j = caml_ephe_key_offset; j < x.length; j++){
        var key = x[j];
        if(key instanceof globalThis.WeakRef){
          key = key.deref()
          if(key === old) count++;
        }
      }
      if(count == 1) x[1].unregister(old);
    }
  }
  x[caml_ephe_key_offset + i] = undefined;
  return 0
}


//Provides: caml_ephe_create
//Requires: caml_weak_create, caml_ephe_data_offset
function caml_ephe_create (n) {
  var x = caml_weak_create(n);
  return x;
}

//Provides: caml_weak_create
//Requires: caml_ephe_key_offset, caml_invalid_argument,caml_ephe_data_offset
function caml_weak_create (n) {
  if (n < 0) caml_invalid_argument ("Weak.create");
  var x = [251,"caml_ephe_list_head"];
  x.length = caml_ephe_key_offset + n;
  return x;
}

//Provides: caml_weak_set
//Requires: caml_invalid_argument
//Requires: caml_ephe_set_key, caml_ephe_unset_key
function caml_weak_set(x, i, v) {
  if(v == 0) caml_ephe_unset_key(x,i)
  else caml_ephe_set_key(x,i,v[1])
  return 0;
}
//Provides: caml_ephe_get_key
//Requires: caml_ephe_key_offset, caml_invalid_argument
//Alias: caml_weak_get
function caml_ephe_get_key(x, i) {
  if(i < 0 || caml_ephe_key_offset + i >= x.length)
    caml_invalid_argument ("Weak.get_key");
  var weak = x[caml_ephe_key_offset + i ];
  if(globalThis.WeakRef && weak instanceof globalThis.WeakRef) weak = weak.deref();
  return (weak===undefined)?0:[0, weak];
}
//Provides: caml_ephe_get_key_copy
//Requires: caml_ephe_get_key,caml_ephe_key_offset
//Requires: caml_obj_dup, caml_invalid_argument
//Alias: caml_weak_get_copy
function caml_ephe_get_key_copy(x, i) {
  if(i < 0 || caml_ephe_key_offset + i >= x.length)
    caml_invalid_argument ("Weak.get_copy");
  var y = caml_ephe_get_key(x, i);
  if (y === 0) return y;
  var z = y[1];
  if (z instanceof Array) return [0, caml_obj_dup(z)];
  return y;
}

//Provides: caml_ephe_check_key mutable
//Requires: caml_ephe_key_offset
//Alias: caml_weak_check
function caml_ephe_check_key(x, i) {
  var weak = x[caml_ephe_key_offset + i];
  if(globalThis.WeakRef && weak instanceof globalThis.WeakRef) weak = weak.deref();
  if(weak===undefined)
    return 0;
  else
    return 1;
}

//Provides: caml_ephe_blit_key
//Requires: caml_array_blit
//Requires: caml_ephe_key_offset
//Alias: caml_weak_blit
function caml_ephe_blit_key(a1, i1, a2, i2, len) {
  // minus one because caml_array_blit works on ocaml array
  caml_array_blit(a1, caml_ephe_key_offset + i1 - 1,
                  a2, caml_ephe_key_offset + i2 - 1,
                  len);
  return 0;
}

//Provides: caml_ephe_blit_data
//Requires: caml_ephe_data_offset, caml_ephe_set_data, caml_ephe_unset_data
function caml_ephe_blit_data(src, dst){
  var n = src[caml_ephe_data_offset];
  if(n === undefined) caml_ephe_unset_data(dst);
  else caml_ephe_set_data(dst, n);
  return 0;
}

//Provides: caml_ephe_get_data
//Requires: caml_ephe_data_offset
function caml_ephe_get_data(x){
  if(x[caml_ephe_data_offset] === undefined)
    return 0;
  else
    return [0, x[caml_ephe_data_offset]];
}

//Provides: caml_ephe_get_data_copy
//Requires: caml_ephe_data_offset
//Requires: caml_obj_dup
function caml_ephe_get_data_copy(x){
  if(x[caml_ephe_data_offset] === undefined)
    return 0;
  else
    return [0, caml_obj_dup(x[caml_ephe_data_offset])];
}

//Provides: caml_ephe_set_data
//Requires: caml_ephe_data_offset, caml_ephe_key_offset, caml_ephe_unset_data
function caml_ephe_set_data(x, data){
  if(globalThis.FinalizationRegistry && globalThis.WeakRef) {
    if(! (x[1] instanceof globalThis.FinalizationRegistry)) {
      x[1] = new globalThis.FinalizationRegistry(function () { caml_ephe_unset_data(x) });
      //register all keys
      for(var j = caml_ephe_key_offset; j < x.length; j++){
        var key = x[j];
        if(key instanceof globalThis.WeakRef) {
          key = key.deref();
          if(key) x[1].register(key, undefined, key);
        }
      }
    }
  }
  x[caml_ephe_data_offset] = data;
  return 0;
}

//Provides: caml_ephe_unset_data
//Requires: caml_ephe_data_offset, caml_ephe_key_offset
function caml_ephe_unset_data(x){
  if(globalThis.FinalizationRegistry && globalThis.WeakRef) {
    if(x[1] instanceof globalThis.FinalizationRegistry){
      //unregister all keys
      for(var j = caml_ephe_key_offset; j < x.length; j++){
        var key = x[j];
        if(key instanceof globalThis.WeakRef) {
          key = key.deref();
          if(key) x[1].unregister(key);
        }
      }
    }
  }
  x[caml_ephe_data_offset] = undefined;
  return 0;
}

//Provides: caml_ephe_check_data
//Requires: caml_ephe_data_offset
function caml_ephe_check_data(x){
  if(x[caml_ephe_data_offset] === undefined)
    return 0;
  else
    return 1;
}
