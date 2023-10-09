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

//Provides: caml_update_dummy
function caml_update_dummy (x, y) {
  if( typeof y==="function" ) { x.fun = y; return 0; }
  if( y.fun ) { x.fun = y.fun; return 0; }
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}

//Provides: caml_alloc_dummy_infix
//Requires: caml_call_gen
function caml_alloc_dummy_infix () {
  return function f (x) { return caml_call_gen(f.fun, [x]) }
}

//Provides: caml_obj_is_block const (const)
function caml_obj_is_block (x) { return +(x instanceof Array); }


//Provides: caml_obj_tag
//Requires: caml_is_ml_bytes, caml_is_ml_string
function caml_obj_tag (x) {
  if ((x instanceof Array) && x[0] == (x[0] >>> 0))
    return x[0]
  else if (caml_is_ml_bytes(x))
    return 252
  else if (caml_is_ml_string(x))
    return 252
  else if ((x instanceof Function) || typeof x == "function")
    return 247
  else if (x && x.caml_custom)
    return 255
  else
    return 1000
}

//Provides: caml_obj_set_tag (mutable, const)
function caml_obj_set_tag (x, tag) { x[0] = tag; return 0; }
//Provides: caml_obj_block const (const,const)
function caml_obj_block (tag, size) {
  var o = new Array(size+1);
  o[0]=tag;
  for (var i = 1; i <= size; i++) o[i] = 0;
  return o;
}

//Provides: caml_obj_with_tag
function caml_obj_with_tag(tag,x) {
  var l = x.length;
  var a = new Array(l);
  a[0] = tag;
  for(var i = 1; i < l; i++ ) a[i] = x[i];
  return a;
}

//Provides: caml_obj_dup mutable (mutable)
function caml_obj_dup (x) {
  var l = x.length;
  var a = new Array(l);
  for(var i = 0; i < l; i++ ) a[i] = x[i];
  return a;
}

//Provides: caml_obj_truncate (mutable, const)
//Requires: caml_invalid_argument
function caml_obj_truncate (x, s) {
  if (s<=0 || s + 1 > x.length)
    caml_invalid_argument ("Obj.truncate");
  if (x.length != s + 1) x.length = s + 1;
  return 0;
}

//Provides: caml_obj_make_forward
function caml_obj_make_forward (b,v) {
  b[0]=250;
  b[1]=v;
  return 0
}

//Provides: caml_obj_compare_and_swap
function caml_obj_compare_and_swap(x,i,old,n){
  if(x[i+1] == old) {
    x[i+1] = n;
    return 1;
  }
  return 0
}

//Provides: caml_obj_is_shared
function caml_obj_is_shared(x){
  return 1
}

//Provides: caml_lazy_make_forward const (mutable)
function caml_lazy_make_forward (v) { return [250, v]; }

///////////// CamlinternalOO
//Provides: caml_get_public_method const
var caml_method_cache = [];
function caml_get_public_method (obj, tag, cacheid) {
  var meths = obj[1];
  var ofs = caml_method_cache[cacheid];
  if (ofs === undefined) {
    // Make sure the array is not sparse
    for (var i = caml_method_cache.length; i < cacheid; i++)
      caml_method_cache[i] = 0;
  } else if (meths[ofs] === tag) {
    return meths[ofs - 1];
  }
  var li = 3, hi = meths[1] * 2 + 1, mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < meths[mi+1]) hi = mi-2;
    else li = mi;
  }
  caml_method_cache[cacheid] = li + 1;
  /* return 0 if tag is not there */
  return (tag == meths[li+1] ? meths[li] : 0);
}

//Provides: caml_oo_last_id
var caml_oo_last_id = 0;

//Provides: caml_set_oo_id
//Requires: caml_oo_last_id
function caml_set_oo_id (b) {
  b[2]=caml_oo_last_id++;
  return b;
}

//Provides: caml_fresh_oo_id const
//Requires: caml_oo_last_id
function caml_fresh_oo_id() {
  return caml_oo_last_id++;
}

//Provides: caml_obj_raw_field
function caml_obj_raw_field(o,i) { return o[i+1] }

//Provides: caml_obj_set_raw_field
function caml_obj_set_raw_field(o,i,v) { return o[i+1] = v }

//Provides: caml_obj_reachable_words
function caml_obj_reachable_words(o) { return 0; }

//Provides: caml_obj_add_offset
//Requires: caml_failwith
function caml_obj_add_offset(v,offset) {
  caml_failwith("Obj.add_offset is not supported");
}

//Provides: caml_obj_update_tag
function caml_obj_update_tag(b,o,n) {
    if(b[0]==o) { b[0] = n; return 1 }
    return 0
}

//Provides: caml_lazy_update_to_forcing
//Requires: caml_obj_update_tag
function caml_lazy_update_to_forcing(o) {
  if ((o instanceof Array) && o[0] == (o[0] >>> 0) &&
      caml_obj_update_tag(o, 246, 244)) {
    return 0;
  } else {
    return 1;
  }
}

//Provides: caml_lazy_update_to_forward
//Requires: caml_obj_update_tag
  function caml_lazy_update_to_forward(o) {
  caml_obj_update_tag(o,244,250);
  return 0; // unit
}


//Provides: caml_lazy_reset_to_lazy
//Requires: caml_obj_update_tag
function caml_lazy_reset_to_lazy(o) {
  caml_obj_update_tag(o,244,246);
  return 0;
}

//Provides: caml_lazy_read_result
//Requires: caml_obj_tag
function caml_lazy_read_result(o) {
  return (caml_obj_tag(o) == 250)?o[1]:o;
}


//Provides: caml_is_continuation_tag
//Version: < 5
function caml_is_continuation_tag(t) {
  return 0;
}

//Provides: caml_is_continuation_tag
//Version: >= 5
function caml_is_continuation_tag(t) {
  return (t == 245) ? 1 : 0;
}

//Provides: caml_custom_identifier
//Requires: caml_string_of_jsstring
function caml_custom_identifier (o) {
  return caml_string_of_jsstring(o.custom_tag);
}
