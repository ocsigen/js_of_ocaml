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
function caml_update_dummy(x, y) {
  if (y.fun) {
    x.fun = y.fun;
    return 0;
  }
  if (typeof y === "function") {
    x.fun = y;
    return 0;
  }
  var i = y.length;
  while (i--) x[i] = y[i];
  // Take over the block descriptor of y (see caml_block_class)
  if (y.constructor !== Array)
    Object.setPrototypeOf(x, Object.getPrototypeOf(y));
  return 0;
}

//Provides: caml_alloc_dummy_infix
//Requires: caml_call_gen
//Version: < 5.4
function caml_alloc_dummy_infix() {
  return function f(x) {
    return caml_call_gen(f.fun, [x]);
  };
}

//Provides: caml_alloc_dummy_lazy
//Version: >= 5.4
function caml_alloc_dummy_lazy(_unit) {
  return [0, 0];
}

//Provides: caml_update_dummy_lazy
//Requires: caml_obj_tag
//Requires: caml_update_dummy
//Version: >= 5.4
function caml_update_dummy_lazy(dummy, newval) {
  switch (caml_obj_tag(newval)) {
    case 246: // Lazy
    case 244: // Forcing
    case 250: // Forward
      caml_update_dummy(dummy, newval);
      break;
    default:
      dummy[1] = newval;
      dummy[0] = 250;
      break;
  }
  return 0;
}

//Provides: caml_obj_is_stack
//Version: >= 5.2, < 5.3
//OxCaml
function caml_obj_is_stack(_x) {
  return 0;
}

//Provides: caml_succ_scannable_prefix_len
//Version: >= 5.2, < 5.3
//OxCaml
function caml_succ_scannable_prefix_len(_x) {
  return 0;
}

//Provides: caml_obj_uniquely_reachable_words
//Requires: caml_failwith
//Version: >= 5.2, < 5.3
//OxCaml
function caml_obj_uniquely_reachable_words(_x) {
  caml_failwith("Obj.uniquely_reachable_words is not available in javascript.");
}

//Provides: caml_obj_tag
//Requires: caml_is_ml_bytes, caml_is_ml_string
function caml_obj_tag(x) {
  if (x === null) return 1010;
  else if (Array.isArray(x) && x[0] === x[0] >>> 0) return x[0];
  else if (caml_is_ml_bytes(x)) return 252;
  else if (caml_is_ml_string(x)) return 252;
  else if (x instanceof Function || typeof x === "function") return 247;
  else if (x?.caml_custom) return 255;
  else return 1000;
}

//Provides: caml_obj_set_tag (mutable, const)
//Version: < 5.0
function caml_obj_set_tag(x, tag) {
  x[0] = tag;
  return 0;
}
//Provides: caml_obj_block const (const,const)
function caml_obj_block(tag, size) {
  // TODO: fail for value that are not represented as an array
  var o = new Array(size + 1);
  o[0] = tag;
  for (var i = 1; i <= size; i++) o[i] = 0;
  return o;
}

//Provides: caml_obj_with_tag
function caml_obj_with_tag(tag, x) {
  var l = x.length;
  var a = new Array(l);
  a[0] = tag;
  for (var i = 1; i < l; i++) a[i] = x[i];
  return a;
}

//Provides: caml_obj_dup mutable (mutable)
function caml_obj_dup(x) {
  if (x === null) return null;
  if (typeof x === "number") return x;
  var r = x.slice();
  // Keep the block descriptor (see caml_block_class), which slice drops
  if (x.constructor !== Array)
    Object.setPrototypeOf(r, Object.getPrototypeOf(x));
  return r;
}

//Provides: caml_obj_truncate (mutable, const)
//Requires: caml_invalid_argument
//Version: < 5.0
function caml_obj_truncate(x, s) {
  if (s <= 0 || s + 1 > x.length) caml_invalid_argument("Obj.truncate");
  if (x.length !== s + 1) x.length = s + 1;
  return 0;
}

//Provides: caml_obj_make_forward
//Version: < 5.0
function caml_obj_make_forward(b, v) {
  b[0] = 250;
  b[1] = v;
  return 0;
}

//Provides: caml_obj_compare_and_swap
//Version: >= 5.0
function caml_obj_compare_and_swap(x, i, old, n) {
  if (x[i + 1] === old) {
    x[i + 1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_obj_is_shared
//Version: >= 5.0
function caml_obj_is_shared(_x) {
  return 1;
}

//Provides: caml_lazy_make_forward const (mutable)
function caml_lazy_make_forward(v) {
  return [250, v];
}

//Provides: caml_method_cache
var caml_method_cache = [];

//Provides: caml_oo_cache_id const
//Requires: caml_method_cache
function caml_oo_cache_id() {
  var cacheid = caml_method_cache.length;
  caml_method_cache[cacheid] = 0;
  return cacheid;
}

///////////// CamlinternalOO
//Provides: caml_get_cached_method const
//Requires: caml_method_cache
function caml_get_cached_method(obj, tag, cacheid) {
  var meths = obj[1];
  var ofs = caml_method_cache[cacheid];
  if (meths[ofs + 4] === tag) {
    return meths[ofs + 3];
  }
  var li = 3,
    hi = meths[1] * 2 + 1,
    mi;
  while (li < hi) {
    mi = ((li + hi) >> 1) | 1;
    if (tag < meths[mi + 1]) hi = mi - 2;
    else li = mi;
  }
  caml_method_cache[cacheid] = li - 3;
  return meths[li];
}

//Provides: caml_get_public_method const
function caml_get_public_method(obj, tag) {
  var meths = obj[1];
  var li = 3,
    hi = meths[1] * 2 + 1,
    mi;
  while (li < hi) {
    mi = ((li + hi) >> 1) | 1;
    if (tag < meths[mi + 1]) hi = mi - 2;
    else li = mi;
  }
  /* return 0 if tag is not there */
  return tag === meths[li + 1] ? meths[li] : 0;
}

//Provides: caml_oo_last_id
var caml_oo_last_id = 0;

//Provides: caml_set_oo_id
//Requires: caml_oo_last_id
function caml_set_oo_id(b) {
  b[2] = caml_oo_last_id++;
  return b;
}

//Provides: caml_fresh_oo_id const
//Requires: caml_oo_last_id
function caml_fresh_oo_id() {
  return caml_oo_last_id++;
}

//Provides: caml_obj_raw_field
function caml_obj_raw_field(o, i) {
  return o[i + 1];
}

//Provides: caml_obj_set_raw_field
function caml_obj_set_raw_field(o, i, v) {
  return (o[i + 1] = v);
}

//Provides: caml_obj_reachable_words
function caml_obj_reachable_words(_o) {
  return 0;
}

//Provides: caml_obj_add_offset
//Requires: caml_failwith
function caml_obj_add_offset(_v, _offset) {
  caml_failwith("Obj.add_offset is not supported");
}

//Provides: caml_obj_update_tag
//Version: >= 5.0
function caml_obj_update_tag(b, o, n) {
  if (b[0] === o) {
    b[0] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_lazy_update_to_forcing
//Requires: caml_obj_update_tag
//Version: >= 5.0
function caml_lazy_update_to_forcing(o) {
  if (
    Array.isArray(o) &&
    o[0] === o[0] >>> 0 &&
    caml_obj_update_tag(o, 246, 244)
  ) {
    return 0;
  } else {
    return 1;
  }
}

//Provides: caml_lazy_update_to_forward
//Requires: caml_obj_update_tag
//Version: >= 5.0
function caml_lazy_update_to_forward(o) {
  caml_obj_update_tag(o, 244, 250);
  return 0; // unit
}

//Provides: caml_lazy_reset_to_lazy
//Requires: caml_obj_update_tag
//Version: >= 5.0
function caml_lazy_reset_to_lazy(o) {
  caml_obj_update_tag(o, 244, 246);
  return 0;
}

//Provides: caml_lazy_read_result
//Requires: caml_obj_tag
//Version: >= 5.0
function caml_lazy_read_result(o) {
  return caml_obj_tag(o) === 250 ? o[1] : o;
}

//Provides: caml_is_continuation_tag
//Version: < 5
function caml_is_continuation_tag(_t) {
  return 0;
}

//Provides: caml_is_continuation_tag
//Version: >= 5
function caml_is_continuation_tag(t) {
  return t === 245 ? 1 : 0;
}

//Provides: caml_custom_identifier
//Requires: caml_string_of_jsstring
function caml_custom_identifier(o) {
  return caml_string_of_jsstring(o.caml_custom || "");
}

//Provides: caml_ml_gc_ramp_up
//Requires: caml_callback
//Version: >= 5.4
function caml_ml_gc_ramp_up(f) {
  var a = caml_callback(f, [0]);
  var suspended = 0;
  return [0, a, suspended];
}

//Provides: caml_ml_gc_ramp_down
//Version: >= 5.4
function caml_ml_gc_ramp_down(_suspended_collection_work) {
  return 0;
}

//Provides: caml_int_as_pointer
//Requires: caml_failwith
//Version: >= 5.2, < 5.3
//OxCaml
function caml_int_as_pointer(i) {
  // Special-case null pointers for [or_null].
  if (i === 0) return null;
  caml_failwith("%int_as_pointer is not supported in javascript.");
}

//Provides: caml_compiler_block_descs_ref
//If: introspect
var caml_compiler_block_descs_ref = [0, 0];

//Provides: caml_compiler_block_descs
//Requires: caml_compiler_block_descs_ref
//If: introspect
function caml_compiler_block_descs(_unit) {
  return caml_compiler_block_descs_ref;
}

//Provides: caml_register_block_descs
//Requires: caml_compiler_block_descs_ref
//If: introspect
// Prepend the descriptors of a program or a compilation unit to the list
// [Introspect.Desc.compiler_descriptors ()] watches: it registers the new
// prefix, up to the list it has already seen.
function caml_register_block_descs(descs) {
  var elts = [];
  for (var l = descs; l !== 0; l = l[2]) elts.push(l[1]);
  var res = caml_compiler_block_descs_ref[1];
  for (var i = elts.length - 1; i >= 0; i--) res = [0, elts[i], res];
  caml_compiler_block_descs_ref[1] = res;
  return 0;
}

//Provides: caml_obj_reserved_bits
//If: introspect
// Must match [Config.reserved_header_bits] of the compiler that produced
// the bytecode: descriptor indices are hashes truncated to that many bits.
function caml_obj_reserved_bits(_unit) {
  return 22;
}

//Provides: caml_block_classes
//If: introspect
var caml_block_classes = new Map();

//Provides: caml_block_class
//Requires: caml_block_classes
//If: introspect
// Blocks with a descriptor are instances of an Array subclass which carries
// the descriptor index, so that the objects themselves are left untouched.
// Derived arrays (slice, concat, map...) are plain arrays: they are either
// blocks allocated by the runtime, which carry no descriptor, or JavaScript
// arrays handed to foreign code (caml_js_from_array).
function caml_block_class(desc) {
  var cls = caml_block_classes.get(desc);
  if (cls === undefined) {
    cls = class extends Array {
      static desc = desc;
      static get [Symbol.species]() {
        return Array;
      }
    };
    caml_block_classes.set(desc, cls);
  }
  return cls;
}

//Provides: caml_obj_get_reserved
//If: introspect
function caml_obj_get_reserved(obj) {
  if (Array.isArray(obj)) {
    var desc = obj.constructor.desc;
    if (desc !== undefined) return desc;
  }
  return 0;
}

//Provides: caml_obj_set_reserved
//Requires: caml_block_class
//If: introspect
function caml_obj_set_reserved(obj, desc) {
  if (!Array.isArray(obj) || obj.length < 2) return 0;
  Object.setPrototypeOf(obj, caml_block_class(desc).prototype);
  return 1;
}

//Provides: caml_read_bdsc_section
//If: introspect
// The descriptors of the program are registered with
// [caml_register_block_descs] instead.
function caml_read_bdsc_section(_unit) {
  return 0;
}
