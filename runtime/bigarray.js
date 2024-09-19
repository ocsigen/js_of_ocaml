// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard, Andy Ray
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
//
// Bigarray.
//
// - all bigarray types including Int64 and Complex.
// - fortran + c layouts
// - sub/slice/reshape
// - retain fast path for 1d array access

//Provides: caml_ba_init const
function caml_ba_init() {
  return 0;
}

//Provides: caml_ba_get_size
//Requires: caml_invalid_argument
function caml_ba_get_size(dims) {
  const n_dims = dims.length;
  let size = 1;
  for (let i = 0; i < n_dims; i++) {
    if (dims[i] < 0)
      caml_invalid_argument("Bigarray.create: negative dimension");
    size *= dims[i];
  }
  return size;
}

//Provides: caml_ba_get_size_per_element
function caml_ba_get_size_per_element(kind) {
  switch (kind) {
    case 7:
    case 10:
    case 11:
      return 2;
    default:
      return 1;
  }
}

//Provides: caml_ba_create_buffer
//Requires: caml_ba_get_size_per_element
//Requires: caml_invalid_argument
function caml_ba_create_buffer(kind, size) {
  let view;
  switch (kind) {
    case 0:
      view = Float32Array;
      break;
    case 1:
      view = Float64Array;
      break;
    case 2:
      view = Int8Array;
      break;
    case 3:
      view = Uint8Array;
      break;
    case 4:
      view = Int16Array;
      break;
    case 5:
      view = Uint16Array;
      break;
    case 6:
      view = Int32Array;
      break;
    case 7:
      view = Int32Array;
      break;
    case 8:
      view = Int32Array;
      break;
    case 9:
      view = Int32Array;
      break;
    case 10:
      view = Float32Array;
      break;
    case 11:
      view = Float64Array;
      break;
    case 12:
      view = Uint8Array;
      break;
  }
  if (!view) caml_invalid_argument("Bigarray.create: unsupported kind");
  const data = new view(size * caml_ba_get_size_per_element(kind));
  return data;
}

//Provides: caml_ba_custom_name
//Version: < 4.11
const caml_ba_custom_name = "_bigarray";

//Provides: caml_ba_custom_name
//Version: >= 4.11
const caml_ba_custom_name = "_bigarr02";

//Provides: Ml_Bigarray
//Requires: caml_array_bound_error, caml_invalid_argument, caml_ba_custom_name
//Requires: caml_int64_create_lo_hi, caml_int64_hi32, caml_int64_lo32
function Ml_Bigarray(kind, layout, dims, buffer) {
  this.kind = kind;
  this.layout = layout;
  this.dims = dims;
  this.data = buffer;
}

Ml_Bigarray.prototype.caml_custom = caml_ba_custom_name;

Ml_Bigarray.prototype.offset = function (arg) {
  let ofs = 0;
  if (typeof arg === "number") arg = [arg];
  if (!Array.isArray(arg)) caml_invalid_argument("bigarray.js: invalid offset");
  if (this.dims.length !== arg.length)
    caml_invalid_argument("Bigarray.get/set: bad number of dimensions");
  if (this.layout === 0 /* c_layout */) {
    for (let i = 0; i < this.dims.length; i++) {
      if (arg[i] < 0 || arg[i] >= this.dims[i]) caml_array_bound_error();
      ofs = ofs * this.dims[i] + arg[i];
    }
  } else {
    for (let i = this.dims.length - 1; i >= 0; i--) {
      if (arg[i] < 1 || arg[i] > this.dims[i]) {
        caml_array_bound_error();
      }
      ofs = ofs * this.dims[i] + (arg[i] - 1);
    }
  }
  return ofs;
};

Ml_Bigarray.prototype.get = function (ofs) {
  switch (this.kind) {
    case 7: {
      // Int64
      const l = this.data[ofs * 2 + 0];
      const h = this.data[ofs * 2 + 1];
      return caml_int64_create_lo_hi(l, h);
    }
    case 10:
    case 11: {
      // Complex32, Complex64
      const r = this.data[ofs * 2 + 0];
      const i = this.data[ofs * 2 + 1];
      return [254, r, i];
    }
    default:
      return this.data[ofs];
  }
};

Ml_Bigarray.prototype.set = function (ofs, v) {
  switch (this.kind) {
    case 7:
      // Int64
      this.data[ofs * 2 + 0] = caml_int64_lo32(v);
      this.data[ofs * 2 + 1] = caml_int64_hi32(v);
      break;
    case 10:
    case 11:
      // Complex32, Complex64
      this.data[ofs * 2 + 0] = v[1];
      this.data[ofs * 2 + 1] = v[2];
      break;
    default:
      this.data[ofs] = v;
      break;
  }
  return 0;
};

Ml_Bigarray.prototype.fill = function (v) {
  switch (this.kind) {
    case 7: {
      // Int64
      const a = caml_int64_lo32(v);
      const b = caml_int64_hi32(v);
      if (a === b) {
        this.data.fill(a);
      } else {
        for (let i = 0; i < this.data.length; i++) {
          this.data[i] = i % 2 === 0 ? a : b;
        }
      }
      break;
    }
    case 10:
    case 11: {
      // Complex32, Complex64
      const im = v[1];
      const re = v[2];
      if (im === re) {
        this.data.fill(im);
      } else {
        for (let i = 0; i < this.data.length; i++) {
          this.data[i] = i % 2 === 0 ? im : re;
        }
      }
      break;
    }
    default:
      this.data.fill(v);
      break;
  }
};

Ml_Bigarray.prototype.compare = function (b, total) {
  if (this.layout !== b.layout || this.kind !== b.kind) {
    const k1 = this.kind | (this.layout << 8);
    const k2 = b.kind | (b.layout << 8);
    return k2 - k1;
  }
  if (this.dims.length !== b.dims.length) {
    return b.dims.length - this.dims.length;
  }
  for (let i = 0; i < this.dims.length; i++)
    if (this.dims[i] !== b.dims[i]) return this.dims[i] < b.dims[i] ? -1 : 1;
  switch (this.kind) {
    case 0:
    case 1:
    case 10:
    case 11: {
      // Floats
      let x;
      let y;
      for (let i = 0; i < this.data.length; i++) {
        x = this.data[i];
        y = b.data[i];
        if (x < y) return -1;
        if (x > y) return 1;
        if (x !== y) {
          if (!total) return Number.NaN;
          if (x === x) return 1;
          if (y === y) return -1;
        }
      }
      break;
    }
    case 7:
      // Int64
      for (let i = 0; i < this.data.length; i += 2) {
        // Check highest bits first
        if (this.data[i + 1] < b.data[i + 1]) return -1;
        if (this.data[i + 1] > b.data[i + 1]) return 1;
        if (this.data[i] >>> 0 < b.data[i] >>> 0) return -1;
        if (this.data[i] >>> 0 > b.data[i] >>> 0) return 1;
      }
      break;
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 8:
    case 9:
    case 12:
      for (let i = 0; i < this.data.length; i++) {
        if (this.data[i] < b.data[i]) return -1;
        if (this.data[i] > b.data[i]) return 1;
      }
      break;
  }
  return 0;
};

//Provides: Ml_Bigarray_c_1_1
//Requires: Ml_Bigarray, caml_array_bound_error, caml_invalid_argument
function Ml_Bigarray_c_1_1(kind, layout, dims, buffer) {
  this.kind = kind;
  this.layout = layout;
  this.dims = dims;
  this.data = buffer;
}

Ml_Bigarray_c_1_1.prototype = new Ml_Bigarray();
Ml_Bigarray_c_1_1.prototype.offset = function (arg) {
  if (typeof arg !== "number") {
    if (Array.isArray(arg) && arg.length === 1) arg = arg[0];
    else caml_invalid_argument("Ml_Bigarray_c_1_1.offset");
  }
  if (arg < 0 || arg >= this.dims[0]) caml_array_bound_error();
  return arg;
};

Ml_Bigarray_c_1_1.prototype.get = function (ofs) {
  return this.data[ofs];
};

Ml_Bigarray_c_1_1.prototype.set = function (ofs, v) {
  this.data[ofs] = v;
  return 0;
};

Ml_Bigarray_c_1_1.prototype.fill = function (v) {
  this.data.fill(v);
  return 0;
};

//Provides: caml_ba_compare
function caml_ba_compare(a, b, total) {
  return a.compare(b, total);
}

//Provides: caml_ba_create_unsafe
//Requires: Ml_Bigarray, Ml_Bigarray_c_1_1, caml_ba_get_size, caml_ba_get_size_per_element
//Requires: caml_invalid_argument
function caml_ba_create_unsafe(kind, layout, dims, data) {
  const size_per_element = caml_ba_get_size_per_element(kind);
  if (caml_ba_get_size(dims) * size_per_element !== data.length) {
    caml_invalid_argument("length doesn't match dims");
  }
  if (
    layout === 0 && // c_layout
    dims.length === 1 && // Array1
    size_per_element === 1
  )
    // 1-to-1 mapping
    return new Ml_Bigarray_c_1_1(kind, layout, dims, data);
  return new Ml_Bigarray(kind, layout, dims, data);
}

//Provides: caml_ba_create
//Requires: caml_js_from_array
//Requires: caml_ba_get_size, caml_ba_create_unsafe
//Requires: caml_ba_create_buffer
function caml_ba_create(kind, layout, dims_ml) {
  const dims = caml_js_from_array(dims_ml);
  const data = caml_ba_create_buffer(kind, caml_ba_get_size(dims));
  return caml_ba_create_unsafe(kind, layout, dims, data);
}

//Provides: caml_ba_change_layout
//Requires: caml_ba_create_unsafe
function caml_ba_change_layout(ba, layout) {
  if (ba.layout === layout) return ba;
  const new_dims = [];
  for (let i = 0; i < ba.dims.length; i++)
    new_dims[i] = ba.dims[ba.dims.length - i - 1];
  return caml_ba_create_unsafe(ba.kind, layout, new_dims, ba.data);
}

//Provides: caml_ba_kind
function caml_ba_kind(ba) {
  return ba.kind;
}

//Provides: caml_ba_layout
function caml_ba_layout(ba) {
  return ba.layout;
}

//Provides: caml_ba_num_dims
function caml_ba_num_dims(ba) {
  return ba.dims.length;
}

//Provides: caml_ba_dim
//Requires: caml_invalid_argument
function caml_ba_dim(ba, i) {
  if (i < 0 || i >= ba.dims.length) caml_invalid_argument("Bigarray.dim");
  return ba.dims[i];
}

//Provides: caml_ba_dim_1
//Requires: caml_ba_dim
function caml_ba_dim_1(ba) {
  return caml_ba_dim(ba, 0);
}

//Provides: caml_ba_dim_2
//Requires: caml_ba_dim
function caml_ba_dim_2(ba) {
  return caml_ba_dim(ba, 1);
}

//Provides: caml_ba_dim_3
//Requires: caml_ba_dim
function caml_ba_dim_3(ba) {
  return caml_ba_dim(ba, 2);
}

//Provides: caml_ba_get_generic
//Requires: caml_js_from_array
function caml_ba_get_generic(ba, i) {
  const ofs = ba.offset(caml_js_from_array(i));
  return ba.get(ofs);
}

//Provides: caml_ba_uint8_get16
//Requires: caml_array_bound_error
function caml_ba_uint8_get16(ba, i0) {
  const ofs = ba.offset(i0);
  if (ofs + 1 >= ba.data.length) caml_array_bound_error();
  const b1 = ba.get(ofs);
  const b2 = ba.get(ofs + 1);
  return b1 | (b2 << 8);
}

//Provides: caml_ba_uint8_get32
//Requires: caml_array_bound_error
function caml_ba_uint8_get32(ba, i0) {
  const ofs = ba.offset(i0);
  if (ofs + 3 >= ba.data.length) caml_array_bound_error();
  const b1 = ba.get(ofs + 0);
  const b2 = ba.get(ofs + 1);
  const b3 = ba.get(ofs + 2);
  const b4 = ba.get(ofs + 3);
  return (b1 << 0) | (b2 << 8) | (b3 << 16) | (b4 << 24);
}

//Provides: caml_ba_uint8_get64
//Requires: caml_array_bound_error, caml_int64_of_bytes
function caml_ba_uint8_get64(ba, i0) {
  const ofs = ba.offset(i0);
  if (ofs + 7 >= ba.data.length) caml_array_bound_error();
  const b1 = ba.get(ofs + 0);
  const b2 = ba.get(ofs + 1);
  const b3 = ba.get(ofs + 2);
  const b4 = ba.get(ofs + 3);
  const b5 = ba.get(ofs + 4);
  const b6 = ba.get(ofs + 5);
  const b7 = ba.get(ofs + 6);
  const b8 = ba.get(ofs + 7);
  return caml_int64_of_bytes([b8, b7, b6, b5, b4, b3, b2, b1]);
}

//Provides: caml_ba_get_1
function caml_ba_get_1(ba, i0) {
  return ba.get(ba.offset(i0));
}

//Provides: caml_ba_get_2
function caml_ba_get_2(ba, i0, i1) {
  return ba.get(ba.offset([i0, i1]));
}

//Provides: caml_ba_get_3
function caml_ba_get_3(ba, i0, i1, i2) {
  return ba.get(ba.offset([i0, i1, i2]));
}

//Provides: caml_ba_set_generic
//Requires: caml_js_from_array
function caml_ba_set_generic(ba, i, v) {
  ba.set(ba.offset(caml_js_from_array(i)), v);
  return 0;
}

//Provides: caml_ba_uint8_set16
//Requires: caml_array_bound_error
function caml_ba_uint8_set16(ba, i0, v) {
  const ofs = ba.offset(i0);
  if (ofs + 1 >= ba.data.length) caml_array_bound_error();
  ba.set(ofs + 0, v & 0xff);
  ba.set(ofs + 1, (v >>> 8) & 0xff);
  return 0;
}

//Provides: caml_ba_uint8_set32
//Requires: caml_array_bound_error
function caml_ba_uint8_set32(ba, i0, v) {
  const ofs = ba.offset(i0);
  if (ofs + 3 >= ba.data.length) caml_array_bound_error();
  ba.set(ofs + 0, v & 0xff);
  ba.set(ofs + 1, (v >>> 8) & 0xff);
  ba.set(ofs + 2, (v >>> 16) & 0xff);
  ba.set(ofs + 3, (v >>> 24) & 0xff);
  return 0;
}

//Provides: caml_ba_uint8_set64
//Requires: caml_array_bound_error, caml_int64_to_bytes
function caml_ba_uint8_set64(ba, i0, v) {
  const ofs = ba.offset(i0);
  if (ofs + 7 >= ba.data.length) caml_array_bound_error();
  const v_ = caml_int64_to_bytes(v);
  for (let i = 0; i < 8; i++) ba.set(ofs + i, v_[7 - i]);
  return 0;
}

//Provides: caml_ba_set_1
function caml_ba_set_1(ba, i0, v) {
  ba.set(ba.offset(i0), v);
  return 0;
}

//Provides: caml_ba_set_2
function caml_ba_set_2(ba, i0, i1, v) {
  ba.set(ba.offset([i0, i1]), v);
  return 0;
}

//Provides: caml_ba_set_3
function caml_ba_set_3(ba, i0, i1, i2, v) {
  ba.set(ba.offset([i0, i1, i2]), v);
  return 0;
}

//Provides: caml_ba_fill
function caml_ba_fill(ba, v) {
  ba.fill(v);
  return 0;
}

//Provides: caml_ba_blit
//Requires: caml_invalid_argument
function caml_ba_blit(src, dst) {
  if (dst.dims.length !== src.dims.length)
    caml_invalid_argument("Bigarray.blit: dimension mismatch");
  for (let i = 0; i < dst.dims.length; i++)
    if (dst.dims[i] !== src.dims[i])
      caml_invalid_argument("Bigarray.blit: dimension mismatch");
  dst.data.set(src.data);
  return 0;
}

//Provides: caml_ba_sub
//Requires: caml_invalid_argument, caml_ba_create_unsafe, caml_ba_get_size
//Requires: caml_ba_get_size_per_element
function caml_ba_sub(ba, ofs, len) {
  let changed_dim;
  let mul = 1;
  if (ba.layout === 0) {
    for (let i = 1; i < ba.dims.length; i++) mul *= ba.dims[i];
    changed_dim = 0;
  } else {
    for (let i = 0; i < ba.dims.length - 1; i++) mul *= ba.dims[i];
    changed_dim = ba.dims.length - 1;
    ofs -= 1;
  }
  if (ofs < 0 || len < 0 || ofs + len > ba.dims[changed_dim]) {
    caml_invalid_argument("Bigarray.sub: bad sub-array");
  }
  const new_dims = [];
  for (let i = 0; i < ba.dims.length; i++) new_dims[i] = ba.dims[i];
  new_dims[changed_dim] = len;
  mul *= caml_ba_get_size_per_element(ba.kind);
  const new_data = ba.data.subarray(ofs * mul, (ofs + len) * mul);
  return caml_ba_create_unsafe(ba.kind, ba.layout, new_dims, new_data);
}

//Provides: caml_ba_slice
//Requires: caml_js_from_array, caml_ba_create_unsafe, caml_invalid_argument, caml_ba_get_size
//Requires: caml_ba_get_size_per_element
function caml_ba_slice(ba, vind) {
  vind = caml_js_from_array(vind);
  const num_inds = vind.length;
  const index = [];
  let sub_dims = [];

  if (num_inds > ba.dims.length)
    caml_invalid_argument("Bigarray.slice: too many indices");

  // Compute offset and check bounds
  if (ba.layout === 0) {
    let i = 0;
    for (; i < num_inds; i++) index[i] = vind[i];
    for (; i < ba.dims.length; i++) index[i] = 0;
    sub_dims = ba.dims.slice(num_inds);
  } else {
    for (let i = 0; i < num_inds; i++)
      index[ba.dims.length - num_inds + i] = vind[i];
    for (let i = 0; i < ba.dims.length - num_inds; i++) index[i] = 1;
    sub_dims = ba.dims.slice(0, ba.dims.length - num_inds);
  }
  const ofs = ba.offset(index);
  const size = caml_ba_get_size(sub_dims);
  const size_per_element = caml_ba_get_size_per_element(ba.kind);
  const new_data = ba.data.subarray(
    ofs * size_per_element,
    (ofs + size) * size_per_element,
  );
  return caml_ba_create_unsafe(ba.kind, ba.layout, sub_dims, new_data);
}

//Provides: caml_ba_reshape
//Requires: caml_js_from_array, caml_invalid_argument, caml_ba_create_unsafe, caml_ba_get_size
function caml_ba_reshape(ba, vind) {
  vind = caml_js_from_array(vind);
  const new_dim = [];
  const num_dims = vind.length;

  if (num_dims < 0 || num_dims > 16) {
    caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
  }
  let num_elts = 1;
  for (let i = 0; i < num_dims; i++) {
    new_dim[i] = vind[i];
    if (new_dim[i] < 0)
      caml_invalid_argument("Bigarray.reshape: negative dimension");
    num_elts *= new_dim[i];
  }

  const size = caml_ba_get_size(ba.dims);
  // Check that sizes agree
  if (num_elts !== size)
    caml_invalid_argument("Bigarray.reshape: size mismatch");
  return caml_ba_create_unsafe(ba.kind, ba.layout, new_dim, ba.data);
}

//Provides: caml_ba_serialize
//Requires: caml_int64_bits_of_float, caml_int64_to_bytes
//Requires: caml_int32_bits_of_float
function caml_ba_serialize(writer, ba, sz) {
  writer.write(32, ba.dims.length);
  writer.write(32, ba.kind | (ba.layout << 8));
  if (ba.caml_custom === "_bigarr02")
    for (let i = 0; i < ba.dims.length; i++) {
      if (ba.dims[i] < 0xffff) writer.write(16, ba.dims[i]);
      else {
        writer.write(16, 0xffff);
        writer.write(32, 0);
        writer.write(32, ba.dims[i]);
      }
    }
  else for (let i = 0; i < ba.dims.length; i++) writer.write(32, ba.dims[i]);
  switch (ba.kind) {
    case 2: //Int8Array
    case 3: //Uint8Array
    case 12: //Uint8Array
      for (let i = 0; i < ba.data.length; i++) {
        writer.write(8, ba.data[i]);
      }
      break;
    case 4: // Int16Array
    case 5: // Uint16Array
      for (let i = 0; i < ba.data.length; i++) {
        writer.write(16, ba.data[i]);
      }
      break;
    case 6: // Int32Array (int32)
      for (let i = 0; i < ba.data.length; i++) {
        writer.write(32, ba.data[i]);
      }
      break;
    case 8: // Int32Array (int)
    case 9: // Int32Array (nativeint)
      writer.write(8, 0);
      for (let i = 0; i < ba.data.length; i++) {
        writer.write(32, ba.data[i]);
      }
      break;
    case 7: // Int32Array (int64)
      for (let i = 0; i < ba.data.length / 2; i++) {
        const b = caml_int64_to_bytes(ba.get(i));
        for (let j = 0; j < 8; j++) writer.write(8, b[j]);
      }
      break;
    case 1: // Float64Array
      for (let i = 0; i < ba.data.length; i++) {
        const b = caml_int64_to_bytes(caml_int64_bits_of_float(ba.get(i)));
        for (let j = 0; j < 8; j++) writer.write(8, b[j]);
      }
      break;
    case 0: // Float32Array
      for (let i = 0; i < ba.data.length; i++) {
        const b = caml_int32_bits_of_float(ba.get(i));
        writer.write(32, b);
      }
      break;
    case 10: // Float32Array (complex32)
      for (let i = 0; i < ba.data.length / 2; i++) {
        const j = ba.get(i);
        writer.write(32, caml_int32_bits_of_float(j[1]));
        writer.write(32, caml_int32_bits_of_float(j[2]));
      }
      break;
    case 11: // Float64Array (complex64)
      for (let i = 0; i < ba.data.length / 2; i++) {
        const complex = ba.get(i);
        for (let j = 0; j < 8; j++) {
          const b = caml_int64_to_bytes(caml_int64_bits_of_float(complex[1]));
          writer.write(8, b[j]);
        }
        for (let j = 0; j < 8; j++) {
          const b = caml_int64_to_bytes(caml_int64_bits_of_float(complex[2]));
          writer.write(8, b[j]);
        }
      }
      break;
  }
  sz[0] = (4 + ba.dims.length) * 4;
  sz[1] = (4 + ba.dims.length) * 8;
}

//Provides: caml_ba_deserialize
//Requires: caml_ba_create_unsafe, caml_failwith
//Requires: caml_ba_get_size
//Requires: caml_int64_of_bytes, caml_int64_float_of_bits
//Requires: caml_int32_float_of_bits
//Requires: caml_ba_create_buffer
function caml_ba_deserialize(reader, sz, name) {
  const num_dims = reader.read32s();
  if (num_dims < 0 || num_dims > 16)
    caml_failwith("input_value: wrong number of bigarray dimensions");
  const tag = reader.read32s();
  const kind = tag & 0xff;
  const layout = (tag >> 8) & 1;
  const dims = [];
  if (name === "_bigarr02")
    for (let i = 0; i < num_dims; i++) {
      let size_dim = reader.read16u();
      if (size_dim === 0xffff) {
        const size_dim_hi = reader.read32u();
        const size_dim_lo = reader.read32u();
        if (size_dim_hi !== 0)
          caml_failwith("input_value: bigarray dimension overflow in 32bit");
        size_dim = size_dim_lo;
      }
      dims.push(size_dim);
    }
  else for (let i = 0; i < num_dims; i++) dims.push(reader.read32u());
  const size = caml_ba_get_size(dims);
  const data = caml_ba_create_buffer(kind, size);
  const ba = caml_ba_create_unsafe(kind, layout, dims, data);
  switch (kind) {
    case 2: //Int8Array
      for (let i = 0; i < size; i++) {
        data[i] = reader.read8s();
      }
      break;
    case 3: //Uint8Array
    case 12: //Uint8Array
      for (let i = 0; i < size; i++) {
        data[i] = reader.read8u();
      }
      break;
    case 4: // Int16Array
      for (let i = 0; i < size; i++) {
        data[i] = reader.read16s();
      }
      break;
    case 5: // Uint16Array
      for (let i = 0; i < size; i++) {
        data[i] = reader.read16u();
      }
      break;
    case 6: // Int32Array (int32)
      for (let i = 0; i < size; i++) {
        data[i] = reader.read32s();
      }
      break;
    case 8: // Int32Array (int)
    case 9: {
      // Int32Array (nativeint)
      const sixty = reader.read8u();
      if (sixty)
        caml_failwith(
          "input_value: cannot read bigarray with 64-bit OCaml ints",
        );
      for (let i = 0; i < size; i++) {
        data[i] = reader.read32s();
      }
      break;
    }
    case 7: {
      // (int64)
      const t = new Array(8);
      for (let i = 0; i < size; i++) {
        for (let j = 0; j < 8; j++) t[j] = reader.read8u();
        const int64 = caml_int64_of_bytes(t);
        ba.set(i, int64);
      }
      break;
    }
    case 1: {
      // Float64Array
      const t = new Array(8);
      for (let i = 0; i < size; i++) {
        for (let j = 0; j < 8; j++) t[j] = reader.read8u();
        const f = caml_int64_float_of_bits(caml_int64_of_bytes(t));
        ba.set(i, f);
      }
      break;
    }
    case 0: // Float32Array
      for (let i = 0; i < size; i++) {
        const f = caml_int32_float_of_bits(reader.read32s());
        ba.set(i, f);
      }
      break;
    case 10: // Float32Array (complex32)
      for (let i = 0; i < size; i++) {
        const re = caml_int32_float_of_bits(reader.read32s());
        const im = caml_int32_float_of_bits(reader.read32s());
        ba.set(i, [254, re, im]);
      }
      break;
    case 11: {
      // Float64Array (complex64)
      const t = new Array(8);
      for (let i = 0; i < size; i++) {
        for (let j = 0; j < 8; j++) t[j] = reader.read8u();
        const re = caml_int64_float_of_bits(caml_int64_of_bytes(t));
        for (let j = 0; j < 8; j++) t[j] = reader.read8u();
        const im = caml_int64_float_of_bits(caml_int64_of_bytes(t));
        ba.set(i, [254, re, im]);
      }
      break;
    }
  }
  sz[0] = (4 + num_dims) * 4;
  return caml_ba_create_unsafe(kind, layout, dims, data);
}

//Deprecated
//Provides: caml_ba_create_from
//Requires: caml_ba_create_unsafe, caml_invalid_argument, caml_ba_get_size_per_element
function caml_ba_create_from(data1, data2, jstyp, kind, layout, dims) {
  if (data2 || caml_ba_get_size_per_element(kind) === 2) {
    caml_invalid_argument(
      "caml_ba_create_from: use return caml_ba_create_unsafe",
    );
  }
  return caml_ba_create_unsafe(kind, layout, dims, data1);
}

//Provides: caml_ba_hash const
//Requires: caml_ba_get_size, caml_hash_mix_int, caml_hash_mix_float
function caml_ba_hash(ba) {
  let num_elts = caml_ba_get_size(ba.dims);
  let h = 0;
  switch (ba.kind) {
    case 2: //Int8Array
    case 3: //Uint8Array
    case 12: {
      //Uint8Array
      if (num_elts > 256) num_elts = 256;
      let w = 0;
      let i = 0;
      for (i = 0; i + 4 <= ba.data.length; i += 4) {
        w =
          ba.data[i + 0] |
          (ba.data[i + 1] << 8) |
          (ba.data[i + 2] << 16) |
          (ba.data[i + 3] << 24);
        h = caml_hash_mix_int(h, w);
      }
      w = 0;
      switch (num_elts & 3) {
        case 3:
          w = ba.data[i + 2] << 16; /* fallthrough */
        case 2:
          w |= ba.data[i + 1] << 8; /* fallthrough */
        case 1:
          w |= ba.data[i + 0];
          h = caml_hash_mix_int(h, w);
      }
      break;
    }
    case 4: // Int16Array
    case 5: {
      // Uint16Array
      if (num_elts > 128) num_elts = 128;
      let w = 0;
      let i = 0;
      for (i = 0; i + 2 <= ba.data.length; i += 2) {
        w = ba.data[i + 0] | (ba.data[i + 1] << 16);
        h = caml_hash_mix_int(h, w);
      }
      if ((num_elts & 1) !== 0) h = caml_hash_mix_int(h, ba.data[i]);
      break;
    }
    case 6: // Int32Array (int32)
      if (num_elts > 64) num_elts = 64;
      for (let i = 0; i < num_elts; i++) h = caml_hash_mix_int(h, ba.data[i]);
      break;
    case 8: // Int32Array (int)
    case 9: // Int32Array (nativeint)
      if (num_elts > 64) num_elts = 64;
      for (let i = 0; i < num_elts; i++) h = caml_hash_mix_int(h, ba.data[i]);
      break;
    case 7: // Int32Array (int64)
      if (num_elts > 32) num_elts = 32;
      num_elts *= 2;
      for (let i = 0; i < num_elts; i++) {
        h = caml_hash_mix_int(h, ba.data[i]);
      }
      break;
    case 10: // Float32Array (complex32)
      num_elts *= 2; /* fallthrough */
    case 0: // Float32Array
      if (num_elts > 64) num_elts = 64;
      for (let i = 0; i < num_elts; i++) h = caml_hash_mix_float(h, ba.data[i]);
      break;
    case 11: // Float64Array (complex64)
      num_elts *= 2; /* fallthrough */
    case 1: // Float64Array
      if (num_elts > 32) num_elts = 32;
      for (let i = 0; i < num_elts; i++) h = caml_hash_mix_float(h, ba.data[i]);
      break;
  }
  return h;
}

//Provides: caml_ba_to_typed_array mutable
function caml_ba_to_typed_array(ba) {
  return ba.data;
}

//Provides: caml_ba_kind_of_typed_array mutable
//Requires: caml_invalid_argument
function caml_ba_kind_of_typed_array(ta) {
  let kind;
  if (ta instanceof Float32Array) kind = 0;
  else if (ta instanceof Float64Array) kind = 1;
  else if (ta instanceof Int8Array) kind = 2;
  else if (ta instanceof Uint8Array) kind = 3;
  else if (ta instanceof Uint8ClampedArray) kind = 3;
  else if (ta instanceof Int16Array) kind = 4;
  else if (ta instanceof Uint16Array) kind = 5;
  else if (ta instanceof Int32Array) kind = 6;
  else if (ta instanceof Uint32Array) kind = 6;
  else caml_invalid_argument("caml_ba_kind_of_typed_array: unsupported kind");
  return kind;
}

//Provides: caml_ba_from_typed_array mutable
//Requires: caml_ba_kind_of_typed_array
//Requires: caml_ba_create_unsafe
function caml_ba_from_typed_array(ta) {
  const kind = caml_ba_kind_of_typed_array(ta);
  const ta_ =
    /* Needed to avoid unsigned setters overflowing
         the range of OCaml [int32] values. */
    ta instanceof Uint32Array
      ? new Int32Array(ta.buffer, ta.byteOffset, ta.length)
      : ta;
  return caml_ba_create_unsafe(kind, 0, [ta_.length], ta_);
}
