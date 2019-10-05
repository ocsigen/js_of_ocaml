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
  var n_dims = dims.length;
  var size = 1;
  for (var i = 0; i < n_dims; i++) {
    if (dims[i] < 0)
      caml_invalid_argument("Bigarray.create: negative dimension");
    size = size * dims[i];
  }
  return size;
}

//Provides: caml_ba_get_width
function caml_ba_get_width(kind){
  switch(kind){
  case 7: case 10: case 11: return 2;
  default: return 1;
  }
}


//Provides: Ml_Bigarray
//Requires: caml_array_bound_error, caml_invalid_argument, caml_ba_get_width
function Ml_Bigarray (kind, layout, dims, buffer) {
  this.kind   = kind ;
  this.layout = layout;
  this.dims   = dims;
  this.data = buffer;
  this.word = caml_ba_get_width(kind);
}

Ml_Bigarray.prototype.offset = function (arg) {
  var ofs = 0;
  if(typeof arg === "number") {
    if(this.layout == 0 /* c_layout */) {
      if (arg < 0 || arg >= this.dims[0])
        caml_array_bound_error();
      ofs = arg;
    } else {
      if (arg < 1 || arg > this.dims[0])
        caml_array_bound_error();
      ofs = arg - 1;
    }
  }
  else if (arg instanceof Array) {
    if (this.dims.length != arg.length)
      caml_invalid_argument("Bigarray.get/set: bad number of dimensions");
    if(this.layout == 0 /* c_layout */) {
      for (var i = 0; i < this.dims.length; i++) {
        if (arg[i] < 0 || arg[i] >= this.dims[i])
          caml_array_bound_error();
        ofs = (ofs * this.dims[i]) + arg[i];
      }
    } else {
      for (var i = this.dims.length - 1; i >= 0; i--) {
        if (arg[i] < 1 || arg[i] > this.dims[i])
          caml_array_bound_error();
        ofs = (ofs * this.dims[i]) + (arg[i] - 1);
      }
    }
  }
  else caml_invalid_argument("bigarray.js: invalid offset");
  return ofs;
}

Ml_Bigarray.prototype.get = function (ofs) {
  switch(this.kind){
  case 7:
    // Int64
    var l = this.data[ofs * 2 + 0];
    var h = this.data[ofs * 2 + 1];
    return [
      255,
      l & 0xffffff,
      ((l >>> 24) & 0xff) | ((h & 0xffff) << 8),
      (h >>> 16) & 0xffff];
  case 10: case 11:
    var r = this.data[ofs * 2 + 0];
    var i = this.data[ofs * 2 + 1];
    return [254, r, i];
  default:
    return this.data[ofs]
  }
}

Ml_Bigarray.prototype.set = function (ofs,v) {
  switch(this.kind){
  case 7:
    // Int64
    this.data[ofs * 2 + 0] = v[1] | ((v[2] & 0xff) << 24);
    this.data[ofs * 2 + 1] = ((v[2] >>> 8) & 0xffff) | (v[3] << 16);
    break;
  case 10: case 11:
    this.data[ofs * 2 + 0] = v[1];
    this.data[ofs * 2 + 1] = v[2];
    break;
  default:
    this.data[ofs] = v;
    break;
  }
  return 0
}


Ml_Bigarray.prototype.fill = function (v) {
  switch(this.kind){
  case 7:
    // Int64
    var a = ((v[1]      )         ) | ((v[2] & 0xff) << 24);
    var b = ((v[2] >>> 8) & 0xffff) | ((v[3]       ) << 16);
    if(a == b){
      this.data.fill(a);
    }
    for(var i = 0; i<this.data.length; i++){
      this.data[i] = (i%2 == 0) ? a : b;
    }
    break;
  case 10: case 11:
    // Complex
    var im = v[1];
    var re = v[2];
    if(im == re){
      this.data.fill(im);
    }
    else {
      for(var i = 0; i<this.data.length; i++){
        this.data[i] = (i%2 == 0) ? im : re;
      }
    }
    break;
  default:
    this.data.fill(v);
    break;
  }
}


Ml_Bigarray.prototype.compare = function (b, total) {
  if (this.layout != b.layout)
    return b.layout - this.layout;
  if (this.dims.length != b.dims.length)
    return b.dims.length - this.dims.length;
  for (var i = 0; i < this.dims.length; i++)
    if (this.dims[i] != b.dims[i])
      return (this.dims[i] < b.dims[i]) ? -1 : 1;
  switch (this.kind) {
  case 0:
  case 1:
  case 10:
  case 11:
    // Floats
    var x, y;
    for (var i = 0; i < this.data.length; i++) {
      x = this.data[i];
      y = b.data[i];
      if (x < y)
        return -1;
      if (x > y)
        return 1;
      if (x != y) {
        if (x != y) {
          if (!total)
            return NaN;
          if (x == x)
            return 1;
          if (y == y)
            return -1;
        }
      }
    }
    break;
  case 7:
    // Int64
    for (var i = 0; i < this.data.length; i++) {
      if (this.data[i] < b.data[i])
        return -1;
      if (this.data[i] > b.data[i])
        return 1;
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
    for (var i = 0; i < this.data.length; i++) {
      if (this.data[i] < b.data[i])
        return -1;
      if (this.data[i] > b.data[i])
        return 1;
    }
    break;
  }
  return 0;
}

//Provides: Ml_Bigarray_c_1_1
//Requires: Ml_Bigarray, caml_array_bound_error
function Ml_Bigarray_c_1_1(kind, layout, dims, buffer) {
  this.kind   = kind ;
  this.layout = layout;
  this.dims   = dims;
  this.data = buffer;
  this.word = 1
}

Ml_Bigarray_c_1_1.prototype = new Ml_Bigarray()
Ml_Bigarray_c_1_1.prototype.offset = function (arg) {
  if (arg < 0 || arg >= this.dims[0])
    caml_array_bound_error();
  return arg;
}

Ml_Bigarray_c_1_1.prototype.get = function (ofs) {
  return this.data[ofs]
}

Ml_Bigarray_c_1_1.prototype.set = function (ofs,v) {
  this.data[ofs] = v;
  return 0
}

Ml_Bigarray_c_1_1.prototype.fill = function (v) {
  this.data.fill(v);
  return 0
}

//Provides: caml_ba_create_unsafe
//Requires: Ml_Bigarray, Ml_Bigarray_c_1_1, caml_ba_get_size, caml_ba_get_width, caml_invalid_argument
function caml_ba_create_unsafe(kind, layout, dims, data){
  var word = caml_ba_get_width(kind);
  if(caml_ba_get_size(dims) * word != data.length) {
    caml_invalid_argument("length doesn't match dims");
  }
  if(layout == 0 &&
     dims.length == 1 &&
     kind != 7 &&
     kind != 10 &&
     kind != 11)
    return new Ml_Bigarray_c_1_1(kind, layout, dims, data);
  return new Ml_Bigarray(kind, layout, dims, data);

}


//Provides: caml_ba_create
//Requires: caml_js_from_array
//Requires: caml_invalid_argument
//Requires: caml_ba_get_size, caml_ba_get_width, caml_ba_create_unsafe
function caml_ba_create(kind, layout, dims_ml) {

  // set up dimensions and calculate size
  var dims = caml_js_from_array(dims_ml);

  //var n_dims = dims.length;
  var size = caml_ba_get_size(dims) * caml_ba_get_width(kind);

  // Allocate TypedArray
  var g = joo_global_object;
  var view;
  switch(kind){
  case 0:  view = g.Float32Array; break;
  case 1:  view = g.Float64Array; break;
  case 2:  view = g.Int8Array; break;
  case 3:  view = g.Uint8Array; break;
  case 4:  view = g.Int16Array; break;
  case 5:  view = g.Uint16Array; break;
  case 6:  view = g.Int32Array; break;
  case 7:  view = g.Int32Array; break;
  case 8:  view = g.Int32Array; break;
  case 9:  view = g.Int32Array; break;
  case 10: view = g.Float32Array; break;
  case 11: view = g.Float64Array; break;
  case 12: view = g.Uint8Array; break;
  }
  if (!view)
    caml_invalid_argument("Bigarray.create: unsupported kind");
  var data = new view(size );

  return caml_ba_create_unsafe(kind, layout, dims, data);
}

//Provides: caml_ba_change_layout
//Requires: caml_ba_create_unsafe
function caml_ba_change_layout(ba, layout) {
  if(ba.layout == layout) return ba;
  return caml_ba_create_unsafe(ba.kind, layout, ba.dims, ba.data);
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
  if (i < 0 || i >= ba.dims.length)
    caml_invalid_argument("Bigarray.dim");
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
  var ofs = ba.offset(caml_js_from_array(i));
  return ba.get(ofs);
}

//Provides: caml_ba_uint8_get16
function caml_ba_uint8_get16(ba, i0) {
  var ofs = ba.offset(i0);
  var b1 = ba.get(ofs);
  var b2 = ba.get(ofs + 1);
  return (b1 | (b2 << 8));
}

//Provides: caml_ba_uint8_get32
function caml_ba_uint8_get32(ba, i0) {
  var ofs = ba.offset(i0);
  var b1 = ba.get(ofs+0);
  var b2 = ba.get(ofs+1);
  var b3 = ba.get(ofs+2);
  var b4 = ba.get(ofs+3);
  return ( (b1 << 0)  |
           (b2 << 8)  |
           (b3 << 16) |
           (b4 << 24) );
}

//Provides: caml_ba_uint8_get64
function caml_ba_uint8_get64(ba, i0) {
  var ofs = ba.offset(i0);
  var b1 = ba.get(ofs+0);
  var b2 = ba.get(ofs+1);
  var b3 = ba.get(ofs+2);
  var b4 = ba.get(ofs+3);
  var b5 = ba.get(ofs+4);
  var b6 = ba.get(ofs+5);
  var b7 = ba.get(ofs+6);
  var b8 = ba.get(ofs+7);
  return [255,
          ((b1 << 0)  |
           (b2 << 8)  |
           (b3 << 16)),
          ((b4 << 0)  |
           (b5 << 8)  |
           (b6 << 16)),
          ((b7 << 0)  |
           (b8 << 8))];
}

//Provides: caml_ba_get_1
function caml_ba_get_1(ba, i0) {
  return ba.get(ba.offset(i0));
}

//Provides: caml_ba_get_2
function caml_ba_get_2(ba, i0, i1) {
  return ba.get(ba.offset([i0,i1]));
}

//Provides: caml_ba_get_3
function caml_ba_get_3(ba, i0, i1, i2) {
  return ba.get(ba.offset([i0,i1,i2]));
}

//Provides: caml_ba_set_generic
//Requires: caml_js_from_array
function caml_ba_set_generic(ba, i, v) {
  ba.set(ba.offset(caml_js_from_array(i)), v);
  return 0
}

//Provides: caml_ba_uint8_set16
function caml_ba_uint8_set16(ba, i0, v) {
  var off = ba.offset(i0);
  ba.set(off+0,  v        & 0xff);
  ba.set(off+1, (v >>> 8) & 0xff);
  return 0;
}

//Provides: caml_ba_uint8_set32
function caml_ba_uint8_set32(ba, i0, v) {
  var off = ba.offset(i0);
  ba.set(off+0,  v         & 0xff);
  ba.set(off+1, (v >>> 8)  & 0xff);
  ba.set(off+2, (v >>> 16) & 0xff);
  ba.set(off+3, (v >>> 24) & 0xff);
  return 0;
}

//Provides: caml_ba_uint8_set64
function caml_ba_uint8_set64(ba, i0, v) {
  var off = ba.offset(i0);
  ba.set(off+0, (v[1])       & 0xff);
  ba.set(off+1, (v[1] >>  8) & 0xff);
  ba.set(off+2, (v[1] >> 16) & 0xff);
  ba.set(off+3, (v[2])       & 0xff);
  ba.set(off+4, (v[2] >>  8) & 0xff);
  ba.set(off+5, (v[2] >> 16) & 0xff);
  ba.set(off+6, (v[3])       & 0xff);
  ba.set(off+7, (v[3] >>  8) & 0xff);
  return 0;
}

//Provides: caml_ba_set_1
function caml_ba_set_1(ba, i0, v) {
  ba.set(ba.offset(i0), v);
  return 0
}

//Provides: caml_ba_set_2
function caml_ba_set_2(ba, i0, i1, v) {
  ba.set(ba.offset([i0,i1]), v);
  return 0;
}

//Provides: caml_ba_set_3
function caml_ba_set_3(ba, i0, i1, i2, v) {
  ba.set(ba.offset([i0,i1,i2]), v);
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
  if (dst.dims.length != src.dims.length)
    caml_invalid_argument("Bigarray.blit: dimension mismatch");
  for (var i = 0; i < dst.dims.length; i++)
    if (dst.dims[i] != src.dims[i])
      caml_invalid_argument("Bigarray.blit: dimension mismatch");
  dst.data.set(src.data);
  return 0;
}

//Provides: caml_ba_sub
//Requires: caml_invalid_argument, caml_ba_create_unsafe, caml_ba_get_size
function caml_ba_sub(ba, ofs, len) {
  var changed_dim;
  var mul = 1;
  var ofs_o = ofs;

  if (ba.layout == 0) {
    for (var i = 1; i < ba.dims.length; i++)
      mul = mul * ba.dims[i];
    changed_dim = 0;
  } else {
    for (var i = 0; i < (ba.dims.length - 1); i++)
      mul = mul * ba.dims[i];
    changed_dim = ba.dims.length - 1;
    ofs = ofs - 1;
  }
  if (ofs < 0 || len < 0 || (ofs + len) > ba.dims[changed_dim]){
    caml_invalid_argument("Bigarray.sub: bad sub-array");
  }
  var ofs = ba.offset(ofs_o);
  var new_dims = [];
  for (var i = 0; i < ba.dims.length; i++)
    new_dims[i] = ba.dims[i];
  new_dims[changed_dim] = len;
  var new_data = ba.data.subarray(ofs * mul * ba.word, (ofs + len) * mul * ba.word);
  return caml_ba_create_unsafe(ba.kind, ba.layout, new_dims, new_data);
}

//Provides: caml_ba_slice
//Requires: caml_js_from_array, caml_ba_create_unsafe, caml_invalid_argument, caml_ba_get_size
function caml_ba_slice(ba, vind) {
  vind = caml_js_from_array(vind);
  var num_inds = vind.length;
  var index = [];
  var sub_dims = [];
  var ofs;

  if (num_inds >= ba.dims.length)
    caml_invalid_argument("Bigarray.slice: too many indices");

  // Compute offset and check bounds
  if (ba.layout == 0) {
    for (var i = 0; i < num_inds; i++)
      index[i] = vind[i];
    for (; i < ba.dims.length; i++)
      index[i] = 0;
    sub_dims = ba.dims.slice(num_inds);
  } else {
    for (var i = 0; i < num_inds; i++)
      index[ba.dims.length - num_inds + i] = vind[i];
    for (var i = 0; i < ba.dims.length - num_inds; i++)
      index[i] = 1;
    sub_dims = ba.dims.slice(0, num_inds);
  }
  ofs = ba.offset(index);
  var size = caml_ba_get_size(sub_dims);
  var new_data = ba.data.subarray(ofs * ba.word, (ofs + size) * ba.word);
  return caml_ba_create_unsafe(ba.kind, ba.layout, sub_dims, new_data);
}

//Provides: caml_ba_reshape
//Requires: caml_js_from_array, caml_invalid_argument, caml_ba_create_unsafe, caml_ba_get_size
function caml_ba_reshape(ba, vind) {
  vind = caml_js_from_array(vind);
  var new_dim = [];
  var num_dims = ba.dims.length;

  if (num_dims < 1)
    caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
  var num_elts = 1;
  for (var i = 0; i < num_dims; i++) {
    new_dim[i] = ba.dim[i];
    if (new_dim[i] < 0)
      caml_invalid_argument("Bigarray.reshape: negative dimension");
    num_elts = num_elts * new_dim[i];
  }

  var size = caml_ba_get_size(ba.dims);
  // Check that sizes agree
  if (num_elts != size)
    caml_invalid_argument("Bigarray.reshape: size mismatch");
  return caml_ba_create_unsafe(ba.kind, ba.layout, new_dim, ba.data);
}

//Deprecated
//Provides: caml_ba_create_from
//Requires: caml_ba_create_unsafe, caml_invalid_argument, caml_ba_get_width
function caml_ba_create_from(data1, data2, jstyp, kind, layout, dims){
  if(data2 || caml_ba_get_width(kind) == 2){
    caml_invalid_argument("caml_ba_create_from: use return caml_ba_create_unsafe");
  }
  return caml_ba_create_unsafe(kind, layout, dims, data1);
}
