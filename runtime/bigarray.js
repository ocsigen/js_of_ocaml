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
//
// Note; int64+complex support if provided by allocating a second TypedArray
// Note; accessor functions are selected when the bigarray is created.  It is assumed
//       that this results in just a function pointer and will thus be fast.

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

//Provides: Ml_Bigarray
//Requires: caml_array_bound_error, caml_invalid_argument
function Ml_Bigarray (kind, layout, dims, buffer) {
  this.kind   = kind ;
  this.layout = layout;
  this.dims   = dims;
  this.data = buffer;
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
  switch(this.kind){  
  case 7:
    // Int64
    ofs *= 2; break;
  case 10: case 11:
    // Complex
    ofs *= 2; break;
  }
  return ofs;
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

//Provides: caml_ba_create
//Requires: caml_js_from_array
//Requires: caml_invalid_argument
//Requires: caml_ba_get_size
//Requires: Ml_Bigarray
function caml_ba_create(kind, layout, dims_ml) {

  // set up dimensions and calculate size
  var dims = caml_js_from_array(dims_ml);

  //var n_dims = dims.length;
  var size = caml_ba_get_size(dims);

  // Allocate TypedArray
  var g = joo_global_object;
  var caml_ba_views = [
    g.Float32Array, g.Float64Array, g.Int8Array, g.Uint8Array,
    g.Int16Array, g.Uint16Array, g.Int32Array, g.Int32Array,
    g.Int32Array, g.Int32Array, g.Float32Array, g.Float64Array, g.Uint8Array];
  
  var view = caml_ba_views[kind];
  if (!view)
    caml_invalid_argument("Bigarray.create: unsupported kind");
  var data = new view(size);

  return (new Ml_Bigarray(kind, layout, dims, data));
}

//Provides: caml_ba_change_layout
//Requires: Ml_Bigarray 
function caml_ba_change_layout(ba, layout) {
  if(ba.layout == layout) return ba;
  return (new Ml_Bigarray(ba.kind, layout, ba.dims, ba.data))
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

//Provides: caml_ba_get_ofs
//Requires: 
function caml_ba_get_ofs(ba, ofs) {
  switch(ba.kind){
  case 7:
    // Int64
    var l = ba.data[ofs + 0];
    var h = ba.data[ofs + 1];
    return [
      255,
      l & 0xffffff,
      ((l >>> 24) & 0xff) | ((h & 0xffff) << 8),
      (h >>> 16) & 0xffff];
  case 10: case 11:
    var r = ba.data[ofs + 0];
    var i = ba.data[ofs + 1];
    return [254, r, i];
  default:
    return ba.data[ofs]
  }
}

//Provides: caml_ba_get_generic
//Requires: caml_ba_get_ofs, caml_js_from_array
function caml_ba_get_generic(ba, i) {
  var ofs = ba.offset(caml_js_from_array(i));
  return caml_ba_get_ofs(ba, ofs);
}

//Provides: caml_ba_uint8_get16
function caml_ba_uint8_get16(ba, i0) {
  var ofs = ba.offset(i0);
  var b1 = ba.data[ofs];
  var b2 = ba.data[ofs + 1];
  return (b1 | (b2 << 8));
}

//Provides: caml_ba_uint8_get32
function caml_ba_uint8_get32(ba, i0) {
  var ofs = ba.offset(i0);
  var b1 = ba.data[ofs+0];
  var b2 = ba.data[ofs+1];
  var b3 = ba.data[ofs+2];
  var b4 = ba.data[ofs+3];
  return ( (b1 << 0)  | 
           (b2 << 8)  |
           (b3 << 16) |
           (b4 << 24) );
}

//Provides: caml_ba_uint8_get64
function caml_ba_uint8_get64(ba, i0) {
  var ofs = ba.offset(i0);
  var b1 = ba.data[ofs+0];
  var b2 = ba.data[ofs+1];
  var b3 = ba.data[ofs+2];
  var b4 = ba.data[ofs+3];
  var b5 = ba.data[ofs+4];
  var b6 = ba.data[ofs+5];
  var b7 = ba.data[ofs+6];
  var b8 = ba.data[ofs+7];
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
//Requires: caml_ba_get_ofs
function caml_ba_get_1(ba, i0) {
  return caml_ba_get_ofs(ba, ba.offset(i0));
}

//Provides: caml_ba_get_2
//Requires: caml_ba_get_ofs
function caml_ba_get_2(ba, i0, i1) {
  return caml_ba_get_ofs(ba, ba.offset([i0,i1]));
}

//Provides: caml_ba_get_3
//Requires: caml_ba_get_ofs
function caml_ba_get_3(ba, i0, i1, i2) {
  return caml_ba_get_ofs(ba, ba.offset([i0,i1,i2]));
}

//Provides: caml_ba_set_ofs
//Requires: 
function caml_ba_set_ofs(ba, ofs, v) {
  switch(ba.kind){
  case 7:
    // Int64
    ba.data[ofs + 0] = v[1] | ((v[2] & 0xff) << 24);
    ba.data[ofs + 1] = ((v[2] >>> 8) & 0xffff) | (v[3] << 16);
    break;
  case 10: case 11:
    ba.data[ofs + 0] = v[1];
    ba.data[ofs + 1] = v[2];
    break;
  default:
    ba.data[ofs] = v;
    break;
  }
  return 0
}

//Provides: caml_ba_set_generic
//Requires: caml_js_from_array, caml_ba_set_ofs
function caml_ba_set_generic(ba, i, v) {
  caml_ba_set_ofs(ba, ba.offset(caml_js_from_array(i)), v);
  return 0
}

//Provides: caml_ba_uint8_set16
function caml_ba_uint8_set16(ba, i0, v) {
  var off = ba.offset(i0);
  ba.data[off+0] = (v & 0xff);
  ba.data[off+1] = ((v >>> 8) & 0xff);
  return 0;
}

//Provides: caml_ba_uint8_set32
function caml_ba_uint8_set32(ba, i0, v) {
  var off = ba.offset(i0);
  ba.data[off+0] = ( v         & 0xff);
  ba.data[off+1] = ((v >>> 8)  & 0xff);
  ba.data[off+2] = ((v >>> 16) & 0xff);
  ba.data[off+3] = ((v >>> 24) & 0xff);
  return 0;
}

//Provides: caml_ba_uint8_set64
function caml_ba_uint8_set64(ba, i0, v) {
  var off = ba.offset(i0);
  ba.data[off+0] = ((v[1])       & 0xff);
  ba.data[off+1] = ((v[1] >> 8)  & 0xff);
  ba.data[off+2] = ((v[1] >> 16) & 0xff);
  ba.data[off+3] = ((v[2])       & 0xff);
  ba.data[off+4] = ((v[2] >> 8)  & 0xff);
  ba.data[off+5] = ((v[2] >> 16) & 0xff);
  ba.data[off+6] = ((v[3])       & 0xff);
  ba.data[off+7] = ((v[3] >> 8)  & 0xff);
  return 0;
}

//Provides: caml_ba_set_1
//Requires: caml_ba_set_ofs
function caml_ba_set_1(ba, i0, v) {
  caml_ba_set_ofs(ba, ba.offset(i0), v);
  return 0
}

//Provides: caml_ba_set_2
//Requires: caml_ba_set_ofs
function caml_ba_set_2(ba, i0, i1, v) {
  caml_ba_set_ofs(ba, ba.offset([i0,i1]), v)
  return 0;
}

//Provides: caml_ba_set_3
//Requires: caml_ba_set_ofs
function caml_ba_set_3(ba, i0, i1, i2, v) {
  caml_ba_set_ofs(ba, ba.offset([i0,i1,i2]), v);
  return 0;
}

//Provides: caml_ba_fill
function caml_ba_fill(ba, v) {
  switch(ba.kind){  
  case 7:
    // Int64
    var a = ((v[1]      )         ) | ((v[2] & 0xff) << 24);
    var b = ((v[2] >>> 8) & 0xffff) | ((v[3]       ) << 16);
    if(a == b){
      ba.data.fill(a);
    }
    for(var i = 0; i<ba.data.length; i++){
      ba.data[i] = (i%2 == 0) ? a : b;
    }
    break;
  case 10: case 11:
    // Complex
    var im = v[1];
    var re = v[2];
    if(im == re){
      ba.data.fill(im);
    }
    else {
      for(var i = 0; i<ba.data.length; i++){
        ba.data[i] = (i%2 == 0) ? im : re;
      }
    }
    break;
  default:
    ba.data.fill(v)
    break;
  }
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
//Requires: caml_invalid_argument, Ml_Bigarray
function caml_ba_sub(ba, ofs, len) {
  var ofs = ba.offset(ofs);
  switch(ba.kind){
  case 7:
    // Int64
    len *= 2; break;
  case 10: case 11:
    // Complex
    len *= 2; break;
  }
  var changed_dim;
  var mul = 1;

  if (ba.layout == 0) {
    for (var i = 1; i < this.dims.length; i++)
      mul = mul * this.dims[i];
    changed_dim = 0;
  } else {
    for (var i = 0; i < (this.dims.length - 1); i++)
      mul = mul * this.dims[i];
    changed_dim = this.dims.length - 1;
    ofs = ofs - 1;
  }

  if (ofs < 0 || len < 0 || (ofs + len) > this.dims[changed_dim])
    caml_invalid_argument("Bigarray.sub: bad sub-array");

  var new_data = this.data.subarray(ofs * mul, (ofs + len) * mul);

  var new_dims = [];
  for (var i = 0; i < this.dims.length; i++)
    new_dims[i] = this.dims[i];
  new_dims[changed_dim] = len;

  return (new Ml_Bigarray(ba.kind, ba.layout, new_dims, new_data));
}

//Provides: caml_ba_slice
//Requires: caml_js_from_array, Ml_Bigarray, caml_invalid_argument, caml_ba_get_size
function caml_ba_slice(ba, vind) {
  vind = caml_js_from_array(vind);
  var num_inds = vind.length;
  var index = [];
  var sub_dims = [];
  var ofs;

  if (num_inds >= this.dims.length)
    caml_invalid_argument("Bigarray.slice: too many indices");

  // Compute offset and check bounds
  if (this.layout == 0) {
    for (var i = 0; i < num_inds; i++)
      index[i] = vind[i];
    for (; i < this.dims.length; i++)
      index[i] = 0;
    sub_dims = this.dims.slice(num_inds);
  } else {
    for (var i = 0; i < num_inds; i++)
      index[this.dims.length - num_inds + i] = vind[i];
    for (var i = 0; i < this.dims.length - num_inds; i++)
      index[i] = 1;
    sub_dims = this.dims.slice(0, num_inds);
  }
  ofs = this.offset(index);


  var size = caml_ba_get_size(sub_dims);
  switch(ba.kind){
  case 7:
    // Int64
    size *= 2; break;
  case 10: case 11:
    // Complex
    size *= 2; break;
  }
  var new_data = this.data.subarray(ofs, ofs + size);
  return (new Ml_Bigarray(ba.kind, ba.layout, sub_dims, new_data));
}

//Provides: caml_ba_reshape
//Requires: caml_js_from_array, caml_invalid_argument, Ml_Bigarray, caml_ba_get_size
function caml_ba_reshape(ba, vind) {
  vind = caml_js_from_array(vind);
  var new_dim = [];
  var num_dims = this.dims.length;

  if (num_dims < 1)
    caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
  var num_elts = 1;
  for (var i = 0; i < num_dims; i++) {
    new_dim[i] = this.dim[i];
    if (new_dim[i] < 0)
      caml_invalid_argument("Bigarray.reshape: negative dimension");
    num_elts = num_elts * new_dim[i];
  }

  var size = caml_ba_get_size(this.dims);
  // Check that sizes agree
  if (num_elts != size)
    caml_invalid_argument("Bigarray.reshape: size mismatch");

  return (new Ml_Bigarray(ba.kind, ba.layout, new_dim, this.data));
}
