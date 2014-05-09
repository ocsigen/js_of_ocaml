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

declare var joo_global_object : any;
declare function caml_invalid_argument(msg:string): void;
declare function caml_js_from_array(i:number[]):number[];
declare function caml_array_bound_error():void;

interface BaseArray extends ArrayBufferView {
    BYTES_PER_ELEMENT: number;
    length: number;
    [index: number]: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: BaseArray, offset?: number): void;
    set(array: number[], offset?: number): void;
    subarray(begin: number, end?: number): BaseArray;
}

declare enum Data_type {
  General=0,  // all types which can fit in a single TypedArray
  Int64=1,    // int64, split over two TypedArrays
  Complex=2   // Complex32+64, split over two TypedArrays
}

interface Bigarray {
  data : BaseArray;
  data2 : BaseArray;
  data_type : Data_type;

  num_dims: number;
  nth_dim(i:number) : number;
  kind: number;
  layout: number;
  size: number;

  sub(ofs:number,len:number): Bigarray;
  slice(new_dims:number[]): Bigarray;
  fill(v: any): void;
  blit(from:Bigarray): void;
  reshape(vdim:number[]): Bigarray;

	get(index: number[]): any;
  get1(i0: number): any;
  set(index: number[], v: any): void;
  set1(i0: number, v: any): void;

  compare(v:Bigarray, total : boolean) : number;
}

//Provides: caml_ba_init const
function caml_ba_init () {}

//Provides: caml_ba_init_views
//Requires: caml_ba_views
function caml_ba_init_views() {
  if (!caml_ba_views) {
    var g = joo_global_object;
    caml_ba_views = [
      [g.Float32Array, g.Float64Array, g.Int8Array, g.Uint8Array,
       g.Int16Array, g.Uint16Array, g.Int32Array, g.Int32Array,
       g.Int32Array, g.Int32Array, g.Float32Array, g.Float64Array, g.Uint8Array],
      [Data_type.General, Data_type.General, Data_type.General, Data_type.General,
       Data_type.General, Data_type.General, Data_type.General, Data_type.Int64,
       Data_type.General, Data_type.General, Data_type.Complex, Data_type.Complex, Data_type.General]
    ];
  }
}

//Provides: caml_ba_get_size
//Requires: caml_invalid_argument
function caml_ba_get_size(dims:number[]) : number {
  var n_dims = dims.length;
  var size = 1;
  for (var i=0; i<n_dims; i++) {
    if (dims[i] < 0) caml_invalid_argument("Bigarray.create: negative dimension");
    size = size * dims[i];
  }
  return size;
}

//Provides: caml_ba_views
var caml_ba_views;

//Provides: caml_ba_create_from
//Requires: caml_ba_get_size
//Requires: caml_invalid_argument
//Requires: caml_array_bound_error
function caml_ba_create_from(data:BaseArray, data2:BaseArray, data_type:Data_type, kind: number, layout: number, dims: number[]) : Bigarray {

  var n_dims = dims.length;
  var size = caml_ba_get_size(dims);

  //
  // Functions to compute the offsets for C or Fortran layout arrays
  // from the given array of indices.
  //

  function offset_c(index:number[]) : number {
    var ofs = 0;
    if (n_dims != index.length) caml_invalid_argument("Bigarray.get/set: bad number of dimensions");
    for (var i=0; i<n_dims; i++) {
      if (index[i] < 0 || index[i] >= dims[i]) caml_array_bound_error();
      ofs = (ofs * dims[i]) + index[i];
    }
    return ofs;
  }

  function offset_fortran(index:number[]) : number {
    var ofs = 0;
    if (n_dims != index.length) caml_invalid_argument("Bigarray.get/set: wrong number of indices")
    for (var i=n_dims-1; i>=0; i--) {
      if (index[i] < 1 || index[i] > dims[i]) caml_array_bound_error();
      ofs = (ofs * dims[i]) + (index[i]-1);
    }
    return ofs;
  }

  var offset = layout == 0 ? offset_c : offset_fortran;

  var dim0 = dims[0];

  //
  // Element get functions.
  //

  function get_std(index) : any {
  var ofs = offset(index);
    var v = data[ofs];
    return v;
  }

  function get_int64(index) : any {
    var off = offset(index);
    var l = data[off];
    var h = data2[off];
    return [255,
      l & 0xffffff,
      ((l >>> 24) & 0xff) | ((h & 0xffff) << 8),
      (h >>> 16) & 0xffff];
  }

  function get_complex(index) : any {
    var off = offset(index);
    var r = data[off];
    var i = data2[off];
    return [254, r, i];
  }

  var get : any = data_type == Data_type.Int64 ? get_int64 :
                 (data_type == Data_type.Complex ? get_complex : get_std);

  function get1_c(i:number) {
    if (i<0 || i>=dim0) caml_array_bound_error();
    return data[i];
  }
  function get1_fortran(i:number) {
    if (i<1 || i>dim0) caml_array_bound_error();
    return data[i-1];
  }
  function get1_any(i:number) {
    return get([i]);
  }

  var get1 = data_type == Data_type.General ? (layout == 0 ? get1_c : get1_fortran) : get1_any;

  //
  // Element set functions
  //

  function set_std_raw(off,v) {
    data[off] = v;
  }

  function set_int64_raw(off, v) {
    data[off] = v[1] | ((v[2] & 0xff) << 24);
    data2[off] = ((v[2] >>> 8) & 0xffff) | (v[3] << 16);
  }

  function set_complex_raw(off, v) {
    data[off] = v[1];
    data2[off] = v[2];
  }

  function set_std(index,v) {
    var ofs = offset(index);
    return set_std_raw(ofs, v);
  }
  function set_int64(index, v) { return set_int64_raw(offset(index), v); }
  function set_complex(index, v) { return set_complex_raw(offset(index), v); }

  var set = data_type == Data_type.Int64 ? set_int64 :
           (data_type == Data_type.Complex ? set_complex : set_std);

  function set1_c(i:number,v:number) {
    if (i<0 || i>=dim0) caml_array_bound_error();
    data[i] = v;
  }
  function set1_fortran(i:number,v:number) {
    if (i<1 || i>dim0) caml_array_bound_error();
    data[i-1] = v;
  }
  function set1_any(i:number,v:any) {
    set([i],v);
  }

  var set1 = data_type == Data_type.General ? (layout == 0 ? set1_c : set1_fortran) : set1_any;

  //
  // other
  //

  function nth_dim(i) {
    if (i<0 || i>=n_dims) caml_invalid_argument("Bigarray.dim");
    return dims[i];
  }

  function fill(v:any) {
    if (data_type == Data_type.General)
      for (var i=0; i<data.length; i++) set_std_raw(i,v);
    if (data_type == Data_type.Int64)
      for (var i=0; i<data.length; i++) set_int64_raw(i,v);
    if (data_type == Data_type.Complex)
      for (var i=0; i<data.length; i++) set_complex_raw(i,v);
  }
  function blit(from:Bigarray) {
    if (n_dims != from.num_dims) caml_invalid_argument("Bigarray.blit: dimension mismatch");
    for (var i=0; i<n_dims; i++)
      if (dims[i] != from.nth_dim(i)) caml_invalid_argument("Bigarray.blit: dimension mismatch");
    data.set(from.data);
    if (data_type != Data_type.General) data2.set(from.data2);
  }

  function sub(ofs:number,len:number):Bigarray {
    var changed_dim;
    var mul = 1;

    if (layout == 0) {
      for (var i=1; i<n_dims; i++) mul = mul*dims[i];
      changed_dim = 0;
    } else {
      for (var i=0; i<(n_dims-1); i++) mul = mul*dims[i];
      changed_dim = n_dims-1;
      ofs = ofs-1;
    }

    if (ofs < 0 || len < 0 || (ofs + len) > dims[changed_dim])
      caml_invalid_argument("Bigarray.sub: bad sub-array");

    var new_data = data.subarray(ofs*mul, (ofs+len)*mul);
    var new_data2 = data_type == Data_type.General ? null : data2.subarray(ofs*mul,(ofs+len)*mul);

    var new_dims = [];
    for (var i=0; i<n_dims; i++) new_dims[i] = dims[i];
    new_dims[changed_dim] = len;

    return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, new_dims);
  }

  function slice(vind:number[]):Bigarray {
    var num_inds = vind.length;
    var index: number[] = [];
    var sub_dims: number[] = [];
    var ofs: number;

    if (num_inds >= n_dims)
        caml_invalid_argument("Bigarray.slice: too many indices");

    // Compute offset and check bounds
    if (layout == 0) {
      // We slice from the left
      for (var i = 0; i < num_inds; i++) index[i] = vind[i];
      for (/*nothing*/; i < n_dims; i++) index[i] = 0;
      ofs = offset(index);
      sub_dims = dims.slice(num_inds);
    } else {
      // We slice from the right
      for (var i = 0; i < num_inds; i++)
        index[n_dims - num_inds + i] = vind[i];
      for (var i = 0; i < n_dims - num_inds; i++) index[i] = 1;
      ofs = offset(index);
      sub_dims = dims.slice(0, num_inds);
    }

    var size = caml_ba_get_size(sub_dims);
    var new_data = data.subarray(ofs, ofs+size);
    var new_data2 = data_type == Data_type.General ? null : data2.subarray(ofs,ofs+size);

    return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, sub_dims);
  }

  function reshape(vdim:number[]): Bigarray {
    var new_dim:number[] = [];
    var num_dims = vdim.length;

    if (num_dims < 1)
      caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
    var num_elts = 1;
    for (var i = 0; i < num_dims; i++) {
      new_dim[i] = vdim[i];
      if (new_dim[i] < 0)
        caml_invalid_argument("Bigarray.reshape: negative dimension");
      num_elts = num_elts * new_dim[i];
    }
    // Check that sizes agree
    if (num_elts != size)
      caml_invalid_argument("Bigarray.reshape: size mismatch");

    return caml_ba_create_from(data, data2, data_type, kind, layout, new_dim);
  }

  function compare (b: Bigarray, total: boolean) : number {
    if(layout != b.layout) return b.layout - layout;
    if(n_dims != b.num_dims) return b.num_dims - n_dims;
    for(var i = 0; i < n_dims; i++)
      if(nth_dim(i)!=b.nth_dim(i))
        return (nth_dim(i)<b.nth_dim(i))?-1:1;
    switch(kind){
      case 0: //float32
      case 1: //float64
      case 10: //complex32
      case 11: //complex64
      var x,y;
      for(var i = 0; i < data.length;i++){
        x = data[i]; y = b.data[i]
        //first array
        if(x < y) return -1;
        if(x > y) return 1;
        if(x != y) {
          if(x != y) {
            if(!total) return NaN;
            if(x == x) return 1;
            if(y == y) return -1;
          }
        }
        if(data2){
          //second array
          x = data2[i]; y = b.data2[i]
          if(x < y) return -1;
          if(x > y) return 1;
          if(x != y) {
            if(x != y) {
              if(!total) return NaN;
              if(x == x) return 1;
              if(y == y) return -1;
            }
          }
        }
      };
      break;

      case 2: //int8
      case 3: //uint8
      case 4: //int16
      case 5: //uint16
      case 6: //int32
      case 8: //int
      case 9: //nativeint
      case 12: //char
      for(var i = 0; i < data.length;i++){
        if(data[i] < b.data[i]) return -1;
        if(data[i] > b.data[i]) return 1;
      };
      break;

      case 7: //int64
      for(var i = 0; i < data.length;i++){
        if(data2[i] < b.data2[i]) return -1;
        if(data2[i] > b.data2[i]) return 1;
        if(data[i] < b.data[i]) return -1;
        if(data[i] > b.data[i]) return 1;
      };
      break;
      //default: should no append
    }
    return 0;}

  return {
    data : data,
    data2 : data2,
    data_type : data_type,
    num_dims : n_dims,
    nth_dim : nth_dim,
    kind : kind,
    layout : layout,
    size : size,
    sub : sub,
    slice : slice,
    blit : blit,
    fill : fill,
    reshape : reshape,
    get : get,
    get1 : get1,
    set : set,
    set1 : set1,
    compare: compare
  }
}

//Provides: caml_ba_create
//Requires: caml_ba_create_from
//Requires: caml_js_from_array
//Requires: caml_ba_views
//Requires: caml_ba_init_views
//Requires: caml_invalid_argument
//Requires: caml_ba_get_size
function caml_ba_create(kind: number, layout: number, dims_ml: any) : Bigarray {

  // Initialize TypedArray views
  caml_ba_init_views();

  // set up dimensions and calculate size
  var dims : number[] = caml_js_from_array(dims_ml);
  //var n_dims = dims.length;
  var size = caml_ba_get_size(dims);

  // Allocate TypedArray
  var view = caml_ba_views[0][kind];
  if (!view) caml_invalid_argument("Bigarray.create: unsupported kind");
  var data = new view(size);

  // 2nd TypedArray for int64, complex32 and complex64
  var data_type = caml_ba_views[1][kind];
  var data2 = null;
  if (data_type != Data_type.General) {
    data2 = new view(size);
  }

  return caml_ba_create_from(data, data2, data_type, kind, layout, dims);
}

//Provides: caml_ba_kind
function caml_ba_kind(ba: Bigarray) { return ba.kind; }

//Provides: caml_ba_layout
function caml_ba_layout(ba: Bigarray) { return ba.layout; }

//Provides: caml_ba_num_dims
function caml_ba_num_dims(ba: Bigarray, _dim: number) { return ba.num_dims; }

//Provides: caml_ba_dim
function caml_ba_dim(ba: Bigarray, dim: number) { return ba.nth_dim(dim); }

//Provides: caml_ba_dim_1
function caml_ba_dim_1(ba: Bigarray) { return ba.nth_dim(0); }

//Provides: caml_ba_dim_2
function caml_ba_dim_2(ba: Bigarray) { return ba.nth_dim(1); }

//Provides: caml_ba_dim_3
function caml_ba_dim_3(ba: Bigarray) { return ba.nth_dim(2); }

//Provides: caml_ba_get_generic
//Requires: caml_js_from_array
function caml_ba_get_generic(ba: Bigarray, index:number[]) { return ba.get(caml_js_from_array(index)); }

//Provides: caml_ba_get_1
function caml_ba_get_1(ba: Bigarray, i0:number) { return ba.get1(i0); }

//Provides: caml_ba_get_2
function caml_ba_get_2(ba: Bigarray, i0:number, i1:number) { return ba.get([i0,i1]); }

//Provides: caml_ba_get_3
function caml_ba_get_3(ba: Bigarray, i0:number, i1:number, i2:number) { return ba.get([i0,i1,i2]); }

//Provides: caml_ba_set_generic
//Requires: caml_js_from_array
function caml_ba_set_generic(ba: Bigarray, index:number[], v:number) { return ba.set(caml_js_from_array(index),v); }

//Provides: caml_ba_set_1
function caml_ba_set_1(ba: Bigarray, i0:number, v:number) { return ba.set1(i0,v); }

//Provides: caml_ba_set_2
function caml_ba_set_2(ba: Bigarray, i0:number, i1:number, v:number) { return ba.set([i0,i1],v); }

//Provides: caml_ba_set_3
function caml_ba_set_3(ba: Bigarray, i0:number, i1:number, i2:number, v:number) { return ba.set([i0,i1,i2],v); }

//Provides: caml_ba_blit
function caml_ba_blit(src: Bigarray, dst: Bigarray) { dst.blit(src); }

//Provides: caml_ba_fill
function caml_ba_fill(ba: Bigarray, init: number) { ba.fill(init); }

//Provides: caml_ba_sub
function caml_ba_sub(ba: Bigarray, ofs: number, len:number): Bigarray { return ba.sub(ofs,len); }

//Provides: caml_ba_slice
//Requires: caml_js_from_array
function caml_ba_slice(ba: Bigarray, vind: number[]): Bigarray { return ba.slice(caml_js_from_array(vind)); }

//Provides: caml_ba_reshape
//Requires: caml_js_from_array
function caml_ba_reshape(ba: Bigarray, vind: number[]): Bigarray { return ba.reshape(caml_js_from_array(vind)); }
