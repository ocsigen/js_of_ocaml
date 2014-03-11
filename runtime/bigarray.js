//Provides: caml_ba_init const
function caml_ba_init () {}


//Provides: caml_ba_views
var caml_ba_views;

//Provides: caml_ba_create const
//Requires: caml_invalid_argument
//Requires: caml_ba_views
function caml_ba_create (kind, layout, dim) {
  if (dim.length != 2)
    caml_invalid_argument("Bigarray.create: bad number of dimensions");
  if (layout != 0)
    caml_invalid_argument("Bigarray.create: unsupported layout");
  if (dim [1] < 0)
    caml_invalid_argument("Bigarray.create: negative dimension");
  if (!caml_ba_views) {
    var g = joo_global_object;
    caml_ba_views =
      [g.Float32Array, g.Float64Array, g.Int8Array, g.Uint8Array,
       g.Int16Array, g.Uint16Array, g.Int32Array, null,
       g.Int32Array, g.Int32Array, null, null, g.Uint8Array];
  }
  var View = caml_ba_views[kind];
  if (!View) caml_invalid_argument("Bigarray.create: unsupported kind");
  return new View (dim[1]);
}

//Provides: caml_ba_dim_1 const

function caml_ba_dim_1 (b) { return b.length; }

//Provides: caml_ba_kind const
//Requires: caml_ba_views
// Not exactly the bigarray kind, but accurate enough to create
// another array of the same kind
function caml_ba_kind (b) {
  for (var i = 0; caml_ba_views[i]; i++)
    if (b instanceof caml_ba_views[i]) return i;
  return 0; // Should not happen
}

//Provides: caml_ba_layout const

function caml_ba_layout () { return 0; }

//Provides: caml_ba_sub const
//Requires: caml_invalid_argument

function caml_ba_sub (b, ofs, len) {
  if (ofs < 0 || len < 0 || ofs + len > b.length)
    caml_invalid_argument("Bigarray.sub: bad sub-array");
  return b.subarray(ofs, ofs + len);
}

//Provides: caml_ba_blit
//Requires: caml_invalid_argument

function caml_ba_blit (src, dst) {
  if (dst.length != src.length)
    caml_invalid_argument ("Bigarray.blit: dimension mismatch");
  dst.set(src);
  return 0;
}

//Provides: caml_ba_fill

function caml_ba_fill (b, init) {
  for (var i = 0; i < b.length; i++) b[i] = init;
}

//Provides: caml_ba_get_1 mutable
//Requires: caml_array_bound_error

function caml_ba_get_1 (b, ind) {
  if ((ind < 0) || (ind >= b.length)) caml_array_bound_error();
  return b[ind];
}

//Provides: caml_ba_set_1
//Requires: caml_array_bound_error

function caml_ba_set_1 (b, ind, newval) {
  if ((ind < 0) || (ind >= b.length)) caml_array_bound_error();
  b[ind] = newval;
  return 0;
}
