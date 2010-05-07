///////////// Core
function caml_call_gen(f, args) {
  var n = f.length;
  var d = n - args.length;
  if (d == 0)
    return f.apply(this, args);
  else if (d < 0)
    return caml_call_gen(f.apply(this, args.slice(0,n)), args.slice(n));
  else
    return function (x){ return caml_call_gen(f, args.concat([x])); };
}

var caml_global_data = [];

function caml_register_global (n, v) { caml_global_data[n] = v; }

function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }

function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlString (msg));
}

function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}

function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}

///////////// Pervasive
function caml_make_array (a) { return a.slice(); }

function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}
function caml_array_get (array, index) {
  res = array[index+1];
  if (res == undefined) caml_array_bound_error();
  return res;
}
var caml_array_set_addr = caml_array_set;
var caml_array_get_addr = caml_array_get;

function caml_make_vect (len, init) {
  var b = new Array();
  b[0] = 0;
  for (i = 1; i <= len; i++) b[i] = init;
  return b;
}

function caml_obj_block (tag, size) {
  var b = new Array();
  b[0] = tag;
  for (i = 1; i <= size; i++) b[i] = 0;
  return b;
}

function caml_compare (a, b) {
  if (a === b) return 0;
  if (a instanceof MlString) {
    if (b instanceof MlString)
      return a.compare(b)
    else
      return (-1);
  } else if (a instanceof Array) {
    if (b instanceof Array) {
      if (a.length != b.length)
        return (a.length - b.length);
      for (var i = 0;i < a.length;i++) {
        var t = caml_compare (a[i], b[i]);
        if (t != 0) return t;
      }
      return 0;
    } else
      return (-1);
  } else if (b instanceof MlString || b instanceof Array)
      return 1;
  else if (a < b) return (-1); else if (a == b) return 0; else return 1;
}
function caml_equal (x, y) { return (caml_compare(x,y) == 0)+0; }
function caml_greaterequal (x, y) { return (caml_compare(x,y) >= 0)+0; }
function caml_lessequal (x, y) { return (caml_compare(x,y) <= 0)+0; }

// FIX: Assumes 32 bit arithmetic...
function caml_int_of_float(x) { return x|0; };
function caml_int_of_string(x) { return x|0; };

// FIX: implement?
function caml_register_named_value(dz,dx) { return 0;}

// FIX: dummy function
function caml_ml_open_descriptor_in (c) { return 0; }
function caml_ml_open_descriptor_out (c) { return 0; }
function caml_ml_out_channels_list (c) { return 0; }
function caml_ml_flush (c) { return 0; }

///////////// String
function caml_create_string(len) { return new MlString(len); }
function caml_blit_string(s1, i1, s2, i2, len) {
  s2.replace (i2, s1.contents, i1, len); return 0;
}
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); return 0; }
function caml_string_notequal(s1, s2) { return s1.notEqual(s2)+0; }
function caml_is_printable(c) { return (c > 31 && c < 127)+0; }

///////////// Format
// FIX: use format string
function caml_format_float (fmt, x) {
  return new MlString(x.toString(10));
}
function caml_format_int(fmt, i) { return new MlString(String(i)); }
// FIX: caml_int32_format, caml_int64_format, caml_nativeint_format

///////////// Hashtbl
function caml_hash_univ_param (count, limit, obj) {
  var hash_accu = 0;
  if (obj instanceof MlString) {
    var s = obj.contents;
    for (var p = 0;p < s.length - 1; p++)
      hash_accu = (hash_accu * 19 + s.charCodeAt(p)) & 0x3FFFFFFF;
    return (hash_accu & 0x3FFFFFFF);
  } else {
      // FIX not implemented!
      document.write("hash(", obj, "):", typeof obj);
  }
}

///////////// Sys
var caml_initial_time = (new Date ()).getTime () * 0.001;
function caml_sys_time (unit) {
  return ((new Date ()).getTime () * 0.001 - caml_initial_time);
}
function caml_sys_get_config (e) { return [0, "Unix", 32]; }

///////////// CamlinternalOO
function caml_get_public_method (obj, tag) {
  var meths = obj[1];
  var li = 3, hi = meths[1] * 2 + 1, mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < meths[mi+1]) hi = mi-2;
    else li = mi;
  }
  /* return 0 if tag is not there */
  return (tag == meths[li+1] ? meths[li] : 0);
}
