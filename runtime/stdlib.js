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

var caml_named_values = [];

function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}

var caml_global_data = [];

function caml_register_global (n, v) { caml_global_data[n] = v; }

function caml_raise_constant (tag) { throw [0, tag]; }

function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }

function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlString (msg));
}

function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}

function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[2], msg);
}

function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}

function caml_raise_zero_divide () {
  caml_raise_constant(caml_global_data[5]);
}

function caml_update_dummy (x, y) {
  var i = y.length;
  while (i--) x[i] = y[i];
  return 0;
}

function caml_obj_tag (x) { return (x instanceof Array)?x[0]:1000; }

function caml_mul(x,y) {
  return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0;
}

//slightly slower
// function mul32(x,y) {
//   var xlo = x & 0xffff;
//   var xhi = x - xlo;
//   return (((xhi * y) |0) + xlo * y)|0;
// }

function caml_div(x,y) {
    if (y == 0) caml_raise_zero_divide ();
    return (x/y)|0;
}

function caml_mod(x,y) {
    if (y == 0) caml_raise_zero_divide ();
    return x%y;
}

///////////// Pervasive
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}
function caml_array_get (array, index) {
  var res = array[index+1];
  if (res == undefined) caml_array_bound_error();
  return res;
}

function caml_make_vect (len, init) {
  var b = [];
  b[0] = 0;
  for (var i = 1; i <= len; i++) b[i] = init;
  return b;
}

// FIX: extra parameter (total?)
//function compare_val(v1, v2, total) {
//  var sp = [];
//  loop:for (;;) {
//    var c = 0;
//    if (v1 !== v2 || not total) {
//      if (v1 instanceof MlString) {
//        if (v2 instanceof MlString) {
//          c = v1===v2?0:v1.compare(v2);
//      } else
//          return (-1); // should not happen
//      } else if (v1 instanceof Array) {
//        if (v2 instanceof Array) {
//          if (v1[0] == 255 && v2[0] == 255) // 64 bit integer
//            c = caml_int64_compare(v1, v2);
//          else if (v1[0] == 248 && v2[0] == 248) // object
//            c = v1[2] - v2[2];
//          else {
//            // Object...
//            if (v1.length != v2.length)
//              return (v1.length - v2.length);
//            else {
//        // check 
//              sp.push(v1, v2, 1, v1.length - 1)
//            }
//          }
//        } else
//          return 1; // block > long
//      } else if (v2 instanceof Array)
//          return -1; // long < block
//      else {
//        if (v1 < v2) return -1;
//        if (v1 > v2) return 1;
//        if (v1 !== v2) {
//          if (not total) return null;
//          if (v1 === v1) return 1;
//          if (v2 === v2) return -1;
//        }
//      }
//      if (c || sp.length == 0) return c;
//      var i = sp.length - 1;
//      var l = sp[i];
//      var n = sp[i - 1]++;
//      v1 = sp[i-3][n];
//      v2 = sp[i-2][n];
//      if (n == l) { sp.pop();sp.pop();sp.pop();sp.pop(); }
//    }
//  }
//}
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
      if (a[0] == 255) return caml_int64_compare(a, b); // 64 bit integer
      for (var i = 0; i < a.length; i++) {
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
function caml_int_compare (a, b) {
  if (a < b) return (-1); else if (a == b) return 0; else return 1;
}
function caml_equal (x, y) { return +(caml_compare(x,y) == 0); }
function caml_notequal (x, y) { return +(caml_compare(x,y) != 0); }
function caml_greaterequal (x, y) { return +(caml_compare(x,y) >= 0); }
function caml_greaterthan (x, y) { return +(caml_compare(x,y) > 0); }
function caml_lessequal (x, y) { return +(caml_compare(x,y) <= 0); }
function caml_lessthan (x, y) { return +(caml_compare(x,y) < 0); }

///////////// String
function caml_create_string(len) { return new MlString(len); }
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); return 0; }
function caml_string_compare(s1, s2) { return s1.compare(s2); }
function caml_string_equal(s1, s2) { return +s1.equal(s2); }
function caml_string_notequal(s1, s2) { return +s1.notEqual(s2); }
function caml_is_printable(c) { return +(c > 31 && c < 127); }
function caml_blit_string(s1, i1, s2, i2, len) {
  s2.replace (i2, s1.contents, i1, len); return 0;
}

///////////// Format
// FIX: use format string
function caml_format_float (fmt, x) {
  return new MlString(x.toString(10));
}
function caml_format_int(fmtV, i) {
  var fmt = fmtV.toString();
  var t = fmt.charCodeAt(fmt.length - 1);
  var b = 10;
  switch (t) {
    case 117:
      i &= 0x7fffffff; break;
    case 88: case 120:
      i &= 0x7fffffff; b = 16; break;
    case 111:
      i &= 0x7fffffff; b = 8;
  }
  var pad = ' ';
  var p = 1;
  loop:
  for (;p < fmt.length; p++)
    switch (fmt.charCodeAt(p)) {
      case 45:
          //FIX: left align
        break;
      case 48:
        pad = '0';
        break;
      case 43:
        // FIX: '+' character if positive
        break;
      case 32:
        pad = ' ';
        break;
      case 35:
        // FIX: alternate formatting style
        break;
      default:
        break loop;
    }
  var l = 0, c;
  for (;p < fmt.length; p++) {
    c = fmt.charCodeAt(p) - 48;
    if ((c < 0) || (c > 9)) break;
    l = l * 10 + c;
  }
  var s = i.toString(b);
  l -= s.length;
  if (l > 0) s = new Array(l + 1).join(pad) + s;
  return new MlString(t==88?s.toUpperCase():s);
}

///////////// Hashtbl
function caml_hash_univ_param (count, limit, obj) {
  var hash_accu = 0;
  if (obj instanceof MlString) {
    var s = obj.contents;
    for (var p = 0; p < s.length - 1; p++)
      hash_accu = (hash_accu * 19 + s.charCodeAt(p)) & 0x3FFFFFFF;
    return (hash_accu & 0x3FFFFFFF);
  } else {
    // FIX not implemented!
    //      document.write("hash(", obj, "):", typeof obj);
    return 1;
  }
}

///////////// Sys
var caml_initial_time = (new Date ()).getTime () * 0.001;
function caml_sys_time () {
  return (new Date ()).getTime () * 0.001 - caml_initial_time;
}
function caml_sys_get_config (e) { return [0, "Unix", 32]; }
function caml_sys_random_seed () {
  return (new Date()).getTime()^(Math.pow(2,32)*Math.random());
}

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

/////////////////////////////

// Dummy functions
function caml_ml_out_channels_list () { return 0; }
function caml_ml_flush () { return 0; }
function caml_ml_open_descriptor_out () { return 0; }
