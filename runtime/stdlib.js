///////////// Core
//Provides: caml_call_gen
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

//Provides: caml_named_values
var caml_named_values = [];

//Provides: caml_register_named_value
//Requires: caml_named_values
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}

//Provides: caml_global_data
var caml_global_data = [];

//Provides: caml_register_global
//Requires: caml_global_data
function caml_register_global (n, v) { caml_global_data[n] = v; }

//Provides: caml_raise_constant
function caml_raise_constant (tag) { throw [0, tag]; }

//Provides: caml_raise_with_arg
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }

//Provides: caml_raise_with_string
//Requires: caml_raise_with_arg, MlString
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlString (msg));
}

//Provides: caml_invalid_argument
//Requires: caml_raise_with_string
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}

//Provides: caml_failwith
//Requires: caml_raise_with_string, caml_global_data
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[2], msg);
}

//Provides: caml_array_bound_error
//Requires: caml_invalid_argument
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_raise_zero_divide
//Requires: caml_raise_constant, caml_global_data
function caml_raise_zero_divide () {
  caml_raise_constant(caml_global_data[5]);
}

//Provides: caml_update_dummy
function caml_update_dummy (x, y) {
  var i = y.length;
  while (i--) x[i] = y[i];
  return 0;
}

//Provides: caml_obj_is_block const
function caml_obj_is_block (x) { return +(x instanceof Array); }
//Provides: caml_obj_tag const
function caml_obj_tag (x) { return (x instanceof Array)?x[0]:1000; }
//Provides: caml_obj_set_tag
function caml_obj_tag (x, tag) { x[0] = tag; return 0; }
//Provides: caml_obj_block const
function caml_obj_block (tag, size) {
  var o = [tag];
  for (var i = 1; i <= size; i++) o[i] = 0;
  return o;
}
//Provides: caml_obj_dup mutable
function caml_obj_dup (x) { return x.slice(); }
//Provides: caml_obj_truncate
function caml_obj_truncate (x, s) { x.length = s + 1; return 0; }

//Provides: caml_lazy_make_forward
function caml_lazy_make_forward (v) {
  return [250, v];
}

//Provides: caml_mul const
function caml_mul(x,y) {
  return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0;
}

//slightly slower
// function mul32(x,y) {
//   var xlo = x & 0xffff;
//   var xhi = x - xlo;
//   return (((xhi * y) |0) + xlo * y)|0;
// }

//Provides: caml_div const
//Requires: caml_raise_zero_divide
function caml_div(x,y) {
    if (y == 0) caml_raise_zero_divide ();
    return (x/y)|0;
}

//Provides: caml_mod const
//Requires: caml_raise_zero_divide
function caml_mod(x,y) {
    if (y == 0) caml_raise_zero_divide ();
    return x%y;
}

///////////// Pervasive
//Provides: caml_array_set
//Requires: caml_array_bound_error
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}

//Provides: caml_array_get mutable
//Requires: caml_array_bound_error
function caml_array_get (array, index) {
  var res = array[index+1];
  if (res == undefined) caml_array_bound_error();
  return res;
}

//Provides: caml_make_vect const
function caml_make_vect (len, init) {
  var b = [0];
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
//Provides: caml_compare mutable
//Requires: MlString, caml_int64_compare
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
//Provides: caml_int_compare mutable
function caml_int_compare (a, b) {
  if (a < b) return (-1); else if (a == b) return 0; else return 1;
}
//Provides: caml_equal mutable
//Requires: caml_compare
function caml_equal (x, y) { return +(caml_compare(x,y) == 0); }
//Provides: caml_notequal mutable
//Requires: caml_compare
function caml_notequal (x, y) { return +(caml_compare(x,y) != 0); }
//Provides: caml_greaterequal mutable
//Requires: caml_compare
function caml_greaterequal (x, y) { return +(caml_compare(x,y) >= 0); }
//Provides: caml_greaterthan mutable
//Requires: caml_compare
function caml_greaterthan (x, y) { return +(caml_compare(x,y) > 0); }
//Provides: caml_lessequal mutable
//Requires: caml_compare
function caml_lessequal (x, y) { return +(caml_compare(x,y) <= 0); }
//Provides: caml_lessthan mutable
//Requires: caml_compare
function caml_lessthan (x, y) { return +(caml_compare(x,y) < 0); }

///////////// String
//Provides: caml_create_string const
//Requires: MlString
function caml_create_string(len) { return new MlString(len); }
//Provides: caml_fill_string
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); return 0; }
//Provides: caml_string_compare mutable
function caml_string_compare(s1, s2) { return s1.compare(s2); }
//Provides: caml_string_equal mutable
function caml_string_equal(s1, s2) { return +s1.equal(s2); }
//Provides: caml_string_notequal mutable
function caml_string_notequal(s1, s2) { return +s1.notEqual(s2); }
//Provides: caml_is_printable const
function caml_is_printable(c) { return +(c > 31 && c < 127); }
//Provides: caml_blit_string
function caml_blit_string(s1, i1, s2, i2, len) {
  s2.replace (i2, s1.contents, i1, len); return 0;
}

///////////// Format
//Provides: caml_parse_format
//Requires: caml_invalid_argument
function caml_parse_format (fmt) {
  fmt = fmt.toString ();
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f =
    { justify:'+', signstyle:'-', filler:' ', alternate:false,
      base:0, signedconv:false, width:0, uppercase:false,
      sign:1, prec:6, conv:'f' };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
    case '-':
      f.justify = '-'; break;
    case '+': case ' ':
      f.signstyle = c; break;
    case '0':
      f.filler = '0'; break;
    case '#':
      f.alternate = true; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      f.width = 0;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.width = f.width * 10 + c; i++
      }
      i--;
     break;
    case '.':
      f.prec = 0;
      i++;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.prec = f.prec * 10 + c; i++
      }
      i--;
    case 'd': case 'i':
      f.signedconv = true; /* fallthrough */
    case 'u':
      f.base = 10; break;
    case 'x':
      f.base = 16; break;
    case 'X':
      f.base = 16; f.uppercase = true; break;
    case 'o':
      f.base = 8; break;
    case 'e': case 'f': case 'g':
      f.signedconv = true; f.conv = c; break;
    case 'E': case 'F': case 'G':
      f.signedconv = true; f.uppercase = true;
      f.conv = c.toLowerCase (); break;
    }
  }
  return f;
}

//Provides: caml_finish_formatting
//Requires: MlString
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  /* Adjust len to reflect additional chars (sign, etc) */
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  /* Do the formatting */
  var buffer = "";
  if (f.justify == '+' && f.filler == ' ')
    for (i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (i = len; i < f.width; i++) buffer += ' ';
  return new MlString (buffer);
}

//Provides: caml_format_int const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_int(fmt, i) {
  if (fmt.toString() == "%d") return new MlString(""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  return caml_finish_formatting(f, s);
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float (fmt, x) {
  var s, f = caml_parse_format(fmt);
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(f.prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(f.prec); break;
    case 'g':
      var prec = f.prec?f.prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x.toFixed(0).length > prec) {
        // remove trailing zeroes
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          // remove trailing zeroes
          i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}

///////////// Hashtbl
//Provides: caml_hash_univ_param mutable
//Requires: MlString
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
//Provides: caml_sys_time mutable
var caml_initial_time = Date.now() * 0.001;
function caml_sys_time () { return Date.now() * 0.001 - caml_initial_time; }
//Provides: caml_sys_get_config const
function caml_sys_get_config (e) { return [0, "Unix", 32]; }
//Provides: caml_sys_random_seed mutable
function caml_sys_random_seed () {
  return Date.now()^0xffffffff*Math.random();
}

///////////// CamlinternalOO
//Provides: caml_get_public_method const
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
//Provides: caml_ml_out_channels_list const
function caml_ml_out_channels_list () { return 0; }
//Provides: caml_ml_flush const
function caml_ml_flush () { return 0; }
//Provides: caml_ml_open_descriptor_out const
function caml_ml_open_descriptor_out () { return 0; }
//Provides: caml_ml_open_descriptor_in const
function caml_ml_open_descriptor_in () { return 0; }
//Provides: caml_sys_get_argv const
function caml_sys_get_argv () { return ["a.out"]; }
//Provides: caml_ml_output const
function caml_ml_output () { return 0; }
