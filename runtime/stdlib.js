// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
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

///////////// Core
//Provides: caml_call_gen
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  var n = f.length;
  var d = n - args.length;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0)
    return caml_call_gen(f.apply(null, args.slice(0,n)), args.slice(n));
  else
    return function (x){ return caml_call_gen(f, args.concat([x])); };
}

//Provides: caml_named_values
var caml_named_values = {};

//Provides: caml_register_named_value
//Requires: caml_named_values
function caml_register_named_value(nm,v) {
  caml_named_values[nm.toString()] = v; return 0;
}

//Provides: caml_named_value
//Requires: caml_named_values
function caml_named_value(nm) {
  return caml_named_values[nm]
}

//Provides: caml_global_data
var caml_global_data = [0];

//Provides: caml_register_global(const,shallow)
//Requires: caml_global_data
function caml_register_global (n, v) { caml_global_data[n + 1] = v; }

//Provides: caml_get_global_data
//Requires: caml_global_data
function caml_get_global_data () { return caml_global_data; }

//Raise exception


//Provides: caml_raise_constant
//Version: < 4.02
function caml_raise_constant (tag) { throw [0, tag]; }

//Provides: caml_raise_constant
//Version: >= 4.02
function caml_raise_constant (tag) { throw tag; }

//Provides: caml_raise_with_arg
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }

//Provides: caml_raise_with_string
//Requires: caml_raise_with_arg,MlWrappedString
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlWrappedString (msg));
}

//Provides: caml_raise_sys_error
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
  caml_raise_with_string(caml_global_data[2], msg);
}

//Provides: caml_failwith
//Requires: caml_raise_with_string, caml_global_data
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}
//Provides: caml_wrap_exception
//Requires: caml_global_data,MlWrappedString,caml_named_value
function caml_wrap_exception(e) {
  if(e instanceof Array) return e;
  //Stack_overflow: chrome, safari
  if(joo_global_object.RangeError
     && e instanceof joo_global_object.RangeError
     && e.message
     && e.message.match(/maximum call stack/i))
    return [0,caml_global_data[9]];
  //Stack_overflow: firefox
  if(joo_global_object.InternalError
     && e instanceof joo_global_object.InternalError
     && e.message
     && e.message.match(/too much recursion/i))
    return [0,caml_global_data[9]];
  //Wrap Error in Js.Error exception
  if(e instanceof joo_global_object.Error)
    return [0,caml_named_value("jsError"),e];
  //fallback: wrapped in Failure
  return [0,caml_global_data[3],new MlWrappedString (String(e))];
}

//Provides: caml_invalid_argument
//Requires: caml_raise_with_string, caml_global_data
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[4], msg);
}

//Provides: caml_raise_end_of_file
//Requires: caml_raise_constant, caml_global_data
function caml_raise_end_of_file () {
  caml_raise_constant(caml_global_data[5]);
}

//Provides: caml_raise_zero_divide
//Requires: caml_raise_constant, caml_global_data
function caml_raise_zero_divide () {
  caml_raise_constant(caml_global_data[6]);
}

//Provides: caml_raise_not_found
//Requires: caml_raise_constant, caml_global_data
function caml_raise_not_found () {
  caml_raise_constant(caml_global_data[7]); }


//Provides: caml_array_bound_error
//Requires: caml_invalid_argument
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_update_dummy
function caml_update_dummy (x, y) {
  if( typeof y==="function" ) { x.fun = y; return 0; }
  if( y.fun ) { x.fun = y.fun; return 0; }
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}

//Provides: caml_obj_is_block const
function caml_obj_is_block (x) { return +(x instanceof Array); }
//Provides: caml_obj_tag const
function caml_obj_tag (x) { return (x instanceof Array)?x[0]:1000; }
//Provides: caml_obj_set_tag
function caml_obj_set_tag (x, tag) { x[0] = tag; return 0; }
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
function caml_lazy_make_forward (v) { return [250, v]; }

//Provides: caml_mul const
if (!Math.imul)
  Math.imul =
    function (x,y) { return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; };
var caml_mul = Math.imul;

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
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}

//Provides: caml_array_get mutable
//Requires: caml_array_bound_error
function caml_array_get (array, index) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  return array[index+1];
}

//Provides: caml_make_vect const
function caml_make_vect (len, init) {
  var b = [0]; for (var i = 1; i <= len; i++) b[i] = init; return b;
}

//Provides: caml_compare_val
//Requires: MlString, caml_int64_compare, caml_int_compare
//Requires: caml_invalid_argument
function caml_compare_val (a, b, total) {
  var stack = [];
  for(;;) {
    if (!(total && a === b)) {
      if (a instanceof MlString) {
        if (b instanceof MlString) {
            if (a !== b) {
		var x = a.compare(b);
		if (x != 0) return x;
	    }
        } else
          // Should not happen
          return 1;
      } else if (a instanceof Array && a[0] === (a[0]|0)) {
        var ta = a[0];
        // ignore double_array_tag
        if (ta === 254) ta=0;
        // Forward object
        if (ta === 250) {
          a = a[1];
          continue;
        } else if (b instanceof Array && b[0] === (b[0]|0)) {
          var tb = b[0];
          // ignore double_array_tag
          if (tb === 254) tb=0;
          // Forward object
          if (tb === 250) {
            b = b[1];
            continue;
          } else if (ta != tb) {
            return (ta < tb)?-1:1;
          } else {
            switch (ta) {
            case 248: {
		// Object
		var x = caml_int_compare(a[2], b[2]);
		if (x != 0) return x;
		break;
	    }
            case 251: {
                caml_invalid_argument("equal: abstract value");
            }
            case 255: {
		// Int64
		var x = caml_int64_compare(a, b);
		if (x != 0) return x;
		break;
	    }
            default:
              if (a.length != b.length) return (a.length < b.length)?-1:1;
              if (a.length > 1) stack.push(a, b, 1);
            }
          }
        } else
          return 1;
      } else if (b instanceof MlString ||
                 (b instanceof Array && b[0] === (b[0]|0))) {
        return -1;
      } else if (typeof a != "number" && a && a.compare) {
        return a.compare(b,total);
      } else {
        if (a < b) return -1;
        if (a > b) return 1;
        if (a != b) {
          if (!total) return NaN;
          if (a == a) return 1;
          if (b == b) return -1;
        }
      }
    }
    if (stack.length == 0) return 0;
    var i = stack.pop();
    b = stack.pop();
    a = stack.pop();
    if (i + 1 < a.length) stack.push(a, b, i + 1);
    a = a[i];
    b = b[i];
  }
}
//Provides: caml_compare
//Requires: caml_compare_val
function caml_compare (a, b) { return caml_compare_val (a, b, true); }
//Provides: caml_int_compare mutable
function caml_int_compare (a, b) {
  if (a < b) return (-1); if (a == b) return 0; return 1;
}
//Provides: caml_equal mutable
//Requires: caml_compare_val
function caml_equal (x, y) { return +(caml_compare_val(x,y,false) == 0); }
//Provides: caml_notequal mutable
//Requires: caml_compare_val
function caml_notequal (x, y) { return +(caml_compare_val(x,y,false) != 0); }
//Provides: caml_greaterequal mutable
//Requires: caml_compare_val
function caml_greaterequal (x, y) { return +(caml_compare_val(x,y,false) >= 0); }
//Provides: caml_greaterthan mutable
//Requires: caml_compare_val
function caml_greaterthan (x, y) { return +(caml_compare_val(x,y,false) > 0); }
//Provides: caml_lessequal mutable
//Requires: caml_compare_val
function caml_lessequal (x, y) { return +(caml_compare_val(x,y,false) <= 0); }
//Provides: caml_lessthan mutable
//Requires: caml_compare_val
function caml_lessthan (x, y) { return +(caml_compare_val(x,y,false) < 0); }

//Provides: caml_parse_sign_and_base
//Requires: MlString
function caml_parse_sign_and_base (s) {
  var i = 0, base = 10, sign = s.get(0) == 45?(i++,-1):1;
  if (s.get(i) == 48)
    switch (s.get(i + 1)) {
    case 120: case 88: base = 16; i += 2; break;
    case 111: case 79: base =  8; i += 2; break;
    case  98: case 66: base =  2; i += 2; break;
    }
  return [i, sign, base];
}

//Provides: caml_parse_digit
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57)  return c - 48;
  if (c >= 65 && c <= 90)  return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}

//Provides: caml_int_of_string mutable
//Requires: caml_parse_sign_and_base, caml_parse_digit, MlString, caml_failwith
function caml_int_of_string (s) {
  var r = caml_parse_sign_and_base (s);
  var i = r[0], sign = r[1], base = r[2];
  var threshold = -1 >>> 0;
  var c = s.get(i);
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("int_of_string");
  var res = d;
  for (;;) {
    i++;
    c = s.get(i);
    if (c == 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
    if (res > threshold) caml_failwith("int_of_string");
  }
  if (i != s.getLen()) caml_failwith("int_of_string");
  // For base different from 10, we expect an unsigned representation,
  // hence any value of 'res' (less than 'threshold') is acceptable.
  // But we have to convert the result back to a signed integer.
  res = sign * res;
  if ((base == 10) && ((res | 0) != res))
    /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
    caml_failwith("int_of_string");
  return res | 0;
}

//Provides: caml_float_of_string mutable
//Requires: caml_failwith
function caml_float_of_string(s) {
  var res;
  s = s.getFullBytes();
  res = +s;
  if ((s.length > 0) && (res === res)) return res;
  s = s.replace(/_/g,"");
  res = +s;
  if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) return res;
  if(/^ *0x[0-9a-f_]+p[+-]?[0-9_]+/i.test(s)){
    var pidx = s.indexOf('p');
    pidx = (pidx==-1)?s.indexOf('P'):pidx;
    var exp = +s.substring(pidx + 1);
    res = +s.substring(0,pidx);
    return res * Math.pow(2,exp);
  }
  caml_failwith("float_of_string");
}

//Provides: caml_is_printable const
function caml_is_printable(c) { return +(c > 31 && c < 127); }

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
      sign:1, prec:-1, conv:'f' };
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
//Requires: MlWrappedString
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
    for (var i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (var i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (var i = len; i < f.width; i++) buffer += ' ';
  return new MlWrappedString (buffer);
}

//Provides: caml_format_int const
//Requires: caml_parse_format, caml_finish_formatting, MlWrappedString,caml_str_repeat
function caml_format_int(fmt, i) {
  if (fmt.toString() == "%d") return new MlWrappedString(""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - s.length;
    if (n > 0) s = caml_str_repeat (n, '0') + s;
  }
  return caml_finish_formatting(f, s);
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float (fmt, x) {
  var s, f = caml_parse_format(fmt);
  var prec = (f.prec < 0)?6:f.prec;
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(prec); break;
    case 'g':
      prec = prec?prec:1;
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
          var i = s.length - 1; while (s.charAt(i) == '0') i--;
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
//Requires: MlString, caml_int64_to_bytes, caml_int64_bits_of_float
function caml_hash_univ_param (count, limit, obj) {
  var hash_accu = 0;
  function hash_aux (obj) {
    limit --;
    if (count < 0 || limit < 0) return;
    if (obj instanceof Array && obj[0] === (obj[0]|0)) {
      switch (obj[0]) {
      case 248:
        // Object
        count --;
        hash_accu = (hash_accu * 65599 + obj[2]) | 0;
        break;
      case 250:
        // Forward
        limit++; hash_aux(obj); break;
      case 255:
        // Int64
        count --;
        hash_accu = (hash_accu * 65599 + obj[1] + (obj[2] << 24)) | 0;
        break;
      default:
        count --;
        hash_accu = (hash_accu * 19 + obj[0]) | 0;
        for (var i = obj.length - 1; i > 0; i--) hash_aux (obj[i]);
      }
    } else if (obj instanceof MlString) {
      count --;
      var a = obj.array, l = obj.getLen ();
      if (a) {
        for (var i = 0; i < l; i++) hash_accu = (hash_accu * 19 + a[i]) | 0;
      } else {
        var b = obj.getFullBytes ();
        for (var i = 0; i < l; i++)
          hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
      }
    } else if (obj === (obj|0)) {
      // Integer
      count --;
      hash_accu = (hash_accu * 65599 + obj) | 0;
    } else if (obj === +obj) {
      // Float
      count--;
      var p = caml_int64_to_bytes (caml_int64_bits_of_float (obj));
      for (var i = 7; i >= 0; i--) hash_accu = (hash_accu * 19 + p[i]) | 0;
    }
  }
  hash_aux (obj);
  return hash_accu & 0x3FFFFFFF;
}

//Provides: caml_hash mutable
//Requires: MlString, caml_int64_bits_of_float, caml_mul
var caml_hash =
function () {
  var HASH_QUEUE_SIZE = 256;
  function ROTL32(x,n) { return ((x << n) | (x >>> (32-n))); }
  function MIX(h,d) {
    d = caml_mul(d, 0xcc9e2d51);
    d = ROTL32(d, 15);
    d = caml_mul(d, 0x1b873593);
    h ^= d;
    h = ROTL32(h, 13);
    return ((((h * 5)|0) + 0xe6546b64)|0);
  }
  function FINAL_MIX(h) {
    h ^= h >>> 16;
    h = caml_mul (h, 0x85ebca6b);
    h ^= h >>> 13;
    h = caml_mul (h, 0xc2b2ae35);
    h ^= h >>> 16;
    return h;
  }
  function caml_hash_mix_int64 (h, v) {
    var lo = v[1] | (v[2] << 24);
    var hi = (v[2] >>> 8) | (v[3] << 16);
    h = MIX(h, lo);
    h = MIX(h, hi);
    return h;
  }
  function caml_hash_mix_int64_2 (h, v) {
    var lo = v[1] | (v[2] << 24);
    var hi = (v[2] >>> 8) | (v[3] << 16);
    h = MIX(h, hi ^ lo);
    return h;
  }
  function caml_hash_mix_string_str(h, s) {
    var len = s.length, i, w;
    for (i = 0; i + 4 <= len; i += 4) {
      w = s.charCodeAt(i)
          | (s.charCodeAt(i+1) << 8)
          | (s.charCodeAt(i+2) << 16)
          | (s.charCodeAt(i+3) << 24);
      h = MIX(h, w);
    }
    w = 0;
    switch (len & 3) {
    case 3: w  = s.charCodeAt(i+2) << 16;
    case 2: w |= s.charCodeAt(i+1) << 8;
    case 1: w |= s.charCodeAt(i);
            h = MIX(h, w);
    default:
    }
    h ^= len;
    return h;
  }
  function caml_hash_mix_string_arr(h, s) {
    var len = s.length, i, w;
    for (i = 0; i + 4 <= len; i += 4) {
      w = s[i]
          | (s[i+1] << 8)
          | (s[i+2] << 16)
          | (s[i+3] << 24);
      h = MIX(h, w);
    }
    w = 0;
    switch (len & 3) {
    case 3: w  = s[i+2] << 16;
    case 2: w |= s[i+1] << 8;
    case 1: w |= s[i];
            h = MIX(h, w);
    default:
    }
    h ^= len;
    return h;
  }
  return function (count, limit, seed, obj) {
    var queue, rd, wr, sz, num, h, v, i, len;
    sz = limit;
    if (sz < 0 || sz > HASH_QUEUE_SIZE) sz = HASH_QUEUE_SIZE;
    num = count;
    h = seed;
    queue = [obj]; rd = 0; wr = 1;
    while (rd < wr && num > 0) {
      v = queue[rd++];
      if (v instanceof Array && v[0] === (v[0]|0)) {
        switch (v[0]) {
        case 248:
          // Object
          h = MIX(h, v[2]);
          num--;
          break;
        case 250:
          // Forward
          queue[--rd] = v[1];
          break;
        case 255:
          // Int64
          h = caml_hash_mix_int64_2 (h, v);
          num --;
          break;
        default:
          var tag = ((v.length - 1) << 10) | v[0];
          h = MIX(h, tag);
          for (i = 1, len = v.length; i < len; i++) {
            if (wr >= sz) break;
            queue[wr++] = v[i];
          }
          break;
        }
      } else if (v instanceof MlString) {
        var a = v.array;
        if (a) {
          h = caml_hash_mix_string_arr(h, a);
        } else {
          var b = v.getFullBytes ();
          h = caml_hash_mix_string_str(h, b);
        }
        num--;
        break;
      } else if (v === (v|0)) {
        // Integer
        h = MIX(h, v+v+1);
        num--;
      } else if (v === +v) {
        // Float
        h = caml_hash_mix_int64(h, caml_int64_bits_of_float (v));
        num--;
        break;
      }
    }
    h = FINAL_MIX(h);
    return h & 0x3FFFFFFF;
  }
} ();

///////////// Sys
//Provides: caml_sys_time mutable
var caml_initial_time = new Date() * 0.001;
function caml_sys_time () { return new Date() * 0.001 - caml_initial_time; }
//Provides: caml_sys_get_config const
//Requires: MlWrappedString
function caml_sys_get_config () {
  return [0, new MlWrappedString("Unix"), 32, 0];
}
//Provides: caml_sys_random_seed mutable
//The function needs to return an array since OCaml 4.0...
function caml_sys_random_seed () {
  var x = new Date()^0xffffffff*Math.random();
  return {valueOf:function(){return x;},0:0,1:x,length:2};
}
//Provides: caml_sys_const_big_endian const
function caml_sys_const_big_endian () { return 0; }
//Provides: caml_sys_const_word_size const
function caml_sys_const_word_size () { return 32; }
//Provides: caml_sys_const_int_size const
function caml_sys_const_int_size () { return 32; }

//Provides: caml_sys_const_max_wosize const
function caml_sys_const_max_wosize () { return 0x7FFFFFFF; /* max_int */}

//Provides: caml_sys_const_ostype_cygwin const
function caml_sys_const_ostype_cygwin () { return 0; }
//Provides: caml_sys_const_ostype_unix const
function caml_sys_const_ostype_unix () { return 1; }
//Provides: caml_sys_const_ostype_win32 const
function caml_sys_const_ostype_win32 () { return 0; }

///////////// Array
//Provides: caml_array_sub mutable
function caml_array_sub (a, i, len) {
  return [0].concat(a.slice(i+1, i+1+len));
}

//Provides: caml_array_append mutable
function caml_array_append(a1, a2) {
  return a1.concat(a2.slice(1));
}

//Provides: caml_array_concat mutable
function caml_array_concat(l) {
  var a = [0];
  while (l !== 0) {
    var b = l[1];
    for (var i = 1; i < b.length; i++) a.push(b[i]);
    l = l[2];
  }
  return a;
}

//Provides: caml_array_blit
function caml_array_blit(a1, i1, a2, i2, len) {
  if (i2 <= i1) {
    for (var j = 1; j <= len; j++) a2[i2 + j] = a1[i1 + j];
  } else {
    for (var j = len; j >= 1; j--) a2[i2 + j] = a1[i1 + j];
  }
}

///////////// CamlinternalOO
//Provides: caml_get_public_method const
var caml_method_cache = [];
function caml_get_public_method (obj, tag, cacheid) {
  var meths = obj[1];
  var ofs = caml_method_cache[cacheid];
  if (ofs === null) {
    // Make sure the array is not sparse
    for (var i = caml_method_cache.length; i < cacheid; i++)
      caml_method_cache[i] = 0;
  } else if (meths[ofs] === tag) {
//      console.log("cache hit");
    return meths[ofs - 1];
  }
//  console.log("cache miss");
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

//Provides: caml_final_register const
function caml_final_register () { return 0; }
//Provides: caml_final_release const
function caml_final_release () { return 0; }
//Provides: caml_backtrace_status const
function caml_backtrace_status () { return 0; }
//Provides: caml_get_exception_backtrace const
function caml_get_exception_backtrace () { return 0; }
//Provides: caml_get_exception_raw_backtrace const
function caml_get_exception_raw_backtrace () { return 0; }
//Provides: caml_record_backtrace
function caml_record_backtrace () { return 0; }
//Provides: caml_convert_raw_backtrace const
function caml_convert_raw_backtrace () { return 0; }
//Provides: caml_get_current_callstack const
function caml_get_current_callstack () { return 0; }
//Provides: caml_sys_getenv
//Requires: caml_raise_not_found
//Requires: MlWrappedString
function caml_sys_getenv (name) {
  var g = joo_global_object;
  var n = name.toString();
  //nodejs env
  if(g.process
     && g.process.env
     && g.process.env[n] != undefined)
    return new MlWrappedString(g.process.env[n]);
  caml_raise_not_found ();
}
//Provides: caml_sys_exit
//Requires: caml_invalid_argument
function caml_sys_exit (code) {
  var g = joo_global_object;
  if(g.quit) g.quit(code);
  //nodejs
  if(g.process && g.process.exit) g.process.exit(code);
  caml_invalid_argument("Function 'exit' not implemented");
}

//Provides: caml_sys_get_argv const
//Requires: MlWrappedString
function caml_sys_get_argv () {
  var g = joo_global_object;
  var main = "a.out";
  var args = []

  if(g.process
     && g.process.argv
     && g.process.argv.length > 0) {
    //nodejs
    main = g.process.argv[0];
    args = g.process.argv.slice(1);
  }

  var p = new MlWrappedString(main);
  var args2 = [0, p];
  for(var i = 0; i < args.length; i++)
    args2.push(new MlWrappedString(args[i]));
  return [0, p, args2];
}

//Provides: unix_inet_addr_of_string
function unix_inet_addr_of_string () {return 0;}


//Provides: caml_set_oo_id
var caml_oo_last_id = 0;
function caml_set_oo_id (b) {
  b[2]=caml_oo_last_id++;
  return b;
}

//Provides: caml_install_signal_handler const
function caml_install_signal_handler(){return 0}
