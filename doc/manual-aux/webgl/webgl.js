// This program was compiled from OCaml by js_of_ocaml 1.0
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlWrappedString (msg));
}
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[4], msg);
}
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}
function MlString(param) {
  if (param != null) {
    this.bytes = this.fullBytes = param;
    this.last = this.len = param.length;
  }
}
MlString.prototype = {
  string:null,
  bytes:null,
  fullBytes:null,
  array:null,
  len:null,
  last:0,
  toJsString:function() {
    return this.string = decodeURIComponent (escape(this.getFullBytes()));
  },
  toBytes:function() {
    if (this.string != null)
      var b = unescape (encodeURIComponent (this.string));
    else {
      var b = "", a = this.array, l = a.length;
      for (var i = 0; i < l; i ++) b += String.fromCharCode (a[i]);
    }
    this.bytes = this.fullBytes = b;
    this.last = this.len = b.length;
    return b;
  },
  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },
  getFullBytes:function() {
    var b = this.fullBytes;
    if (b !== null) return b;
    b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    this.fullBytes = b;
    return b;
  },
  toArray:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = this.fullBytes = null;
    this.last = this.len;
    this.array = a;
    return a;
  },
  getArray:function() {
    var a = this.array;
    if (!a) a = this.toArray();
    return a;
  },
  getLen:function() {
    var len = this.len;
    if (len !== null) return len;
    this.toBytes();
    return this.len;
  },
  toString:function() { var s = this.string; return s?s:this.toJsString(); },
  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },
  blitToArray:function(i1, a2, i2, l) {
    var a1 = this.array;
    if (a1)
      for (var i = 0; i < l; i++) a2 [i2 + i] = a1 [i1 + i];
    else {
      var b = this.bytes;
      if (b == null) b = this.toBytes();
      var l1 = this.last - i1;
      if (l <= l1)
        for (var i = 0; i < l; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
      else {
        for (var i = 0; i < l1; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
        for (; i < l; i++) a2 [i2 + i] = 0;
      }
    }
  },
  get:function (i) {
    var a = this.array;
    if (a) return a[i];
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return (i<this.last)?b.charCodeAt(i):0;
  },
  safeGet:function (i) {
    if (!this.len) this.toBytes();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    return this.get(i);
  },
  set:function (i, c) {
    var a = this.array;
    if (!a) {
      if (this.last == i) {
        this.bytes += String.fromCharCode (c & 0xff);
        this.last ++;
        return 0;
      }
      a = this.toArray();
    } else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    a[i] = c & 0xff;
    return 0;
  },
  safeSet:function (i, c) {
    if (this.len == null) this.toBytes ();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    this.set(i, c);
  },
  fill:function (ofs, len, c) {
    if (ofs >= this.last && this.last && c == 0) return;
    var a = this.array;
    if (!a) a = this.toArray();
    else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    var l = ofs + len;
    for (var i = ofs; i < l; i++) a[i] = c;
  },
  compare:function (s2) {
    if (this.string != null && s2.string != null) {
      if (this.string < s2.string) return -1;
      if (this.string > s2.string) return 1;
      return 0;
    }
    var b1 = this.getFullBytes ();
    var b2 = s2.getFullBytes ();
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
    return 0;
  },
  equal:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string == s2.string;
    return this.getFullBytes () == s2.getFullBytes ();
  },
  lessThan:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string < s2.string;
    return this.getFullBytes () < s2.getFullBytes ();
  },
  lessEqual:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string <= s2.string;
    return this.getFullBytes () <= s2.getFullBytes ();
  }
}
function MlWrappedString (s) { this.string = s; }
MlWrappedString.prototype = new MlString();
function MlMakeString (l) { this.bytes = ""; this.len = l; }
MlMakeString.prototype = new MlString ();
function caml_array_get (array, index) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  return array[index+1];
}
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}
function caml_blit_string(s1, i1, s2, i2, len) {
  if (len === 0) return;
  if (i2 === s2.last && s2.bytes != null) {
    var b = s1.bytes;
    if (b == null) b = s1.toBytes ();
    if (i1 > 0 || s1.last > len) b = b.slice(i1, i1 + len);
    s2.bytes += b;
    s2.last += b.length;
    return;
  }
  var a = s2.array;
  if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
  s1.blitToArray (i1, a, i2, len);
}
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
function caml_classify_float (x) {
  if (isFinite (x)) {
    if (Math.abs(x) >= 2.2250738585072014e-308) return 0;
    if (x != 0) return 1;
    return 2;
  }
  return isNaN(x)?4:3;
}
function caml_create_string(len) {
  if (len < 0) caml_invalid_argument("String.create");
  return new MlMakeString(len);
}
function caml_int64_compare(x,y) {
  var x3 = x[3] << 16;
  var y3 = y[3] << 16;
  if (x3 > y3) return 1;
  if (x3 < y3) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}
function caml_int_compare (a, b) {
  if (a < b) return (-1); if (a == b) return 0; return 1;
}
function caml_compare_val (a, b, total) {
  var stack = [];
  for(;;) {
    if (!(total && a === b)) {
      if (a instanceof MlString) {
        if (b instanceof MlString) {
            if (a != b) {
		var x = a.compare(b);
		if (x != 0) return x;
	    }
        } else
          return 1;
      } else if (a instanceof Array && a[0] === (a[0]|0)) {
        var ta = a[0];
        if (ta === 250) {
          a = a[1];
          continue;
        } else if (b instanceof Array && b[0] === (b[0]|0)) {
          var tb = b[0];
          if (tb === 250) {
            b = b[1];
            continue;
          } else if (ta != tb) {
            return (ta < tb)?-1:1;
          } else {
            switch (ta) {
            case 248: {
		var x = caml_int_compare(a[2], b[2]);
		if (x != 0) return x;
		break;
	    }
            case 255: {
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
      } else {
        if (a < b) return -1;
        if (a > b) return 1;
        if (total && a != b) {
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
function caml_equal (x, y) { return +(caml_compare_val(x,y,false) == 0); }
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); }
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
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
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
          var i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}
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
function caml_compare (a, b) { return caml_compare_val (a, b, true); }
function caml_greaterequal (x, y) { return +(caml_compare(x,y,false) >= 0); }
function caml_int64_is_negative(x) {
  return (x[3] << 16) < 0;
}
function caml_int64_neg (x) {
  var y1 = - x[1];
  var y2 = - x[2] + (y1 >> 24);
  var y3 = - x[3] + (y2 >> 24);
  return [255, y1 & 0xffffff, y2 & 0xffffff, y3 & 0xffff];
}
function caml_int64_of_int32 (x) {
  return [255, x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff]
}
function caml_int64_ucompare(x,y) {
  if (x[3] > y[3]) return 1;
  if (x[3] < y[3]) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}
function caml_int64_lsl1 (x) {
  x[3] = (x[3] << 1) | (x[2] >> 23);
  x[2] = ((x[2] << 1) | (x[1] >> 23)) & 0xffffff;
  x[1] = (x[1] << 1) & 0xffffff;
}
function caml_int64_lsr1 (x) {
  x[1] = ((x[1] >>> 1) | (x[2] << 23)) & 0xffffff;
  x[2] = ((x[2] >>> 1) | (x[3] << 23)) & 0xffffff;
  x[3] = x[3] >>> 1;
}
function caml_int64_sub (x, y) {
  var z1 = x[1] - y[1];
  var z2 = x[2] - y[2] + (z1 >> 24);
  var z3 = x[3] - y[3] + (z2 >> 24);
  return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}
function caml_int64_udivmod (x, y) {
  var offset = 0;
  var modulus = x.slice ();
  var divisor = y.slice ();
  var quotient = [255, 0, 0, 0];
  while (caml_int64_ucompare (modulus, divisor) > 0) {
    offset++;
    caml_int64_lsl1 (divisor);
  }
  while (offset >= 0) {
    offset --;
    caml_int64_lsl1 (quotient);
    if (caml_int64_ucompare (modulus, divisor) >= 0) {
      quotient[1] ++;
      modulus = caml_int64_sub (modulus, divisor);
    }
    caml_int64_lsr1 (divisor);
  }
  return [0,quotient, modulus];
}
function caml_int64_to_int32 (x) {
  return x[1] | (x[2] << 24);
}
function caml_int64_is_zero(x) {
  return (x[3]|x[2]|x[1]) == 0;
}
function caml_int64_format (fmt, x) {
  var f = caml_parse_format(fmt);
  if (f.signedconv && caml_int64_is_negative(x)) {
    f.sign = -1; x = caml_int64_neg(x);
  }
  var buffer = "";
  var wbase = caml_int64_of_int32(f.base);
  var cvtbl = "0123456789abcdef";
  do {
    var p = caml_int64_udivmod(x, wbase);
    x = p[1];
    buffer = cvtbl.charAt(caml_int64_to_int32(p[2])) + buffer;
  } while (! caml_int64_is_zero(x));
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - buffer.length;
    if (n > 0) buffer = caml_str_repeat (n, '0') + buffer;
  }
  return caml_finish_formatting(f, buffer);
}
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
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57)  return c - 48;
  if (c >= 65 && c <= 90)  return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}
var caml_global_data = [0];
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}
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
  res = sign * res;
  if ((res | 0) != res) caml_failwith("int_of_string");
  return res;
}
function caml_is_printable(c) { return +(c > 31 && c < 127); }
function caml_js_from_byte_string (s) {return s.getFullBytes();}
function caml_js_get_console () {
  var c = window.console?window.console:{};
  var m = ["log", "debug", "info", "warn", "error", "assert", "dir", "dirxml",
           "trace", "group", "groupCollapsed", "groupEnd", "time", "timeEnd"];
  function f () {}
  for (var i = 0; i < m.length; i++) if (!c[m[i]]) c[m[i]]=f;
  return c;
}
function caml_js_on_ie () {
  var ua = window.navigator?window.navigator.userAgent:"";
  return ua.indexOf("MSIE") != -1 && ua.indexOf("Opera") != 0;
}
function caml_js_to_byte_string (s) {return new MlString (s);}
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[undefined];
    return caml_call_gen(f, args);
  }
}
function caml_make_vect (len, init) {
  var b = [0]; for (var i = 1; i <= len; i++) b[i] = init; return b;
}
function MlStringFromArray (a) {
  var len = a.length; this.array = a; this.len = this.last = len;
}
MlStringFromArray.prototype = new MlString ();
var caml_md5_string =
function () {
  function add (x, y) { return (x + y) | 0; }
  function xx(q,a,b,x,s,t) {
    a = add(add(a, q), add(x, t));
    return add((a << s) | (a >>> (32 - s)), b);
  }
  function ff(a,b,c,d,x,s,t) {
    return xx((b & c) | ((~b) & d), a, b, x, s, t);
  }
  function gg(a,b,c,d,x,s,t) {
    return xx((b & d) | (c & (~d)), a, b, x, s, t);
  }
  function hh(a,b,c,d,x,s,t) { return xx(b ^ c ^ d, a, b, x, s, t); }
  function ii(a,b,c,d,x,s,t) { return xx(c ^ (b | (~d)), a, b, x, s, t); }
  function md5(buffer, length) {
    var i = length;
    buffer[i >> 2] |= 0x80 << (8 * (i & 3));
    for (i = (i & ~0x3) + 4;(i & 0x3F) < 56 ;i += 4)
      buffer[i >> 2] = 0;
    buffer[i >> 2] = length << 3;
    i += 4;
    buffer[i >> 2] = (length >> 29) & 0x1FFFFFFF;
    var w = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476];
    for(i = 0; i < buffer.length; i += 16) {
      var a = w[0], b = w[1], c = w[2], d = w[3];
      a = ff(a, b, c, d, buffer[i+ 0], 7, 0xD76AA478);
      d = ff(d, a, b, c, buffer[i+ 1], 12, 0xE8C7B756);
      c = ff(c, d, a, b, buffer[i+ 2], 17, 0x242070DB);
      b = ff(b, c, d, a, buffer[i+ 3], 22, 0xC1BDCEEE);
      a = ff(a, b, c, d, buffer[i+ 4], 7, 0xF57C0FAF);
      d = ff(d, a, b, c, buffer[i+ 5], 12, 0x4787C62A);
      c = ff(c, d, a, b, buffer[i+ 6], 17, 0xA8304613);
      b = ff(b, c, d, a, buffer[i+ 7], 22, 0xFD469501);
      a = ff(a, b, c, d, buffer[i+ 8], 7, 0x698098D8);
      d = ff(d, a, b, c, buffer[i+ 9], 12, 0x8B44F7AF);
      c = ff(c, d, a, b, buffer[i+10], 17, 0xFFFF5BB1);
      b = ff(b, c, d, a, buffer[i+11], 22, 0x895CD7BE);
      a = ff(a, b, c, d, buffer[i+12], 7, 0x6B901122);
      d = ff(d, a, b, c, buffer[i+13], 12, 0xFD987193);
      c = ff(c, d, a, b, buffer[i+14], 17, 0xA679438E);
      b = ff(b, c, d, a, buffer[i+15], 22, 0x49B40821);
      a = gg(a, b, c, d, buffer[i+ 1], 5, 0xF61E2562);
      d = gg(d, a, b, c, buffer[i+ 6], 9, 0xC040B340);
      c = gg(c, d, a, b, buffer[i+11], 14, 0x265E5A51);
      b = gg(b, c, d, a, buffer[i+ 0], 20, 0xE9B6C7AA);
      a = gg(a, b, c, d, buffer[i+ 5], 5, 0xD62F105D);
      d = gg(d, a, b, c, buffer[i+10], 9, 0x02441453);
      c = gg(c, d, a, b, buffer[i+15], 14, 0xD8A1E681);
      b = gg(b, c, d, a, buffer[i+ 4], 20, 0xE7D3FBC8);
      a = gg(a, b, c, d, buffer[i+ 9], 5, 0x21E1CDE6);
      d = gg(d, a, b, c, buffer[i+14], 9, 0xC33707D6);
      c = gg(c, d, a, b, buffer[i+ 3], 14, 0xF4D50D87);
      b = gg(b, c, d, a, buffer[i+ 8], 20, 0x455A14ED);
      a = gg(a, b, c, d, buffer[i+13], 5, 0xA9E3E905);
      d = gg(d, a, b, c, buffer[i+ 2], 9, 0xFCEFA3F8);
      c = gg(c, d, a, b, buffer[i+ 7], 14, 0x676F02D9);
      b = gg(b, c, d, a, buffer[i+12], 20, 0x8D2A4C8A);
      a = hh(a, b, c, d, buffer[i+ 5], 4, 0xFFFA3942);
      d = hh(d, a, b, c, buffer[i+ 8], 11, 0x8771F681);
      c = hh(c, d, a, b, buffer[i+11], 16, 0x6D9D6122);
      b = hh(b, c, d, a, buffer[i+14], 23, 0xFDE5380C);
      a = hh(a, b, c, d, buffer[i+ 1], 4, 0xA4BEEA44);
      d = hh(d, a, b, c, buffer[i+ 4], 11, 0x4BDECFA9);
      c = hh(c, d, a, b, buffer[i+ 7], 16, 0xF6BB4B60);
      b = hh(b, c, d, a, buffer[i+10], 23, 0xBEBFBC70);
      a = hh(a, b, c, d, buffer[i+13], 4, 0x289B7EC6);
      d = hh(d, a, b, c, buffer[i+ 0], 11, 0xEAA127FA);
      c = hh(c, d, a, b, buffer[i+ 3], 16, 0xD4EF3085);
      b = hh(b, c, d, a, buffer[i+ 6], 23, 0x04881D05);
      a = hh(a, b, c, d, buffer[i+ 9], 4, 0xD9D4D039);
      d = hh(d, a, b, c, buffer[i+12], 11, 0xE6DB99E5);
      c = hh(c, d, a, b, buffer[i+15], 16, 0x1FA27CF8);
      b = hh(b, c, d, a, buffer[i+ 2], 23, 0xC4AC5665);
      a = ii(a, b, c, d, buffer[i+ 0], 6, 0xF4292244);
      d = ii(d, a, b, c, buffer[i+ 7], 10, 0x432AFF97);
      c = ii(c, d, a, b, buffer[i+14], 15, 0xAB9423A7);
      b = ii(b, c, d, a, buffer[i+ 5], 21, 0xFC93A039);
      a = ii(a, b, c, d, buffer[i+12], 6, 0x655B59C3);
      d = ii(d, a, b, c, buffer[i+ 3], 10, 0x8F0CCC92);
      c = ii(c, d, a, b, buffer[i+10], 15, 0xFFEFF47D);
      b = ii(b, c, d, a, buffer[i+ 1], 21, 0x85845DD1);
      a = ii(a, b, c, d, buffer[i+ 8], 6, 0x6FA87E4F);
      d = ii(d, a, b, c, buffer[i+15], 10, 0xFE2CE6E0);
      c = ii(c, d, a, b, buffer[i+ 6], 15, 0xA3014314);
      b = ii(b, c, d, a, buffer[i+13], 21, 0x4E0811A1);
      a = ii(a, b, c, d, buffer[i+ 4], 6, 0xF7537E82);
      d = ii(d, a, b, c, buffer[i+11], 10, 0xBD3AF235);
      c = ii(c, d, a, b, buffer[i+ 2], 15, 0x2AD7D2BB);
      b = ii(b, c, d, a, buffer[i+ 9], 21, 0xEB86D391);
      w[0] = add(a, w[0]);
      w[1] = add(b, w[1]);
      w[2] = add(c, w[2]);
      w[3] = add(d, w[3]);
    }
    var t = [];
    for (var i = 0; i < 4; i++)
      for (var j = 0; j < 4; j++)
        t[i * 4 + j] = (w[i] >> (8 * j)) & 0xFF;
    return t;
  }
  return function (s, ofs, len) {
    var buf = [];
    if (s.array) {
      var a = s.array;
      for (var i = 0; i < len; i+=4) {
        var j = i + ofs;
        buf[i>>2] = a[j] | (a[j+1] << 8) | (a[j+2] << 16) | (a[j+3] << 24);
      }
      for (; i < len; i++) buf[i>>2] |= a[i + ofs] << (8 * (i & 3));
    } else {
      var b = s.getFullBytes();
      for (var i = 0; i < len; i+=4) {
        var j = i + ofs;
        buf[i>>2] =
          b.charCodeAt(j) | (b.charCodeAt(j+1) << 8) |
          (b.charCodeAt(j+2) << 16) | (b.charCodeAt(j+3) << 24);
      }
      for (; i < len; i++) buf[i>>2] |= b.charCodeAt(i + ofs) << (8 * (i & 3));
    }
    return new MlStringFromArray(md5(buf, len));
  }
} ();
function caml_ml_out_channels_list () { return 0; }
function caml_raise_constant (tag) { throw [0, tag]; }
function caml_raise_zero_divide () {
  caml_raise_constant(caml_global_data[6]);
}
function caml_mod(x,y) {
  if (y == 0) caml_raise_zero_divide ();
  return x%y;
}
function caml_mul(x,y) {
  return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0;
}
function caml_notequal (x, y) { return +(caml_compare_val(x,y,false) != 0); }
function caml_obj_is_block (x) { return +(x instanceof Array); }
function caml_obj_tag (x) { return (x instanceof Array)?x[0]:1000; }
function caml_register_global (n, v) { caml_global_data[n + 1] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_string_equal(s1, s2) {
  var b1 = s1.fullBytes;
  var b2 = s2.fullBytes;
  if (b1 != null && b2 != null) return (b1 == b2)?1:0;
  return (s1.getFullBytes () == s2.getFullBytes ())?1:0;
}
function caml_string_notequal(s1, s2) { return 1-caml_string_equal(s1, s2); }
function caml_sys_get_config () {
  return [0, new MlWrappedString("Unix"), 32];
}
function caml_update_dummy (x, y) {
  if( typeof y==="function" ) { x.fun = y; return 0; }
  if( y.fun ) { x.fun = y.fun; return 0; }
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}
(function()
   {function _jw_(_vn_,_vo_,_vp_,_vq_,_vr_,_vs_,_vt_)
     {return _vn_.length==6
              ?_vn_(_vo_,_vp_,_vq_,_vr_,_vs_,_vt_)
              :caml_call_gen(_vn_,[_vo_,_vp_,_vq_,_vr_,_vs_,_vt_]);}
    function _fk_(_vj_,_vk_,_vl_,_vm_)
     {return _vj_.length==3
              ?_vj_(_vk_,_vl_,_vm_)
              :caml_call_gen(_vj_,[_vk_,_vl_,_vm_]);}
    function _fP_(_vg_,_vh_,_vi_)
     {return _vg_.length==2?_vg_(_vh_,_vi_):caml_call_gen(_vg_,[_vh_,_vi_]);}
    function _cQ_(_ve_,_vf_)
     {return _ve_.length==1?_ve_(_vf_):caml_call_gen(_ve_,[_vf_]);}
    var
     _a_=[0,new MlString("Failure")],
     _b_=[0,new MlString("Invalid_argument")],
     _c_=[0,new MlString("Not_found")],
     _d_=[0,new MlString("Assert_failure")],
     _e_=new MlString("File \"%s\", line %d, characters %d-%d: %s"),
     _f_=new MlString("monkey.model"),
     _g_=new MlString("canvas");
    caml_register_global(5,[0,new MlString("Division_by_zero")]);
    caml_register_global(3,_b_);
    caml_register_global(2,_a_);
    var
     _cm_=[0,new MlString("Out_of_memory")],
     _cl_=[0,new MlString("Match_failure")],
     _ck_=[0,new MlString("Stack_overflow")],
     _cj_=new MlString("%.12g"),
     _ci_=new MlString("."),
     _ch_=new MlString("%d"),
     _cg_=new MlString("true"),
     _cf_=new MlString("false"),
     _ce_=new MlString("Pervasives.do_at_exit"),
     _cd_=new MlString("\\b"),
     _cc_=new MlString("\\t"),
     _cb_=new MlString("\\n"),
     _ca_=new MlString("\\r"),
     _b$_=new MlString("\\\\"),
     _b__=new MlString("\\'"),
     _b9_=new MlString(""),
     _b8_=new MlString("String.blit"),
     _b7_=new MlString("String.sub"),
     _b6_=new MlString("Queue.Empty"),
     _b5_=new MlString("Buffer.add: cannot grow buffer"),
     _b4_=new MlString("%"),
     _b3_=new MlString(""),
     _b2_=new MlString(""),
     _b1_=new MlString("\""),
     _b0_=new MlString("\""),
     _bZ_=new MlString("'"),
     _bY_=new MlString("'"),
     _bX_=new MlString("."),
     _bW_=new MlString("printf: bad positional specification (0)."),
     _bV_=new MlString("%_"),
     _bU_=[0,new MlString("printf.ml"),144,8],
     _bT_=new MlString("''"),
     _bS_=new MlString("Printf: premature end of format string ``"),
     _bR_=new MlString("''"),
     _bQ_=new MlString(" in format string ``"),
     _bP_=new MlString(", at char number "),
     _bO_=new MlString("Printf: bad conversion %"),
     _bN_=new MlString("Sformat.index_of_int: negative argument "),
     _bM_=new MlString(""),
     _bL_=new MlString(", %s%s"),
     _bK_=new MlString("Out of memory"),
     _bJ_=new MlString("Stack overflow"),
     _bI_=new MlString("Pattern matching failed"),
     _bH_=new MlString("Assertion failed"),
     _bG_=new MlString("(%s%s)"),
     _bF_=new MlString(""),
     _bE_=new MlString(""),
     _bD_=new MlString("(%s)"),
     _bC_=new MlString("%d"),
     _bB_=new MlString("%S"),
     _bA_=new MlString("_"),
     _bz_=new MlString("x"),
     _by_=[0,new MlString("src/core/lwt.ml"),483,20],
     _bx_=[0,new MlString("src/core/lwt.ml"),486,8],
     _bw_=[0,new MlString("src/core/lwt.ml"),461,20],
     _bv_=[0,new MlString("src/core/lwt.ml"),464,8],
     _bu_=[0,new MlString("src/core/lwt.ml"),440,20],
     _bt_=[0,new MlString("src/core/lwt.ml"),443,8],
     _bs_=new MlString("Lwt.fast_connect"),
     _br_=new MlString("Lwt.connect"),
     _bq_=new MlString("Lwt.wakeup_exn"),
     _bp_=new MlString("Lwt.wakeup"),
     _bo_=new MlString("Lwt.Canceled"),
     _bn_=new MlString("script"),
     _bm_=new MlString("canvas"),
     _bl_=new MlString("\\$&"),
     _bk_=new MlString("$$$$"),
     _bj_=new MlString("g"),
     _bi_=new MlString("g"),
     _bh_=new MlString("[$]"),
     _bg_=new MlString("[\\][()\\\\|+*.?{}^$]"),
     _bf_=[0,new MlString(""),0],
     _be_=new MlString(""),
     _bd_=new MlString(""),
     _bc_=new MlString("#"),
     _bb_=new MlString(""),
     _ba_=new MlString("?"),
     _a$_=new MlString(""),
     _a__=new MlString("/"),
     _a9_=new MlString("/"),
     _a8_=new MlString(":"),
     _a7_=new MlString(""),
     _a6_=new MlString("http://"),
     _a5_=new MlString(""),
     _a4_=new MlString("#"),
     _a3_=new MlString(""),
     _a2_=new MlString("?"),
     _a1_=new MlString(""),
     _a0_=new MlString("/"),
     _aZ_=new MlString("/"),
     _aY_=new MlString(":"),
     _aX_=new MlString(""),
     _aW_=new MlString("https://"),
     _aV_=new MlString(""),
     _aU_=new MlString("#"),
     _aT_=new MlString(""),
     _aS_=new MlString("?"),
     _aR_=new MlString(""),
     _aQ_=new MlString("/"),
     _aP_=new MlString("file://"),
     _aO_=new MlString(""),
     _aN_=new MlString(""),
     _aM_=new MlString(""),
     _aL_=new MlString(""),
     _aK_=new MlString(""),
     _aJ_=new MlString(""),
     _aI_=new MlString("="),
     _aH_=new MlString("&"),
     _aG_=new MlString("file"),
     _aF_=new MlString("file:"),
     _aE_=new MlString("http"),
     _aD_=new MlString("http:"),
     _aC_=new MlString("https"),
     _aB_=new MlString("https:"),
     _aA_=new MlString("%2B"),
     _az_=new MlString("Url.Local_exn"),
     _ay_=new MlString("+"),
     _ax_=new MlString("Url.Not_an_http_protocol"),
     _aw_=
      new
       MlString
       ("^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?/([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),
     _av_=
      new MlString("^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#])*)?(#(.*))?$"),
     _au_=new MlString("browser can't read file: unimplemented"),
     _at_=new MlString("utf8"),
     _as_=[0,new MlString("file.ml"),132,15],
     _ar_=new MlString("string"),
     _aq_=new MlString("can't retrieve file name: not implemented"),
     _ap_=new MlString(""),
     _ao_=new MlString("POST"),
     _an_=new MlString("multipart/form-data; boundary="),
     _am_=new MlString("POST"),
     _al_=
      [0,
       new MlString("POST"),
       [0,new MlString("application/x-www-form-urlencoded")],
       126925477],
     _ak_=[0,new MlString("POST"),0,126925477],
     _aj_=new MlString("GET"),
     _ai_=new MlString("?"),
     _ah_=new MlString("Content-type"),
     _ag_=new MlString("="),
     _af_=new MlString("="),
     _ae_=new MlString("&"),
     _ad_=new MlString("Content-Type: application/octet-stream\r\n"),
     _ac_=new MlString("\"\r\n"),
     _ab_=new MlString("\"; filename=\""),
     _aa_=new MlString("Content-Disposition: form-data; name=\""),
     _$_=new MlString("\r\n"),
     ___=new MlString("\r\n"),
     _Z_=new MlString("\r\n"),
     _Y_=new MlString("--"),
     _X_=new MlString("\r\n"),
     _W_=new MlString("\"\r\n\r\n"),
     _V_=new MlString("Content-Disposition: form-data; name=\""),
     _U_=new MlString("--\r\n"),
     _T_=new MlString("--"),
     _S_=new MlString("js_of_ocaml-------------------"),
     _R_=new MlString("Msxml2.XMLHTTP"),
     _Q_=new MlString("Msxml3.XMLHTTP"),
     _P_=new MlString("Microsoft.XMLHTTP"),
     _O_=[0,new MlString("xmlHttpRequest.ml"),79,2],
     _N_=new MlString("XmlHttpRequest.Wrong_headers"),
     _M_=new MlString("webgl"),
     _L_=new MlString("experimental-webgl"),
     _K_=new MlString("uncaught exception: %s"),
     _J_=new MlString("%.1f"),
     _I_=new MlString("loading"),
     _H_=new MlString("fps"),
     _G_=new MlString("fragment-shader"),
     _F_=new MlString("vertex-shader"),
     _E_=new MlString("program loaded"),
     _D_=new MlString("u_proj"),
     _C_=new MlString("u_lightPos"),
     _B_=new MlString("u_ambientLight"),
     _A_=new MlString("a_position"),
     _z_=new MlString("a_normal"),
     _y_=new MlString("ready"),
     _x_=new MlString("\n"),
     _w_=[0,1,[0,2,[0,3,[0,4,0]]]],
     _v_=new MlString("f"),
     _u_=new MlString("v"),
     _t_=new MlString("vn"),
     _s_=[0,1,[0,2,0]],
     _r_=new MlString("can't find script element %s"),
     _q_=new MlString("Unable to link the shader program."),
     _p_=new MlString("An error occurred compiling the shaders: \n%s\n%s"),
     _o_=new MlString("can't initialise webgl context"),
     _n_=new MlString("can't find canvas element %s"),
     _m_=new MlString("WebGL error"),
     _l_=new MlString("(v|vn|f)\\ ([^\\ ]+)\\ ([^\\ ]+)\\ ([^\\ ]+)"),
     _k_=new MlString("([0-9]+)//([0-9]+)");
    function _j_(s_h_){throw [0,_a_,s_h_];}
    function _cn_(s_i_){throw [0,_b_,s_i_];}
    function _cy_(s1_co_,s2_cq_)
     {var
       l1_cp_=s1_co_.getLen(),
       l2_cr_=s2_cq_.getLen(),
       s_cs_=caml_create_string(l1_cp_+l2_cr_|0);
      caml_blit_string(s1_co_,0,s_cs_,0,l1_cp_);
      caml_blit_string(s2_cq_,0,s_cs_,l1_cp_,l2_cr_);
      return s_cs_;}
    function string_of_int_cL_(n_ct_){return caml_format_int(_ch_,n_ct_);}
    function string_of_float_cM_(f_cu_)
     {var _cv_=caml_format_float(_cj_,f_cu_),i_cw_=0,l_cx_=_cv_.getLen();
      for(;;)
       {if(l_cx_<=i_cw_)
         var _cz_=_cy_(_cv_,_ci_);
        else
         {var
           _cA_=_cv_.safeGet(i_cw_),
           _cB_=48<=_cA_?58<=_cA_?0:1:45===_cA_?1:0;
          if(_cB_){var _cC_=i_cw_+1|0,i_cw_=_cC_;continue;}
          var _cz_=_cv_;}
        return _cz_;}}
    function _cE_(l1_cD_,l2_cF_)
     {if(l1_cD_)
       {var hd_cG_=l1_cD_[1];return [0,hd_cG_,_cE_(l1_cD_[2],l2_cF_)];}
      return l2_cF_;}
    function do_at_exit_cN_(param_cK_)
     {var param_cH_=caml_ml_out_channels_list(0);
      for(;;)
       {if(param_cH_)
         {var l_cI_=param_cH_[2];
          try {}catch(_cJ_){}
          var param_cH_=l_cI_;
          continue;}
        return 0;}}
    caml_register_named_value(_ce_,do_at_exit_cN_);
    function _db_(l_cO_,f_cP_)
     {if(0===l_cO_)return [0];
      var res_cR_=caml_make_vect(l_cO_,_cQ_(f_cP_,0)),_cS_=1,_cT_=l_cO_-1|0;
      if(!(_cT_<_cS_))
       {var i_cU_=_cS_;
        for(;;)
         {res_cR_[i_cU_+1]=_cQ_(f_cP_,i_cU_);
          var _cV_=i_cU_+1|0;
          if(_cT_!==i_cU_){var i_cU_=_cV_;continue;}
          break;}}
      return res_cR_;}
    function _dc_(a_cW_)
     {var i_cX_=a_cW_.length-1-1|0,res_cY_=0;
      for(;;)
       {if(0<=i_cX_)
         {var
           _c0_=[0,a_cW_[i_cX_+1],res_cY_],
           _cZ_=i_cX_-1|0,
           i_cX_=_cZ_,
           res_cY_=_c0_;
          continue;}
        return res_cY_;}}
    function _dd_(l_c1_)
     {if(l_c1_)
       {var accu_c2_=0,param_c3_=l_c1_,tl_c9_=l_c1_[2],hd_c6_=l_c1_[1];
        for(;;)
         {if(param_c3_)
           {var
             t_c5_=param_c3_[2],
             _c4_=accu_c2_+1|0,
             accu_c2_=_c4_,
             param_c3_=t_c5_;
            continue;}
          var a_c7_=caml_make_vect(accu_c2_,hd_c6_),i_c8_=1,param_c__=tl_c9_;
          for(;;)
           {if(param_c__)
             {var tl_c$_=param_c__[2];
              a_c7_[i_c8_+1]=param_c__[1];
              var _da_=i_c8_+1|0,i_c8_=_da_,param_c__=tl_c$_;
              continue;}
            return a_c7_;}}}
      return [0];}
    function _dv_(l_de_)
     {var l1_df_=l_de_,l2_dg_=0;
      for(;;)
       {if(l1_df_)
         {var
           l_dh_=l1_df_[2],
           _di_=[0,l1_df_[1],l2_dg_],
           l1_df_=l_dh_,
           l2_dg_=_di_;
          continue;}
        return l2_dg_;}}
    function _dk_(param_dj_)
     {if(param_dj_)
       {var l_dl_=param_dj_[1];return _cE_(l_dl_,_dk_(param_dj_[2]));}
      return 0;}
    function _dp_(f_dn_,param_dm_)
     {if(param_dm_)
       {var l_do_=param_dm_[2],r_dq_=_cQ_(f_dn_,param_dm_[1]);
        return [0,r_dq_,_dp_(f_dn_,l_do_)];}
      return 0;}
    function _dw_(f_dt_,param_dr_)
     {var param_ds_=param_dr_;
      for(;;)
       {if(param_ds_)
         {var l_du_=param_ds_[2];
          _cQ_(f_dt_,param_ds_[1]);
          var param_ds_=l_du_;
          continue;}
        return 0;}}
    function _dT_(n_dx_,c_dz_)
     {var s_dy_=caml_create_string(n_dx_);
      caml_fill_string(s_dy_,0,n_dx_,c_dz_);
      return s_dy_;}
    function _dU_(s_dC_,ofs_dA_,len_dB_)
     {if(0<=ofs_dA_&&0<=len_dB_&&!((s_dC_.getLen()-len_dB_|0)<ofs_dA_))
       {var r_dD_=caml_create_string(len_dB_);
        caml_blit_string(s_dC_,ofs_dA_,r_dD_,0,len_dB_);
        return r_dD_;}
      return _cn_(_b7_);}
    function _dV_(s1_dG_,ofs1_dF_,s2_dI_,ofs2_dH_,len_dE_)
     {if
       (0<=
        len_dE_&&
        0<=
        ofs1_dF_&&
        !((s1_dG_.getLen()-len_dE_|0)<ofs1_dF_)&&
        0<=
        ofs2_dH_&&
        !((s2_dI_.getLen()-len_dE_|0)<ofs2_dH_))
       return caml_blit_string(s1_dG_,ofs1_dF_,s2_dI_,ofs2_dH_,len_dE_);
      return _cn_(_b8_);}
    function _dW_(sep_dP_,l_dJ_)
     {if(l_dJ_)
       {var hd_dK_=l_dJ_[1],num_dL_=[0,0],len_dM_=[0,0],tl_dO_=l_dJ_[2];
        _dw_
         (function(s_dN_)
           {num_dL_[1]+=1;len_dM_[1]=len_dM_[1]+s_dN_.getLen()|0;return 0;},
          l_dJ_);
        var
         r_dQ_=
          caml_create_string
           (len_dM_[1]+caml_mul(sep_dP_.getLen(),num_dL_[1]-1|0)|0);
        caml_blit_string(hd_dK_,0,r_dQ_,0,hd_dK_.getLen());
        var pos_dR_=[0,hd_dK_.getLen()];
        _dw_
         (function(s_dS_)
           {caml_blit_string(sep_dP_,0,r_dQ_,pos_dR_[1],sep_dP_.getLen());
            pos_dR_[1]=pos_dR_[1]+sep_dP_.getLen()|0;
            caml_blit_string(s_dS_,0,r_dQ_,pos_dR_[1],s_dS_.getLen());
            pos_dR_[1]=pos_dR_[1]+s_dS_.getLen()|0;
            return 0;},
          tl_dO_);
        return r_dQ_;}
      return _b9_;}
    var
     _dX_=caml_sys_get_config(0)[2],
     _dY_=caml_mul(_dX_/8|0,(1<<(_dX_-10|0))-1|0)-1|0,
     _d3_=252,
     _d2_=253,
     _d1_=[0,_b6_];
    function _d0_(q_dZ_){return q_dZ_[1];}
    function _ej_(n_d4_)
     {var
       n_d5_=1<=n_d4_?n_d4_:1,
       n_d6_=_dY_<n_d5_?_dY_:n_d5_,
       s_d7_=caml_create_string(n_d6_);
      return [0,s_d7_,0,n_d6_,s_d7_];}
    function _ek_(b_d8_){return _dU_(b_d8_[1],0,b_d8_[2]);}
    function _ed_(b_d9_,more_d$_)
     {var new_len_d__=[0,b_d9_[3]];
      for(;;)
       {if(new_len_d__[1]<(b_d9_[2]+more_d$_|0))
         {new_len_d__[1]=2*new_len_d__[1]|0;continue;}
        if(_dY_<new_len_d__[1])
         if((b_d9_[2]+more_d$_|0)<=_dY_)new_len_d__[1]=_dY_;else _j_(_b5_);
        var new_buffer_ea_=caml_create_string(new_len_d__[1]);
        _dV_(b_d9_[1],0,new_buffer_ea_,0,b_d9_[2]);
        b_d9_[1]=new_buffer_ea_;
        b_d9_[3]=new_len_d__[1];
        return 0;}}
    function _el_(b_eb_,c_ee_)
     {var pos_ec_=b_eb_[2];
      if(b_eb_[3]<=pos_ec_)_ed_(b_eb_,1);
      b_eb_[1].safeSet(pos_ec_,c_ee_);
      b_eb_[2]=pos_ec_+1|0;
      return 0;}
    function _em_(b_eh_,s_ef_)
     {var len_eg_=s_ef_.getLen(),new_position_ei_=b_eh_[2]+len_eg_|0;
      if(b_eh_[3]<new_position_ei_)_ed_(b_eh_,len_eg_);
      _dV_(s_ef_,0,b_eh_[1],b_eh_[2],len_eg_);
      b_eh_[2]=new_position_ei_;
      return 0;}
    function index_of_int_eq_(i_en_)
     {return 0<=i_en_?i_en_:_j_(_cy_(_bN_,string_of_int_cL_(i_en_)));}
    function add_int_index_er_(i_eo_,idx_ep_)
     {return index_of_int_eq_(i_eo_+idx_ep_|0);}
    var _es_=_cQ_(add_int_index_er_,1);
    function _ez_(fmt_et_){return _dU_(fmt_et_,0,fmt_et_.getLen());}
    function bad_conversion_eB_(sfmt_eu_,i_ev_,c_ex_)
     {var
       _ew_=_cy_(_bQ_,_cy_(sfmt_eu_,_bR_)),
       _ey_=_cy_(_bP_,_cy_(string_of_int_cL_(i_ev_),_ew_));
      return _cn_(_cy_(_bO_,_cy_(_dT_(1,c_ex_),_ey_)));}
    function bad_conversion_format_fq_(fmt_eA_,i_eD_,c_eC_)
     {return bad_conversion_eB_(_ez_(fmt_eA_),i_eD_,c_eC_);}
    function incomplete_format_fr_(fmt_eE_)
     {return _cn_(_cy_(_bS_,_cy_(_ez_(fmt_eE_),_bT_)));}
    function extract_format_eY_(fmt_eF_,start_eN_,stop_eP_,widths_eR_)
     {function skip_positional_spec_eM_(start_eG_)
       {if
         ((fmt_eF_.safeGet(start_eG_)-48|0)<
          0||
          9<
          (fmt_eF_.safeGet(start_eG_)-48|0))
         return start_eG_;
        var i_eH_=start_eG_+1|0;
        for(;;)
         {var _eI_=fmt_eF_.safeGet(i_eH_);
          if(48<=_eI_)
           {if(!(58<=_eI_)){var _eK_=i_eH_+1|0,i_eH_=_eK_;continue;}
            var _eJ_=0;}
          else
           if(36===_eI_){var _eL_=i_eH_+1|0,_eJ_=1;}else var _eJ_=0;
          if(!_eJ_)var _eL_=start_eG_;
          return _eL_;}}
      var
       start_eO_=skip_positional_spec_eM_(start_eN_+1|0),
       b_eQ_=_ej_((stop_eP_-start_eO_|0)+10|0);
      _el_(b_eQ_,37);
      var i_eS_=start_eO_,widths_eT_=_dv_(widths_eR_);
      for(;;)
       {if(i_eS_<=stop_eP_)
         {var _eU_=fmt_eF_.safeGet(i_eS_);
          if(42===_eU_)
           {if(widths_eT_)
             {var t_eV_=widths_eT_[2];
              _em_(b_eQ_,string_of_int_cL_(widths_eT_[1]));
              var
               i_eW_=skip_positional_spec_eM_(i_eS_+1|0),
               i_eS_=i_eW_,
               widths_eT_=t_eV_;
              continue;}
            throw [0,_d_,_bU_];}
          _el_(b_eQ_,_eU_);
          var _eX_=i_eS_+1|0,i_eS_=_eX_;
          continue;}
        return _ek_(b_eQ_);}}
    function extract_format_int_gR_
     (conv_e4_,fmt_e2_,start_e1_,stop_e0_,widths_eZ_)
     {var sfmt_e3_=extract_format_eY_(fmt_e2_,start_e1_,stop_e0_,widths_eZ_);
      if(78!==conv_e4_&&110!==conv_e4_)return sfmt_e3_;
      sfmt_e3_.safeSet(sfmt_e3_.getLen()-1|0,117);
      return sfmt_e3_;}
    function sub_format_fs_
     (incomplete_format_e$_,bad_conversion_format_fj_,conv_fo_,fmt_e5_,i_fn_)
     {var len_e6_=fmt_e5_.getLen();
      function sub_fmt_fl_(c_e7_,i_fi_)
       {var close_e8_=40===c_e7_?41:125;
        function sub_fh_(j_e9_)
         {var j_e__=j_e9_;
          for(;;)
           {if(len_e6_<=j_e__)return _cQ_(incomplete_format_e$_,fmt_e5_);
            if(37===fmt_e5_.safeGet(j_e__))
             {var _fa_=j_e__+1|0;
              if(len_e6_<=_fa_)
               var _fb_=_cQ_(incomplete_format_e$_,fmt_e5_);
              else
               {var _fc_=fmt_e5_.safeGet(_fa_),_fd_=_fc_-40|0;
                if(_fd_<0||1<_fd_)
                 {var _fe_=_fd_-83|0;
                  if(_fe_<0||2<_fe_)
                   var _ff_=1;
                  else
                   switch(_fe_)
                    {case 1:var _ff_=1;break;
                     case 2:var _fg_=1,_ff_=0;break;
                     default:var _fg_=0,_ff_=0;}
                  if(_ff_){var _fb_=sub_fh_(_fa_+1|0),_fg_=2;}}
                else
                 var _fg_=0===_fd_?0:1;
                switch(_fg_)
                 {case 1:
                   var
                    _fb_=
                     _fc_===close_e8_
                      ?_fa_+1|0
                      :_fk_(bad_conversion_format_fj_,fmt_e5_,i_fi_,_fc_);
                   break;
                  case 2:break;
                  default:var _fb_=sub_fh_(sub_fmt_fl_(_fc_,_fa_+1|0)+1|0);}}
              return _fb_;}
            var _fm_=j_e__+1|0,j_e__=_fm_;
            continue;}}
        return sub_fh_(i_fi_);}
      return sub_fmt_fl_(conv_fo_,i_fn_);}
    function sub_format_for_printf_fS_(conv_fp_)
     {return _fk_
              (sub_format_fs_,
               incomplete_format_fr_,
               bad_conversion_format_fq_,
               conv_fp_);}
    function iter_on_format_args_f8_(fmt_ft_,add_conv_fE_,add_char_fO_)
     {var lim_fu_=fmt_ft_.getLen()-1|0;
      function scan_fmt_fQ_(i_fv_)
       {var i_fw_=i_fv_;
        a:
        for(;;)
         {if(i_fw_<lim_fu_)
           {if(37===fmt_ft_.safeGet(i_fw_))
             {var skip_fx_=0,i_fy_=i_fw_+1|0;
              for(;;)
               {if(lim_fu_<i_fy_)
                 var _fz_=incomplete_format_fr_(fmt_ft_);
                else
                 {var _fA_=fmt_ft_.safeGet(i_fy_);
                  if(58<=_fA_)
                   {if(95===_fA_)
                     {var _fC_=i_fy_+1|0,_fB_=1,skip_fx_=_fB_,i_fy_=_fC_;
                      continue;}}
                  else
                   if(32<=_fA_)
                    switch(_fA_-32|0)
                     {case 1:
                      case 2:
                      case 4:
                      case 5:
                      case 6:
                      case 7:
                      case 8:
                      case 9:
                      case 12:
                      case 15:break;
                      case 0:
                      case 3:
                      case 11:
                      case 13:var _fD_=i_fy_+1|0,i_fy_=_fD_;continue;
                      case 10:
                       var _fF_=_fk_(add_conv_fE_,skip_fx_,i_fy_,105),i_fy_=_fF_;
                       continue;
                      default:var _fG_=i_fy_+1|0,i_fy_=_fG_;continue;}
                  var i_fH_=i_fy_;
                  c:
                  for(;;)
                   {if(lim_fu_<i_fH_)
                     var _fI_=incomplete_format_fr_(fmt_ft_);
                    else
                     {var _fJ_=fmt_ft_.safeGet(i_fH_);
                      if(126<=_fJ_)
                       var _fK_=0;
                      else
                       switch(_fJ_)
                        {case 78:
                         case 88:
                         case 100:
                         case 105:
                         case 111:
                         case 117:
                         case 120:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,105),_fK_=1;break;
                         case 69:
                         case 70:
                         case 71:
                         case 101:
                         case 102:
                         case 103:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,102),_fK_=1;break;
                         case 33:
                         case 37:
                         case 44:var _fI_=i_fH_+1|0,_fK_=1;break;
                         case 83:
                         case 91:
                         case 115:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,115),_fK_=1;break;
                         case 97:
                         case 114:
                         case 116:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,_fJ_),_fK_=1;
                          break;
                         case 76:
                         case 108:
                         case 110:
                          var j_fL_=i_fH_+1|0;
                          if(lim_fu_<j_fL_)
                           {var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,105),_fK_=1;}
                          else
                           {var _fM_=fmt_ft_.safeGet(j_fL_)-88|0;
                            if(_fM_<0||32<_fM_)
                             var _fN_=1;
                            else
                             switch(_fM_)
                              {case 0:
                               case 12:
                               case 17:
                               case 23:
                               case 29:
                               case 32:
                                var
                                 _fI_=
                                  _fP_
                                   (add_char_fO_,_fk_(add_conv_fE_,skip_fx_,i_fH_,_fJ_),105),
                                 _fK_=1,
                                 _fN_=0;
                                break;
                               default:var _fN_=1;}
                            if(_fN_)
                             {var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,105),_fK_=1;}}
                          break;
                         case 67:
                         case 99:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,99),_fK_=1;break;
                         case 66:
                         case 98:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,66),_fK_=1;break;
                         case 41:
                         case 125:
                          var _fI_=_fk_(add_conv_fE_,skip_fx_,i_fH_,_fJ_),_fK_=1;
                          break;
                         case 40:
                          var
                           _fI_=scan_fmt_fQ_(_fk_(add_conv_fE_,skip_fx_,i_fH_,_fJ_)),
                           _fK_=1;
                          break;
                         case 123:
                          var
                           i_fR_=_fk_(add_conv_fE_,skip_fx_,i_fH_,_fJ_),
                           j_fT_=_fk_(sub_format_for_printf_fS_,_fJ_,fmt_ft_,i_fR_),
                           i_fU_=i_fR_;
                          for(;;)
                           {if(i_fU_<(j_fT_-2|0))
                             {var
                               _fV_=_fP_(add_char_fO_,i_fU_,fmt_ft_.safeGet(i_fU_)),
                               i_fU_=_fV_;
                              continue;}
                            var _fW_=j_fT_-1|0,i_fH_=_fW_;
                            continue c;}
                         default:var _fK_=0;}
                      if(!_fK_)
                       var _fI_=bad_conversion_format_fq_(fmt_ft_,i_fH_,_fJ_);}
                    var _fz_=_fI_;
                    break;}}
                var i_fw_=_fz_;
                continue a;}}
            var _fX_=i_fw_+1|0,i_fw_=_fX_;
            continue;}
          return i_fw_;}}
      scan_fmt_fQ_(0);
      return 0;}
    function count_arguments_of_format_h6_(fmt_f9_)
     {var ac_fY_=[0,0,0,0];
      function add_conv_f7_(skip_f3_,i_f4_,c_fZ_)
       {var _f0_=41!==c_fZ_?1:0,_f1_=_f0_?125!==c_fZ_?1:0:_f0_;
        if(_f1_)
         {var inc_f2_=97===c_fZ_?2:1;
          if(114===c_fZ_)ac_fY_[3]=ac_fY_[3]+1|0;
          if(skip_f3_)
           ac_fY_[2]=ac_fY_[2]+inc_f2_|0;
          else
           ac_fY_[1]=ac_fY_[1]+inc_f2_|0;}
        return i_f4_+1|0;}
      iter_on_format_args_f8_
       (fmt_f9_,add_conv_f7_,function(i_f5_,c_f6_){return i_f5_+1|0;});
      return ac_fY_[1];}
    function scan_positional_spec_gN_(fmt_f__,got_spec_gb_,n_gj_,i_f$_)
     {var _ga_=fmt_f__.safeGet(i_f$_);
      if((_ga_-48|0)<0||9<(_ga_-48|0))return _fP_(got_spec_gb_,0,i_f$_);
      var accu_gc_=_ga_-48|0,j_gd_=i_f$_+1|0;
      for(;;)
       {var _ge_=fmt_f__.safeGet(j_gd_);
        if(48<=_ge_)
         {if(!(58<=_ge_))
           {var
             _gh_=j_gd_+1|0,
             _gg_=(10*accu_gc_|0)+(_ge_-48|0)|0,
             accu_gc_=_gg_,
             j_gd_=_gh_;
            continue;}
          var _gf_=0;}
        else
         if(36===_ge_)
          if(0===accu_gc_)
           {var _gi_=_j_(_bW_),_gf_=1;}
          else
           {var
             _gi_=
              _fP_(got_spec_gb_,[0,index_of_int_eq_(accu_gc_-1|0)],j_gd_+1|0),
             _gf_=1;}
         else
          var _gf_=0;
        if(!_gf_)var _gi_=_fP_(got_spec_gb_,0,i_f$_);
        return _gi_;}}
    function next_index_gI_(spec_gk_,n_gl_)
     {return spec_gk_?n_gl_:_cQ_(_es_,n_gl_);}
    function get_index_gx_(spec_gm_,n_gn_){return spec_gm_?spec_gm_[1]:n_gn_;}
    function _jv_
     (to_s_io_,get_out_gp_,outc_iA_,outs_ip_,flush_h$_,k_iG_,fmt_go_)
     {var out_gq_=_cQ_(get_out_gp_,fmt_go_);
      function pr_h__(k_gv_,n_iF_,fmt_gr_,v_gA_)
       {var len_gu_=fmt_gr_.getLen();
        function doprn_h7_(n_ix_,i_gs_)
         {var i_gt_=i_gs_;
          for(;;)
           {if(len_gu_<=i_gt_)return _cQ_(k_gv_,out_gq_);
            var _gw_=fmt_gr_.safeGet(i_gt_);
            if(37===_gw_)
             {var
               get_arg_gE_=
                function(spec_gz_,n_gy_)
                 {return caml_array_get(v_gA_,get_index_gx_(spec_gz_,n_gy_));},
               scan_flags_gK_=
                function(spec_gM_,n_gF_,widths_gH_,i_gB_)
                 {var i_gC_=i_gB_;
                  for(;;)
                   {var _gD_=fmt_gr_.safeGet(i_gC_)-32|0;
                    if(!(_gD_<0||25<_gD_))
                     switch(_gD_)
                      {case 1:
                       case 2:
                       case 4:
                       case 5:
                       case 6:
                       case 7:
                       case 8:
                       case 9:
                       case 12:
                       case 15:break;
                       case 10:
                        return scan_positional_spec_gN_
                                (fmt_gr_,
                                 function(wspec_gG_,i_gL_)
                                  {var _gJ_=[0,get_arg_gE_(wspec_gG_,n_gF_),widths_gH_];
                                   return scan_flags_gK_
                                           (spec_gM_,next_index_gI_(wspec_gG_,n_gF_),_gJ_,i_gL_);},
                                 n_gF_,
                                 i_gC_+1|0);
                       default:var _gO_=i_gC_+1|0,i_gC_=_gO_;continue;}
                    var _gP_=fmt_gr_.safeGet(i_gC_);
                    if(124<=_gP_)
                     var _gQ_=0;
                    else
                     switch(_gP_)
                      {case 78:
                       case 88:
                       case 100:
                       case 105:
                       case 111:
                       case 117:
                       case 120:
                        var
                         x_gS_=get_arg_gE_(spec_gM_,n_gF_),
                         s_gT_=
                          caml_format_int
                           (extract_format_int_gR_(_gP_,fmt_gr_,i_gt_,i_gC_,widths_gH_),
                            x_gS_),
                         _gV_=
                          cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_gT_,i_gC_+1|0),
                         _gQ_=1;
                        break;
                       case 69:
                       case 71:
                       case 101:
                       case 102:
                       case 103:
                        var
                         x_gW_=get_arg_gE_(spec_gM_,n_gF_),
                         s_gX_=
                          caml_format_float
                           (extract_format_eY_(fmt_gr_,i_gt_,i_gC_,widths_gH_),x_gW_),
                         _gV_=
                          cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_gX_,i_gC_+1|0),
                         _gQ_=1;
                        break;
                       case 76:
                       case 108:
                       case 110:
                        var _gY_=fmt_gr_.safeGet(i_gC_+1|0)-88|0;
                        if(_gY_<0||32<_gY_)
                         var _gZ_=1;
                        else
                         switch(_gY_)
                          {case 0:
                           case 12:
                           case 17:
                           case 23:
                           case 29:
                           case 32:
                            var i_g0_=i_gC_+1|0,_g1_=_gP_-108|0;
                            if(_g1_<0||2<_g1_)
                             var _g2_=0;
                            else
                             {switch(_g1_)
                               {case 1:var _g2_=0,_g3_=0;break;
                                case 2:
                                 var
                                  x_g4_=get_arg_gE_(spec_gM_,n_gF_),
                                  _g5_=
                                   caml_format_int
                                    (extract_format_eY_(fmt_gr_,i_gt_,i_g0_,widths_gH_),x_g4_),
                                  _g3_=1;
                                 break;
                                default:
                                 var
                                  x_g6_=get_arg_gE_(spec_gM_,n_gF_),
                                  _g5_=
                                   caml_format_int
                                    (extract_format_eY_(fmt_gr_,i_gt_,i_g0_,widths_gH_),x_g6_),
                                  _g3_=1;}
                              if(_g3_){var s_g7_=_g5_,_g2_=1;}}
                            if(!_g2_)
                             {var
                               x_g8_=get_arg_gE_(spec_gM_,n_gF_),
                               s_g7_=
                                caml_int64_format
                                 (extract_format_eY_(fmt_gr_,i_gt_,i_g0_,widths_gH_),x_g8_);}
                            var
                             _gV_=
                              cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_g7_,i_g0_+1|0),
                             _gQ_=1,
                             _gZ_=0;
                            break;
                           default:var _gZ_=1;}
                        if(_gZ_)
                         {var
                           x_g9_=get_arg_gE_(spec_gM_,n_gF_),
                           s_g__=
                            caml_format_int
                             (extract_format_int_gR_(110,fmt_gr_,i_gt_,i_gC_,widths_gH_),
                              x_g9_),
                           _gV_=
                            cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_g__,i_gC_+1|0),
                           _gQ_=1;}
                        break;
                       case 83:
                       case 115:
                        var x_g$_=get_arg_gE_(spec_gM_,n_gF_);
                        if(115===_gP_)
                         var x_ha_=x_g$_;
                        else
                         {var n_hb_=[0,0],_hc_=0,_hd_=x_g$_.getLen()-1|0;
                          if(!(_hd_<_hc_))
                           {var i_he_=_hc_;
                            for(;;)
                             {var
                               _hf_=x_g$_.safeGet(i_he_),
                               _hg_=
                                14<=_hf_
                                 ?34===_hf_?1:92===_hf_?1:0
                                 :11<=_hf_?13<=_hf_?1:0:8<=_hf_?1:0,
                               _hh_=_hg_?2:caml_is_printable(_hf_)?1:4;
                              n_hb_[1]=n_hb_[1]+_hh_|0;
                              var _hi_=i_he_+1|0;
                              if(_hd_!==i_he_){var i_he_=_hi_;continue;}
                              break;}}
                          if(n_hb_[1]===x_g$_.getLen())
                           var _hj_=x_g$_;
                          else
                           {var s__hk_=caml_create_string(n_hb_[1]);
                            n_hb_[1]=0;
                            var _hl_=0,_hm_=x_g$_.getLen()-1|0;
                            if(!(_hm_<_hl_))
                             {var i_hn_=_hl_;
                              for(;;)
                               {var _ho_=x_g$_.safeGet(i_hn_),_hp_=_ho_-34|0;
                                if(_hp_<0||58<_hp_)
                                 if(-20<=_hp_)
                                  var _hq_=1;
                                 else
                                  {switch(_hp_+34|0)
                                    {case 8:
                                      s__hk_.safeSet(n_hb_[1],92);
                                      n_hb_[1]+=1;
                                      s__hk_.safeSet(n_hb_[1],98);
                                      var _hr_=1;
                                      break;
                                     case 9:
                                      s__hk_.safeSet(n_hb_[1],92);
                                      n_hb_[1]+=1;
                                      s__hk_.safeSet(n_hb_[1],116);
                                      var _hr_=1;
                                      break;
                                     case 10:
                                      s__hk_.safeSet(n_hb_[1],92);
                                      n_hb_[1]+=1;
                                      s__hk_.safeSet(n_hb_[1],110);
                                      var _hr_=1;
                                      break;
                                     case 13:
                                      s__hk_.safeSet(n_hb_[1],92);
                                      n_hb_[1]+=1;
                                      s__hk_.safeSet(n_hb_[1],114);
                                      var _hr_=1;
                                      break;
                                     default:var _hq_=1,_hr_=0;}
                                   if(_hr_)var _hq_=0;}
                                else
                                 var
                                  _hq_=
                                   (_hp_-1|0)<0||56<(_hp_-1|0)
                                    ?(s__hk_.safeSet(n_hb_[1],92),
                                      n_hb_[1]+=
                                      1,
                                      s__hk_.safeSet(n_hb_[1],_ho_),
                                      0)
                                    :1;
                                if(_hq_)
                                 if(caml_is_printable(_ho_))
                                  s__hk_.safeSet(n_hb_[1],_ho_);
                                 else
                                  {s__hk_.safeSet(n_hb_[1],92);
                                   n_hb_[1]+=1;
                                   s__hk_.safeSet(n_hb_[1],48+(_ho_/100|0)|0);
                                   n_hb_[1]+=1;
                                   s__hk_.safeSet(n_hb_[1],48+((_ho_/10|0)%10|0)|0);
                                   n_hb_[1]+=1;
                                   s__hk_.safeSet(n_hb_[1],48+(_ho_%10|0)|0);}
                                n_hb_[1]+=1;
                                var _hs_=i_hn_+1|0;
                                if(_hm_!==i_hn_){var i_hn_=_hs_;continue;}
                                break;}}
                            var _hj_=s__hk_;}
                          var x_ha_=_cy_(_b0_,_cy_(_hj_,_b1_));}
                        if(i_gC_===(i_gt_+1|0))
                         var s_ht_=x_ha_;
                        else
                         {var
                           _hu_=
                            extract_format_eY_(fmt_gr_,i_gt_,i_gC_,widths_gH_);
                          try
                           {var neg_hv_=0,i_hw_=1;
                            for(;;)
                             {if(_hu_.getLen()<=i_hw_)
                               var _hx_=[0,0,neg_hv_];
                              else
                               {var _hy_=_hu_.safeGet(i_hw_);
                                if(49<=_hy_)
                                 if(58<=_hy_)
                                  var _hz_=0;
                                 else
                                  {var
                                    _hx_=
                                     [0,
                                      caml_int_of_string
                                       (_dU_(_hu_,i_hw_,(_hu_.getLen()-i_hw_|0)-1|0)),
                                      neg_hv_],
                                    _hz_=1;}
                                else
                                 {if(45===_hy_)
                                   {var _hB_=i_hw_+1|0,_hA_=1,neg_hv_=_hA_,i_hw_=_hB_;
                                    continue;}
                                  var _hz_=0;}
                                if(!_hz_){var _hC_=i_hw_+1|0,i_hw_=_hC_;continue;}}
                              var match_hD_=_hx_;
                              break;}}
                          catch(_hE_)
                           {if(_hE_[1]!==_a_)throw _hE_;
                            var match_hD_=bad_conversion_eB_(_hu_,0,115);}
                          var
                           p_hF_=match_hD_[1],
                           _hG_=x_ha_.getLen(),
                           _hH_=0,
                           neg_hL_=match_hD_[2],
                           _hK_=32;
                          if(p_hF_===_hG_&&0===_hH_)
                           {var _hI_=x_ha_,_hJ_=1;}
                          else
                           var _hJ_=0;
                          if(!_hJ_)
                           if(p_hF_<=_hG_)
                            var _hI_=_dU_(x_ha_,_hH_,_hG_);
                           else
                            {var res_hM_=_dT_(p_hF_,_hK_);
                             if(neg_hL_)
                              _dV_(x_ha_,_hH_,res_hM_,0,_hG_);
                             else
                              _dV_(x_ha_,_hH_,res_hM_,p_hF_-_hG_|0,_hG_);
                             var _hI_=res_hM_;}
                          var s_ht_=_hI_;}
                        var
                         _gV_=
                          cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_ht_,i_gC_+1|0),
                         _gQ_=1;
                        break;
                       case 67:
                       case 99:
                        var x_hN_=get_arg_gE_(spec_gM_,n_gF_);
                        if(99===_gP_)
                         var s_hO_=_dT_(1,x_hN_);
                        else
                         {if(39===x_hN_)
                           var _hP_=_b__;
                          else
                           if(92===x_hN_)
                            var _hP_=_b$_;
                           else
                            {if(14<=x_hN_)
                              var _hQ_=0;
                             else
                              switch(x_hN_)
                               {case 8:var _hP_=_cd_,_hQ_=1;break;
                                case 9:var _hP_=_cc_,_hQ_=1;break;
                                case 10:var _hP_=_cb_,_hQ_=1;break;
                                case 13:var _hP_=_ca_,_hQ_=1;break;
                                default:var _hQ_=0;}
                             if(!_hQ_)
                              if(caml_is_printable(x_hN_))
                               {var s_hR_=caml_create_string(1);
                                s_hR_.safeSet(0,x_hN_);
                                var _hP_=s_hR_;}
                              else
                               {var s_hS_=caml_create_string(4);
                                s_hS_.safeSet(0,92);
                                s_hS_.safeSet(1,48+(x_hN_/100|0)|0);
                                s_hS_.safeSet(2,48+((x_hN_/10|0)%10|0)|0);
                                s_hS_.safeSet(3,48+(x_hN_%10|0)|0);
                                var _hP_=s_hS_;}}
                          var s_hO_=_cy_(_bY_,_cy_(_hP_,_bZ_));}
                        var
                         _gV_=
                          cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_hO_,i_gC_+1|0),
                         _gQ_=1;
                        break;
                       case 66:
                       case 98:
                        var
                         _hU_=i_gC_+1|0,
                         _hT_=get_arg_gE_(spec_gM_,n_gF_)?_cg_:_cf_,
                         _gV_=cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),_hT_,_hU_),
                         _gQ_=1;
                        break;
                       case 40:
                       case 123:
                        var
                         xf_hV_=get_arg_gE_(spec_gM_,n_gF_),
                         j_hW_=_fk_(sub_format_for_printf_fS_,_gP_,fmt_gr_,i_gC_+1|0);
                        if(123===_gP_)
                         {var
                           b_hX_=_ej_(xf_hV_.getLen()),
                           add_char_h1_=
                            function(i_hZ_,c_hY_){_el_(b_hX_,c_hY_);return i_hZ_+1|0;};
                          iter_on_format_args_f8_
                           (xf_hV_,
                            function(skip_h0_,i_h3_,c_h2_)
                             {if(skip_h0_)_em_(b_hX_,_bV_);else _el_(b_hX_,37);
                              return add_char_h1_(i_h3_,c_h2_);},
                            add_char_h1_);
                          var
                           _h4_=_ek_(b_hX_),
                           _gV_=cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),_h4_,j_hW_),
                           _gQ_=1;}
                        else
                         {var
                           _h5_=next_index_gI_(spec_gM_,n_gF_),
                           m_h8_=
                            add_int_index_er_
                             (count_arguments_of_format_h6_(xf_hV_),_h5_),
                           _gV_=
                            pr_h__
                             (function(param_h9_){return doprn_h7_(m_h8_,j_hW_);},
                              _h5_,
                              xf_hV_,
                              v_gA_),
                           _gQ_=1;}
                        break;
                       case 33:
                        _cQ_(flush_h$_,out_gq_);
                        var _gV_=doprn_h7_(n_gF_,i_gC_+1|0),_gQ_=1;
                        break;
                       case 37:
                        var _gV_=cont_s_gU_(n_gF_,_b4_,i_gC_+1|0),_gQ_=1;break;
                       case 41:
                        var _gV_=cont_s_gU_(n_gF_,_b3_,i_gC_+1|0),_gQ_=1;break;
                       case 44:
                        var _gV_=cont_s_gU_(n_gF_,_b2_,i_gC_+1|0),_gQ_=1;break;
                       case 70:
                        var x_ia_=get_arg_gE_(spec_gM_,n_gF_);
                        if(0===widths_gH_)
                         var s_ib_=string_of_float_cM_(x_ia_);
                        else
                         {var
                           sfmt_ic_=
                            extract_format_eY_(fmt_gr_,i_gt_,i_gC_,widths_gH_);
                          if(70===_gP_)sfmt_ic_.safeSet(sfmt_ic_.getLen()-1|0,103);
                          var s_id_=caml_format_float(sfmt_ic_,x_ia_);
                          if(3<=caml_classify_float(x_ia_))
                           var _ie_=s_id_;
                          else
                           {var i_if_=0,l_ig_=s_id_.getLen();
                            for(;;)
                             {if(l_ig_<=i_if_)
                               var _ih_=_cy_(s_id_,_bX_);
                              else
                               {var
                                 _ii_=s_id_.safeGet(i_if_)-46|0,
                                 _ij_=
                                  _ii_<0||23<_ii_
                                   ?55===_ii_?1:0
                                   :(_ii_-1|0)<0||21<(_ii_-1|0)?1:0;
                                if(!_ij_){var _ik_=i_if_+1|0,i_if_=_ik_;continue;}
                                var _ih_=s_id_;}
                              var _ie_=_ih_;
                              break;}}
                          var s_ib_=_ie_;}
                        var
                         _gV_=
                          cont_s_gU_(next_index_gI_(spec_gM_,n_gF_),s_ib_,i_gC_+1|0),
                         _gQ_=1;
                        break;
                       case 97:
                        var
                         printer_il_=get_arg_gE_(spec_gM_,n_gF_),
                         n_im_=_cQ_(_es_,get_index_gx_(spec_gM_,n_gF_)),
                         arg_in_=get_arg_gE_(0,n_im_),
                         _ir_=i_gC_+1|0,
                         _iq_=next_index_gI_(spec_gM_,n_im_);
                        if(to_s_io_)
                         _fP_(outs_ip_,out_gq_,_fP_(printer_il_,0,arg_in_));
                        else
                         _fP_(printer_il_,out_gq_,arg_in_);
                        var _gV_=doprn_h7_(_iq_,_ir_),_gQ_=1;
                        break;
                       case 116:
                        var
                         printer_is_=get_arg_gE_(spec_gM_,n_gF_),
                         _iu_=i_gC_+1|0,
                         _it_=next_index_gI_(spec_gM_,n_gF_);
                        if(to_s_io_)
                         _fP_(outs_ip_,out_gq_,_cQ_(printer_is_,0));
                        else
                         _cQ_(printer_is_,out_gq_);
                        var _gV_=doprn_h7_(_it_,_iu_),_gQ_=1;
                        break;
                       default:var _gQ_=0;}
                    if(!_gQ_)
                     var _gV_=bad_conversion_format_fq_(fmt_gr_,i_gC_,_gP_);
                    return _gV_;}},
               _iz_=i_gt_+1|0,
               _iw_=0;
              return scan_positional_spec_gN_
                      (fmt_gr_,
                       function(spec_iy_,i_iv_)
                        {return scan_flags_gK_(spec_iy_,n_ix_,_iw_,i_iv_);},
                       n_ix_,
                       _iz_);}
            _fP_(outc_iA_,out_gq_,_gw_);
            var _iB_=i_gt_+1|0,i_gt_=_iB_;
            continue;}}
        function cont_s_gU_(n_iE_,s_iC_,i_iD_)
         {_fP_(outs_ip_,out_gq_,s_iC_);return doprn_h7_(n_iE_,i_iD_);}
        return doprn_h7_(n_iF_,0);}
      var
       kpr_iH_=_fP_(pr_h__,k_iG_,index_of_int_eq_(0)),
       _iI_=count_arguments_of_format_h6_(fmt_go_);
      if(_iI_<0||6<_iI_)
       {var
         loop_iV_=
          function(i_iJ_,args_iP_)
           {if(_iI_<=i_iJ_)
             {var
               a_iK_=caml_make_vect(_iI_,0),
               _iN_=
                function(i_iL_,arg_iM_)
                 {return caml_array_set(a_iK_,(_iI_-i_iL_|0)-1|0,arg_iM_);},
               i_iO_=0,
               param_iQ_=args_iP_;
              for(;;)
               {if(param_iQ_)
                 {var _iR_=param_iQ_[2],_iS_=param_iQ_[1];
                  if(_iR_)
                   {_iN_(i_iO_,_iS_);
                    var _iT_=i_iO_+1|0,i_iO_=_iT_,param_iQ_=_iR_;
                    continue;}
                  _iN_(i_iO_,_iS_);}
                return _fP_(kpr_iH_,fmt_go_,a_iK_);}}
            return function(x_iU_)
             {return loop_iV_(i_iJ_+1|0,[0,x_iU_,args_iP_]);};},
         _iW_=loop_iV_(0,0);}
      else
       switch(_iI_)
        {case 1:
          var
           _iW_=
            function(x_iY_)
             {var a_iX_=caml_make_vect(1,0);
              caml_array_set(a_iX_,0,x_iY_);
              return _fP_(kpr_iH_,fmt_go_,a_iX_);};
          break;
         case 2:
          var
           _iW_=
            function(x_i0_,y_i1_)
             {var a_iZ_=caml_make_vect(2,0);
              caml_array_set(a_iZ_,0,x_i0_);
              caml_array_set(a_iZ_,1,y_i1_);
              return _fP_(kpr_iH_,fmt_go_,a_iZ_);};
          break;
         case 3:
          var
           _iW_=
            function(x_i3_,y_i4_,z_i5_)
             {var a_i2_=caml_make_vect(3,0);
              caml_array_set(a_i2_,0,x_i3_);
              caml_array_set(a_i2_,1,y_i4_);
              caml_array_set(a_i2_,2,z_i5_);
              return _fP_(kpr_iH_,fmt_go_,a_i2_);};
          break;
         case 4:
          var
           _iW_=
            function(x_i7_,y_i8_,z_i9_,t_i__)
             {var a_i6_=caml_make_vect(4,0);
              caml_array_set(a_i6_,0,x_i7_);
              caml_array_set(a_i6_,1,y_i8_);
              caml_array_set(a_i6_,2,z_i9_);
              caml_array_set(a_i6_,3,t_i__);
              return _fP_(kpr_iH_,fmt_go_,a_i6_);};
          break;
         case 5:
          var
           _iW_=
            function(x_ja_,y_jb_,z_jc_,t_jd_,u_je_)
             {var a_i$_=caml_make_vect(5,0);
              caml_array_set(a_i$_,0,x_ja_);
              caml_array_set(a_i$_,1,y_jb_);
              caml_array_set(a_i$_,2,z_jc_);
              caml_array_set(a_i$_,3,t_jd_);
              caml_array_set(a_i$_,4,u_je_);
              return _fP_(kpr_iH_,fmt_go_,a_i$_);};
          break;
         case 6:
          var
           _iW_=
            function(x_jg_,y_jh_,z_ji_,t_jj_,u_jk_,v_jl_)
             {var a_jf_=caml_make_vect(6,0);
              caml_array_set(a_jf_,0,x_jg_);
              caml_array_set(a_jf_,1,y_jh_);
              caml_array_set(a_jf_,2,z_ji_);
              caml_array_set(a_jf_,3,t_jj_);
              caml_array_set(a_jf_,4,u_jk_);
              caml_array_set(a_jf_,5,v_jl_);
              return _fP_(kpr_iH_,fmt_go_,a_jf_);};
          break;
         default:var _iW_=_fP_(kpr_iH_,fmt_go_,[0]);}
      return _iW_;}
    function _ju_(fmt_jm_){return _ej_(2*fmt_jm_.getLen()|0);}
    function _jr_(k_jp_,b_jn_)
     {var s_jo_=_ek_(b_jn_);b_jn_[2]=0;return _cQ_(k_jp_,s_jo_);}
    function _jz_(k_jq_)
     {var _jt_=_cQ_(_jr_,k_jq_);
      return _jw_(_jv_,1,_ju_,_el_,_em_,function(_js_){return 0;},_jt_);}
    function _jA_(fmt_jy_)
     {return _fP_(_jz_,function(s_jx_){return s_jx_;},fmt_jy_);}
    var _jB_=[0,0];
    function _jI_(x_jC_,i_jD_)
     {var f_jE_=x_jC_[i_jD_+1];
      return caml_obj_is_block(f_jE_)
              ?caml_obj_tag(f_jE_)===_d3_
                ?_fP_(_jA_,_bB_,f_jE_)
                :caml_obj_tag(f_jE_)===_d2_?string_of_float_cM_(f_jE_):_bA_
              :_fP_(_jA_,_bC_,f_jE_);}
    function _jH_(x_jF_,i_jG_)
     {if(x_jF_.length-1<=i_jG_)return _bM_;
      var _jJ_=_jH_(x_jF_,i_jG_+1|0);
      return _fk_(_jA_,_bL_,_jI_(x_jF_,i_jG_),_jJ_);}
    32===_dX_;
    function _jM_(param_jL_)
     {var seq_jK_=[];
      caml_update_dummy(seq_jK_,[0,seq_jK_,seq_jK_]);
      return seq_jK_;}
    var Canceled_jN_=[0,_bo_],current_data_jO_=[0,0],max_removed_kA_=42;
    function repr_rec_jS_(t_jP_)
     {var _jQ_=t_jP_[1];
      {if(3===_jQ_[0])
        {var t__jR_=_jQ_[1],t___jT_=repr_rec_jS_(t__jR_);
         if(t___jT_!==t__jR_)t_jP_[1]=[3,t___jT_];
         return t___jT_;}
       return t_jP_;}}
    function repr_kp_(t_jU_){return repr_rec_jS_(t_jU_);}
    function run_waiters_kh_(waiters_jV_,state_jZ_)
     {var ws_jW_=waiters_jV_,rem_jX_=0,save_ka_=current_data_jO_[1];
      for(;;)
       {if(typeof ws_jW_==="number")
         {if(rem_jX_)
           {var
             rem_j$_=rem_jX_[2],
             ws_j__=rem_jX_[1],
             ws_jW_=ws_j__,
             rem_jX_=rem_j$_;
            continue;}}
        else
         switch(ws_jW_[0])
          {case 1:
            var _jY_=ws_jW_[1];
            if(rem_jX_)
             {var rem_j1_=rem_jX_[2],ws_j0_=rem_jX_[1];
              _cQ_(_jY_,state_jZ_);
              var ws_jW_=ws_j0_,rem_jX_=rem_j1_;
              continue;}
            _cQ_(_jY_,state_jZ_);
            break;
           case 2:
            var
             ws1_j2_=ws_jW_[1],
             _j3_=[0,ws_jW_[2],rem_jX_],
             ws_jW_=ws1_j2_,
             rem_jX_=_j3_;
            continue;
           default:
            var _j4_=ws_jW_[1][1];
            if(_j4_)
             {var _j5_=_j4_[1];
              if(rem_jX_)
               {var rem_j7_=rem_jX_[2],ws_j6_=rem_jX_[1];
                _cQ_(_j5_,state_jZ_);
                var ws_jW_=ws_j6_,rem_jX_=rem_j7_;
                continue;}
              _cQ_(_j5_,state_jZ_);}
            else
             if(rem_jX_)
              {var
                rem_j9_=rem_jX_[2],
                ws_j8_=rem_jX_[1],
                ws_jW_=ws_j8_,
                rem_jX_=rem_j9_;
               continue;}}
        current_data_jO_[1]=save_ka_;
        return 0;}}
    function wakeup_ly_(t_kb_,v_ke_)
     {var t_kc_=repr_rec_jS_(t_kb_),_kd_=t_kc_[1];
      switch(_kd_[0])
       {case 1:if(_kd_[1][1]===Canceled_jN_)return 0;break;
        case 2:
         var state_kf_=[0,v_ke_],waiters_kg_=_kd_[1][2];
         t_kc_[1]=state_kf_;
         return run_waiters_kh_(waiters_kg_,state_kf_);
        default:}
      return _cn_(_bp_);}
    function append_ko_(l1_ki_,l2_kj_)
     {return typeof l1_ki_==="number"
              ?l2_kj_
              :typeof l2_kj_==="number"?l1_ki_:[2,l1_ki_,l2_kj_];}
    function cleanup_kl_(ws_kk_)
     {if(typeof ws_kk_!=="number")
       switch(ws_kk_[0])
        {case 2:
          var l1_km_=ws_kk_[1],_kn_=cleanup_kl_(ws_kk_[2]);
          return append_ko_(cleanup_kl_(l1_km_),_kn_);
         case 1:break;
         default:if(!ws_kk_[1][1])return 0;}
      return ws_kk_;}
    function connect_lg_(t1_kq_,t2_ks_)
     {var t1_kr_=repr_kp_(t1_kq_),t2_kt_=repr_kp_(t2_ks_),_ku_=t1_kr_[1];
      {if(2===_ku_[0])
        {var sleeper1_kv_=_ku_[1];
         if(t1_kr_===t2_kt_)return 0;
         var _kw_=t2_kt_[1];
         {if(2===_kw_[0])
           {var sleeper2_kx_=_kw_[1];
            t2_kt_[1]=[3,t1_kr_];
            sleeper1_kv_[1][1]=[1,sleeper2_kx_[1]];
            var
             waiters_ky_=append_ko_(sleeper1_kv_[2],sleeper2_kx_[2]),
             removed_kz_=sleeper1_kv_[3]+sleeper2_kx_[3]|0;
            return max_removed_kA_<removed_kz_
                    ?(sleeper1_kv_[3]=
                      0,
                      sleeper1_kv_[2]=
                      cleanup_kl_(waiters_ky_),
                      0)
                    :(sleeper1_kv_[3]=removed_kz_,sleeper1_kv_[2]=waiters_ky_,0);}
          t1_kr_[1]=_kw_;
          return run_waiters_kh_(sleeper1_kv_[2],_kw_);}}
       return _cn_(_br_);}}
    function fast_connect_lh_(t_kB_,state_kE_)
     {var t_kC_=repr_kp_(t_kB_),_kD_=t_kC_[1];
      {if(2===_kD_[0])
        {var waiters_kF_=_kD_[1][2];
         t_kC_[1]=state_kE_;
         return run_waiters_kh_(waiters_kF_,state_kE_);}
       return _cn_(_bs_);}}
    function return_lx_(v_kG_){return [0,[0,v_kG_]];}
    function fail_k7_(e_kH_){return [0,[1,e_kH_]];}
    function temp_k9_(r_kI_){return [0,[2,[0,r_kI_,0,0]]];}
    function task_lz_(param_kS_)
     {var t_kJ_=[],_kR_=0,_kQ_=0;
      caml_update_dummy
       (t_kJ_,
        [0,
         [2,
          [0,
           [0,
            [0,
             function(param_kP_)
              {var t_kK_=repr_rec_jS_(t_kJ_),_kL_=t_kK_[1];
               if(2===_kL_[0])
                {var state_kM_=[1,[0,Canceled_jN_]],waiters_kN_=_kL_[1][2];
                 t_kK_[1]=state_kM_;
                 var _kO_=run_waiters_kh_(waiters_kN_,state_kM_);}
               else
                var _kO_=0;
               return _kO_;}]],
           _kQ_,
           _kR_]]]);
      return [0,t_kJ_,t_kJ_];}
    function add_immutable_waiter_k4_(sleeper_kT_,waiter_kU_)
     {var
       _kV_=
        typeof sleeper_kT_[2]==="number"
         ?[1,waiter_kU_]
         :[2,[1,waiter_kU_],sleeper_kT_[2]];
      sleeper_kT_[2]=_kV_;
      return 0;}
    function on_cancel_lA_(t_kW_,f_kY_)
     {var _kX_=repr_kp_(t_kW_)[1];
      switch(_kX_[0])
       {case 1:if(_kX_[1][1]===Canceled_jN_)return _cQ_(f_kY_,0);break;
        case 2:
         var sleeper_k3_=_kX_[1],data_k0_=current_data_jO_[1];
         return add_immutable_waiter_k4_
                 (sleeper_k3_,
                  function(param_kZ_)
                   {if(1===param_kZ_[0]&&param_kZ_[1][1]===Canceled_jN_)
                     {current_data_jO_[1]=data_k0_;
                      try {var _k1_=_cQ_(f_kY_,0);}catch(_k2_){return 0;}
                      return _k1_;}
                    return 0;});
        default:}
      return 0;}
    function bind_li_(t_k5_,f_lc_)
     {var _k6_=repr_kp_(t_k5_)[1];
      switch(_k6_[0])
       {case 1:return fail_k7_(_k6_[1]);
        case 2:
         var
          sleeper_k8_=_k6_[1],
          res_k__=temp_k9_(sleeper_k8_[1]),
          data_la_=current_data_jO_[1];
         add_immutable_waiter_k4_
          (sleeper_k8_,
           function(param_k$_)
            {switch(param_k$_[0])
              {case 0:
                var v_lb_=param_k$_[1];
                current_data_jO_[1]=data_la_;
                try
                 {var _ld_=_cQ_(f_lc_,v_lb_),_le_=_ld_;}
                catch(_lf_){var _le_=fail_k7_(_lf_);}
                return connect_lg_(res_k__,_le_);
               case 1:return fast_connect_lh_(res_k__,[1,param_k$_[1]]);
               default:throw [0,_d_,_bu_];}});
         return res_k__;
        case 3:throw [0,_d_,_bt_];
        default:return _cQ_(f_lc_,_k6_[1]);}}
    function _lB_(t_lk_,f_lj_){return bind_li_(t_lk_,f_lj_);}
    function _lC_(t_ll_,f_lt_)
     {var _lm_=repr_kp_(t_ll_)[1];
      switch(_lm_[0])
       {case 1:var _ln_=[0,[1,_lm_[1]]];break;
        case 2:
         var
          sleeper_lo_=_lm_[1],
          res_lp_=temp_k9_(sleeper_lo_[1]),
          data_lr_=current_data_jO_[1];
         add_immutable_waiter_k4_
          (sleeper_lo_,
           function(param_lq_)
            {switch(param_lq_[0])
              {case 0:
                var v_ls_=param_lq_[1];
                current_data_jO_[1]=data_lr_;
                try
                 {var _lu_=[0,_cQ_(f_lt_,v_ls_)],_lv_=_lu_;}
                catch(_lw_){var _lv_=[1,_lw_];}
                return fast_connect_lh_(res_lp_,_lv_);
               case 1:return fast_connect_lh_(res_lp_,[1,param_lq_[1]]);
               default:throw [0,_d_,_bw_];}});
         var _ln_=res_lp_;
         break;
        case 3:throw [0,_d_,_bv_];
        default:var _ln_=return_lx_(_cQ_(f_lt_,_lm_[1]));}
      return _ln_;}
    var
     _lD_=[0],
     _lE_=[0,caml_make_vect(55,0),0],
     seed_lF_=caml_equal(_lD_,[0])?[0,0]:_lD_,
     l_lG_=seed_lF_.length-1,
     _lH_=0,
     _lI_=54;
    if(!(_lI_<_lH_))
     {var i_lJ_=_lH_;
      for(;;)
       {caml_array_set(_lE_[1],i_lJ_,i_lJ_);
        var _lK_=i_lJ_+1|0;
        if(_lI_!==i_lJ_){var i_lJ_=_lK_;continue;}
        break;}}
    var
     accu_lL_=[0,_bz_],
     _lM_=0,
     _lN_=55,
     _lO_=caml_greaterequal(_lN_,l_lG_)?_lN_:l_lG_,
     _lP_=54+_lO_|0;
    if(!(_lP_<_lM_))
     {var i_lQ_=_lM_;
      for(;;)
       {var
         j_lR_=i_lQ_%55|0,
         _lS_=accu_lL_[1],
         _lT_=
          _cy_
           (_lS_,
            string_of_int_cL_(caml_array_get(seed_lF_,caml_mod(i_lQ_,l_lG_))));
        accu_lL_[1]=caml_md5_string(_lT_,0,_lT_.getLen());
        var _lU_=accu_lL_[1];
        caml_array_set
         (_lE_[1],
          j_lR_,
          caml_array_get(_lE_[1],j_lR_)^
          (((_lU_.safeGet(0)+(_lU_.safeGet(1)<<8)|0)+(_lU_.safeGet(2)<<16)|0)+
           (_lU_.safeGet(3)<<24)|
           0));
        var _lV_=i_lQ_+1|0;
        if(_lP_!==i_lQ_){var i_lQ_=_lV_;continue;}
        break;}}
    _lE_[2]=0;
    var pause_hook_lY_=[0,function(_lW_){return 0;}],_lX_=_jM_(0),_l0_=[0,0];
    function _l__(param_l3_)
     {if(_lX_[2]===_lX_)return 0;
      var tmp_lZ_=_jM_(0);
      tmp_lZ_[1][2]=_lX_[2];
      _lX_[2][1]=tmp_lZ_[1];
      tmp_lZ_[1]=_lX_[1];
      _lX_[1][2]=tmp_lZ_;
      _lX_[1]=_lX_;
      _lX_[2]=_lX_;
      _l0_[1]=0;
      var curr_l1_=tmp_lZ_[2];
      for(;;)
       {if(curr_l1_!==tmp_lZ_)
         {if(curr_l1_[4])wakeup_ly_(curr_l1_[3],0);
          var _l2_=curr_l1_[2],curr_l1_=_l2_;
          continue;}
        return 0;}}
    function _l7_(f_l5_,l_l4_)
     {if(l_l4_)
       {var l_l6_=l_l4_[2],__pa_lwt_0_l9_=_cQ_(f_l5_,l_l4_[1]);
        return bind_li_
                (__pa_lwt_0_l9_,
                 function(param_l8_){return _l7_(f_l5_,l_l6_);});}
      return return_lx_(0);}
    var null_l$_=null,undefined_ma_=undefined;
    function _my_(x_mb_,f_mc_)
     {return x_mb_==null_l$_?null_l$_:_cQ_(f_mc_,x_mb_);}
    function _ml_(x_md_,f_me_,g_mf_)
     {return x_md_==null_l$_?_cQ_(f_me_,0):_cQ_(g_mf_,x_md_);}
    function _mz_(x_mg_,f_mh_){return x_mg_==null_l$_?_cQ_(f_mh_,0):x_mg_;}
    function _mA_(x_mm_)
     {function _mk_(x_mi_){return [0,x_mi_];}
      return _ml_(x_mm_,function(param_mj_){return 0;},_mk_);}
    function _mB_(x_mn_){return x_mn_!==undefined_ma_?1:0;}
    function _mw_(x_mo_,f_mp_,g_mq_)
     {return x_mo_===undefined_ma_?_cQ_(f_mp_,0):_cQ_(g_mq_,x_mo_);}
    function _mC_(x_mr_,f_ms_)
     {return x_mr_===undefined_ma_?_cQ_(f_ms_,0):x_mr_;}
    function _mD_(x_mx_)
     {function _mv_(x_mt_){return [0,x_mt_];}
      return _mw_(x_mx_,function(param_mu_){return 0;},_mv_);}
    var
     _true_mE_=true,
     _false_mF_=false,
     regExp_mG_=RegExp,
     array_constructor_mH_=Array;
    function array_get_mM_(_mI_,_mJ_){return _mI_[_mJ_];}
    function str_array_mN_(_mK_){return _mK_;}
    function match_result_mO_(_mL_){return _mL_;}
    var date_constr_mP_=Date,math_mT_=Math;
    function escape_mS_(s_mQ_){return escape(s_mQ_);}
    function _mU_(e_mR_)
     {return e_mR_ instanceof array_constructor_mH_
              ?0
              :[0,new MlWrappedString(e_mR_.toString())];}
    _jB_[1]=[0,_mU_,_jB_[1]];
    function _mX_(_mV_){return _mV_;}
    function _mY_(_mW_){return _mW_;}
    function handler_m4_(f_m0_)
     {return _mY_
              (caml_js_wrap_callback
                (function(e_mZ_)
                  {if(e_mZ_)
                    {var res_m1_=_cQ_(f_m0_,e_mZ_);
                     if(!(res_m1_|0))e_mZ_.preventDefault();
                     return res_m1_;}
                   var _m2_=event,res_m3_=_cQ_(f_m0_,_m2_);
                   _m2_.returnValue=res_m3_;
                   return res_m3_;}));}
    var
     onIE_m5_=caml_js_on_ie(0)|0,
     window_m6_=window,
     document_m7_=window_m6_.document,
     html_element_m8_=window.HTMLElement,
     float32Array_na_=window.Float32Array,
     _m__=
      _mX_(html_element_m8_)===undefined_ma_
       ?function(e_m9_)
         {return _mX_(e_m9_.innerHTML)===undefined_ma_?null_l$_:_mY_(e_m9_);}
       :function(e_m$_)
         {return e_m$_ instanceof html_element_m8_?_mY_(e_m$_):null_l$_;};
    function _ne_(tag_nb_,e_nc_)
     {var _nd_=tag_nb_.toString();
      return e_nc_.tagName.toLowerCase()===_nd_?_mY_(e_nc_):null_l$_;}
    function _nh_(e_nf_){return _ne_(_bm_,e_nf_);}
    function _nj_(e_ng_){return _ne_(_bn_,e_ng_);}
    pause_hook_lY_[1]=
    function(param_ni_)
     {return 1===param_ni_
              ?(window_m6_.setTimeout(caml_js_wrap_callback(_l__),0),0)
              :0;};
    var _nk_=caml_js_get_console(0);
    function regexp_nw_(s_nl_)
     {var _nm_=_bj_.toString();
      return new regExp_mG_(caml_js_from_byte_string(s_nl_),_nm_);}
    function string_match_nx_(r_nn_,s_np_,i_no_)
     {r_nn_.lastIndex=i_no_;
      var
       _nq_=r_nn_.exec(caml_js_from_byte_string(s_np_)),
       _nr_=_nq_==null_l$_?null_l$_:match_result_mO_(_nq_);
      return _mA_(_nr_);}
    function matched_group_ny_(r_nt_,i_ns_)
     {var
       _nu_=array_get_mM_(r_nt_,i_ns_),
       _nv_=_nu_===undefined_ma_?undefined_ma_:caml_js_to_byte_string(_nu_);
      return _mD_(_nv_);}
    var
     quote_repl_re_nz_=new regExp_mG_(_bh_.toString(),_bi_.toString()),
     _nB_=regexp_nw_(_bg_),
     l_nA_=window_m6_.location;
    function split_nE_(c_nC_,s_nD_)
     {return str_array_mN_(s_nD_.split(_dT_(1,c_nC_).toString()));}
    var Local_exn_nF_=[0,_az_];
    function interrupt_nH_(param_nG_){throw [0,Local_exn_nF_];}
    var
     af2e6a4db_nI_=caml_js_from_byte_string(_ay_),
     _nJ_=
      regexp_nw_
       (caml_js_to_byte_string(af2e6a4db_nI_.replace(_nB_,_bl_.toString())));
    function urldecode_js_string_string_nR_(s_nK_)
     {return caml_js_to_byte_string(unescape(s_nK_));}
    function urlencode_nS_(_opt__nL_,s_nN_)
     {var with_plus_nM_=_opt__nL_?_opt__nL_[1]:1;
      if(with_plus_nM_)
       {var
         _nO_=
          caml_js_to_byte_string(escape_mS_(caml_js_from_byte_string(s_nN_)));
        _nJ_.lastIndex=0;
        var
         a41432fb9_nP_=caml_js_from_byte_string(_nO_),
         a11bb050d_nQ_=caml_js_from_byte_string(_aA_);
        return caml_js_to_byte_string
                (a41432fb9_nP_.replace
                  (_nJ_,
                   a11bb050d_nQ_.replace(quote_repl_re_nz_,_bk_.toString())));}
      return caml_js_to_byte_string
              (escape_mS_(caml_js_from_byte_string(s_nN_)));}
    var Not_an_http_protocol_ot_=[0,_ax_];
    function path_of_path_string_nZ_(s_nT_)
     {try
       {var length_nU_=s_nT_.getLen();
        if(0===length_nU_)
         var _nV_=_bf_;
        else
         {var i_nW_=0,_nY_=47,_nX_=s_nT_.getLen();
          for(;;)
           {if(_nX_<=i_nW_)throw [0,_c_];
            if(s_nT_.safeGet(i_nW_)!==_nY_)
             {var _n2_=i_nW_+1|0,i_nW_=_n2_;continue;}
            if(0===i_nW_)
             var
              _n0_=
               [0,_be_,path_of_path_string_nZ_(_dU_(s_nT_,1,length_nU_-1|0))];
            else
             {var
               _n1_=
                path_of_path_string_nZ_
                 (_dU_(s_nT_,i_nW_+1|0,(length_nU_-i_nW_|0)-1|0)),
               _n0_=[0,_dU_(s_nT_,0,i_nW_),_n1_];}
            var _nV_=_n0_;
            break;}}}
      catch(_n3_){if(_n3_[1]===_c_)return [0,s_nT_,0];throw _n3_;}
      return _nV_;}
    function encode_arguments_ou_(l_n7_)
     {return _dW_
              (_aH_,
               _dp_
                (function(param_n4_)
                  {var
                    n_n5_=param_n4_[1],
                    _n6_=_cy_(_aI_,urlencode_nS_(0,param_n4_[2]));
                   return _cy_(urlencode_nS_(0,n_n5_),_n6_);},
                 l_n7_));}
    function decode_arguments_js_string_ov_(s_n8_)
     {var arr_n9_=split_nE_(38,s_n8_),len_os_=arr_n9_.length;
      function aux_oo_(acc_on_,idx_n__)
       {var idx_n$_=idx_n__;
        for(;;)
         {if(0<=idx_n$_)
           {try
             {var
               _ol_=idx_n$_-1|0,
               _om_=
                function(s_og_)
                 {function _oi_(param_oa_)
                   {var y_oe_=param_oa_[2],x_od_=param_oa_[1];
                    function get_oc_(t_ob_)
                     {return urldecode_js_string_string_nR_
                              (_mC_(t_ob_,interrupt_nH_));}
                    var _of_=get_oc_(y_oe_);
                    return [0,get_oc_(x_od_),_of_];}
                  var arr_bis_oh_=split_nE_(61,s_og_);
                  if(2===arr_bis_oh_.length)
                   {var
                     _oj_=array_get_mM_(arr_bis_oh_,1),
                     _ok_=_mX_([0,array_get_mM_(arr_bis_oh_,0),_oj_]);}
                  else
                   var _ok_=undefined_ma_;
                  return _mw_(_ok_,interrupt_nH_,_oi_);},
               _op_=
                aux_oo_
                 ([0,
                   _mw_(array_get_mM_(arr_n9_,idx_n$_),interrupt_nH_,_om_),
                   acc_on_],
                  _ol_);}
            catch(_oq_)
             {if(_oq_[1]===Local_exn_nF_)
               {var _or_=idx_n$_-1|0,idx_n$_=_or_;continue;}
              throw _oq_;}
            return _op_;}
          return acc_on_;}}
      return aux_oo_(0,len_os_-1|0);}
    var
     url_re_ow_=new regExp_mG_(caml_js_from_byte_string(_aw_)),
     file_re_o7_=new regExp_mG_(caml_js_from_byte_string(_av_));
    function string_of_url_o6_(param_ox_)
     {switch(param_ox_[0])
       {case 1:
         var
          match_oy_=param_ox_[1],
          frag_oz_=match_oy_[6],
          args_oA_=match_oy_[5],
          port_oB_=match_oy_[2],
          path_oE_=match_oy_[3],
          host_oD_=match_oy_[1],
          _oC_=
           caml_string_notequal(frag_oz_,_a5_)
            ?_cy_(_a4_,urlencode_nS_(0,frag_oz_))
            :_a3_,
          _oF_=args_oA_?_cy_(_a2_,encode_arguments_ou_(args_oA_)):_a1_,
          _oH_=_cy_(_oF_,_oC_),
          _oJ_=
           _cy_
            (_aZ_,
             _cy_
              (_dW_
                (_a0_,
                 _dp_
                  (function(eta_oG_){return urlencode_nS_(0,eta_oG_);},
                   path_oE_)),
               _oH_)),
          _oI_=443===port_oB_?_aX_:_cy_(_aY_,string_of_int_cL_(port_oB_)),
          _oK_=_cy_(_oI_,_oJ_);
         return _cy_(_aW_,_cy_(urlencode_nS_(0,host_oD_),_oK_));
        case 2:
         var
          match_oL_=param_ox_[1],
          frag_oM_=match_oL_[4],
          args_oN_=match_oL_[3],
          path_oP_=match_oL_[1],
          _oO_=
           caml_string_notequal(frag_oM_,_aV_)
            ?_cy_(_aU_,urlencode_nS_(0,frag_oM_))
            :_aT_,
          _oQ_=args_oN_?_cy_(_aS_,encode_arguments_ou_(args_oN_)):_aR_,
          _oS_=_cy_(_oQ_,_oO_);
         return _cy_
                 (_aP_,
                  _cy_
                   (_dW_
                     (_aQ_,
                      _dp_
                       (function(eta_oR_){return urlencode_nS_(0,eta_oR_);},
                        path_oP_)),
                    _oS_));
        default:
         var
          match_oT_=param_ox_[1],
          frag_oU_=match_oT_[6],
          args_oV_=match_oT_[5],
          port_oW_=match_oT_[2],
          path_oZ_=match_oT_[3],
          host_oY_=match_oT_[1],
          _oX_=
           caml_string_notequal(frag_oU_,_bd_)
            ?_cy_(_bc_,urlencode_nS_(0,frag_oU_))
            :_bb_,
          _o0_=args_oV_?_cy_(_ba_,encode_arguments_ou_(args_oV_)):_a$_,
          _o2_=_cy_(_o0_,_oX_),
          _o4_=
           _cy_
            (_a9_,
             _cy_
              (_dW_
                (_a__,
                 _dp_
                  (function(eta_o1_){return urlencode_nS_(0,eta_o1_);},
                   path_oZ_)),
               _o2_)),
          _o3_=80===port_oW_?_a7_:_cy_(_a8_,string_of_int_cL_(port_oW_)),
          _o5_=_cy_(_o3_,_o4_);
         return _cy_(_a6_,_cy_(urlencode_nS_(0,host_oY_),_o5_));}}
    urldecode_js_string_string_nR_(l_nA_.hostname);
    try
     {caml_int_of_string(caml_js_to_byte_string(l_nA_.port));}
    catch(_o8_){if(_o8_[1]!==_a_)throw _o8_;}
    path_of_path_string_nZ_(urldecode_js_string_string_nR_(l_nA_.pathname));
    decode_arguments_js_string_ov_(l_nA_.search);
    urldecode_js_string_string_nR_(l_nA_.href);
    var _pf_=window.FileReader,formData_pe_=window.FormData;
    function _pd_(form_contents_o9_,form_elt_o$_)
     {if(891486873<=form_contents_o9_[1])
       {var list_o__=form_contents_o9_[2];
        list_o__[1]=[0,form_elt_o$_,list_o__[1]];
        return 0;}
      var
       f_pa_=form_contents_o9_[2],
       _pb_=form_elt_o$_[2],
       _pc_=form_elt_o$_[1];
      return 781515420<=_pb_[1]
              ?f_pa_.append(_pc_.toString(),_pb_[2])
              :f_pa_.append(_pc_.toString(),_pb_[2]);}
    function _ph_(param_pg_){return ActiveXObject;}
    var _pR_=[0,_N_];
    function error_po_(f_pj_)
     {return _fP_
              (_jz_,
               function(s_pi_)
                {_nk_.error(s_pi_.toString());return _j_(s_pi_);},
               f_pj_);}
    function debug_pS_(f_pl_)
     {return _fP_
              (_jz_,function(s_pk_){return _nk_.log(s_pk_.toString());},f_pl_);}
    function check_error_pT_(gl_pm_)
     {var _pn_=gl_pm_.NO;
      return caml_notequal(gl_pm_.getError(),_pn_)?error_po_(_m_):0;}
    function load_shader_pU_(gl_pp_,shader_pr_,text_pq_)
     {gl_pp_.shaderSource(shader_pr_,text_pq_);
      gl_pp_.compileShader(shader_pr_);
      if(gl_pp_.getShaderParameter(shader_pr_,gl_pp_.COMPILE_STATUS)|0)
       return 0;
      var _ps_=new MlWrappedString(gl_pp_.getShaderInfoLog(shader_pr_));
      return _fk_(error_po_,_p_,new MlWrappedString(text_pq_),_ps_);}
    function get_source_pV_(src_id_pt_)
     {function _pv_(param_pu_){return _fP_(error_po_,_r_,src_id_pt_);}
      return _mz_
              (_my_(document_m7_.getElementById(src_id_pt_.toString()),_nj_),
               _pv_).text;}
    function float32array_pW_(a_pw_)
     {var
       array_px_=new float32Array_na_(a_pw_.length-1),
       _py_=0,
       _pz_=a_pw_.length-1-1|0;
      if(!(_pz_<_py_))
       {var i_pA_=_py_;
        for(;;)
         {array_px_[i_pA_]=a_pw_[i_pA_+1];
          var _pB_=i_pA_+1|0;
          if(_pz_!==i_pA_){var i_pA_=_pB_;continue;}
          break;}}
      return array_px_;}
    function _pH_(i_pC_,j_pD_){return (i_pC_*4|0)+j_pD_|0;}
    function _pX_(m1_pK_,m2_pI_)
     {return _db_
              (16,
               function(p_pE_)
                {var
                  _pF_=p_pE_%4|0,
                  _pG_=p_pE_/4|0,
                  _pJ_=caml_array_get(m2_pI_,_pH_(3,_pF_)),
                  _pL_=caml_array_get(m1_pK_,_pH_(_pG_,3))*_pJ_,
                  _pM_=caml_array_get(m2_pI_,_pH_(2,_pF_)),
                  _pN_=caml_array_get(m1_pK_,_pH_(_pG_,2))*_pM_,
                  _pO_=caml_array_get(m2_pI_,_pH_(1,_pF_)),
                  _pP_=caml_array_get(m1_pK_,_pH_(_pG_,1))*_pO_,
                  _pQ_=caml_array_get(m2_pI_,_pH_(0,_pF_));
                 return caml_array_get(m1_pK_,_pH_(_pG_,0))*
                        _pQ_+
                        _pP_+
                        _pN_+
                        _pL_;});}
    var line_regexp_pY_=regexp_nw_(_l_),couple_regexp_p0_=regexp_nw_(_k_);
    function read_coord_couple_rN_(c_pZ_)
     {var _p1_=string_match_nx_(couple_regexp_p0_,c_pZ_,0);
      if(_p1_)
       {var _p2_=_dp_(_cQ_(matched_group_ny_,_p1_[1]),_s_);
        if(_p2_)
         {var _p3_=_p2_[1];
          if(_p3_)
           {var _p4_=_p2_[2];
            if(_p4_)
             {var _p5_=_p4_[1];
              if(_p5_&&!_p4_[2])
               {var v_p6_=_p3_[1],_p7_=caml_int_of_string(_p5_[1]);
                return [0,[0,caml_int_of_string(v_p6_),_p7_]];}}}}
        return 0;}
      return 0;}
    var pi_qx_=4*Math.atan(1);
    function start_u1_(param_p8_)
     {var
       pos_p9_=param_p8_[1],
       norm_p$_=param_p8_[2],
       fps_text_p__=document_m7_.createTextNode(_I_.toString()),
       _qa_=_my_(document_m7_.getElementById(_H_.toString()),_m__);
      if(_qa_!=null_l$_)_qa_.appendChild(fps_text_p__);
      function _qc_(param_qb_){return _fP_(error_po_,_n_,_g_);}
      var
       canvas_qd_=
        _mz_(_my_(document_m7_.getElementById(_g_.toString()),_nh_),_qc_);
      function _qf_(param_qe_){return error_po_(_o_);}
      var
       ctx_qg_=canvas_qd_.getContext(_M_.toString()),
       _qh_=
        1-(ctx_qg_==null_l$_?1:0)
         ?ctx_qg_
         :canvas_qd_.getContext(_L_.toString()),
       gl_qi_=_mz_(_qh_,_qf_),
       _qj_=get_source_pV_(_G_),
       _qk_=get_source_pV_(_F_),
       vertexShader_ql_=gl_qi_.createShader(gl_qi_.VERTEX_SHADER),
       fragmentShader_qm_=gl_qi_.createShader(gl_qi_.FRAGMENT_SHADER);
      load_shader_pU_(gl_qi_,vertexShader_ql_,_qk_);
      load_shader_pU_(gl_qi_,fragmentShader_qm_,_qj_);
      var prog_qn_=gl_qi_.createProgram();
      gl_qi_.attachShader(prog_qn_,vertexShader_ql_);
      gl_qi_.attachShader(prog_qn_,fragmentShader_qm_);
      gl_qi_.linkProgram(prog_qn_);
      if(!(gl_qi_.getProgramParameter(prog_qn_,gl_qi_.LINK_STATUS)|0))
       error_po_(_q_);
      gl_qi_.useProgram(prog_qn_);
      check_error_pT_(gl_qi_);
      debug_pS_(_E_);
      gl_qi_.enable(gl_qi_.DEPTH_TEST);
      gl_qi_.depthFunc(gl_qi_.LESS);
      var
       proj_loc_qo_=gl_qi_.getUniformLocation(prog_qn_,_D_.toString()),
       lightPos_loc_qp_=gl_qi_.getUniformLocation(prog_qn_,_C_.toString()),
       ambientLight_loc_qq_=gl_qi_.getUniformLocation(prog_qn_,_B_.toString()),
       lightPos_qr_=float32array_pW_([-1,3,0,-1]),
       ambientLight_qs_=float32array_pW_([-1,0.1,0.1,0.1]);
      gl_qi_.uniform3fv(lightPos_loc_qp_,lightPos_qr_);
      gl_qi_.uniform3fv(ambientLight_loc_qq_,ambientLight_qs_);
      var pos_attr_qt_=gl_qi_.getAttribLocation(prog_qn_,_A_.toString());
      gl_qi_.enableVertexAttribArray(pos_attr_qt_);
      var array_buffer_qu_=gl_qi_.createBuffer();
      gl_qi_.bindBuffer(gl_qi_.ARRAY_BUFFER,array_buffer_qu_);
      gl_qi_.bufferData(gl_qi_.ARRAY_BUFFER,pos_p9_,gl_qi_.STATIC_DRAW);
      gl_qi_.vertexAttribPointer(pos_attr_qt_,3,gl_qi_.FLOAT,_false_mF_,0,0);
      var norm_attr_qv_=gl_qi_.getAttribLocation(prog_qn_,_z_.toString());
      gl_qi_.enableVertexAttribArray(norm_attr_qv_);
      var norm_buffer_qw_=gl_qi_.createBuffer();
      gl_qi_.bindBuffer(gl_qi_.ARRAY_BUFFER,norm_buffer_qw_);
      gl_qi_.bufferData(gl_qi_.ARRAY_BUFFER,norm_p$_,gl_qi_.STATIC_DRAW);
      gl_qi_.vertexAttribPointer(norm_attr_qv_,3,gl_qi_.FLOAT,_false_mF_,0,0);
      var
       _qy_=pi_qx_/2,
       mat_qz_=
        _pX_
         ([-1,
           1,
           0,
           0,
           0,
           0,
           Math.cos(_qy_),
           Math.sin(_qy_),
           0,
           0,
           -Math.sin(_qy_),
           Math.cos(_qy_),
           0,
           0,
           0,
           0,
           1],
          _pX_
           ([-1,0.5,0,0,0,0,0.5,0,0,0,0,0.5,0,0,0,0,1],
            [-1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]));
      check_error_pT_(gl_qi_);
      debug_pS_(_y_);
      function get_time_qB_(param_qA_)
       {return new date_constr_mP_().getTime();}
      var last_draw_qC_=[0,get_time_qB_(0)],_qD_=[0,0,0];
      function f_q2_(param_q3_)
       {var _qE_=1*(new date_constr_mP_().getTime()/1000);
        gl_qi_.uniformMatrix4fv
         (proj_loc_qo_,
          _false_mF_,
          float32array_pW_
           (_pX_
             (mat_qz_,
              [-1,
               Math.cos(_qE_),
               0,
               -Math.sin(_qE_),
               0,
               0,
               1,
               0,
               0,
               Math.sin(_qE_),
               0,
               Math.cos(_qE_),
               0,
               0,
               0,
               0,
               1])));
        gl_qi_.clear(gl_qi_.DEPTH_BUFFER_BIT|gl_qi_.COLOR_BUFFER_BIT);
        gl_qi_.drawArrays(gl_qi_.TRIANGLES,0,pos_p9_.length/3|0);
        check_error_pT_(gl_qi_);
        var now_qF_=get_time_qB_(0),_qG_=now_qF_-last_draw_qC_[1];
        _qD_[1]=_qD_[1]+1|0;
        if(1===_qD_[1])
         {var cell_qH_=[];
          caml_update_dummy(cell_qH_,[0,_qG_,cell_qH_]);
          _qD_[2]=cell_qH_;}
        else
         {var tail_qI_=_qD_[2],cell_qJ_=[0,_qG_,tail_qI_[2]];
          tail_qI_[2]=cell_qJ_;
          _qD_[2]=cell_qJ_;}
        last_draw_qC_[1]=now_qF_;
        if(50<_d0_(_qD_))
         {if(0===_qD_[1])throw [0,_d1_];
          _qD_[1]=_qD_[1]-1|0;
          var tail_qK_=_qD_[2],head_qL_=tail_qK_[2];
          if(head_qL_===tail_qK_)_qD_[2]=0;else tail_qK_[2]=head_qL_[2];}
        var _qM_=0,_qN_=_d0_(_qD_),_qU_=1000;
        if(0===_qD_[1])
         var _qO_=_qM_;
        else
         {var tail_qP_=_qD_[2],accu_qQ_=_qM_,cell_qR_=tail_qP_[2];
          for(;;)
           {var _qS_=accu_qQ_+cell_qR_[1];
            if(cell_qR_!==tail_qP_)
             {var _qT_=cell_qR_[2],accu_qQ_=_qS_,cell_qR_=_qT_;continue;}
            var _qO_=_qS_;
            break;}}
        fps_text_p__.data=_fP_(_jA_,_J_,1/_qO_*_qN_*_qU_).toString();
        var
         match_qV_=task_lz_(0),
         t_qW_=match_qV_[1],
         w_qX_=match_qV_[2],
         _qZ_=0.02*1000,
         id_q0_=
          window_m6_.setTimeout
           (caml_js_wrap_callback
             (function(param_qY_){return wakeup_ly_(w_qX_,0);}),
            _qZ_);
        on_cancel_lA_
         (t_qW_,function(param_q1_){return window_m6_.clearTimeout(id_q0_);});
        return _lB_(t_qW_,f_q2_);}
      return f_q2_(0);}
    window_m6_.onload=
    handler_m4_
     (function(param_vd_)
       {function _ri_(exn_q5_)
         {var param_q4_=_jB_[1];
          for(;;)
           {if(param_q4_)
             {var tl_q__=param_q4_[2],hd_q6_=param_q4_[1];
              try
               {var _q7_=_cQ_(hd_q6_,exn_q5_),_q8_=_q7_;}
              catch(_q$_){var _q8_=0;}
              if(!_q8_){var param_q4_=tl_q__;continue;}
              var _q9_=_q8_[1];}
            else
             if(exn_q5_[1]===_cm_)
              var _q9_=_bK_;
             else
              if(exn_q5_[1]===_ck_)
               var _q9_=_bJ_;
              else
               if(exn_q5_[1]===_cl_)
                {var
                  match_ra_=exn_q5_[2],
                  char_rb_=match_ra_[3],
                  _q9_=
                   _jw_
                    (_jA_,
                     _e_,
                     match_ra_[1],
                     match_ra_[2],
                     char_rb_,
                     char_rb_+5|0,
                     _bI_);}
               else
                if(exn_q5_[1]===_d_)
                 {var
                   match_rc_=exn_q5_[2],
                   char_rd_=match_rc_[3],
                   _q9_=
                    _jw_
                     (_jA_,
                      _e_,
                      match_rc_[1],
                      match_rc_[2],
                      char_rd_,
                      char_rd_+6|0,
                      _bH_);}
                else
                 {var _re_=exn_q5_.length-1,constructor_rh_=exn_q5_[0+1][0+1];
                  if(_re_<0||2<_re_)
                   {var
                     _rf_=_jH_(exn_q5_,2),
                     _rg_=_fk_(_jA_,_bG_,_jI_(exn_q5_,1),_rf_);}
                  else
                   switch(_re_)
                    {case 1:var _rg_=_bE_;break;
                     case 2:var _rg_=_fP_(_jA_,_bD_,_jI_(exn_q5_,1));break;
                     default:var _rg_=_bF_;}
                  var _q9_=_cy_(constructor_rh_,_rg_);}
            return _fP_(error_po_,_K_,_q9_);}}
        try
         {var
           _si_=0,
           _sj_=0,
           _sk_=0,
           _sl_=0,
           _sm_=0,
           _sn_=0,
           _sp_=
            function(frame_rj_)
             {var
               a_rk_=
                str_array_mN_(frame_rj_[4].toString().split(_x_.toString()));
              window_m6_.arr=a_rk_;
              var vertex_rl_=[0,0],norm_rm_=[0,0],face_rn_=[0,0];
              function aux_r0_(i_rZ_)
               {function _r2_(s_ro_)
                 {var
                   _rp_=
                    string_match_nx_
                     (line_regexp_pY_,new MlWrappedString(s_ro_),0);
                  if(_rp_)
                   {var _rq_=_dp_(_cQ_(matched_group_ny_,_rp_[1]),_w_);
                    if(_rq_)
                     {var _rr_=_rq_[1];
                      if(_rr_)
                       {var _rs_=_rr_[1];
                        if(caml_string_notequal(_rs_,_v_))
                         if(caml_string_notequal(_rs_,_u_))
                          if(caml_string_notequal(_rs_,_t_))
                           var _rt_=0;
                          else
                           {var _ru_=_rq_[2];
                            if(_ru_)
                             {var _rv_=_ru_[1];
                              if(_rv_)
                               {var _rw_=_ru_[2];
                                if(_rw_)
                                 {var _rx_=_rw_[1];
                                  if(_rx_)
                                   {var _ry_=_rw_[2];
                                    if(_ry_)
                                     {var _rz_=_ry_[1];
                                      if(_rz_&&!_ry_[2])
                                       {var _rA_=[0,[1,[0,+_rv_[1],+_rx_[1],+_rz_[1]]]],_rt_=1;}
                                      else
                                       var _rt_=0;}
                                    else
                                     var _rt_=0;}
                                  else
                                   var _rt_=0;}
                                else
                                 var _rt_=0;}
                              else
                               var _rt_=0;}
                            else
                             var _rt_=0;}
                         else
                          {var _rB_=_rq_[2];
                           if(_rB_)
                            {var _rC_=_rB_[1];
                             if(_rC_)
                              {var _rD_=_rB_[2];
                               if(_rD_)
                                {var _rE_=_rD_[1];
                                 if(_rE_)
                                  {var _rF_=_rD_[2];
                                   if(_rF_)
                                    {var _rG_=_rF_[1];
                                     if(_rG_&&!_rF_[2])
                                      {var _rA_=[0,[0,[0,+_rC_[1],+_rE_[1],+_rG_[1]]]],_rt_=1;}
                                     else
                                      var _rt_=0;}
                                   else
                                    var _rt_=0;}
                                 else
                                  var _rt_=0;}
                               else
                                var _rt_=0;}
                             else
                              var _rt_=0;}
                           else
                            var _rt_=0;}
                        else
                         {var _rH_=_rq_[2];
                          if(_rH_)
                           {var _rI_=_rH_[1];
                            if(_rI_)
                             {var _rJ_=_rH_[2];
                              if(_rJ_)
                               {var _rK_=_rJ_[1];
                                if(_rK_)
                                 {var _rL_=_rJ_[2];
                                  if(_rL_)
                                   {var _rM_=_rL_[1];
                                    if(_rM_&&!_rL_[2])
                                     {var
                                       _rO_=
                                        _dp_
                                         (read_coord_couple_rN_,
                                          [0,_rI_[1],[0,_rK_[1],[0,_rM_[1],0]]]);
                                      if(_rO_)
                                       {var _rP_=_rO_[1];
                                        if(_rP_)
                                         {var _rQ_=_rO_[2];
                                          if(_rQ_)
                                           {var _rR_=_rQ_[1];
                                            if(_rR_)
                                             {var _rS_=_rQ_[2];
                                              if(_rS_)
                                               {var _rT_=_rS_[1];
                                                if(_rT_&&!_rS_[2])
                                                 {var _rA_=[0,[2,[0,_rP_[1],_rR_[1],_rT_[1]]]],_rt_=1,_rU_=0;}
                                                else
                                                 var _rU_=1;}
                                              else
                                               var _rU_=1;}
                                            else
                                             var _rU_=1;}
                                          else
                                           var _rU_=1;}
                                        else
                                         var _rU_=1;}
                                      else
                                       var _rU_=1;
                                      if(_rU_){var _rA_=0,_rt_=1;}}
                                    else
                                     var _rt_=0;}
                                  else
                                   var _rt_=0;}
                                else
                                 var _rt_=0;}
                              else
                               var _rt_=0;}
                            else
                             var _rt_=0;}
                          else
                           var _rt_=0;}}
                      else
                       var _rt_=0;}
                    else
                     var _rt_=0;
                    if(!_rt_)var _rA_=0;}
                  else
                   var _rA_=0;
                  if(_rA_)
                   {var _rV_=_rA_[1];
                    switch(_rV_[0])
                     {case 1:
                       var match_rW_=_rV_[1];
                       norm_rm_[1]=
                       [0,[0,match_rW_[1],match_rW_[2],match_rW_[3]],norm_rm_[1]];
                       break;
                      case 2:
                       var match_rX_=_rV_[1];
                       face_rn_[1]=
                       [0,[0,match_rX_[1],match_rX_[2],match_rX_[3]],face_rn_[1]];
                       break;
                      default:
                       var match_rY_=_rV_[1];
                       vertex_rl_[1]=
                       [0,[0,match_rY_[1],match_rY_[2],match_rY_[3]],vertex_rl_[1]];}}
                  return aux_r0_(i_rZ_+1|0);}
                function _r3_(param_r1_){return 0;}
                return _mw_(array_get_mM_(a_rk_,i_rZ_),_r3_,_r2_);}
              aux_r0_(0);
              var
               _r4_=_dd_(_dv_(face_rn_[1])),
               _r5_=_dd_(_dv_(norm_rm_[1])),
               _r6_=_dd_(_dv_(vertex_rl_[1])),
               vertex__sf_=
                _db_
                 (_r4_.length-1,
                  function(i_r7_)
                   {var
                     _r8_=caml_array_get(_r4_,i_r7_),
                     match_r9_=caml_array_get(_r6_,_r8_[1][1]-1|0),
                     match_r__=caml_array_get(_r6_,_r8_[2][1]-1|0),
                     match_r$_=caml_array_get(_r6_,_r8_[3][1]-1|0);
                    return [0,
                            match_r9_[1],
                            [0,
                             match_r9_[2],
                             [0,
                              match_r9_[3],
                              [0,
                               match_r__[1],
                               [0,
                                match_r__[2],
                                [0,
                                 match_r__[3],
                                 [0,match_r$_[1],[0,match_r$_[2],[0,match_r$_[3],0]]]]]]]]];}),
               norm__sg_=
                _db_
                 (_r4_.length-1,
                  function(i_sa_)
                   {var
                     _sb_=caml_array_get(_r4_,i_sa_),
                     match_sc_=caml_array_get(_r5_,_sb_[1][2]-1|0),
                     match_sd_=caml_array_get(_r5_,_sb_[2][2]-1|0),
                     match_se_=caml_array_get(_r5_,_sb_[3][2]-1|0);
                    return [0,
                            match_sc_[1],
                            [0,
                             match_sc_[2],
                             [0,
                              match_sc_[3],
                              [0,
                               match_sd_[1],
                               [0,
                                match_sd_[2],
                                [0,
                                 match_sd_[3],
                                 [0,match_se_[1],[0,match_se_[2],[0,match_se_[3],0]]]]]]]]];}),
               vertex_sh_=float32array_pW_(_dd_(_dk_(_dc_(vertex__sf_))));
              return [0,
                      vertex_sh_,
                      float32array_pW_(_dd_(_dk_(_dc_(norm__sg_))))];},
           headers_so_=_sn_?_sn_[1]:0,
           get_args_sq_=_sk_?_sk_[1]:0,
           check_headers_sr_=_si_?_si_[1]:function(param_ss_,_st_){return 1;};
          if(_sj_)
           {var form_arg_su_=_sj_[1];
            if(_sl_)
             {var post_args_sw_=_sl_[1];
              _dw_
               (function(param_sv_)
                 {return _pd_
                          (form_arg_su_,
                           [0,param_sv_[1],[0,-976970511,param_sv_[2].toString()]]);},
                post_args_sw_);}
            var form_arg_sx_=[0,form_arg_su_];}
          else
           if(_sl_)
            {var
              post_args_sy_=_sl_[1],
              _sz_=_mD_(_mX_(formData_pe_)),
              contents_sA_=
               _sz_?[0,808620462,new (_sz_[1])()]:[0,891486873,[0,0]];
             _dw_
              (function(param_sB_)
                {return _pd_
                         (contents_sA_,
                          [0,param_sB_[1],[0,-976970511,param_sB_[2].toString()]]);},
               post_args_sy_);
             var form_arg_sx_=[0,contents_sA_];}
           else
            var form_arg_sx_=0;
          if(form_arg_sx_)
           {var _sC_=form_arg_sx_[1];
            if(_sm_)
             var _sD_=[0,_ao_,_sm_,126925477];
            else
             {if(891486873<=_sC_[1])
               {var yes_sE_=0,no_sF_=0,param_sG_=_sC_[2][1];
                for(;;)
                 {if(param_sG_)
                   {var
                     l_sH_=param_sG_[2],
                     x_sI_=param_sG_[1],
                     _sJ_=781515420<=x_sI_[2][1]?0:1;
                    if(_sJ_)
                     {var _sK_=[0,x_sI_,yes_sE_],yes_sE_=_sK_,param_sG_=l_sH_;
                      continue;}
                    var _sL_=[0,x_sI_,no_sF_],no_sF_=_sL_,param_sG_=l_sH_;
                    continue;}
                  var _sM_=_dv_(no_sF_);
                  _dv_(yes_sE_);
                  if(_sM_)
                   {var
                     nine_digits_sO_=
                      function(param_sN_)
                       {return string_of_int_cL_(math_mT_.random()*1000000000|0);},
                     _sP_=nine_digits_sO_(0),
                     _sQ_=_cy_(_S_,_cy_(nine_digits_sO_(0),_sP_)),
                     _sR_=[0,_am_,[0,_cy_(_an_,_sQ_)],[0,164354597,_sQ_]];}
                  else
                   var _sR_=_al_;
                  var _sS_=_sR_;
                  break;}}
              else
               var _sS_=_ak_;
              var _sD_=_sS_;}
            var match_sT_=_sD_;}
          else
           var match_sT_=[0,_aj_,_sm_,126925477];
          var
           post_encode_sU_=match_sT_[3],
           content_type_sV_=match_sT_[2],
           method__sX_=match_sT_[1],
           _sW_=caml_js_from_byte_string(_f_),
           _tu_=
            function(handle_sY_)
             {var
               res_sZ_=match_result_mO_(handle_sY_),
               _s0_=
                caml_js_to_byte_string
                 (_mC_(array_get_mM_(res_sZ_,1),interrupt_nH_).toLowerCase());
              if
               (caml_string_notequal(_s0_,_aG_)&&
                caml_string_notequal(_s0_,_aF_))
               {if
                 (caml_string_notequal(_s0_,_aE_)&&
                  caml_string_notequal(_s0_,_aD_))
                 {if
                   (caml_string_notequal(_s0_,_aC_)&&
                    caml_string_notequal(_s0_,_aB_))
                   {var _s2_=1,_s1_=0;}
                  else
                   var _s1_=1;
                  if(_s1_){var ssl_s3_=1,_s2_=2;}}
                else
                 var _s2_=0;
                switch(_s2_)
                 {case 1:var _s4_=0;break;
                  case 2:var _s4_=1;break;
                  default:var ssl_s3_=0,_s4_=1;}
                if(_s4_)
                 {var
                   path_str_s5_=
                    urldecode_js_string_string_nR_
                     (_mC_(array_get_mM_(res_sZ_,5),interrupt_nH_)),
                   _s7_=
                    function(param_s6_){return caml_js_from_byte_string(_aK_);},
                   _s9_=
                    urldecode_js_string_string_nR_
                     (_mC_(array_get_mM_(res_sZ_,9),_s7_)),
                   _s__=
                    function(param_s8_){return caml_js_from_byte_string(_aL_);},
                   _s$_=
                    decode_arguments_js_string_ov_
                     (_mC_(array_get_mM_(res_sZ_,7),_s__)),
                   _tb_=path_of_path_string_nZ_(path_str_s5_),
                   _tc_=
                    function(param_ta_){return caml_js_from_byte_string(_aM_);},
                   _td_=
                    caml_js_to_byte_string(_mC_(array_get_mM_(res_sZ_,4),_tc_)),
                   _te_=
                    caml_string_notequal(_td_,_aJ_)
                     ?caml_int_of_string(_td_)
                     :ssl_s3_?443:80,
                   url_tf_=
                    [0,
                     urldecode_js_string_string_nR_
                      (_mC_(array_get_mM_(res_sZ_,2),interrupt_nH_)),
                     _te_,
                     _tb_,
                     path_str_s5_,
                     _s$_,
                     _s9_],
                   _tg_=ssl_s3_?[1,url_tf_]:[0,url_tf_];
                  return [0,_tg_];}}
              throw [0,Not_an_http_protocol_ot_];},
           _tv_=
            function(param_tt_)
             {function _tr_(handle_th_)
               {var
                 res_ti_=match_result_mO_(handle_th_),
                 path_str_tj_=
                  urldecode_js_string_string_nR_
                   (_mC_(array_get_mM_(res_ti_,2),interrupt_nH_));
                function _tl_(param_tk_)
                 {return caml_js_from_byte_string(_aN_);}
                var
                 _tn_=
                  caml_js_to_byte_string(_mC_(array_get_mM_(res_ti_,6),_tl_));
                function _to_(param_tm_)
                 {return caml_js_from_byte_string(_aO_);}
                var
                 _tp_=
                  decode_arguments_js_string_ov_
                   (_mC_(array_get_mM_(res_ti_,4),_to_));
                return [0,
                        [2,
                         [0,
                          path_of_path_string_nZ_(path_str_tj_),
                          path_str_tj_,
                          _tp_,
                          _tn_]]];}
              function _ts_(param_tq_){return 0;}
              return _ml_(file_re_o7_.exec(_sW_),_ts_,_tr_);},
           _tw_=_ml_(url_re_ow_.exec(_sW_),_tv_,_tu_);
          if(_tw_)
           {var _tx_=_tw_[1];
            switch(_tx_[0])
             {case 0:
               var url_ty_=_tx_[1],_tz_=url_ty_.slice(),_tA_=url_ty_[5];
               _tz_[5]=0;
               var match_tB_=[0,string_of_url_o6_([0,_tz_]),_tA_],_tC_=1;
               break;
              case 1:
               var url_tD_=_tx_[1],_tE_=url_tD_.slice(),_tF_=url_tD_[5];
               _tE_[5]=0;
               var match_tB_=[0,string_of_url_o6_([1,_tE_]),_tF_],_tC_=1;
               break;
              default:var _tC_=0;}}
          else
           var _tC_=0;
          if(!_tC_)var match_tB_=[0,_f_,0];
          var
           url_tG_=match_tB_[1],
           _tH_=_cE_(match_tB_[2],get_args_sq_),
           url_tI_=
            _tH_?_cy_(url_tG_,_cy_(_ai_,encode_arguments_ou_(_tH_))):url_tG_,
           match_tJ_=task_lz_(0),
           w_tK_=match_tJ_[2],
           res_tL_=match_tJ_[1];
          try
           {var _tM_=new XMLHttpRequest(),req_tN_=_tM_;}
          catch(_u4_)
           {try
             {var
               a5f6575ba_tO_=_ph_(0),
               _tP_=new a5f6575ba_tO_(_R_.toString()),
               req_tN_=_tP_;}
            catch(_tW_)
             {try
               {var
                 a64f1392a_tQ_=_ph_(0),
                 _tR_=new a64f1392a_tQ_(_Q_.toString()),
                 req_tN_=_tR_;}
              catch(_tV_)
               {try
                 {var
                   a30a5da5a_tS_=_ph_(0),
                   _tT_=new a30a5da5a_tS_(_P_.toString());}
                catch(_tU_){throw [0,_d_,_O_];}
                var req_tN_=_tT_;}}}
          req_tN_.open(method__sX_.toString(),url_tI_.toString(),_true_mE_);
          if(content_type_sV_)
           req_tN_.setRequestHeader
            (_ah_.toString(),content_type_sV_[1].toString());
          _dw_
           (function(param_tX_)
             {return req_tN_.setRequestHeader
                      (param_tX_[1].toString(),param_tX_[2].toString());},
            headers_so_);
          var
           headers_t3_=
            function(s_t1_)
             {function _t0_(v_tY_){return [0,new MlWrappedString(v_tY_)];}
              function _t2_(param_tZ_){return 0;}
              return _ml_
                      (req_tN_.getResponseHeader(caml_js_from_byte_string(s_t1_)),
                       _t2_,
                       _t0_);},
           checked_t4_=[0,0],
           do_check_headers_ub_=
            function(param_ua_)
             {if
               (checked_t4_[1]||
                _fP_(check_headers_sr_,req_tN_.status,headers_t3_))
               var _t5_=0;
              else
               {var
                 _t7_=[0,_pR_,[0,req_tN_.status,headers_t3_]],
                 t_t6_=repr_rec_jS_(w_tK_),
                 _t8_=t_t6_[1];
                switch(_t8_[0])
                 {case 1:var _t9_=_t8_[1][1]===Canceled_jN_?1:0;break;
                  case 2:
                   var state_t__=[1,_t7_],waiters_t$_=_t8_[1][2];
                   t_t6_[1]=state_t__;
                   run_waiters_kh_(waiters_t$_,state_t__);
                   var _t9_=1;
                   break;
                  default:var _t9_=0;}
                if(!_t9_)_cn_(_bq_);
                req_tN_.abort();
                var _t5_=1;}
              _t5_;
              checked_t4_[1]=1;
              return 0;};
          req_tN_.onreadystatechange=
          caml_js_wrap_callback
           (function(param_ug_)
             {switch(req_tN_.readyState)
               {case 2:if(!onIE_m5_)return do_check_headers_ub_(0);break;
                case 3:if(onIE_m5_)return do_check_headers_ub_(0);break;
                case 4:
                 do_check_headers_ub_(0);
                 var
                  _uf_=
                   function(param_ue_)
                    {var _uc_=_mA_(req_tN_.responseXML);
                     if(_uc_)
                      {var doc_ud_=_uc_[1];
                       return _mY_(doc_ud_.documentElement)===null_l$_
                               ?0
                               :[0,doc_ud_];}
                     return 0;};
                 return wakeup_ly_
                         (w_tK_,
                          [0,
                           url_tI_,
                           req_tN_.status,
                           headers_t3_,
                           new MlWrappedString(req_tN_.responseText),
                           _uf_]);
                default:}
              return 0;});
          if(form_arg_sx_)
           {var _uh_=form_arg_sx_[1];
            if(891486873<=_uh_[1])
             {var l_ui_=_uh_[2];
              if(typeof post_encode_sU_==="number")
               {var _uo_=l_ui_[1];
                req_tN_.send
                 (_mY_
                   (_dW_
                      (_ae_,
                       _dp_
                        (function(param_uj_)
                          {var _uk_=param_uj_[2],_ul_=param_uj_[1];
                           if(781515420<=_uk_[1])
                            {var
                              _um_=
                               _cy_
                                (_ag_,urlencode_nS_(0,new MlWrappedString(_uk_[2].name)));
                             return _cy_(urlencode_nS_(0,_ul_),_um_);}
                           var
                            _un_=
                             _cy_(_af_,urlencode_nS_(0,new MlWrappedString(_uk_[2])));
                           return _cy_(urlencode_nS_(0,_ul_),_un_);},
                         _uo_)).toString
                     ()));}
              else
               {var
                 boundary_up_=post_encode_sU_[2],
                 _us_=
                  function(data_uq_)
                   {var data_ur_=_mY_(data_uq_.join(_ap_.toString()));
                    return _mB_(req_tN_.sendAsBinary)
                            ?req_tN_.sendAsBinary(data_ur_)
                            :req_tN_.send(data_ur_);},
                 _uu_=l_ui_[1],
                 b_ut_=new array_constructor_mH_(),
                 _uZ_=
                  function(param_uv_)
                   {b_ut_.push(_cy_(_T_,_cy_(boundary_up_,_U_)).toString());
                    return b_ut_;};
                _lC_
                 (_lC_
                   (_l7_
                     (function(v_uw_)
                       {b_ut_.push(_cy_(_Y_,_cy_(boundary_up_,_Z_)).toString());
                        var _ux_=v_uw_[2],_uy_=v_uw_[1];
                        if(781515420<=_ux_[1])
                         {var
                           value_uz_=_ux_[2],
                           _uG_=-1041425454,
                           _uH_=
                            function(file_uF_)
                             {var
                               _uC_=_ad_.toString(),
                               _uB_=_ac_.toString(),
                               _uA_=_mD_(value_uz_.name);
                              if(_uA_)
                               var _uD_=_uA_[1];
                              else
                               {var
                                 _uE_=_mD_(value_uz_.fileName),
                                 _uD_=_uE_?_uE_[1]:_j_(_aq_);}
                              b_ut_.push
                               (_cy_(_aa_,_cy_(_uy_,_ab_)).toString(),_uD_,_uB_,_uC_);
                              b_ut_.push(___.toString(),file_uF_,_$_.toString());
                              return return_lx_(0);},
                           _uI_=_mD_(_mX_(_pf_));
                          if(_uI_)
                           {var
                             reader_uJ_=new (_uI_[1])(),
                             match_uK_=task_lz_(0),
                             res_uL_=match_uK_[1],
                             w_uP_=match_uK_[2];
                            reader_uJ_.onloadend=
                            handler_m4_
                             (function(param_uQ_)
                               {if(2===reader_uJ_.readyState)
                                 {var
                                   _uM_=reader_uJ_.result,
                                   _uN_=
                                    caml_equal(typeof _uM_,_ar_.toString())?_mY_(_uM_):null_l$_,
                                   _uO_=_mA_(_uN_);
                                  if(!_uO_)throw [0,_d_,_as_];
                                  wakeup_ly_(w_uP_,_uO_[1]);}
                                return _false_mF_;});
                            on_cancel_lA_
                             (res_uL_,function(param_uR_){return reader_uJ_.abort();});
                            if(typeof _uG_==="number")
                             if(-550809787===_uG_)
                              reader_uJ_.readAsDataURL(value_uz_);
                             else
                              if(936573133<=_uG_)
                               reader_uJ_.readAsText(value_uz_);
                              else
                               reader_uJ_.readAsBinaryString(value_uz_);
                            else
                             reader_uJ_.readAsText(value_uz_,_uG_[2]);
                            var _uS_=res_uL_;}
                          else
                           {var fail_uU_=function(param_uT_){return _j_(_au_);};
                            if(typeof _uG_==="number")
                             var
                              _uV_=
                               -550809787===_uG_
                                ?_mB_(value_uz_.getAsDataURL)
                                  ?value_uz_.getAsDataURL()
                                  :fail_uU_(0)
                                :936573133<=_uG_
                                  ?_mB_(value_uz_.getAsText)
                                    ?value_uz_.getAsText(_at_.toString())
                                    :fail_uU_(0)
                                  :_mB_(value_uz_.getAsBinary)
                                    ?value_uz_.getAsBinary()
                                    :fail_uU_(0);
                            else
                             {var
                               e_uW_=_uG_[2],
                               _uV_=
                                _mB_(value_uz_.getAsText)
                                 ?value_uz_.getAsText(e_uW_)
                                 :fail_uU_(0);}
                            var _uS_=return_lx_(_uV_);}
                          return _lB_(_uS_,_uH_);}
                        var value_uY_=_ux_[2],_uX_=_X_.toString();
                        b_ut_.push
                         (_cy_(_V_,_cy_(_uy_,_W_)).toString(),value_uY_,_uX_);
                        return return_lx_(0);},
                      _uu_),
                    _uZ_),
                  _us_);}}
            else
             req_tN_.send(_uh_[2]);}
          else
           req_tN_.send(null_l$_);
          on_cancel_lA_(res_tL_,function(param_u0_){return req_tN_.abort();});
          var _u2_=_lB_(_lC_(res_tL_,_sp_),start_u1_),t_u3_=_u2_;}
        catch(_u5_){var t_u3_=fail_k7_(_u5_);}
        var _u6_=repr_kp_(t_u3_)[1];
        switch(_u6_[0])
         {case 1:_ri_(_u6_[1]);break;
          case 2:
           var
            sleeper_u7_=_u6_[1],
            res_u8_=temp_k9_(sleeper_u7_[1]),
            data_u__=current_data_jO_[1];
           add_immutable_waiter_k4_
            (sleeper_u7_,
             function(state_u9_)
              {switch(state_u9_[0])
                {case 0:return fast_connect_lh_(res_u8_,state_u9_);
                 case 1:
                  var exn_u$_=state_u9_[1];
                  current_data_jO_[1]=data_u__;
                  try
                   {var _va_=_ri_(exn_u$_),_vb_=_va_;}
                  catch(_vc_){var _vb_=fail_k7_(_vc_);}
                  return connect_lg_(res_u8_,_vb_);
                 default:throw [0,_d_,_by_];}});
           break;
          case 3:throw [0,_d_,_bx_];
          default:}
        return _true_mE_;});
    do_at_exit_cN_(0);
    return;}
  ());
