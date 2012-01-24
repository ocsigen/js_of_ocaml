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
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(f.prec);
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
function caml_js_call(f, o, args) { return f.apply(o, args.slice(1)); }
var caml_js_regexps = { amp:/&/g, lt:/</g, quot:/\"/g, all:/[&<\"]/ };
function caml_js_html_escape (s) {
  if (!caml_js_regexps.all.test(s)) return s;
  return s.replace(caml_js_regexps.amp, "&amp;")
          .replace(caml_js_regexps.lt, "&lt;")
          .replace(caml_js_regexps.quot, "&quot;");
}
function caml_js_on_ie () {
  var ua = window.navigator?window.navigator.userAgent:"";
  return ua.indexOf("MSIE") != -1 && ua.indexOf("Opera") != 0;
}
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
function caml_register_global (n, v) { caml_global_data[n + 1] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_sys_get_config () {
  return [0, new MlWrappedString("Unix"), 32];
}
function caml_update_dummy (x, y) {
  if( typeof y==="function" ) { x.fun = y; return 0; }
  if( y.fun ) { x.fun = y.fun; return 0; }
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}
(function()
   {function _g7_(_pz_,_pA_,_pB_,_pC_,_pD_,_pE_,_pF_)
     {return _pz_.length==6
              ?_pz_(_pA_,_pB_,_pC_,_pD_,_pE_,_pF_)
              :caml_call_gen(_pz_,[_pA_,_pB_,_pC_,_pD_,_pE_,_pF_]);}
    function _cR_(_pv_,_pw_,_px_,_py_)
     {return _pv_.length==3
              ?_pv_(_pw_,_px_,_py_)
              :caml_call_gen(_pv_,[_pw_,_px_,_py_]);}
    function _dj_(_ps_,_pt_,_pu_)
     {return _ps_.length==2?_ps_(_pt_,_pu_):caml_call_gen(_ps_,[_pt_,_pu_]);}
    function _a6_(_pq_,_pr_)
     {return _pq_.length==1?_pq_(_pr_):caml_call_gen(_pq_,[_pr_]);}
    var
     _a_=[0,new MlString("Failure")],
     _b_=[0,new MlString("Invalid_argument")],
     _c_=[0,new MlString("Assert_failure")];
    caml_register_global(5,[0,new MlString("Division_by_zero")]);
    caml_register_global(3,_b_);
    caml_register_global(2,_a_);
    var
     _aN_=new MlString("%.12g"),
     _aM_=new MlString("."),
     _aL_=new MlString("%d"),
     _aK_=new MlString("true"),
     _aJ_=new MlString("false"),
     _aI_=new MlString("Pervasives.do_at_exit"),
     _aH_=new MlString("\\b"),
     _aG_=new MlString("\\t"),
     _aF_=new MlString("\\n"),
     _aE_=new MlString("\\r"),
     _aD_=new MlString("\\\\"),
     _aC_=new MlString("\\'"),
     _aB_=new MlString("String.blit"),
     _aA_=new MlString("String.sub"),
     _az_=new MlString("Buffer.add: cannot grow buffer"),
     _ay_=new MlString("%"),
     _ax_=new MlString(""),
     _aw_=new MlString(""),
     _av_=new MlString("\""),
     _au_=new MlString("\""),
     _at_=new MlString("'"),
     _as_=new MlString("'"),
     _ar_=new MlString("."),
     _aq_=new MlString("printf: bad positional specification (0)."),
     _ap_=new MlString("%_"),
     _ao_=[0,new MlString("printf.ml"),144,8],
     _an_=new MlString("''"),
     _am_=new MlString("Printf: premature end of format string ``"),
     _al_=new MlString("''"),
     _ak_=new MlString(" in format string ``"),
     _aj_=new MlString(", at char number "),
     _ai_=new MlString("Printf: bad conversion %"),
     _ah_=new MlString("Sformat.index_of_int: negative argument "),
     _ag_=new MlString("x"),
     _af_=[0,new MlString("src/core/lwt.ml"),573,20],
     _ae_=[0,new MlString("src/core/lwt.ml"),575,8],
     _ad_=[0,new MlString("src/core/lwt.ml"),440,20],
     _ac_=[0,new MlString("src/core/lwt.ml"),443,8],
     _ab_=new MlString("Lwt.fast_connect"),
     _aa_=new MlString("Lwt.connect"),
     _$_=new MlString("Lwt.wakeup"),
     ___=new MlString("Lwt.Canceled"),
     _Z_=new MlString("canvas"),
     _Y_=new MlString("img"),
     _X_=new MlString("br"),
     _W_=new MlString("p"),
     _V_=new MlString("div"),
     _U_=new MlString("label"),
     _T_=new MlString("input"),
     _S_=new MlString("select"),
     _R_=new MlString("option"),
     _Q_=new MlString("\""),
     _P_=new MlString(" name=\""),
     _O_=new MlString("\""),
     _N_=new MlString(" type=\""),
     _M_=new MlString("<"),
     _L_=new MlString(">"),
     _K_=new MlString(""),
     _J_=new MlString("on"),
     _I_=new MlString("mouseup"),
     _H_=new MlString("mousemove"),
     _G_=new MlString("2d"),
     _F_=new MlString("Dom_html.Canvas_not_available"),
     _E_=new MlString("lighter"),
     _D_=new MlString("copy"),
     _C_=new MlString("% 2.f"),
     _B_=new MlString(""),
     _A_=new MlString("controls"),
     _z_=new MlString("Click and drag mouse to rotate."),
     _y_=new MlString("Resume"),
     _x_=new MlString("Pause"),
     _w_=new MlString("Fixed position"),
     _v_=new MlString("Follow rotation"),
     _u_=new MlString("Reset orientation"),
     _t_=new MlString("Date:"),
     _s_=
      [0,
       new MlString("December solstice"),
       [0,new MlString("Equinox"),[0,new MlString("June solstice"),0]]],
     _r_=new MlString("Lighting"),
     _q_=new MlString("Clip"),
     _p_=new MlString("Frames per second: "),
     _o_=
      new
       MlString
       ("Credit: <a href='http://visibleearth.nasa.gov/'>Visual Earth</a>, Nasa"),
     _n_=[0,new MlString("planet.ml"),415,0],
     _m_=[0,new MlString("planet.ml"),416,0],
     _l_=[0,new MlString("planet.ml"),417,0],
     _k_=[0,new MlString("planet.ml"),418,0],
     _j_=new MlString("copy"),
     _i_=new MlString("checkbox"),
     _h_=new MlString("button"),
     v_g_=[254,0,0,1],
     _f_=new MlString("../planet/texture.jpg");
    function _e_(s_d_){throw [0,_a_,s_d_];}
    function _aP_(s_aO_){throw [0,_b_,s_aO_];}
    function _aV_(s1_aQ_,s2_aS_)
     {var
       l1_aR_=s1_aQ_.getLen(),
       l2_aT_=s2_aS_.getLen(),
       s_aU_=caml_create_string(l1_aR_+l2_aT_|0);
      caml_blit_string(s1_aQ_,0,s_aU_,0,l1_aR_);
      caml_blit_string(s2_aS_,0,s_aU_,l1_aR_,l2_aT_);
      return s_aU_;}
    function string_of_int_aX_(n_aW_){return caml_format_int(_aL_,n_aW_);}
    function do_at_exit_a2_(param_a1_)
     {var param_aY_=caml_ml_out_channels_list(0);
      for(;;)
       {if(param_aY_)
         {var l_aZ_=param_aY_[2];
          try {}catch(_a0_){}
          var param_aY_=l_aZ_;
          continue;}
        return 0;}}
    caml_register_named_value(_aI_,do_at_exit_a2_);
    function _ba_(f_a5_,a_a3_)
     {var l_a4_=a_a3_.length-1;
      if(0===l_a4_)return [0];
      var
       r_a7_=caml_make_vect(l_a4_,_a6_(f_a5_,a_a3_[0+1])),
       _a8_=1,
       _a9_=l_a4_-1|0;
      if(!(_a9_<_a8_))
       {var i_a__=_a8_;
        for(;;)
         {r_a7_[i_a__+1]=_a6_(f_a5_,a_a3_[i_a__+1]);
          var _a$_=i_a__+1|0;
          if(_a9_!==i_a__){var i_a__=_a$_;continue;}
          break;}}
      return r_a7_;}
    function _be_(n_bb_,c_bd_)
     {var s_bc_=caml_create_string(n_bb_);
      caml_fill_string(s_bc_,0,n_bb_,c_bd_);
      return s_bc_;}
    function _bj_(s_bh_,ofs_bf_,len_bg_)
     {if(0<=ofs_bf_&&0<=len_bg_&&!((s_bh_.getLen()-len_bg_|0)<ofs_bf_))
       {var r_bi_=caml_create_string(len_bg_);
        caml_blit_string(s_bh_,ofs_bf_,r_bi_,0,len_bg_);
        return r_bi_;}
      return _aP_(_aA_);}
    function _bp_(s1_bm_,ofs1_bl_,s2_bo_,ofs2_bn_,len_bk_)
     {if
       (0<=
        len_bk_&&
        0<=
        ofs1_bl_&&
        !((s1_bm_.getLen()-len_bk_|0)<ofs1_bl_)&&
        0<=
        ofs2_bn_&&
        !((s2_bo_.getLen()-len_bk_|0)<ofs2_bn_))
       return caml_blit_string(s1_bm_,ofs1_bl_,s2_bo_,ofs2_bn_,len_bk_);
      return _aP_(_aB_);}
    var
     _bq_=caml_sys_get_config(0)[2],
     _br_=caml_mul(_bq_/8|0,(1<<(_bq_-10|0))-1|0)-1|0;
    function _bw_(n_bs_)
     {var
       n_bt_=1<=n_bs_?n_bs_:1,
       n_bu_=_br_<n_bt_?_br_:n_bt_,
       s_bv_=caml_create_string(n_bu_);
      return [0,s_bv_,0,n_bu_,s_bv_];}
    function _by_(b_bx_){return _bj_(b_bx_[1],0,b_bx_[2]);}
    function _bD_(b_bz_,more_bB_)
     {var new_len_bA_=[0,b_bz_[3]];
      for(;;)
       {if(new_len_bA_[1]<(b_bz_[2]+more_bB_|0))
         {new_len_bA_[1]=2*new_len_bA_[1]|0;continue;}
        if(_br_<new_len_bA_[1])
         if((b_bz_[2]+more_bB_|0)<=_br_)new_len_bA_[1]=_br_;else _e_(_az_);
        var new_buffer_bC_=caml_create_string(new_len_bA_[1]);
        _bp_(b_bz_[1],0,new_buffer_bC_,0,b_bz_[2]);
        b_bz_[1]=new_buffer_bC_;
        b_bz_[3]=new_len_bA_[1];
        return 0;}}
    function _bH_(b_bE_,c_bG_)
     {var pos_bF_=b_bE_[2];
      if(b_bE_[3]<=pos_bF_)_bD_(b_bE_,1);
      b_bE_[1].safeSet(pos_bF_,c_bG_);
      b_bE_[2]=pos_bF_+1|0;
      return 0;}
    function _bM_(b_bK_,s_bI_)
     {var len_bJ_=s_bI_.getLen(),new_position_bL_=b_bK_[2]+len_bJ_|0;
      if(b_bK_[3]<new_position_bL_)_bD_(b_bK_,len_bJ_);
      _bp_(s_bI_,0,b_bK_[1],b_bK_[2],len_bJ_);
      b_bK_[2]=new_position_bL_;
      return 0;}
    function index_of_int_bO_(i_bN_)
     {return 0<=i_bN_?i_bN_:_e_(_aV_(_ah_,string_of_int_aX_(i_bN_)));}
    function add_int_index_bR_(i_bP_,idx_bQ_)
     {return index_of_int_bO_(i_bP_+idx_bQ_|0);}
    var _bS_=_a6_(add_int_index_bR_,1);
    function _bU_(fmt_bT_){return _bj_(fmt_bT_,0,fmt_bT_.getLen());}
    function bad_conversion_b0_(sfmt_bV_,i_bW_,c_bY_)
     {var
       _bX_=_aV_(_ak_,_aV_(sfmt_bV_,_al_)),
       _bZ_=_aV_(_aj_,_aV_(string_of_int_aX_(i_bW_),_bX_));
      return _aP_(_aV_(_ai_,_aV_(_be_(1,c_bY_),_bZ_)));}
    function bad_conversion_format_b4_(fmt_b1_,i_b3_,c_b2_)
     {return bad_conversion_b0_(_bU_(fmt_b1_),i_b3_,c_b2_);}
    function incomplete_format_b6_(fmt_b5_)
     {return _aP_(_aV_(_am_,_aV_(_bU_(fmt_b5_),_an_)));}
    function extract_format_cs_(fmt_b7_,start_cd_,stop_cf_,widths_ch_)
     {function skip_positional_spec_cc_(start_b8_)
       {if
         ((fmt_b7_.safeGet(start_b8_)-48|0)<
          0||
          9<
          (fmt_b7_.safeGet(start_b8_)-48|0))
         return start_b8_;
        var i_b9_=start_b8_+1|0;
        for(;;)
         {var _b__=fmt_b7_.safeGet(i_b9_);
          if(48<=_b__)
           {if(!(58<=_b__)){var _ca_=i_b9_+1|0,i_b9_=_ca_;continue;}
            var _b$_=0;}
          else
           if(36===_b__){var _cb_=i_b9_+1|0,_b$_=1;}else var _b$_=0;
          if(!_b$_)var _cb_=start_b8_;
          return _cb_;}}
      var
       start_ce_=skip_positional_spec_cc_(start_cd_+1|0),
       b_cg_=_bw_((stop_cf_-start_ce_|0)+10|0);
      _bH_(b_cg_,37);
      var l1_ci_=widths_ch_,l2_cj_=0;
      for(;;)
       {if(l1_ci_)
         {var
           l_ck_=l1_ci_[2],
           _cl_=[0,l1_ci_[1],l2_cj_],
           l1_ci_=l_ck_,
           l2_cj_=_cl_;
          continue;}
        var i_cm_=start_ce_,widths_cn_=l2_cj_;
        for(;;)
         {if(i_cm_<=stop_cf_)
           {var _co_=fmt_b7_.safeGet(i_cm_);
            if(42===_co_)
             {if(widths_cn_)
               {var t_cp_=widths_cn_[2];
                _bM_(b_cg_,string_of_int_aX_(widths_cn_[1]));
                var
                 i_cq_=skip_positional_spec_cc_(i_cm_+1|0),
                 i_cm_=i_cq_,
                 widths_cn_=t_cp_;
                continue;}
              throw [0,_c_,_ao_];}
            _bH_(b_cg_,_co_);
            var _cr_=i_cm_+1|0,i_cm_=_cr_;
            continue;}
          return _by_(b_cg_);}}}
    function extract_format_int_cz_
     (conv_cy_,fmt_cw_,start_cv_,stop_cu_,widths_ct_)
     {var sfmt_cx_=extract_format_cs_(fmt_cw_,start_cv_,stop_cu_,widths_ct_);
      if(78!==conv_cy_&&110!==conv_cy_)return sfmt_cx_;
      sfmt_cx_.safeSet(sfmt_cx_.getLen()-1|0,117);
      return sfmt_cx_;}
    function sub_format_cX_
     (incomplete_format_cG_,bad_conversion_format_cQ_,conv_cV_,fmt_cA_,i_cU_)
     {var len_cB_=fmt_cA_.getLen();
      function sub_fmt_cS_(c_cC_,i_cP_)
       {var close_cD_=40===c_cC_?41:125;
        function sub_cO_(j_cE_)
         {var j_cF_=j_cE_;
          for(;;)
           {if(len_cB_<=j_cF_)return _a6_(incomplete_format_cG_,fmt_cA_);
            if(37===fmt_cA_.safeGet(j_cF_))
             {var _cH_=j_cF_+1|0;
              if(len_cB_<=_cH_)
               var _cI_=_a6_(incomplete_format_cG_,fmt_cA_);
              else
               {var _cJ_=fmt_cA_.safeGet(_cH_),_cK_=_cJ_-40|0;
                if(_cK_<0||1<_cK_)
                 {var _cL_=_cK_-83|0;
                  if(_cL_<0||2<_cL_)
                   var _cM_=1;
                  else
                   switch(_cL_)
                    {case 1:var _cM_=1;break;
                     case 2:var _cN_=1,_cM_=0;break;
                     default:var _cN_=0,_cM_=0;}
                  if(_cM_){var _cI_=sub_cO_(_cH_+1|0),_cN_=2;}}
                else
                 var _cN_=0===_cK_?0:1;
                switch(_cN_)
                 {case 1:
                   var
                    _cI_=
                     _cJ_===close_cD_
                      ?_cH_+1|0
                      :_cR_(bad_conversion_format_cQ_,fmt_cA_,i_cP_,_cJ_);
                   break;
                  case 2:break;
                  default:var _cI_=sub_cO_(sub_fmt_cS_(_cJ_,_cH_+1|0)+1|0);}}
              return _cI_;}
            var _cT_=j_cF_+1|0,j_cF_=_cT_;
            continue;}}
        return sub_cO_(i_cP_);}
      return sub_fmt_cS_(conv_cV_,i_cU_);}
    function sub_format_for_printf_cY_(conv_cW_)
     {return _cR_
              (sub_format_cX_,
               incomplete_format_b6_,
               bad_conversion_format_b4_,
               conv_cW_);}
    function iter_on_format_args_dr_(fmt_cZ_,add_conv_c__,add_char_di_)
     {var lim_c0_=fmt_cZ_.getLen()-1|0;
      function scan_fmt_dk_(i_c1_)
       {var i_c2_=i_c1_;
        a:
        for(;;)
         {if(i_c2_<lim_c0_)
           {if(37===fmt_cZ_.safeGet(i_c2_))
             {var skip_c3_=0,i_c4_=i_c2_+1|0;
              for(;;)
               {if(lim_c0_<i_c4_)
                 var _c5_=incomplete_format_b6_(fmt_cZ_);
                else
                 {var _c6_=fmt_cZ_.safeGet(i_c4_);
                  if(58<=_c6_)
                   {if(95===_c6_)
                     {var _c8_=i_c4_+1|0,_c7_=1,skip_c3_=_c7_,i_c4_=_c8_;
                      continue;}}
                  else
                   if(32<=_c6_)
                    switch(_c6_-32|0)
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
                      case 13:var _c9_=i_c4_+1|0,i_c4_=_c9_;continue;
                      case 10:
                       var _c$_=_cR_(add_conv_c__,skip_c3_,i_c4_,105),i_c4_=_c$_;
                       continue;
                      default:var _da_=i_c4_+1|0,i_c4_=_da_;continue;}
                  var i_db_=i_c4_;
                  c:
                  for(;;)
                   {if(lim_c0_<i_db_)
                     var _dc_=incomplete_format_b6_(fmt_cZ_);
                    else
                     {var _dd_=fmt_cZ_.safeGet(i_db_);
                      if(126<=_dd_)
                       var _de_=0;
                      else
                       switch(_dd_)
                        {case 78:
                         case 88:
                         case 100:
                         case 105:
                         case 111:
                         case 117:
                         case 120:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,105),_de_=1;break;
                         case 69:
                         case 70:
                         case 71:
                         case 101:
                         case 102:
                         case 103:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,102),_de_=1;break;
                         case 33:
                         case 37:
                         case 44:var _dc_=i_db_+1|0,_de_=1;break;
                         case 83:
                         case 91:
                         case 115:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,115),_de_=1;break;
                         case 97:
                         case 114:
                         case 116:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,_dd_),_de_=1;
                          break;
                         case 76:
                         case 108:
                         case 110:
                          var j_df_=i_db_+1|0;
                          if(lim_c0_<j_df_)
                           {var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,105),_de_=1;}
                          else
                           {var _dg_=fmt_cZ_.safeGet(j_df_)-88|0;
                            if(_dg_<0||32<_dg_)
                             var _dh_=1;
                            else
                             switch(_dg_)
                              {case 0:
                               case 12:
                               case 17:
                               case 23:
                               case 29:
                               case 32:
                                var
                                 _dc_=
                                  _dj_
                                   (add_char_di_,_cR_(add_conv_c__,skip_c3_,i_db_,_dd_),105),
                                 _de_=1,
                                 _dh_=0;
                                break;
                               default:var _dh_=1;}
                            if(_dh_)
                             {var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,105),_de_=1;}}
                          break;
                         case 67:
                         case 99:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,99),_de_=1;break;
                         case 66:
                         case 98:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,66),_de_=1;break;
                         case 41:
                         case 125:
                          var _dc_=_cR_(add_conv_c__,skip_c3_,i_db_,_dd_),_de_=1;
                          break;
                         case 40:
                          var
                           _dc_=scan_fmt_dk_(_cR_(add_conv_c__,skip_c3_,i_db_,_dd_)),
                           _de_=1;
                          break;
                         case 123:
                          var
                           i_dl_=_cR_(add_conv_c__,skip_c3_,i_db_,_dd_),
                           j_dm_=_cR_(sub_format_for_printf_cY_,_dd_,fmt_cZ_,i_dl_),
                           i_dn_=i_dl_;
                          for(;;)
                           {if(i_dn_<(j_dm_-2|0))
                             {var
                               _do_=_dj_(add_char_di_,i_dn_,fmt_cZ_.safeGet(i_dn_)),
                               i_dn_=_do_;
                              continue;}
                            var _dp_=j_dm_-1|0,i_db_=_dp_;
                            continue c;}
                         default:var _de_=0;}
                      if(!_de_)
                       var _dc_=bad_conversion_format_b4_(fmt_cZ_,i_db_,_dd_);}
                    var _c5_=_dc_;
                    break;}}
                var i_c2_=_c5_;
                continue a;}}
            var _dq_=i_c2_+1|0,i_c2_=_dq_;
            continue;}
          return i_c2_;}}
      scan_fmt_dk_(0);
      return 0;}
    function count_arguments_of_format_dD_(fmt_dC_)
     {var ac_ds_=[0,0,0,0];
      function add_conv_dB_(skip_dx_,i_dy_,c_dt_)
       {var _du_=41!==c_dt_?1:0,_dv_=_du_?125!==c_dt_?1:0:_du_;
        if(_dv_)
         {var inc_dw_=97===c_dt_?2:1;
          if(114===c_dt_)ac_ds_[3]=ac_ds_[3]+1|0;
          if(skip_dx_)
           ac_ds_[2]=ac_ds_[2]+inc_dw_|0;
          else
           ac_ds_[1]=ac_ds_[1]+inc_dw_|0;}
        return i_dy_+1|0;}
      iter_on_format_args_dr_
       (fmt_dC_,add_conv_dB_,function(i_dz_,c_dA_){return i_dz_+1|0;});
      return ac_ds_[1];}
    function scan_positional_spec_dQ_(fmt_dE_,got_spec_dH_,n_dP_,i_dF_)
     {var _dG_=fmt_dE_.safeGet(i_dF_);
      if((_dG_-48|0)<0||9<(_dG_-48|0))return _dj_(got_spec_dH_,0,i_dF_);
      var accu_dI_=_dG_-48|0,j_dJ_=i_dF_+1|0;
      for(;;)
       {var _dK_=fmt_dE_.safeGet(j_dJ_);
        if(48<=_dK_)
         {if(!(58<=_dK_))
           {var
             _dN_=j_dJ_+1|0,
             _dM_=(10*accu_dI_|0)+(_dK_-48|0)|0,
             accu_dI_=_dM_,
             j_dJ_=_dN_;
            continue;}
          var _dL_=0;}
        else
         if(36===_dK_)
          if(0===accu_dI_)
           {var _dO_=_e_(_aq_),_dL_=1;}
          else
           {var
             _dO_=
              _dj_(got_spec_dH_,[0,index_of_int_bO_(accu_dI_-1|0)],j_dJ_+1|0),
             _dL_=1;}
         else
          var _dL_=0;
        if(!_dL_)var _dO_=_dj_(got_spec_dH_,0,i_dF_);
        return _dO_;}}
    function next_index_dT_(spec_dR_,n_dS_)
     {return spec_dR_?n_dS_:_a6_(_bS_,n_dS_);}
    function get_index_dW_(spec_dU_,n_dV_){return spec_dU_?spec_dU_[1]:n_dV_;}
    function _gY_
     (to_s_fZ_,get_out_dY_,outc_f$_,outs_f0_,flush_fD_,k_gf_,fmt_dX_)
     {var out_dZ_=_a6_(get_out_dY_,fmt_dX_);
      function pr_fC_(k_d4_,n_ge_,fmt_d0_,v_d8_)
       {var len_d3_=fmt_d0_.getLen();
        function doprn_fz_(n_f8_,i_d1_)
         {var i_d2_=i_d1_;
          for(;;)
           {if(len_d3_<=i_d2_)return _a6_(k_d4_,out_dZ_);
            var _d5_=fmt_d0_.safeGet(i_d2_);
            if(37===_d5_)
             {var
               get_arg_d9_=
                function(spec_d7_,n_d6_)
                 {return caml_array_get(v_d8_,get_index_dW_(spec_d7_,n_d6_));},
               scan_flags_ef_=
                function(spec_eh_,n_eb_,widths_ed_,i_d__)
                 {var i_d$_=i_d__;
                  for(;;)
                   {var _ea_=fmt_d0_.safeGet(i_d$_)-32|0;
                    if(!(_ea_<0||25<_ea_))
                     switch(_ea_)
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
                        return scan_positional_spec_dQ_
                                (fmt_d0_,
                                 function(wspec_ec_,i_eg_)
                                  {var _ee_=[0,get_arg_d9_(wspec_ec_,n_eb_),widths_ed_];
                                   return scan_flags_ef_
                                           (spec_eh_,next_index_dT_(wspec_ec_,n_eb_),_ee_,i_eg_);},
                                 n_eb_,
                                 i_d$_+1|0);
                       default:var _ei_=i_d$_+1|0,i_d$_=_ei_;continue;}
                    var _ej_=fmt_d0_.safeGet(i_d$_);
                    if(124<=_ej_)
                     var _ek_=0;
                    else
                     switch(_ej_)
                      {case 78:
                       case 88:
                       case 100:
                       case 105:
                       case 111:
                       case 117:
                       case 120:
                        var
                         x_el_=get_arg_d9_(spec_eh_,n_eb_),
                         s_em_=
                          caml_format_int
                           (extract_format_int_cz_(_ej_,fmt_d0_,i_d2_,i_d$_,widths_ed_),
                            x_el_),
                         _eo_=
                          cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_em_,i_d$_+1|0),
                         _ek_=1;
                        break;
                       case 69:
                       case 71:
                       case 101:
                       case 102:
                       case 103:
                        var
                         x_ep_=get_arg_d9_(spec_eh_,n_eb_),
                         s_eq_=
                          caml_format_float
                           (extract_format_cs_(fmt_d0_,i_d2_,i_d$_,widths_ed_),x_ep_),
                         _eo_=
                          cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_eq_,i_d$_+1|0),
                         _ek_=1;
                        break;
                       case 76:
                       case 108:
                       case 110:
                        var _er_=fmt_d0_.safeGet(i_d$_+1|0)-88|0;
                        if(_er_<0||32<_er_)
                         var _es_=1;
                        else
                         switch(_er_)
                          {case 0:
                           case 12:
                           case 17:
                           case 23:
                           case 29:
                           case 32:
                            var i_et_=i_d$_+1|0,_eu_=_ej_-108|0;
                            if(_eu_<0||2<_eu_)
                             var _ev_=0;
                            else
                             {switch(_eu_)
                               {case 1:var _ev_=0,_ew_=0;break;
                                case 2:
                                 var
                                  x_ex_=get_arg_d9_(spec_eh_,n_eb_),
                                  _ey_=
                                   caml_format_int
                                    (extract_format_cs_(fmt_d0_,i_d2_,i_et_,widths_ed_),x_ex_),
                                  _ew_=1;
                                 break;
                                default:
                                 var
                                  x_ez_=get_arg_d9_(spec_eh_,n_eb_),
                                  _ey_=
                                   caml_format_int
                                    (extract_format_cs_(fmt_d0_,i_d2_,i_et_,widths_ed_),x_ez_),
                                  _ew_=1;}
                              if(_ew_){var s_eA_=_ey_,_ev_=1;}}
                            if(!_ev_)
                             {var
                               x_eB_=get_arg_d9_(spec_eh_,n_eb_),
                               s_eA_=
                                caml_int64_format
                                 (extract_format_cs_(fmt_d0_,i_d2_,i_et_,widths_ed_),x_eB_);}
                            var
                             _eo_=
                              cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_eA_,i_et_+1|0),
                             _ek_=1,
                             _es_=0;
                            break;
                           default:var _es_=1;}
                        if(_es_)
                         {var
                           x_eC_=get_arg_d9_(spec_eh_,n_eb_),
                           s_eD_=
                            caml_format_int
                             (extract_format_int_cz_(110,fmt_d0_,i_d2_,i_d$_,widths_ed_),
                              x_eC_),
                           _eo_=
                            cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_eD_,i_d$_+1|0),
                           _ek_=1;}
                        break;
                       case 83:
                       case 115:
                        var x_eE_=get_arg_d9_(spec_eh_,n_eb_);
                        if(115===_ej_)
                         var x_eF_=x_eE_;
                        else
                         {var n_eG_=[0,0],_eH_=0,_eI_=x_eE_.getLen()-1|0;
                          if(!(_eI_<_eH_))
                           {var i_eJ_=_eH_;
                            for(;;)
                             {var
                               _eK_=x_eE_.safeGet(i_eJ_),
                               _eL_=
                                14<=_eK_
                                 ?34===_eK_?1:92===_eK_?1:0
                                 :11<=_eK_?13<=_eK_?1:0:8<=_eK_?1:0,
                               _eM_=_eL_?2:caml_is_printable(_eK_)?1:4;
                              n_eG_[1]=n_eG_[1]+_eM_|0;
                              var _eN_=i_eJ_+1|0;
                              if(_eI_!==i_eJ_){var i_eJ_=_eN_;continue;}
                              break;}}
                          if(n_eG_[1]===x_eE_.getLen())
                           var _eO_=x_eE_;
                          else
                           {var s__eP_=caml_create_string(n_eG_[1]);
                            n_eG_[1]=0;
                            var _eQ_=0,_eR_=x_eE_.getLen()-1|0;
                            if(!(_eR_<_eQ_))
                             {var i_eS_=_eQ_;
                              for(;;)
                               {var _eT_=x_eE_.safeGet(i_eS_),_eU_=_eT_-34|0;
                                if(_eU_<0||58<_eU_)
                                 if(-20<=_eU_)
                                  var _eV_=1;
                                 else
                                  {switch(_eU_+34|0)
                                    {case 8:
                                      s__eP_.safeSet(n_eG_[1],92);
                                      n_eG_[1]+=1;
                                      s__eP_.safeSet(n_eG_[1],98);
                                      var _eW_=1;
                                      break;
                                     case 9:
                                      s__eP_.safeSet(n_eG_[1],92);
                                      n_eG_[1]+=1;
                                      s__eP_.safeSet(n_eG_[1],116);
                                      var _eW_=1;
                                      break;
                                     case 10:
                                      s__eP_.safeSet(n_eG_[1],92);
                                      n_eG_[1]+=1;
                                      s__eP_.safeSet(n_eG_[1],110);
                                      var _eW_=1;
                                      break;
                                     case 13:
                                      s__eP_.safeSet(n_eG_[1],92);
                                      n_eG_[1]+=1;
                                      s__eP_.safeSet(n_eG_[1],114);
                                      var _eW_=1;
                                      break;
                                     default:var _eV_=1,_eW_=0;}
                                   if(_eW_)var _eV_=0;}
                                else
                                 var
                                  _eV_=
                                   (_eU_-1|0)<0||56<(_eU_-1|0)
                                    ?(s__eP_.safeSet(n_eG_[1],92),
                                      n_eG_[1]+=
                                      1,
                                      s__eP_.safeSet(n_eG_[1],_eT_),
                                      0)
                                    :1;
                                if(_eV_)
                                 if(caml_is_printable(_eT_))
                                  s__eP_.safeSet(n_eG_[1],_eT_);
                                 else
                                  {s__eP_.safeSet(n_eG_[1],92);
                                   n_eG_[1]+=1;
                                   s__eP_.safeSet(n_eG_[1],48+(_eT_/100|0)|0);
                                   n_eG_[1]+=1;
                                   s__eP_.safeSet(n_eG_[1],48+((_eT_/10|0)%10|0)|0);
                                   n_eG_[1]+=1;
                                   s__eP_.safeSet(n_eG_[1],48+(_eT_%10|0)|0);}
                                n_eG_[1]+=1;
                                var _eX_=i_eS_+1|0;
                                if(_eR_!==i_eS_){var i_eS_=_eX_;continue;}
                                break;}}
                            var _eO_=s__eP_;}
                          var x_eF_=_aV_(_au_,_aV_(_eO_,_av_));}
                        if(i_d$_===(i_d2_+1|0))
                         var s_eY_=x_eF_;
                        else
                         {var
                           _eZ_=
                            extract_format_cs_(fmt_d0_,i_d2_,i_d$_,widths_ed_);
                          try
                           {var neg_e0_=0,i_e1_=1;
                            for(;;)
                             {if(_eZ_.getLen()<=i_e1_)
                               var _e2_=[0,0,neg_e0_];
                              else
                               {var _e3_=_eZ_.safeGet(i_e1_);
                                if(49<=_e3_)
                                 if(58<=_e3_)
                                  var _e4_=0;
                                 else
                                  {var
                                    _e2_=
                                     [0,
                                      caml_int_of_string
                                       (_bj_(_eZ_,i_e1_,(_eZ_.getLen()-i_e1_|0)-1|0)),
                                      neg_e0_],
                                    _e4_=1;}
                                else
                                 {if(45===_e3_)
                                   {var _e6_=i_e1_+1|0,_e5_=1,neg_e0_=_e5_,i_e1_=_e6_;
                                    continue;}
                                  var _e4_=0;}
                                if(!_e4_){var _e7_=i_e1_+1|0,i_e1_=_e7_;continue;}}
                              var match_e8_=_e2_;
                              break;}}
                          catch(_e9_)
                           {if(_e9_[1]!==_a_)throw _e9_;
                            var match_e8_=bad_conversion_b0_(_eZ_,0,115);}
                          var
                           neg_e$_=match_e8_[2],
                           p_e__=match_e8_[1],
                           _fa_=x_eF_.getLen(),
                           _fb_=0,
                           _fe_=32;
                          if(p_e__===_fa_&&0===_fb_)
                           {var _fc_=x_eF_,_fd_=1;}
                          else
                           var _fd_=0;
                          if(!_fd_)
                           if(p_e__<=_fa_)
                            var _fc_=_bj_(x_eF_,_fb_,_fa_);
                           else
                            {var res_ff_=_be_(p_e__,_fe_);
                             if(neg_e$_)
                              _bp_(x_eF_,_fb_,res_ff_,0,_fa_);
                             else
                              _bp_(x_eF_,_fb_,res_ff_,p_e__-_fa_|0,_fa_);
                             var _fc_=res_ff_;}
                          var s_eY_=_fc_;}
                        var
                         _eo_=
                          cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_eY_,i_d$_+1|0),
                         _ek_=1;
                        break;
                       case 67:
                       case 99:
                        var x_fg_=get_arg_d9_(spec_eh_,n_eb_);
                        if(99===_ej_)
                         var s_fh_=_be_(1,x_fg_);
                        else
                         {if(39===x_fg_)
                           var _fi_=_aC_;
                          else
                           if(92===x_fg_)
                            var _fi_=_aD_;
                           else
                            {if(14<=x_fg_)
                              var _fj_=0;
                             else
                              switch(x_fg_)
                               {case 8:var _fi_=_aH_,_fj_=1;break;
                                case 9:var _fi_=_aG_,_fj_=1;break;
                                case 10:var _fi_=_aF_,_fj_=1;break;
                                case 13:var _fi_=_aE_,_fj_=1;break;
                                default:var _fj_=0;}
                             if(!_fj_)
                              if(caml_is_printable(x_fg_))
                               {var s_fk_=caml_create_string(1);
                                s_fk_.safeSet(0,x_fg_);
                                var _fi_=s_fk_;}
                              else
                               {var s_fl_=caml_create_string(4);
                                s_fl_.safeSet(0,92);
                                s_fl_.safeSet(1,48+(x_fg_/100|0)|0);
                                s_fl_.safeSet(2,48+((x_fg_/10|0)%10|0)|0);
                                s_fl_.safeSet(3,48+(x_fg_%10|0)|0);
                                var _fi_=s_fl_;}}
                          var s_fh_=_aV_(_as_,_aV_(_fi_,_at_));}
                        var
                         _eo_=
                          cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_fh_,i_d$_+1|0),
                         _ek_=1;
                        break;
                       case 66:
                       case 98:
                        var
                         _fn_=i_d$_+1|0,
                         _fm_=get_arg_d9_(spec_eh_,n_eb_)?_aK_:_aJ_,
                         _eo_=cont_s_en_(next_index_dT_(spec_eh_,n_eb_),_fm_,_fn_),
                         _ek_=1;
                        break;
                       case 40:
                       case 123:
                        var
                         xf_fo_=get_arg_d9_(spec_eh_,n_eb_),
                         j_fp_=_cR_(sub_format_for_printf_cY_,_ej_,fmt_d0_,i_d$_+1|0);
                        if(123===_ej_)
                         {var
                           b_fq_=_bw_(xf_fo_.getLen()),
                           add_char_ft_=
                            function(i_fs_,c_fr_){_bH_(b_fq_,c_fr_);return i_fs_+1|0;};
                          iter_on_format_args_dr_
                           (xf_fo_,
                            function(skip_fu_,i_fw_,c_fv_)
                             {if(skip_fu_)_bM_(b_fq_,_ap_);else _bH_(b_fq_,37);
                              return add_char_ft_(i_fw_,c_fv_);},
                            add_char_ft_);
                          var
                           _fx_=_by_(b_fq_),
                           _eo_=cont_s_en_(next_index_dT_(spec_eh_,n_eb_),_fx_,j_fp_),
                           _ek_=1;}
                        else
                         {var
                           _fy_=next_index_dT_(spec_eh_,n_eb_),
                           m_fA_=
                            add_int_index_bR_
                             (count_arguments_of_format_dD_(xf_fo_),_fy_),
                           _eo_=
                            pr_fC_
                             (function(param_fB_){return doprn_fz_(m_fA_,j_fp_);},
                              _fy_,
                              xf_fo_,
                              v_d8_),
                           _ek_=1;}
                        break;
                       case 33:
                        _a6_(flush_fD_,out_dZ_);
                        var _eo_=doprn_fz_(n_eb_,i_d$_+1|0),_ek_=1;
                        break;
                       case 37:
                        var _eo_=cont_s_en_(n_eb_,_ay_,i_d$_+1|0),_ek_=1;break;
                       case 41:
                        var _eo_=cont_s_en_(n_eb_,_ax_,i_d$_+1|0),_ek_=1;break;
                       case 44:
                        var _eo_=cont_s_en_(n_eb_,_aw_,i_d$_+1|0),_ek_=1;break;
                       case 70:
                        var x_fE_=get_arg_d9_(spec_eh_,n_eb_);
                        if(0===widths_ed_)
                         {var
                           _fF_=caml_format_float(_aN_,x_fE_),
                           i_fG_=0,
                           l_fH_=_fF_.getLen();
                          for(;;)
                           {if(l_fH_<=i_fG_)
                             var _fI_=_aV_(_fF_,_aM_);
                            else
                             {var
                               _fJ_=_fF_.safeGet(i_fG_),
                               _fK_=48<=_fJ_?58<=_fJ_?0:1:45===_fJ_?1:0;
                              if(_fK_){var _fL_=i_fG_+1|0,i_fG_=_fL_;continue;}
                              var _fI_=_fF_;}
                            var s_fM_=_fI_;
                            break;}}
                        else
                         {var
                           sfmt_fN_=
                            extract_format_cs_(fmt_d0_,i_d2_,i_d$_,widths_ed_);
                          if(70===_ej_)sfmt_fN_.safeSet(sfmt_fN_.getLen()-1|0,103);
                          var s_fO_=caml_format_float(sfmt_fN_,x_fE_);
                          if(3<=caml_classify_float(x_fE_))
                           var _fP_=s_fO_;
                          else
                           {var i_fQ_=0,l_fR_=s_fO_.getLen();
                            for(;;)
                             {if(l_fR_<=i_fQ_)
                               var _fS_=_aV_(s_fO_,_ar_);
                              else
                               {var
                                 _fT_=s_fO_.safeGet(i_fQ_)-46|0,
                                 _fU_=
                                  _fT_<0||23<_fT_
                                   ?55===_fT_?1:0
                                   :(_fT_-1|0)<0||21<(_fT_-1|0)?1:0;
                                if(!_fU_){var _fV_=i_fQ_+1|0,i_fQ_=_fV_;continue;}
                                var _fS_=s_fO_;}
                              var _fP_=_fS_;
                              break;}}
                          var s_fM_=_fP_;}
                        var
                         _eo_=
                          cont_s_en_(next_index_dT_(spec_eh_,n_eb_),s_fM_,i_d$_+1|0),
                         _ek_=1;
                        break;
                       case 97:
                        var
                         printer_fW_=get_arg_d9_(spec_eh_,n_eb_),
                         n_fX_=_a6_(_bS_,get_index_dW_(spec_eh_,n_eb_)),
                         arg_fY_=get_arg_d9_(0,n_fX_),
                         _f2_=i_d$_+1|0,
                         _f1_=next_index_dT_(spec_eh_,n_fX_);
                        if(to_s_fZ_)
                         _dj_(outs_f0_,out_dZ_,_dj_(printer_fW_,0,arg_fY_));
                        else
                         _dj_(printer_fW_,out_dZ_,arg_fY_);
                        var _eo_=doprn_fz_(_f1_,_f2_),_ek_=1;
                        break;
                       case 116:
                        var
                         printer_f3_=get_arg_d9_(spec_eh_,n_eb_),
                         _f5_=i_d$_+1|0,
                         _f4_=next_index_dT_(spec_eh_,n_eb_);
                        if(to_s_fZ_)
                         _dj_(outs_f0_,out_dZ_,_a6_(printer_f3_,0));
                        else
                         _a6_(printer_f3_,out_dZ_);
                        var _eo_=doprn_fz_(_f4_,_f5_),_ek_=1;
                        break;
                       default:var _ek_=0;}
                    if(!_ek_)
                     var _eo_=bad_conversion_format_b4_(fmt_d0_,i_d$_,_ej_);
                    return _eo_;}},
               _f__=i_d2_+1|0,
               _f7_=0;
              return scan_positional_spec_dQ_
                      (fmt_d0_,
                       function(spec_f9_,i_f6_)
                        {return scan_flags_ef_(spec_f9_,n_f8_,_f7_,i_f6_);},
                       n_f8_,
                       _f__);}
            _dj_(outc_f$_,out_dZ_,_d5_);
            var _ga_=i_d2_+1|0,i_d2_=_ga_;
            continue;}}
        function cont_s_en_(n_gd_,s_gb_,i_gc_)
         {_dj_(outs_f0_,out_dZ_,s_gb_);return doprn_fz_(n_gd_,i_gc_);}
        return doprn_fz_(n_ge_,0);}
      var
       kpr_gg_=_dj_(pr_fC_,k_gf_,index_of_int_bO_(0)),
       _gh_=count_arguments_of_format_dD_(fmt_dX_);
      if(_gh_<0||6<_gh_)
       {var
         loop_gu_=
          function(i_gi_,args_go_)
           {if(_gh_<=i_gi_)
             {var
               a_gj_=caml_make_vect(_gh_,0),
               _gm_=
                function(i_gk_,arg_gl_)
                 {return caml_array_set(a_gj_,(_gh_-i_gk_|0)-1|0,arg_gl_);},
               i_gn_=0,
               param_gp_=args_go_;
              for(;;)
               {if(param_gp_)
                 {var _gq_=param_gp_[2],_gr_=param_gp_[1];
                  if(_gq_)
                   {_gm_(i_gn_,_gr_);
                    var _gs_=i_gn_+1|0,i_gn_=_gs_,param_gp_=_gq_;
                    continue;}
                  _gm_(i_gn_,_gr_);}
                return _dj_(kpr_gg_,fmt_dX_,a_gj_);}}
            return function(x_gt_)
             {return loop_gu_(i_gi_+1|0,[0,x_gt_,args_go_]);};},
         _gv_=loop_gu_(0,0);}
      else
       switch(_gh_)
        {case 1:
          var
           _gv_=
            function(x_gx_)
             {var a_gw_=caml_make_vect(1,0);
              caml_array_set(a_gw_,0,x_gx_);
              return _dj_(kpr_gg_,fmt_dX_,a_gw_);};
          break;
         case 2:
          var
           _gv_=
            function(x_gz_,y_gA_)
             {var a_gy_=caml_make_vect(2,0);
              caml_array_set(a_gy_,0,x_gz_);
              caml_array_set(a_gy_,1,y_gA_);
              return _dj_(kpr_gg_,fmt_dX_,a_gy_);};
          break;
         case 3:
          var
           _gv_=
            function(x_gC_,y_gD_,z_gE_)
             {var a_gB_=caml_make_vect(3,0);
              caml_array_set(a_gB_,0,x_gC_);
              caml_array_set(a_gB_,1,y_gD_);
              caml_array_set(a_gB_,2,z_gE_);
              return _dj_(kpr_gg_,fmt_dX_,a_gB_);};
          break;
         case 4:
          var
           _gv_=
            function(x_gG_,y_gH_,z_gI_,t_gJ_)
             {var a_gF_=caml_make_vect(4,0);
              caml_array_set(a_gF_,0,x_gG_);
              caml_array_set(a_gF_,1,y_gH_);
              caml_array_set(a_gF_,2,z_gI_);
              caml_array_set(a_gF_,3,t_gJ_);
              return _dj_(kpr_gg_,fmt_dX_,a_gF_);};
          break;
         case 5:
          var
           _gv_=
            function(x_gL_,y_gM_,z_gN_,t_gO_,u_gP_)
             {var a_gK_=caml_make_vect(5,0);
              caml_array_set(a_gK_,0,x_gL_);
              caml_array_set(a_gK_,1,y_gM_);
              caml_array_set(a_gK_,2,z_gN_);
              caml_array_set(a_gK_,3,t_gO_);
              caml_array_set(a_gK_,4,u_gP_);
              return _dj_(kpr_gg_,fmt_dX_,a_gK_);};
          break;
         case 6:
          var
           _gv_=
            function(x_gR_,y_gS_,z_gT_,t_gU_,u_gV_,v_gW_)
             {var a_gQ_=caml_make_vect(6,0);
              caml_array_set(a_gQ_,0,x_gR_);
              caml_array_set(a_gQ_,1,y_gS_);
              caml_array_set(a_gQ_,2,z_gT_);
              caml_array_set(a_gQ_,3,t_gU_);
              caml_array_set(a_gQ_,4,u_gV_);
              caml_array_set(a_gQ_,5,v_gW_);
              return _dj_(kpr_gg_,fmt_dX_,a_gQ_);};
          break;
         default:var _gv_=_dj_(kpr_gg_,fmt_dX_,[0]);}
      return _gv_;}
    function _g2_(fmt_gX_){return _bw_(2*fmt_gX_.getLen()|0);}
    function _g4_(k_g1_,b_gZ_)
     {var s_g0_=_by_(b_gZ_);b_gZ_[2]=0;return _a6_(k_g1_,s_g0_);}
    function _g__(k_g3_)
     {var _g6_=_a6_(_g4_,k_g3_);
      return _g7_(_gY_,1,_g2_,_bH_,_bM_,function(_g5_){return 0;},_g6_);}
    function _ha_(fmt_g9_)
     {return _dj_(_g__,function(s_g8_){return s_g8_;},fmt_g9_);}
    var _g$_=[0,0];
    32===_bq_;
    function _hd_(param_hc_)
     {var seq_hb_=[];
      caml_update_dummy(seq_hb_,[0,seq_hb_,seq_hb_]);
      return seq_hb_;}
    var Canceled_he_=[0,___],current_data_hf_=[0,0],max_removed_hl_=42;
    function repr_rec_hj_(t_hg_)
     {var _hh_=t_hg_[1];
      {if(3===_hh_[0])
        {var t__hi_=_hh_[1],t___hk_=repr_rec_hj_(t__hi_);
         if(t___hk_!==t__hi_)t_hg_[1]=[3,t___hk_];
         return t___hk_;}
       return t_hg_;}}
    function repr_hn_(t_hm_){return repr_rec_hj_(t_hm_);}
    function run_waiters_hG_(waiters_ho_,state_ht_)
     {var save_hq_=current_data_hf_[1],ws_hp_=waiters_ho_,rem_hr_=0;
      for(;;)
       {if(typeof ws_hp_==="number")
         {if(rem_hr_)
           {var
             rem_hF_=rem_hr_[2],
             ws_hE_=rem_hr_[1],
             ws_hp_=ws_hE_,
             rem_hr_=rem_hF_;
            continue;}}
        else
         switch(ws_hp_[0])
          {case 1:
            var _hs_=ws_hp_[1];
            if(rem_hr_)
             {var rem_hv_=rem_hr_[2],ws_hu_=rem_hr_[1];
              _a6_(_hs_,state_ht_);
              var ws_hp_=ws_hu_,rem_hr_=rem_hv_;
              continue;}
            _a6_(_hs_,state_ht_);
            break;
           case 2:
            var
             ws1_hw_=ws_hp_[1],
             _hx_=[0,ws_hp_[2],rem_hr_],
             ws_hp_=ws1_hw_,
             rem_hr_=_hx_;
            continue;
           default:
            var _hy_=ws_hp_[1][1];
            if(_hy_)
             {var _hz_=_hy_[1];
              if(rem_hr_)
               {var rem_hB_=rem_hr_[2],ws_hA_=rem_hr_[1];
                _a6_(_hz_,state_ht_);
                var ws_hp_=ws_hA_,rem_hr_=rem_hB_;
                continue;}
              _a6_(_hz_,state_ht_);}
            else
             if(rem_hr_)
              {var
                rem_hD_=rem_hr_[2],
                ws_hC_=rem_hr_[1],
                ws_hp_=ws_hC_,
                rem_hr_=rem_hD_;
               continue;}}
        current_data_hf_[1]=save_hq_;
        return 0;}}
    function wakeup_hN_(t_hH_,v_hK_)
     {var t_hI_=repr_rec_hj_(t_hH_),_hJ_=t_hI_[1];
      switch(_hJ_[0])
       {case 1:if(_hJ_[1][1]===Canceled_he_)return 0;break;
        case 2:
         var waiters_hM_=_hJ_[1][2],state_hL_=[0,v_hK_];
         t_hI_[1]=state_hL_;
         return run_waiters_hG_(waiters_hM_,state_hL_);
        default:}
      return _aP_(_$_);}
    function append_hQ_(l1_hO_,l2_hP_)
     {return typeof l1_hO_==="number"
              ?l2_hP_
              :typeof l2_hP_==="number"?l1_hO_:[2,l1_hO_,l2_hP_];}
    function cleanup_hS_(ws_hR_)
     {if(typeof ws_hR_!=="number")
       switch(ws_hR_[0])
        {case 2:
          var l1_hT_=ws_hR_[1],_hU_=cleanup_hS_(ws_hR_[2]);
          return append_hQ_(cleanup_hS_(l1_hT_),_hU_);
         case 1:break;
         default:if(!ws_hR_[1][1])return 0;}
      return ws_hR_;}
    function fail_hW_(e_hV_){return [0,[1,e_hV_]];}
    function task_h7_(param_h6_)
     {var t_hX_=[],_h5_=0,_h4_=0;
      caml_update_dummy
       (t_hX_,
        [0,
         [2,
          [0,
           [0,
            [0,
             function(param_h3_)
              {var t_hY_=repr_rec_hj_(t_hX_),_hZ_=t_hY_[1];
               if(2===_hZ_[0])
                {var waiters_h1_=_hZ_[1][2],state_h0_=[1,[0,Canceled_he_]];
                 t_hY_[1]=state_h0_;
                 var _h2_=run_waiters_hG_(waiters_h1_,state_h0_);}
               else
                var _h2_=0;
               return _h2_;}]],
           _h4_,
           _h5_]]]);
      return [0,t_hX_,t_hX_];}
    function add_immutable_waiter_h$_(sleeper_h8_,waiter_h9_)
     {var
       _h__=
        typeof sleeper_h8_[2]==="number"
         ?[1,waiter_h9_]
         :[2,[1,waiter_h9_],sleeper_h8_[2]];
      sleeper_h8_[2]=_h__;
      return 0;}
    function bind_iz_(t_ia_,f_ih_)
     {var _ib_=repr_hn_(t_ia_)[1];
      switch(_ib_[0])
       {case 1:return fail_hW_(_ib_[1]);
        case 2:
         var
          sleeper_ic_=_ib_[1],
          _id_=[0,[2,[0,sleeper_ic_[1],0,0]]],
          data_if_=current_data_hf_[1];
         add_immutable_waiter_h$_
          (sleeper_ic_,
           function(param_ie_)
            {switch(param_ie_[0])
              {case 0:
                var v_ig_=param_ie_[1];
                current_data_hf_[1]=data_if_;
                try
                 {var _ii_=_a6_(f_ih_,v_ig_),_ij_=_ii_;}
                catch(_ik_){var _ij_=fail_hW_(_ik_);}
                var
                 t1_il_=repr_hn_(_id_),
                 t2_im_=repr_hn_(_ij_),
                 _in_=t1_il_[1];
                if(2===_in_[0])
                 {var sleeper1_io_=_in_[1];
                  if(t1_il_===t2_im_)
                   var _ip_=0;
                  else
                   {var _iq_=t2_im_[1];
                    if(2===_iq_[0])
                     {var sleeper2_ir_=_iq_[1];
                      t2_im_[1]=[3,t1_il_];
                      sleeper1_io_[1][1]=[1,sleeper2_ir_[1]];
                      var
                       waiters_is_=append_hQ_(sleeper1_io_[2],sleeper2_ir_[2]),
                       removed_it_=sleeper1_io_[3]+sleeper2_ir_[3]|0,
                       _ip_=
                        max_removed_hl_<removed_it_
                         ?(sleeper1_io_[3]=
                           0,
                           sleeper1_io_[2]=
                           cleanup_hS_(waiters_is_),
                           0)
                         :(sleeper1_io_[3]=removed_it_,sleeper1_io_[2]=waiters_is_,0);}
                    else
                     {t1_il_[1]=_iq_;
                      var _ip_=run_waiters_hG_(sleeper1_io_[2],_iq_);}}}
                else
                 var _ip_=_aP_(_aa_);
                return _ip_;
               case 1:
                var _iu_=[1,param_ie_[1]],t_iv_=repr_hn_(_id_),_iw_=t_iv_[1];
                if(2===_iw_[0])
                 {var waiters_ix_=_iw_[1][2];
                  t_iv_[1]=_iu_;
                  var _iy_=run_waiters_hG_(waiters_ix_,_iu_);}
                else
                 var _iy_=_aP_(_ab_);
                return _iy_;
               default:throw [0,_c_,_ad_];}});
         return _id_;
        case 3:throw [0,_c_,_ac_];
        default:return _a6_(f_ih_,_ib_[1]);}}
    var
     _iA_=[0],
     _iB_=[0,caml_make_vect(55,0),0],
     seed_iC_=caml_equal(_iA_,[0])?[0,0]:_iA_,
     l_iD_=seed_iC_.length-1,
     _iE_=0,
     _iF_=54;
    if(!(_iF_<_iE_))
     {var i_iG_=_iE_;
      for(;;)
       {caml_array_set(_iB_[1],i_iG_,i_iG_);
        var _iH_=i_iG_+1|0;
        if(_iF_!==i_iG_){var i_iG_=_iH_;continue;}
        break;}}
    var
     accu_iI_=[0,_ag_],
     _iJ_=0,
     _iK_=55,
     _iL_=caml_greaterequal(_iK_,l_iD_)?_iK_:l_iD_,
     _iM_=54+_iL_|0;
    if(!(_iM_<_iJ_))
     {var i_iN_=_iJ_;
      for(;;)
       {var
         j_iO_=i_iN_%55|0,
         _iP_=accu_iI_[1],
         _iQ_=
          _aV_
           (_iP_,
            string_of_int_aX_(caml_array_get(seed_iC_,caml_mod(i_iN_,l_iD_))));
        accu_iI_[1]=caml_md5_string(_iQ_,0,_iQ_.getLen());
        var _iR_=accu_iI_[1];
        caml_array_set
         (_iB_[1],
          j_iO_,
          caml_array_get(_iB_[1],j_iO_)^
          (((_iR_.safeGet(0)+(_iR_.safeGet(1)<<8)|0)+(_iR_.safeGet(2)<<16)|0)+
           (_iR_.safeGet(3)<<24)|
           0));
        var _iS_=i_iN_+1|0;
        if(_iM_!==i_iN_){var i_iN_=_iS_;continue;}
        break;}}
    _iB_[2]=0;
    var pause_hook_iV_=[0,function(_iT_){return 0;}],_iU_=_hd_(0),_iX_=[0,0];
    function _i2_(param_i0_)
     {if(_iU_[2]===_iU_)return 0;
      var tmp_iW_=_hd_(0);
      tmp_iW_[1][2]=_iU_[2];
      _iU_[2][1]=tmp_iW_[1];
      tmp_iW_[1]=_iU_[1];
      _iU_[1][2]=tmp_iW_;
      _iU_[1]=_iU_;
      _iU_[2]=_iU_;
      _iX_[1]=0;
      var curr_iY_=tmp_iW_[2];
      for(;;)
       {if(curr_iY_!==tmp_iW_)
         {if(curr_iY_[4])wakeup_hN_(curr_iY_[3],0);
          var _iZ_=curr_iY_[2],curr_iY_=_iZ_;
          continue;}
        return 0;}}
    var
     null_i1_=null,
     undefined_i3_=undefined,
     _true_i4_=true,
     _false_i5_=false,
     array_constructor_i6_=Array,
     date_constr_i7_=Date;
    _g$_[1]=
    [0,
     function(e_i8_)
      {return e_i8_ instanceof array_constructor_i6_
               ?0
               :[0,new MlWrappedString(e_i8_.toString())];},
     _g$_[1]];
    function _i__(_i9_){return _i9_;}
    function _jb_(p_i$_,n_ja_){p_i$_.appendChild(n_ja_);return 0;}
    var onIE_ji_=caml_js_on_ie(0)|0;
    function handler_jh_(f_jd_)
     {return _i__
              (caml_js_wrap_callback
                (function(e_jc_)
                  {if(e_jc_)
                    {var res_je_=_a6_(f_jd_,e_jc_);
                     if(!(res_je_|0))e_jc_.preventDefault();
                     return res_je_;}
                   var _jf_=event,res_jg_=_a6_(f_jd_,_jf_);
                   _jf_.returnValue=res_jg_;
                   return res_jg_;}));}
    var mouseup_jy_=_I_.toString(),mousemove_jx_=_H_.toString();
    function addEventListener_jw_(e_jj_,typ_jk_,h_jn_,capt_ju_)
     {if(e_jj_.addEventListener===undefined_i3_)
       {var
         ev_jl_=_J_.toString().concat(typ_jk_),
         callback_js_=
          function(e_jm_)
           {var _jr_=[0,h_jn_,e_jm_,[0]];
            return _a6_
                    (function(_jq_,_jp_,_jo_)
                      {return caml_js_call(_jq_,_jp_,_jo_);},
                     _jr_);};
        e_jj_.attachEvent(ev_jl_,callback_js_);
        return function(param_jt_)
         {return e_jj_.detachEvent(ev_jl_,callback_js_);};}
      e_jj_.addEventListener(typ_jk_,h_jn_,capt_ju_);
      return function(param_jv_)
       {return e_jj_.removeEventListener(typ_jk_,h_jn_,capt_ju_);};}
    function removeEventListener_jA_(id_jz_){return _a6_(id_jz_,0);}
    var
     _2d__jB_=_G_.toString(),
     window_jC_=window,
     document_jD_=window_jC_.document;
    function opt_iter_jG_(x_jE_,f_jF_){return x_jE_?_a6_(f_jF_,x_jE_[1]):0;}
    function createElement_jJ_(doc_jI_,name_jH_)
     {return doc_jI_.createElement(name_jH_.toString());}
    function unsafeCreateElement_jM_(doc_jL_,name_jK_)
     {return createElement_jJ_(doc_jL_,name_jK_);}
    function unsafeCreateElementEx_jX_(_type_jN_,name_jO_,doc_jQ_,elt_jP_)
     {if(0===_type_jN_&&0===name_jO_)
       return createElement_jJ_(doc_jQ_,elt_jP_);
      if(onIE_ji_)
       {var a_jR_=new array_constructor_i6_();
        a_jR_.push(_M_.toString(),elt_jP_.toString());
        opt_iter_jG_
         (_type_jN_,
          function(t_jS_)
           {a_jR_.push
             (_N_.toString(),caml_js_html_escape(t_jS_),_O_.toString());
            return 0;});
        opt_iter_jG_
         (name_jO_,
          function(n_jT_)
           {a_jR_.push
             (_P_.toString(),caml_js_html_escape(n_jT_),_Q_.toString());
            return 0;});
        a_jR_.push(_L_.toString());
        return doc_jQ_.createElement(a_jR_.join(_K_.toString()));}
      var res_jU_=createElement_jJ_(doc_jQ_,elt_jP_);
      opt_iter_jG_(_type_jN_,function(t_jV_){return res_jU_.type=t_jV_;});
      opt_iter_jG_(name_jO_,function(n_jW_){return res_jU_.name=n_jW_;});
      return res_jU_;}
    function createInput_j1_(_type_j0_,name_jZ_,doc_jY_)
     {return unsafeCreateElementEx_jX_(_type_j0_,name_jZ_,doc_jY_,_T_);}
    function createLabel_j3_(doc_j2_)
     {return unsafeCreateElement_jM_(doc_j2_,_U_);}
    function createDiv_j5_(doc_j4_)
     {return unsafeCreateElement_jM_(doc_j4_,_V_);}
    var Canvas_not_available_j6_=[0,_F_];
    window.HTMLElement===undefined_i3_;
    pause_hook_iV_[1]=
    function(param_j7_)
     {return 1===param_j7_
              ?(window_jC_.setTimeout(caml_js_wrap_callback(_i2_),0),0)
              :0;};
    var
     width_j8_=600,
     pi_j9_=4*Math.atan(1),
     obliquity_j__=23.5*pi_j9_/180,
     gamma_j$_=2,
     dark_ka_=Math.pow(0.2,gamma_j$_),
     button_type_kb_=_h_.toString();
    function toggle_button_kl_(txt1_kd_,txt2_kf_,action_kj_)
     {var
       state_kc_=[0,0],
       txt1_ke_=txt1_kd_.toString(),
       txt2_kh_=txt2_kf_.toString(),
       b_kg_=createInput_j1_([0,button_type_kb_],0,document_jD_);
      b_kg_.value=txt1_ke_;
      b_kg_.onclick=
      handler_jh_
       (function(param_kk_)
         {state_kc_[1]=1-state_kc_[1];
          var _ki_=state_kc_[1]?txt2_kh_:txt1_ke_;
          b_kg_.value=_ki_;
          _a6_(action_kj_,state_kc_[1]);
          return _true_i4_;});
      return b_kg_;}
    function checkbox_ks_(txt_kr_,checked_kn_,action_ko_)
     {var b_km_=createInput_j1_([0,_i_.toString()],0,document_jD_);
      b_km_.checked=!!checked_kn_;
      b_km_.onclick=
      handler_jh_
       (function(param_kp_)
         {_a6_(action_ko_,b_km_.checked|0);return _true_i4_;});
      var lab_kq_=createLabel_j3_(document_jD_);
      _jb_(lab_kq_,b_km_);
      _jb_(lab_kq_,document_jD_.createTextNode(txt_kr_.toString()));
      return lab_kq_;}
    function vertex_kw_(x_kv_,y_ku_,z_kt_){return [-1,x_kv_,y_ku_,z_kt_];}
    function vect_kz_(param_ky_,_kx_)
     {return [-1,
              _kx_[1]-param_ky_[1],
              _kx_[2]-param_ky_[2],
              _kx_[3]-param_ky_[3]];}
    function matrix_vect_mul_kI_(m_kE_,param_kA_)
     {var
       z_kB_=param_kA_[3],
       y_kC_=param_kA_[2],
       x_kD_=param_kA_[1],
       r3_kF_=m_kE_[3],
       r2_kG_=m_kE_[2],
       r1_kH_=m_kE_[1];
      return [-1,
              x_kD_*r1_kH_[1]+y_kC_*r1_kH_[2]+z_kB_*r1_kH_[3],
              x_kD_*r2_kG_[1]+y_kC_*r2_kG_[2]+z_kB_*r2_kG_[3],
              x_kD_*r3_kF_[1]+y_kC_*r3_kF_[2]+z_kB_*r3_kF_[3]];}
    function matrix_transp_kN_(m_kJ_)
     {var r3_kK_=m_kJ_[3],r2_kL_=m_kJ_[2],r1_kM_=m_kJ_[1];
      return [0,
              [-1,r1_kM_[1],r2_kL_[1],r3_kK_[1]],
              [-1,r1_kM_[2],r2_kL_[2],r3_kK_[2]],
              [-1,r1_kM_[3],r2_kL_[3],r3_kK_[3]]];}
    function matrix_mul_kT_(m_kQ_,m__kO_)
     {var
       m__kP_=matrix_transp_kN_(m__kO_),
       _kR_=matrix_vect_mul_kI_(m__kP_,m_kQ_[3]),
       _kS_=matrix_vect_mul_kI_(m__kP_,m_kQ_[2]);
      return [0,matrix_vect_mul_kI_(m__kP_,m_kQ_[1]),_kS_,_kR_];}
    function xz_rotation_kZ_(phi_kU_)
     {var
       cos_phi_kV_=Math.cos(phi_kU_),
       sin_phi_kW_=Math.sin(phi_kU_),
       _kX_=vertex_kw_(-sin_phi_kW_,0,cos_phi_kV_),
       _kY_=vertex_kw_(0,1,0);
      return [0,vertex_kw_(cos_phi_kV_,0,sin_phi_kW_),_kY_,_kX_];}
    function xy_rotation_k5_(phi_k0_)
     {var
       cos_phi_k1_=Math.cos(phi_k0_),
       sin_phi_k2_=Math.sin(phi_k0_),
       _k3_=vertex_kw_(0,0,1),
       _k4_=vertex_kw_(-sin_phi_k2_,cos_phi_k1_,0);
      return [0,vertex_kw_(cos_phi_k1_,sin_phi_k2_,0),_k4_,_k3_];}
    var matrix_identity_k6_=xz_rotation_kZ_(0);
    function face_k__(v1_k9_,v2_k8_,v3_k7_){return [0,v1_k9_,v2_k8_,v3_k7_];}
    function create_canvas_lc_(w_la_,h_lb_)
     {var c_k$_=unsafeCreateElement_jM_(document_jD_,_Z_);
      if(1-(c_k$_.getContext==null_i1_?1:0))
       {c_k$_.width=w_la_;c_k$_.height=h_lb_;return c_k$_;}
      throw [0,Canvas_not_available_j6_];}
    function min_lf_(u_ld_,v_le_){return u_ld_<v_le_?u_ld_:v_le_;}
    function max_li_(u_lg_,v_lh_){return u_lg_<v_lh_?v_lh_:u_lg_;}
    var
     _lj_=8,
     _lk_=12,
     t_delta_ll_=pi_j9_/_lj_,
     n_lm_=_lj_*_lk_|0,
     vertices_ln_=caml_make_vect(n_lm_+2|0,vertex_kw_(0,0,0)),
     faces_lo_=caml_make_vect(n_lm_*2|0,face_k__(0,0,0)),
     south_lp_=n_lm_+1|0;
    caml_array_set(vertices_ln_,n_lm_,vertex_kw_(0,-1,0));
    caml_array_set(vertices_ln_,south_lp_,vertex_kw_(0,1,0));
    var
     _lq_=0,
     _lr_=_lk_-1|0,
     p_delta_lw_=2*pi_j9_/_lk_,
     t_offset_ly_=(pi_j9_-t_delta_ll_)/2;
    if(!(_lr_<_lq_))
     {var i_ls_=_lq_;
      for(;;)
       {var _lt_=0,_lu_=_lj_-1|0;
        if(!(_lu_<_lt_))
         {var j_lv_=_lt_;
          for(;;)
           {var
             phi_lx_=i_ls_*p_delta_lw_,
             theta_lz_=j_lv_*t_delta_ll_-t_offset_ly_,
             k_lA_=(i_ls_*_lj_|0)+j_lv_|0;
            caml_array_set
             (vertices_ln_,
              k_lA_,
              vertex_kw_
               (Math.cos(phi_lx_)*Math.cos(theta_lz_),
                Math.sin(theta_lz_),
                Math.sin(phi_lx_)*Math.cos(theta_lz_)));
            if(0===j_lv_)
             {caml_array_set
               (faces_lo_,
                2*k_lA_|0,
                face_k__(n_lm_,k_lA_,caml_mod(k_lA_+_lj_|0,n_lm_)));
              caml_array_set
               (faces_lo_,
                (2*k_lA_|0)+1|0,
                face_k__
                 (south_lp_,
                  caml_mod((k_lA_+(2*_lj_|0)|0)-1|0,n_lm_),
                  (k_lA_+_lj_|0)-1|0));}
            else
             {caml_array_set
               (faces_lo_,
                2*k_lA_|0,
                face_k__(k_lA_,caml_mod(k_lA_+_lj_|0,n_lm_),k_lA_-1|0));
              caml_array_set
               (faces_lo_,
                (2*k_lA_|0)+1|0,
                face_k__
                 (k_lA_-1|0,
                  caml_mod(k_lA_+_lj_|0,n_lm_),
                  caml_mod((k_lA_+_lj_|0)-1|0,n_lm_)));}
            var _lB_=j_lv_+1|0;
            if(_lu_!==j_lv_){var j_lv_=_lB_;continue;}
            break;}}
        var _lC_=i_ls_+1|0;
        if(_lr_!==i_ls_){var i_ls_=_lC_;continue;}
        break;}}
    var texture_pl_=_f_.toString();
    window_jC_.onload=
    handler_jh_
     (function(param_pp_)
       {function _pc_(texture_lD_)
         {var
           w_lE_=texture_lD_.width,
           h_lF_=texture_lD_.height,
           canvas_lG_=create_canvas_lc_(w_lE_,h_lF_),
           ctx_lH_=canvas_lG_.getContext(_2d__jB_),
           _lI_=h_lF_/8|0,
           _lJ_=w_lE_/8|0,
           img_lK_=ctx_lH_.getImageData(0,0,_lJ_,_lI_),
           data_lL_=img_lK_.data,
           inv_gamma_l1_=1/gamma_j$_;
          function update_shadow_l5_(obliquity_lM_)
           {var
             _lN_=0,
             _lO_=_lI_-1|0,
             cos_obl_lV_=Math.cos(obliquity_lM_),
             sin_obl_lU_=-Math.sin(obliquity_lM_);
            if(!(_lO_<_lN_))
             {var j_lP_=_lN_;
              for(;;)
               {var _lQ_=0,_lR_=(_lJ_/2|0)-1|0;
                if(!(_lR_<_lQ_))
                 {var i_lS_=_lQ_;
                  for(;;)
                   {var
                     theta_lT_=(j_lP_/_lI_-0.5)*pi_j9_,
                     _lW_=
                      Math.cos(i_lS_/_lJ_*2*pi_j9_)*
                      Math.cos(theta_lT_)*
                      cos_obl_lV_+
                      Math.sin(theta_lT_)*
                      sin_obl_lU_,
                     k_lZ_=4*(i_lS_+j_lP_*_lJ_)|0,
                     k__lY_=4*(_lJ_-i_lS_+j_lP_*_lJ_-1)|0,
                     c_lX_=0<_lW_?dark_ka_:dark_ka_-_lW_*(1-dark_ka_)*1.2,
                     c_l0_=c_lX_<=1?c_lX_:1,
                     c_l2_=255-(255.99*Math.pow(c_l0_,inv_gamma_l1_)|0)|0;
                    data_lL_[k_lZ_+3|0]=c_l2_;
                    data_lL_[k__lY_+3|0]=c_l2_;
                    var _l3_=i_lS_+1|0;
                    if(_lR_!==i_lS_){var i_lS_=_l3_;continue;}
                    break;}}
                var _l4_=j_lP_+1|0;
                if(_lO_!==j_lP_){var j_lP_=_l4_;continue;}
                break;}}
            ctx_lH_.putImageData(img_lK_,0,0);
            ctx_lH_.globalCompositeOperation=_j_.toString();
            ctx_lH_.save();
            ctx_lH_.scale(8*(_lJ_+2|0)/_lJ_,8*(_lI_+2|0)/_lI_);
            ctx_lH_.translate(-1,-1);
            ctx_lH_.drawImage(canvas_lG_,0,0);
            return ctx_lH_.restore();}
          update_shadow_l5_(obliquity_j__);
          var
           w_l6_=texture_lD_.width,
           canvas__l7_=create_canvas_lc_(w_l6_,texture_lD_.height),
           ctx__l8_=canvas__l7_.getContext(_2d__jB_),
           no_lighting_l9_=[0,0],
           canvas_l__=create_canvas_lc_(width_j8_,width_j8_),
           canvas__l$_=create_canvas_lc_(width_j8_,width_j8_);
          _jb_(document_jD_.body,canvas_l__);
          var
           ctx_ma_=canvas_l__.getContext(_2d__jB_),
           ctx__mb_=canvas__l$_.getContext(_2d__jB_),
           r_mc_=width_j8_/2,
           tw_md_=texture_lD_.width,
           th_me_=texture_lD_.height,
           uv_mj_=
            _ba_
             (function(v_mf_)
               {var
                 y_mh_=v_mf_[2],
                 u_mg_=
                  (tw_md_-
                   Math.atan2(v_mf_[3],v_mf_[1])*
                   ((tw_md_/2-0.99)/pi_j9_)|
                   0)%
                  tw_md_,
                 v_mi_=th_me_/2+Math.asin(y_mh_)*((th_me_-0.99)/pi_j9_)|0;
                if(0<=u_mg_)
                 {if(u_mg_<tw_md_)
                   {if(0<=v_mi_)
                     {if(v_mi_<th_me_)return [0,u_mg_,v_mi_];throw [0,_c_,_k_];}
                    throw [0,_c_,_l_];}
                  throw [0,_c_,_m_];}
                throw [0,_c_,_n_];},
              vertices_ln_),
           normals_nd_=
            _ba_
             (function(param_mk_)
               {var
                 v3_mn_=param_mk_[3],
                 v2_mm_=param_mk_[2],
                 v1_ml_=caml_array_get(vertices_ln_,param_mk_[1]),
                 v2_mp_=caml_array_get(vertices_ln_,v2_mm_),
                 _mo_=vect_kz_(v1_ml_,caml_array_get(vertices_ln_,v3_mn_)),
                 _mq_=vect_kz_(v1_ml_,v2_mp_),
                 z2_mr_=_mo_[3],
                 y2_ms_=_mo_[2],
                 x2_mt_=_mo_[1],
                 z1_mu_=_mq_[3],
                 y1_mv_=_mq_[2],
                 x1_mw_=_mq_[1];
                return [-1,
                        y1_mv_*z2_mr_-y2_ms_*z1_mu_,
                        z1_mu_*x2_mt_-z2_mr_*x1_mw_,
                        x1_mw_*y2_ms_-x2_mt_*y1_mv_];},
              faces_lo_),
           face_info_nf_=
            _ba_
             (function(f_mx_)
               {var
                 v3_mA_=f_mx_[3],
                 v2_mz_=f_mx_[2],
                 match_my_=caml_array_get(uv_mj_,f_mx_[1]),
                 v1_mB_=match_my_[2],
                 u1_mC_=match_my_[1],
                 match_mD_=caml_array_get(uv_mj_,v2_mz_),
                 v2_mE_=match_mD_[2],
                 u2_mF_=match_mD_[1],
                 match_mG_=caml_array_get(uv_mj_,v3_mA_),
                 v3_mH_=match_mG_[2],
                 u3_mI_=match_mG_[1],
                 mid_mJ_=tw_md_/2;
                if(u1_mC_==0)
                 {if(mid_mJ_<u2_mF_||mid_mJ_<u3_mI_)
                   var _mK_=1;
                  else
                   {var _mL_=0,_mK_=0;}
                  if(_mK_){var u1_mM_=tw_md_-2,_mL_=1;}}
                else
                 var _mL_=0;
                if(!_mL_)var u1_mM_=u1_mC_;
                if(u2_mF_==0)
                 {if(mid_mJ_<u1_mM_||mid_mJ_<u3_mI_)
                   var _mN_=1;
                  else
                   {var _mO_=0,_mN_=0;}
                  if(_mN_){var u2_mP_=tw_md_-2,_mO_=1;}}
                else
                 var _mO_=0;
                if(!_mO_)var u2_mP_=u2_mF_;
                if(u3_mI_==0)
                 {if(mid_mJ_<u2_mP_||mid_mJ_<u1_mM_)
                   var _mQ_=1;
                  else
                   {var _mR_=0,_mQ_=0;}
                  if(_mQ_){var u3_mS_=tw_md_-2,_mR_=1;}}
                else
                 var _mR_=0;
                if(!_mR_)var u3_mS_=u3_mI_;
                var mth_mT_=th_me_-2;
                if(v1_mB_==0||mth_mT_<=v1_mB_)
                 var _mU_=0;
                else
                 {var u1_mV_=u1_mM_,_mU_=1;}
                if(!_mU_)var u1_mV_=(u2_mP_+u3_mS_)/2;
                if(v2_mE_==0||mth_mT_<=v2_mE_)
                 var _mW_=0;
                else
                 {var u2_mX_=u2_mP_,_mW_=1;}
                if(!_mW_)var u2_mX_=(u1_mV_+u3_mS_)/2;
                if(v3_mH_==0||mth_mT_<=v3_mH_)
                 var _mY_=0;
                else
                 {var u3_mZ_=u3_mS_,_mY_=1;}
                if(!_mY_)var u3_mZ_=(u2_mX_+u1_mV_)/2;
                var
                 u1_m0_=max_li_(1,u1_mV_),
                 u2_m1_=max_li_(1,u2_mX_),
                 u3_m2_=max_li_(1,u3_mZ_),
                 v1_m3_=max_li_(1,v1_mB_),
                 v2_m4_=max_li_(1,v2_mE_),
                 v3_m5_=max_li_(1,v3_mH_),
                 du2_m6_=u2_m1_-u1_m0_,
                 du3_m7_=u3_m2_-u1_m0_,
                 dv2_m8_=v2_m4_-v1_m3_,
                 dv3_m9_=v3_m5_-v1_m3_,
                 su_m__=dv2_m8_*du3_m7_-dv3_m9_*du2_m6_,
                 sv_m$_=du2_m6_*dv3_m9_-du3_m7_*dv2_m8_,
                 u_na_=max_li_(0,min_lf_(u1_m0_,min_lf_(u2_m1_,u3_m2_))-4),
                 v_nb_=max_li_(0,min_lf_(v1_m3_,min_lf_(v2_m4_,v3_m5_))-4),
                 u__nc_=
                  min_lf_(tw_md_,max_li_(u1_m0_,max_li_(u2_m1_,u3_m2_))+4);
                return [0,
                        u1_m0_,
                        v1_m3_,
                        du2_m6_/su_m__,
                        dv2_m8_/sv_m$_,
                        du3_m7_/su_m__,
                        dv3_m9_/sv_m$_,
                        u_na_,
                        v_nb_,
                        u__nc_-u_na_,
                        min_lf_(th_me_,max_li_(v1_m3_,max_li_(v2_m4_,v3_m5_))+4)-
                        v_nb_];},
              faces_lo_),
           paused_ne_=[0,0],
           follow_ng_=[0,0],
           lighting_nh_=[0,1],
           clipped_ni_=[0,1],
           obl_nj_=[0,obliquity_j__],
           m_obliq_nk_=[0,xy_rotation_k5_(-obliquity_j__)],
           m_nl_=[0,matrix_identity_k6_],
           phi_rot_nm_=[0,0],
           rateText_nn_=document_jD_.createTextNode(_B_.toString()),
           ctrl_no_=createDiv_j5_(document_jD_);
          ctrl_no_.className=_A_.toString();
          var d_np_=createDiv_j5_(document_jD_);
          _jb_(d_np_,document_jD_.createTextNode(_z_.toString()));
          _jb_(ctrl_no_,d_np_);
          var form_nq_=createDiv_j5_(document_jD_);
          function br_ns_(param_nr_)
           {return unsafeCreateElement_jM_(document_jD_,_X_);}
          _jb_
           (form_nq_,
            toggle_button_kl_
             (_x_,_y_,function(p_nt_){paused_ne_[1]=p_nt_;return 0;}));
          _jb_(form_nq_,br_ns_(0));
          _jb_
           (form_nq_,
            toggle_button_kl_
             (_v_,_w_,function(f_nu_){follow_ng_[1]=f_nu_;return 0;}));
          _jb_(form_nq_,br_ns_(0));
          var b_nv_=createInput_j1_([0,button_type_kb_],0,document_jD_);
          b_nv_.value=_u_.toString();
          b_nv_.onclick=
          handler_jh_
           (function(param_nw_)
             {m_nl_[1]=matrix_identity_k6_;
              phi_rot_nm_[1]=0;
              m_obliq_nk_[1]=xy_rotation_k5_(-obl_nj_[1]);
              return _true_i4_;});
          _jb_(form_nq_,b_nv_);
          _jb_(form_nq_,br_ns_(0));
          var lab_nx_=createLabel_j3_(document_jD_);
          _jb_(lab_nx_,document_jD_.createTextNode(_t_.toString()));
          var
           _ny_=unsafeCreateElementEx_jX_(0,0,document_jD_,_S_),
           param_nz_=_s_;
          for(;;)
           {if(param_nz_)
             {var
               l_nC_=param_nz_[2],
               a_nB_=param_nz_[1],
               _nA_=unsafeCreateElement_jM_(document_jD_,_R_);
              _jb_(_nA_,document_jD_.createTextNode(a_nB_.toString()));
              _ny_.add(_nA_,null_i1_);
              var param_nz_=l_nC_;
              continue;}
            _ny_.onchange=
            handler_jh_
             (function(param_nF_)
               {var
                 _nD_=_ny_.selectedIndex,
                 o_nE_=0===_nD_?obliquity_j__:1===_nD_?0:-obliquity_j__;
                update_shadow_l5_(o_nE_);
                obl_nj_[1]=o_nE_;
                return _true_i4_;});
            _jb_(lab_nx_,_ny_);
            _jb_(form_nq_,lab_nx_);
            _jb_(ctrl_no_,form_nq_);
            var form_nG_=createDiv_j5_(document_jD_);
            _jb_
             (form_nG_,
              checkbox_ks_
               (_r_,1,function(l_nH_){lighting_nh_[1]=l_nH_;return 0;}));
            _jb_(form_nG_,br_ns_(0));
            _jb_
             (form_nG_,
              checkbox_ks_
               (_q_,1,function(l_nI_){clipped_ni_[1]=l_nI_;return 0;}));
            _jb_(form_nG_,br_ns_(0));
            _jb_(form_nG_,document_jD_.createTextNode(_p_.toString()));
            _jb_(form_nG_,rateText_nn_);
            _jb_(ctrl_no_,form_nG_);
            _jb_(document_jD_.body,ctrl_no_);
            var _nJ_=unsafeCreateElement_jM_(document_jD_,_W_);
            _nJ_.innerHTML=_o_.toString();
            _jb_(document_jD_.body,_nJ_);
            var mx_nK_=[0,0],my_nL_=[0,0];
            canvas_l__.onmousedown=
            handler_jh_
             (function(ev_nM_)
               {mx_nK_[1]=ev_nM_.clientX;
                my_nL_[1]=ev_nM_.clientY;
                var
                 c1_n0_=
                  addEventListener_jw_
                   (document_jD_,
                    mousemove_jx_,
                    handler_jh_
                     (function(ev_nN_)
                       {var
                         x_nO_=ev_nN_.clientX,
                         y_nP_=ev_nN_.clientY,
                         dx_nQ_=x_nO_-mx_nK_[1]|0,
                         dy_nR_=y_nP_-my_nL_[1]|0;
                        if(0!==dy_nR_)
                         {var
                           _nT_=m_nl_[1],
                           _nS_=2*dy_nR_/width_j8_,
                           cos_phi_nU_=Math.cos(_nS_),
                           sin_phi_nV_=Math.sin(_nS_),
                           _nW_=vertex_kw_(0,-sin_phi_nV_,cos_phi_nU_),
                           _nX_=vertex_kw_(0,cos_phi_nU_,sin_phi_nV_);
                          m_nl_[1]=
                          matrix_mul_kT_([0,vertex_kw_(1,0,0),_nX_,_nW_],_nT_);}
                        if(0!==dx_nQ_)
                         {var _nY_=m_nl_[1];
                          m_nl_[1]=
                          matrix_mul_kT_(xz_rotation_kZ_(2*dx_nQ_/width_j8_),_nY_);}
                        mx_nK_[1]=x_nO_;
                        my_nL_[1]=y_nP_;
                        return _true_i4_;}),
                    _true_i4_),
                 c2_nZ_=[0,null_i1_];
                c2_nZ_[1]=
                _i__
                 (addEventListener_jw_
                   (document_jD_,
                    mouseup_jy_,
                    handler_jh_
                     (function(param_n2_)
                       {removeEventListener_jA_(c1_n0_);
                        var _n1_=c2_nZ_[1];
                        if(_n1_!=null_i1_)removeEventListener_jA_(_n1_);
                        return _true_i4_;}),
                    _true_i4_));
                return _false_i5_;});
            var
             ti_n3_=[0,new date_constr_i7_().getTime()],
             fps_n4_=[0,0],
             loop_oU_=
              function(t_oO_,phi_n5_)
               {var rotation_n7_=xz_rotation_kZ_(phi_n5_-phi_rot_nm_[1]);
                if(lighting_nh_[1])
                 {no_lighting_l9_[1]=0;
                  ctx__l8_.drawImage(texture_lD_,0,0);
                  var
                   i_n6_=
                    (2*pi_j9_-phi_n5_%(2*pi_j9_))*w_l6_/2/pi_j9_%w_l6_|0;
                  ctx__l8_.drawImage(canvas_lG_,i_n6_,0);
                  ctx__l8_.drawImage(canvas_lG_,i_n6_-w_l6_,0);}
                else
                 if(!no_lighting_l9_[1])
                  {ctx__l8_.drawImage(texture_lD_,0,0);no_lighting_l9_[1]=1;}
                var
                 m_n8_=
                  matrix_mul_kT_
                   (m_nl_[1],matrix_mul_kT_(m_obliq_nk_[1],rotation_n7_)),
                 _n__=
                  _ba_
                   (function(v_n9_){return matrix_vect_mul_kI_(m_n8_,v_n9_);},
                    vertices_ln_),
                 _n$_=matrix_vect_mul_kI_(matrix_transp_kN_(m_n8_),v_g_);
                ctx__mb_.clearRect(0,0,width_j8_,width_j8_);
                ctx__mb_.save();
                if(clipped_ni_[1])
                 {ctx__mb_.beginPath();
                  ctx__mb_.arc(r_mc_,r_mc_,r_mc_*0.95,0,-2*pi_j9_,_true_i4_);
                  ctx__mb_.clip();}
                ctx__mb_.setTransform(r_mc_-2,0,0,r_mc_-2,r_mc_,r_mc_);
                ctx__mb_.globalCompositeOperation=_E_.toString();
                var _oa_=0,_ob_=faces_lo_.length-1-1|0;
                if(!(_ob_<_oa_))
                 {var i_oc_=_oa_;
                  for(;;)
                   {var
                     _od_=faces_lo_[i_oc_+1],
                     v3_og_=_od_[3],
                     v2_of_=_od_[2],
                     match_oe_=caml_array_get(_n__,_od_[1]),
                     y1_oh_=match_oe_[2],
                     x1_oi_=match_oe_[1],
                     match_oj_=caml_array_get(_n__,v2_of_),
                     y2_ok_=match_oj_[2],
                     x2_ol_=match_oj_[1],
                     match_om_=caml_array_get(_n__,v3_og_),
                     y3_on_=match_om_[2],
                     x3_oo_=match_om_[1],
                     _op_=caml_array_get(normals_nd_,i_oc_);
                    if(0<=_op_[1]*_n$_[1]+_op_[2]*_n$_[2]+_op_[3]*_n$_[3])
                     {ctx__mb_.beginPath();
                      ctx__mb_.moveTo(x1_oi_,y1_oh_);
                      ctx__mb_.lineTo(x2_ol_,y2_ok_);
                      ctx__mb_.lineTo(x3_oo_,y3_on_);
                      ctx__mb_.closePath();
                      ctx__mb_.save();
                      ctx__mb_.clip();
                      var
                       match_oq_=caml_array_get(face_info_nf_,i_oc_),
                       dv_or_=match_oq_[10],
                       du_os_=match_oq_[9],
                       v_ot_=match_oq_[8],
                       u_ou_=match_oq_[7],
                       dv3_ov_=match_oq_[6],
                       du3_ow_=match_oq_[5],
                       dv2_ox_=match_oq_[4],
                       du2_oy_=match_oq_[3],
                       v1_oz_=match_oq_[2],
                       u1_oA_=match_oq_[1],
                       dx2_oB_=x2_ol_-x1_oi_,
                       dx3_oC_=x3_oo_-x1_oi_,
                       dy2_oD_=y2_ok_-y1_oh_,
                       dy3_oE_=y3_on_-y1_oh_,
                       a_oF_=dx2_oB_*dv3_ov_-dx3_oC_*dv2_ox_,
                       b_oG_=dx2_oB_*du3_ow_-dx3_oC_*du2_oy_,
                       d_oH_=dy2_oD_*dv3_ov_-dy3_oE_*dv2_ox_,
                       e_oI_=dy2_oD_*du3_ow_-dy3_oE_*du2_oy_;
                      ctx__mb_.transform
                       (a_oF_,
                        d_oH_,
                        b_oG_,
                        e_oI_,
                        x1_oi_-a_oF_*u1_oA_-b_oG_*v1_oz_,
                        y1_oh_-d_oH_*u1_oA_-e_oI_*v1_oz_);
                      ctx__mb_.drawImage
                       (canvas__l7_,
                        u_ou_,
                        v_ot_,
                        du_os_,
                        dv_or_,
                        u_ou_,
                        v_ot_,
                        du_os_,
                        dv_or_);
                      ctx__mb_.restore();}
                    var _oJ_=i_oc_+1|0;
                    if(_ob_!==i_oc_){var i_oc_=_oJ_;continue;}
                    break;}}
                ctx__mb_.restore();
                ctx_ma_.globalCompositeOperation=_D_.toString();
                ctx_ma_.drawImage(canvas__l$_,0,0);
                try {ctx_ma_.getImageData(0,0,1,1);}catch(_pa_){}
                var
                 t__oK_=new date_constr_i7_().getTime(),
                 hz_oL_=1000/(t__oK_-ti_n3_[1]),
                 _oM_=fps_n4_[1]==0?hz_oL_:0.9*fps_n4_[1]+0.1*hz_oL_;
                fps_n4_[1]=_oM_;
                rateText_nn_.data=_dj_(_ha_,_C_,fps_n4_[1]).toString();
                ti_n3_[1]=t__oK_;
                function _oX_(param_oV_)
                 {var
                   t__oN_=new date_constr_i7_().getTime(),
                   dt_oP_=t__oN_-t_oO_,
                   dt_oQ_=dt_oP_<0?0:1000<dt_oP_?0:dt_oP_,
                   angle_oR_=2*pi_j9_*dt_oQ_/1000/10,
                   _oS_=
                    paused_ne_[1]
                     ?0
                     :follow_ng_[1]?(phi_rot_nm_[1]=phi_rot_nm_[1]+angle_oR_,1):0;
                  _oS_;
                  var _oT_=paused_ne_[1]?phi_n5_:phi_n5_+angle_oR_;
                  return loop_oU_(t__oN_,_oT_);}
                var
                 match_oW_=task_h7_(0),
                 w_oZ_=match_oW_[2],
                 t_oY_=match_oW_[1],
                 _o1_=0.01*1000,
                 id_o2_=
                  window_jC_.setTimeout
                   (caml_js_wrap_callback
                     (function(param_o0_){return wakeup_hN_(w_oZ_,0);}),
                    _o1_);
                function _o4_(param_o3_)
                 {return window_jC_.clearTimeout(id_o2_);}
                var _o5_=repr_hn_(t_oY_)[1];
                switch(_o5_[0])
                 {case 1:
                   var _o6_=_o5_[1][1]===Canceled_he_?(_o4_(0),1):0;break;
                  case 2:
                   var sleeper_o$_=_o5_[1],data_o8_=current_data_hf_[1];
                   add_immutable_waiter_h$_
                    (sleeper_o$_,
                     function(param_o7_)
                      {if(1===param_o7_[0]&&param_o7_[1][1]===Canceled_he_)
                        {current_data_hf_[1]=data_o8_;
                         try {var _o9_=_o4_(0);}catch(_o__){return 0;}
                         return _o9_;}
                       return 0;});
                   var _o6_=1;
                   break;
                  default:var _o6_=0;}
                _o6_;
                return bind_iz_(t_oY_,_oX_);};
            return loop_oU_(new date_constr_i7_().getTime(),0);}}
        var _pb_=unsafeCreateElement_jM_(document_jD_,_Y_);
        function _pf_(param_pd_){return [0,[0,_pb_]];}
        var match_pe_=task_h7_(0),w_ph_=match_pe_[2],t_pi_=match_pe_[1];
        function cont_pj_(x_pg_){return wakeup_hN_(w_ph_,x_pg_);}
        _pb_.onload=
        handler_jh_(function(param_pk_){cont_pj_(0);return _false_i5_;});
        _pb_.src=texture_pl_;
        var _pm_=repr_hn_(bind_iz_(bind_iz_(t_pi_,_pf_),_pc_))[1];
        switch(_pm_[0])
         {case 1:throw _pm_[1];
          case 2:
           var sleeper_po_=_pm_[1];
           add_immutable_waiter_h$_
            (sleeper_po_,
             function(param_pn_)
              {switch(param_pn_[0])
                {case 0:return 0;
                 case 1:throw param_pn_[1];
                 default:throw [0,_c_,_af_];}});
           break;
          case 3:throw [0,_c_,_ae_];
          default:}
        return _false_i5_;});
    do_at_exit_a2_(0);
    return;}
  ());
