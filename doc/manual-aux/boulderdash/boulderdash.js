// This program was compiled from OCaml by js_of_ocaml 0.1
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}
function MlString(param) {
  if (param != null) {
    this.bytes = param;
    this.last = this.len = param.length;
  }
}
MlString.prototype = {
  string:null,
  bytes:null,
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
    this.bytes = b;
    this.last = this.len = b.length;
    return b;
  },
  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },
  getFullBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    return b;
  },
  toArray:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = null;
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
    if (len != null) return len;
    this.toBytes();
    return this.len;
  },
  toString:function() { var s = this.string; return s?s:this.toJsString(); },
  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },
  blit:function(i1, s2, i2, l) {
    if (l == 0) return 0;
    if (s2.bytes != null && i2 == s2.last && this.len == l && this.last == l) {
      s2.bytes += this.getBytes();
      s2.last += l;
      return 0;
    }
    var a = s2.array;
    if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
    this.blitToArray (i1, a, i2, l);
  },
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
      this.bytes = this.string = null;
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
      this.bytes = this.string = null;
    }
    var l = ofs + len;
    for (i = ofs; i < l; i++) a[i] = c;
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
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlWrappedString (msg));
}
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}
function caml_array_get (array, index) {
  var res = array[index+1];
  if (res == undefined) caml_array_bound_error();
  return res;
}
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}
function caml_blit_string(s1, i1, s2, i2, len) { s1.blit (i1, s2, i2, len); }
function caml_call_gen(f, args) {
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
function caml_create_string(len) { return new MlMakeString(len); }
function caml_raise_constant (tag) { throw [0, tag]; }
var caml_global_data = [];
function caml_raise_zero_divide () {
  caml_raise_constant(caml_global_data[5]);
}
function caml_div(x,y) {
  if (y == 0) caml_raise_zero_divide ();
  return (x/y)|0;
}
function caml_int64_compare(x,y) {
  x3 = x[3] << 16;
  y3 = y[3] << 16;
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
  if (a === b && total) return 0;
  if (a instanceof MlString) {
    if (b instanceof MlString)
      return (a == b)?0:a.compare(b)
    else
      return 1;
  } else if (a instanceof Array && a[0] == (a[0]|0)) {
    var ta = a[0];
    if (ta === 250) return caml_compare_val (a[1], b, total);
    if (b instanceof Array && b[0] == (b[0]|0)) {
      var tb = b[0];
      if (tb === 250) return caml_compare_val (a, b[1], total);
      if (ta != tb) return (ta < tb)?-1:1;
      switch (ta) {
      case 248:
        return caml_int_compare(a[2], b[2]);
      case 255:
        return caml_int64_compare(a, b);
      default:
        if (a.length != b.length) return (a.length < b.length)?-1:1;
        for (var i = 1; i < a.length; i++) {
          var t = caml_compare_val (a[i], b[i], total);
          if (t != 0) return t;
        }
        return 0;
      }
    } else
      return 1;
  } else if (b instanceof MlString || (b instanceof Array && b[0] == (b[0]|0)))
    return -1;
  else {
    if (a < b) return -1;
    if (a > b) return 1;
    if (a != b) {
      if (!total) return null;
      if (a == a) return 1;
      if (b == b) return -1;
    }
    return 0;
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
          i = s.length - 1; while (s.charAt(i) == '0') i--;
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
function caml_int64_is_negative(x) {
  return (x[3] << 16) < 0;
}
function caml_int64_neg (x) {
  y1 = - x[1];
  y2 = - x[2] + (y1 >> 24);
  y3 = - x[3] + (y2 >> 24);
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
  z1 = x[1] - y[1];
  z2 = x[2] - y[2] + (z1 >> 24);
  z3 = x[3] - y[3] + (z2 >> 24);
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
    buffer = f.cvtbl.charAt(caml_int64_to_int32(p[2])) + buffer;
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
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[2], msg);
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
  return sign * res;
}
function caml_is_printable(c) { return +(c > 31 && c < 127); }
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
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    return caml_call_gen(f, args);
  }
}
function caml_make_vect (len, init) {
  var b = [0]; for (var i = 1; i <= len; i++) b[i] = init; return b;
}
function caml_ml_out_channels_list () { return 0; }
function caml_mod(x,y) {
  if (y == 0) caml_raise_zero_divide ();
  return x%y;
}
function caml_mul(x,y) {
  return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0;
}
function caml_register_global (n, v) { caml_global_data[n] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_string_notequal(s1, s2) { return +!s1.equal(s2); }
function caml_sys_get_config (e) { return [0, "Unix", 32]; }
var caml_initial_time = Date.now() * 0.001;
function caml_sys_time () { return Date.now() * 0.001 - caml_initial_time; }
function caml_update_dummy (x, y) {
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}
(function(){function hP(p9,p_,p$,qa,qb,qc,qd){return p9.length==6?p9(p_,p$,qa,qb,qc,qd):caml_call_gen(p9,[p_,p$,qa,qb,qc,qd]);}function nC(p4,p5,p6,p7,p8){return p4.length==4?p4(p5,p6,p7,p8):caml_call_gen(p4,[p5,p6,p7,p8]);}function dA(p0,p1,p2,p3){return p0.length==3?p0(p1,p2,p3):caml_call_gen(p0,[p1,p2,p3]);}function d4(pX,pY,pZ){return pX.length==2?pX(pY,pZ):caml_call_gen(pX,[pY,pZ]);}function bt(pV,pW){return pV.length==1?pV(pW):caml_call_gen(pV,[pW]);}var a=[0,new MlString("Failure")],b=[0,new MlString("Invalid_argument")],c=[0,new MlString("Assert_failure")],d=new MlString("select");caml_register_global(5,[0,new MlString("Division_by_zero")]);caml_register_global(3,b);caml_register_global(2,a);var ba=new MlString("%.12g"),a$=new MlString("."),a_=new MlString("%d"),a9=new MlString("true"),a8=new MlString("false"),a7=new MlString("Pervasives.do_at_exit"),a6=new MlString("nth"),a5=new MlString("List.nth"),a4=new MlString("\\b"),a3=new MlString("\\t"),a2=new MlString("\\n"),a1=new MlString("\\r"),a0=new MlString("\\\\"),aZ=new MlString("\\'"),aY=new MlString("String.blit"),aX=new MlString("String.sub"),aW=new MlString("Buffer.add: cannot grow buffer"),aV=new MlString("%"),aU=new MlString(""),aT=new MlString(""),aS=new MlString("\""),aR=new MlString("\""),aQ=new MlString("'"),aP=new MlString("'"),aO=new MlString("."),aN=new MlString("nan"),aM=new MlString("printf: bad positional specification (0)."),aL=new MlString("%_"),aK=[0,new MlString("printf.ml"),143,8],aJ=new MlString("''"),aI=new MlString("Printf: premature end of format string ``"),aH=new MlString("''"),aG=new MlString(" in format string ``"),aF=new MlString(", at char number "),aE=new MlString("Printf: bad conversion %"),aD=new MlString("Sformat.index_of_int: negative argument "),aC=new MlString("Lwt_sequence.Empty"),aB=[0,new MlString("src/core/lwt.ml"),356,20],aA=[0,new MlString("src/core/lwt.ml"),359,8],az=[0,new MlString("src/core/lwt.ml"),315,20],ay=[0,new MlString("src/core/lwt.ml"),318,8],ax=new MlString("Lwt.fast_connect"),aw=new MlString("Lwt.connect"),av=new MlString("Lwt.wakeup"),au=new MlString("Lwt.Canceled"),at=new MlString("table"),as=new MlString("img"),ar=new MlString("br"),aq=new MlString("h1"),ap=new MlString("div"),ao=new MlString("option"),an=new MlString("\""),am=new MlString(" name=\""),al=new MlString("\""),ak=new MlString(" type=\""),aj=new MlString("<"),ai=new MlString(">"),ah=new MlString(""),ag=new MlString("application/x-www-form-urlencoded"),af=new MlString("Content-type"),ae=new MlString("GET"),ad=new MlString("POST"),ac=new MlString("Msxml2.XMLHTTP"),ab=new MlString("Msxml3.XMLHTTP"),aa=new MlString("Microsoft.XMLHTTP"),$=[0,new MlString("xmlHttpRequest.ml"),56,2],_=new MlString("../files/boulderdash/sprites/guy.png"),Z=new MlString("../files/boulderdash/sprites/boulder.png"),Y=new MlString("index out of bounds"),X=new MlString("YOU WIN !"),W=new MlString("YOU LOSE !"),V=new MlString("../files/boulderdash/sprites/end.png"),U=new MlString("../files/boulderdash/sprites/R.png"),T=new MlString("../files/boulderdash/sprites/L.png"),S=new MlString("../files/boulderdash/sprites/U.png"),R=new MlString("../files/boulderdash/sprites/D.png"),Q=new MlString("../files/boulderdash/sprites/push_r.png"),P=new MlString("../files/boulderdash/sprites/bR.png"),O=new MlString("../files/boulderdash/sprites/push_l.png"),N=new MlString("../files/boulderdash/sprites/bL.png"),M=new MlString("eos"),L=new MlString("eos"),K=new MlString("eos"),J=new MlString("%g"),I=new MlString("1"),H=new MlString("malformed level"),G=new MlString("border-collapse:collapse;line-height: 0; opacity: 0; margin-left:auto; margin-right:auto"),F=new MlString("padding: 0; width: 20px; height: 20px;"),E=new MlString("font-family: sans-serif; text-align: center; background-color: #e8e8e8;"),D=new MlString("Boulder Dash in Ocaml"),C=new MlString("Elapsed time: "),B=new MlString(" Remaining diamonds: "),A=new MlString(" "),z=new MlString("Choose a level"),y=[0,new MlString("boulderdash.ml"),285,17],x=new MlString("boulderdash"),w=new MlString("--"),v=new MlString("../files/boulderdash/maps.txt"),u=new MlString("../files/boulderdash/sprites/empty.png"),t=new MlString("../files/boulderdash/sprites/grass.png"),s=new MlString("../files/boulderdash/sprites/diamond.png"),r=new MlString("../files/boulderdash/sprites/boulder.png"),q=new MlString("../files/boulderdash/sprites/door.png"),p=new MlString("../files/boulderdash/sprites/end.png"),o=new MlString("../files/boulderdash/sprites/guy.png"),n=new MlString("../files/boulderdash/sprites/wall.png"),m=new MlString("../files/boulderdash/sprites/bam.png"),l=new MlString("%02d:%02d:%02d"),k=new MlString("--:--:--"),j=new MlString("LOADING..."),i=new MlString("border: 1px black solid; background-color: white ; display: inline ; padding-right: .5em; padding-left: .5em;"),h=new MlString("background-color: red; color: white; display:inline; position: absolute; top:0; right:0;"),g=new MlString("Boulderdash.Death");function f(e){throw [0,a,e];}function bc(bb){throw [0,b,bb];}function bi(bd,bf){var be=bd.getLen(),bg=bf.getLen(),bh=caml_create_string(be+bg|0);caml_blit_string(bd,0,bh,0,be);caml_blit_string(bf,0,bh,be,bg);return bh;}function bk(bj){return caml_format_int(a_,bj);}function bp(bo){var bl=caml_ml_out_channels_list(0);for(;;){if(bl){var bm=bl[2];try {}catch(bn){}var bl=bm;continue;}return 0;}}caml_register_named_value(a7,bp);function bz(bs,bq){var br=bq.length-1;if(0===br)return [0];var bu=caml_make_vect(br,bt(bs,bq[0+1])),bv=1,bw=br-1|0;if(bv<=bw){var bx=bv;for(;;){bu[bx+1]=bt(bs,bq[bx+1]);var by=bx+1|0;if(bw!==bx){var bx=by;continue;}break;}}return bu;}function bM(bA){if(bA){var bD=bA[2],bC=bA[1],bB=0,bE=bA;for(;;){if(bE){var bG=bE[2],bF=bB+1|0,bB=bF,bE=bG;continue;}var bH=caml_make_vect(bB,bC),bI=1,bJ=bD;for(;;){if(bJ){var bK=bJ[2];bH[bI+1]=bJ[1];var bL=bI+1|0,bI=bL,bJ=bK;continue;}return bH;}}}return [0];}function bS(bN){var bO=bN,bP=0;for(;;){if(bO){var bQ=bO[2],bR=[0,bO[1],bP],bO=bQ,bP=bR;continue;}return bP;}}function bW(bU,bT){if(bT){var bV=bT[2],bX=bt(bU,bT[1]);return [0,bX,bW(bU,bV)];}return 0;}function b1(bY,b0){var bZ=caml_create_string(bY);caml_fill_string(bZ,0,bY,b0);return bZ;}function b6(b4,b2,b3){if(0<=b2&&0<=b3&&((b4.getLen()-b3|0)<b2?0:1)){var b5=caml_create_string(b3);caml_blit_string(b4,b2,b5,0,b3);return b5;}return bc(aX);}function ca(b9,b8,b$,b_,b7){if(0<=b7&&0<=b8&&b8<=(b9.getLen()-b7|0)&&0<=b_&&((b$.getLen()-b7|0)<b_?0:1))return caml_blit_string(b9,b8,b$,b_,b7);return bc(aY);}var cb=caml_sys_get_config(0)[2],cc=caml_mul(caml_div(cb,8),(1<<(cb-10|0))-1|0)-1|0;function ch(cd){var ce=1<=cd?cd:1,cf=cc<ce?cc:ce,cg=caml_create_string(cf);return [0,cg,0,cf,cg];}function cj(ci){return b6(ci[1],0,ci[2]);}function co(ck,cm){var cl=[0,ck[3]];for(;;){if(cl[1]<(ck[2]+cm|0)){cl[1]=caml_mul(2,cl[1]);continue;}if(cc<cl[1])if((ck[2]+cm|0)<=cc)cl[1]=cc;else f(aW);var cn=caml_create_string(cl[1]);ca(ck[1],0,cn,0,ck[2]);ck[1]=cn;ck[3]=cl[1];return 0;}}function cs(cp,cr){var cq=cp[2];if(cp[3]<=cq)co(cp,1);cp[1].safeSet(cq,cr);cp[2]=cq+1|0;return 0;}function cx(cv,ct){var cu=ct.getLen(),cw=cv[2]+cu|0;if(cv[3]<cw)co(cv,cu);ca(ct,0,cv[1],cv[2],cu);cv[2]=cw;return 0;}function cz(cy){if(0<=cy)return cy;return f(bi(aD,bk(cy)));}function cC(cA,cB){return cz(cA+cB|0);}var cD=bt(cC,1);function cF(cE){return b6(cE,0,cE.getLen());}function cL(cG,cH,cJ){var cI=bi(aG,bi(cG,aH)),cK=bi(aF,bi(bk(cH),cI));return bc(bi(aE,bi(b1(1,cJ),cK)));}function cP(cM,cO,cN){return cL(cF(cM),cO,cN);}function cR(cQ){return bc(bi(aI,bi(cF(cQ),aJ)));}function da(cS,c0,c2,c4){function cZ(cT){if((cS.safeGet(cT)-48|0)<0||9<(cS.safeGet(cT)-48|0))return cT;var cU=cT+1|0;for(;;){var cV=cS.safeGet(cU);if(48<=cV){if(cV<58){var cX=cU+1|0,cU=cX;continue;}var cW=0;}else if(36===cV){var cY=cU+1|0,cW=1;}else var cW=0;if(!cW)var cY=cT;return cY;}}var c1=cZ(c0+1|0),c3=ch((c2-c1|0)+10|0);cs(c3,37);var c6=bS(c4),c5=c1,c7=c6;for(;;){if(c5<=c2){var c8=cS.safeGet(c5);if(42===c8){if(c7){var c9=c7[2];cx(c3,bk(c7[1]));var c_=cZ(c5+1|0),c5=c_,c7=c9;continue;}throw [0,c,aK];}cs(c3,c8);var c$=c5+1|0,c5=c$;continue;}return cj(c3);}}function dh(dg,de,dd,dc,db){var df=da(de,dd,dc,db);if(78!==dg&&110!==dg)return df;df.safeSet(df.getLen()-1|0,117);return df;}function dG(dp,dz,dE,di,dD){var dj=di.getLen();function dB(dk,dy){var dl=40===dk?41:125;function dx(dm){var dn=dm;for(;;){if(dj<=dn)return bt(dp,di);if(37===di.safeGet(dn)){var dq=dn+1|0;if(dj<=dq)var dr=bt(dp,di);else{var ds=di.safeGet(dq),dt=ds-40|0;if(dt<0||1<dt){var du=dt-83|0;if(du<0||2<du)var dv=1;else switch(du){case 1:var dv=1;break;case 2:var dw=1,dv=0;break;default:var dw=0,dv=0;}if(dv){var dr=dx(dq+1|0),dw=2;}}else var dw=0===dt?0:1;switch(dw){case 1:var dr=ds===dl?dq+1|0:dA(dz,di,dy,ds);break;case 2:break;default:var dr=dx(dB(ds,dq+1|0)+1|0);}}return dr;}var dC=dn+1|0,dn=dC;continue;}}return dx(dy);}return dB(dE,dD);}function dH(dF){return dA(dG,cR,cP,dF);}function ea(dI,dT,d3){var dJ=dI.getLen()-1|0;function d5(dK){var dL=dK;a:for(;;){if(dL<dJ){if(37===dI.safeGet(dL)){var dM=0,dN=dL+1|0;for(;;){if(dJ<dN)var dO=cR(dI);else{var dP=dI.safeGet(dN);if(58<=dP){if(95===dP){var dR=dN+1|0,dQ=1,dM=dQ,dN=dR;continue;}}else if(32<=dP)switch(dP-32|0){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 0:case 3:case 11:case 13:var dS=dN+1|0,dN=dS;continue;case 10:var dU=dA(dT,dM,dN,105),dN=dU;continue;default:var dV=dN+1|0,dN=dV;continue;}var dW=dN;c:for(;;){if(dJ<dW)var dX=cR(dI);else{var dY=dI.safeGet(dW);if(126<=dY)var dZ=0;else switch(dY){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var dX=dA(dT,dM,dW,105),dZ=1;break;case 69:case 70:case 71:case 101:case 102:case 103:var dX=dA(dT,dM,dW,102),dZ=1;break;case 33:case 37:case 44:var dX=dW+1|0,dZ=1;break;case 83:case 91:case 115:var dX=dA(dT,dM,dW,115),dZ=1;break;case 97:case 114:case 116:var dX=dA(dT,dM,dW,dY),dZ=1;break;case 76:case 108:case 110:var d0=dW+1|0;if(dJ<d0){var dX=dA(dT,dM,dW,105),dZ=1;}else{var d1=dI.safeGet(d0)-88|0;if(d1<0||32<d1)var d2=1;else switch(d1){case 0:case 12:case 17:case 23:case 29:case 32:var dX=d4(d3,dA(dT,dM,dW,dY),105),dZ=1,d2=0;break;default:var d2=1;}if(d2){var dX=dA(dT,dM,dW,105),dZ=1;}}break;case 67:case 99:var dX=dA(dT,dM,dW,99),dZ=1;break;case 66:case 98:var dX=dA(dT,dM,dW,66),dZ=1;break;case 41:case 125:var dX=dA(dT,dM,dW,dY),dZ=1;break;case 40:var dX=d5(dA(dT,dM,dW,dY)),dZ=1;break;case 123:var d6=dA(dT,dM,dW,dY),d7=dA(dH,dY,dI,d6),d8=d6;for(;;){if(d8<(d7-2|0)){var d9=d4(d3,d8,dI.safeGet(d8)),d8=d9;continue;}var d_=d7-1|0,dW=d_;continue c;}default:var dZ=0;}if(!dZ)var dX=cP(dI,dW,dY);}var dO=dX;break;}}var dL=dO;continue a;}}var d$=dL+1|0,dL=d$;continue;}return dL;}}d5(0);return 0;}function em(el){var eb=[0,0,0,0];function ek(eg,eh,ec){var ed=41!==ec?1:0,ee=ed?125!==ec?1:0:ed;if(ee){var ef=97===ec?2:1;if(114===ec)eb[3]=eb[3]+1|0;if(eg)eb[2]=eb[2]+ef|0;else eb[1]=eb[1]+ef|0;}return eh+1|0;}ea(el,ek,function(ei,ej){return ei+1|0;});return eb[1];}function ez(en,eq,ey,eo){var ep=en.safeGet(eo);if((ep-48|0)<0||9<(ep-48|0))return d4(eq,0,eo);var er=ep-48|0,es=eo+1|0;for(;;){var et=en.safeGet(es);if(48<=et){if(et<58){var ew=es+1|0,ev=caml_mul(10,er)+(et-48|0)|0,er=ev,es=ew;continue;}var eu=0;}else if(36===et)if(0===er){var ex=f(aM),eu=1;}else{var ex=d4(eq,[0,cz(er-1|0)],es+1|0),eu=1;}else var eu=0;if(!eu)var ex=d4(eq,0,eo);return ex;}}function eC(eA,eB){if(eA)return eB;return bt(cD,eB);}function eF(eD,eE){if(eD)return eD[1];return eE;}function hG(gH,eH,gT,gI,gm,gZ,eG){var eI=bt(eH,eG);function gl(eN,gY,eJ,eR){var eM=eJ.getLen();function gi(gQ,eK){var eL=eK;for(;;){if(eM<=eL)return bt(eN,eI);var eO=eJ.safeGet(eL);if(37===eO){var eS=function(eQ,eP){return caml_array_get(eR,eF(eQ,eP));},e0=function(e2,eW,eY,eT){var eU=eT;for(;;){var eV=eJ.safeGet(eU)-32|0;if(0<=eV&&eV<=25)switch(eV){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 10:return ez(eJ,function(eX,e1){var eZ=[0,eS(eX,eW),eY];return e0(e2,eC(eX,eW),eZ,e1);},eW,eU+1|0);default:var e3=eU+1|0,eU=e3;continue;}var e4=eJ.safeGet(eU);if(124<=e4)var e5=0;else switch(e4){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var e6=eS(e2,eW),e7=caml_format_int(dh(e4,eJ,eL,eU,eY),e6),e9=e8(eC(e2,eW),e7,eU+1|0),e5=1;break;case 69:case 71:case 101:case 102:case 103:var e_=eS(e2,eW),e$=caml_format_float(da(eJ,eL,eU,eY),e_),e9=e8(eC(e2,eW),e$,eU+1|0),e5=1;break;case 76:case 108:case 110:var fa=eJ.safeGet(eU+1|0)-88|0;if(fa<0||32<fa)var fb=1;else switch(fa){case 0:case 12:case 17:case 23:case 29:case 32:var fc=eU+1|0,fd=e4-108|0;if(fd<0||2<fd)var fe=0;else{switch(fd){case 1:var fe=0,ff=0;break;case 2:var fg=eS(e2,eW),fh=caml_format_int(da(eJ,eL,fc,eY),fg),ff=1;break;default:var fi=eS(e2,eW),fh=caml_format_int(da(eJ,eL,fc,eY),fi),ff=1;}if(ff){var fj=fh,fe=1;}}if(!fe){var fk=eS(e2,eW),fj=caml_int64_format(da(eJ,eL,fc,eY),fk);}var e9=e8(eC(e2,eW),fj,fc+1|0),e5=1,fb=0;break;default:var fb=1;}if(fb){var fl=eS(e2,eW),fm=caml_format_int(dh(110,eJ,eL,eU,eY),fl),e9=e8(eC(e2,eW),fm,eU+1|0),e5=1;}break;case 83:case 115:var fn=eS(e2,eW);if(115===e4)var fo=fn;else{var fp=[0,0],fq=0,fr=fn.getLen()-1|0;if(fq<=fr){var fs=fq;for(;;){var ft=fn.safeGet(fs),fu=14<=ft?34===ft?1:92===ft?1:0:11<=ft?13<=ft?1:0:8<=ft?1:0,fv=fu?2:caml_is_printable(ft)?1:4;fp[1]=fp[1]+fv|0;var fw=fs+1|0;if(fr!==fs){var fs=fw;continue;}break;}}if(fp[1]===fn.getLen())var fx=fn;else{var fy=caml_create_string(fp[1]);fp[1]=0;var fz=0,fA=fn.getLen()-1|0;if(fz<=fA){var fB=fz;for(;;){var fC=fn.safeGet(fB),fD=fC-34|0;if(fD<0||58<fD)if(-20<=fD)var fE=1;else{switch(fD+34|0){case 8:fy.safeSet(fp[1],92);fp[1]+=1;fy.safeSet(fp[1],98);var fF=1;break;case 9:fy.safeSet(fp[1],92);fp[1]+=1;fy.safeSet(fp[1],116);var fF=1;break;case 10:fy.safeSet(fp[1],92);fp[1]+=1;fy.safeSet(fp[1],110);var fF=1;break;case 13:fy.safeSet(fp[1],92);fp[1]+=1;fy.safeSet(fp[1],114);var fF=1;break;default:var fE=1,fF=0;}if(fF)var fE=0;}else var fE=(fD-1|0)<0||56<(fD-1|0)?(fy.safeSet(fp[1],92),(fp[1]+=1,(fy.safeSet(fp[1],fC),0))):1;if(fE)if(caml_is_printable(fC))fy.safeSet(fp[1],fC);else{fy.safeSet(fp[1],92);fp[1]+=1;fy.safeSet(fp[1],48+caml_div(fC,100)|0);fp[1]+=1;fy.safeSet(fp[1],48+caml_mod(caml_div(fC,10),10)|0);fp[1]+=1;fy.safeSet(fp[1],48+caml_mod(fC,10)|0);}fp[1]+=1;var fG=fB+1|0;if(fA!==fB){var fB=fG;continue;}break;}}var fx=fy;}var fo=bi(aR,bi(fx,aS));}if(eU===(eL+1|0))var fH=fo;else{var fI=da(eJ,eL,eU,eY);try {var fJ=0,fK=1;for(;;){if(fI.getLen()<=fK)var fL=[0,0,fJ];else{var fM=fI.safeGet(fK);if(49<=fM)if(58<=fM)var fN=0;else{var fL=[0,caml_int_of_string(b6(fI,fK,(fI.getLen()-fK|0)-1|0)),fJ],fN=1;}else{if(45===fM){var fP=fK+1|0,fO=1,fJ=fO,fK=fP;continue;}var fN=0;}if(!fN){var fQ=fK+1|0,fK=fQ;continue;}}var fR=fL;break;}}catch(fS){if(fS[1]===a?0:1)throw fS;var fR=cL(fI,0,115);}var fU=fR[2],fT=fR[1],fV=fo.getLen(),fW=0,fZ=32;if(fT===fV&&0===fW){var fY=fo,fX=1;}else var fX=0;if(!fX)if(fT<=fV)var fY=b6(fo,fW,fV);else{var f0=b1(fT,fZ);if(fU)ca(fo,fW,f0,0,fV);else ca(fo,fW,f0,fT-fV|0,fV);var fY=f0;}var fH=fY;}var e9=e8(eC(e2,eW),fH,eU+1|0),e5=1;break;case 67:case 99:var f1=eS(e2,eW);if(99===e4)var f2=b1(1,f1);else{if(39===f1)var f3=aZ;else if(92===f1)var f3=a0;else{if(14<=f1)var f4=0;else switch(f1){case 8:var f3=a4,f4=1;break;case 9:var f3=a3,f4=1;break;case 10:var f3=a2,f4=1;break;case 13:var f3=a1,f4=1;break;default:var f4=0;}if(!f4)if(caml_is_printable(f1)){var f5=caml_create_string(1);f5.safeSet(0,f1);var f3=f5;}else{var f6=caml_create_string(4);f6.safeSet(0,92);f6.safeSet(1,48+caml_div(f1,100)|0);f6.safeSet(2,48+caml_mod(caml_div(f1,10),10)|0);f6.safeSet(3,48+caml_mod(f1,10)|0);var f3=f6;}}var f2=bi(aP,bi(f3,aQ));}var e9=e8(eC(e2,eW),f2,eU+1|0),e5=1;break;case 66:case 98:var f8=eU+1|0,f7=eS(e2,eW)?a9:a8,e9=e8(eC(e2,eW),f7,f8),e5=1;break;case 40:case 123:var f9=eS(e2,eW),f_=dA(dH,e4,eJ,eU+1|0);if(123===e4){var f$=ch(f9.getLen()),gc=function(gb,ga){cs(f$,ga);return gb+1|0;};ea(f9,function(gd,gf,ge){if(gd)cx(f$,aL);else cs(f$,37);return gc(gf,ge);},gc);var gg=cj(f$),e9=e8(eC(e2,eW),gg,f_),e5=1;}else{var gh=eC(e2,eW),gj=cC(em(f9),gh),e9=gl(function(gk){return gi(gj,f_);},gh,f9,eR),e5=1;}break;case 33:bt(gm,eI);var e9=gi(eW,eU+1|0),e5=1;break;case 37:var e9=e8(eW,aV,eU+1|0),e5=1;break;case 41:var e9=e8(eW,aU,eU+1|0),e5=1;break;case 44:var e9=e8(eW,aT,eU+1|0),e5=1;break;case 70:var gn=eS(e2,eW);if(0===eY){var go=caml_format_float(ba,gn),gp=0,gq=go.getLen();for(;;){if(gq<=gp)var gr=bi(go,a$);else{var gs=go.safeGet(gp),gt=48<=gs?58<=gs?0:1:45===gs?1:0;if(gt){var gu=gp+1|0,gp=gu;continue;}var gr=go;}var gv=gr;break;}}else{var gw=da(eJ,eL,eU,eY);if(70===e4)gw.safeSet(gw.getLen()-1|0,103);var gx=caml_format_float(gw,gn);if(3<=caml_classify_float(gn))var gy=gx;else{var gz=gx.getLen();if(0===gz)var gA=aN;else{var gB=0;for(;;){if(gz<=gB)var gC=bi(gx,aO);else{if(46!==gx.safeGet(gB)){var gD=gB+1|0,gB=gD;continue;}var gC=gx;}var gA=gC;break;}}var gy=gA;}var gv=gy;}var e9=e8(eC(e2,eW),gv,eU+1|0),e5=1;break;case 97:var gE=eS(e2,eW),gF=bt(cD,eF(e2,eW)),gG=eS(0,gF),gK=eU+1|0,gJ=eC(e2,gF);if(gH)d4(gI,eI,d4(gE,0,gG));else d4(gE,eI,gG);var e9=gi(gJ,gK),e5=1;break;case 116:var gL=eS(e2,eW),gN=eU+1|0,gM=eC(e2,eW);if(gH)d4(gI,eI,bt(gL,0));else bt(gL,eI);var e9=gi(gM,gN),e5=1;break;default:var e5=0;}if(!e5)var e9=cP(eJ,eU,e4);return e9;}},gS=eL+1|0,gP=0;return ez(eJ,function(gR,gO){return e0(gR,gQ,gP,gO);},gQ,gS);}d4(gT,eI,eO);var gU=eL+1|0,eL=gU;continue;}}function e8(gX,gV,gW){d4(gI,eI,gV);return gi(gX,gW);}return gi(gY,0);}var g0=d4(gl,gZ,cz(0)),g1=em(eG);if(g1<0||6<g1){var hc=function(g2,g8){if(g1<=g2){var g3=caml_make_vect(g1,0),g6=function(g4,g5){return caml_array_set(g3,(g1-g4|0)-1|0,g5);},g7=0,g9=g8;for(;;){if(g9){var g_=g9[2],g$=g9[1];if(g_){g6(g7,g$);var ha=g7+1|0,g7=ha,g9=g_;continue;}g6(g7,g$);}return d4(g0,eG,g3);}}return function(hb){return hc(g2+1|0,[0,hb,g8]);};},hd=hc(0,0);}else switch(g1){case 1:var hd=function(hf){var he=caml_make_vect(1,0);caml_array_set(he,0,hf);return d4(g0,eG,he);};break;case 2:var hd=function(hh,hi){var hg=caml_make_vect(2,0);caml_array_set(hg,0,hh);caml_array_set(hg,1,hi);return d4(g0,eG,hg);};break;case 3:var hd=function(hk,hl,hm){var hj=caml_make_vect(3,0);caml_array_set(hj,0,hk);caml_array_set(hj,1,hl);caml_array_set(hj,2,hm);return d4(g0,eG,hj);};break;case 4:var hd=function(ho,hp,hq,hr){var hn=caml_make_vect(4,0);caml_array_set(hn,0,ho);caml_array_set(hn,1,hp);caml_array_set(hn,2,hq);caml_array_set(hn,3,hr);return d4(g0,eG,hn);};break;case 5:var hd=function(ht,hu,hv,hw,hx){var hs=caml_make_vect(5,0);caml_array_set(hs,0,ht);caml_array_set(hs,1,hu);caml_array_set(hs,2,hv);caml_array_set(hs,3,hw);caml_array_set(hs,4,hx);return d4(g0,eG,hs);};break;case 6:var hd=function(hz,hA,hB,hC,hD,hE){var hy=caml_make_vect(6,0);caml_array_set(hy,0,hz);caml_array_set(hy,1,hA);caml_array_set(hy,2,hB);caml_array_set(hy,3,hC);caml_array_set(hy,4,hD);caml_array_set(hy,5,hE);return d4(g0,eG,hy);};break;default:var hd=d4(g0,eG,[0]);}return hd;}function hK(hF){return ch(caml_mul(2,hF.getLen()));}function hM(hJ,hH){var hI=cj(hH);hH[2]=0;return bt(hJ,hI);}function hS(hL){var hO=bt(hM,hL);return hP(hG,1,hK,cs,cx,function(hN){return 0;},hO);}function hT(hR){return d4(hS,function(hQ){return hQ;},hR);}32===cb;var hY=[0,aC];function hX(hU){var hV=hU[4],hW=hV?(hU[4]=0,(hU[1][2]=hU[2],(hU[2][1]=hU[1],0))):hV;return hW;}function h1(h0){var hZ=[];caml_update_dummy(hZ,[0,hZ,hZ]);return hZ;}function h3(h2){return h2[2]===h2?1:0;}var h4=[0,au],h_=42;function h8(h5){var h6=h5[1];{if(3===h6[0]){var h7=h6[1],h9=h8(h7);if(h9!==h7)h5[1]=[3,h9];return h9;}return h5;}}function ia(h$){return h8(h$);}function iv(ib,ij){var ic=ib,id=0;for(;;){if(ic instanceof Array)switch(ic[0]){case 1:var ii=ic[1];if(id){bt(ii,ij);var il=id[2],ik=id[1],ic=ik,id=il;continue;}var ih=bt(ii,ij);break;case 2:var io=[0,ic[2],id],im=ic[1],ic=im,id=io;continue;default:var ip=ic[1][1];if(ip){var iq=ip[1];if(id){bt(iq,ij);var is=id[2],ir=id[1],ic=ir,id=is;continue;}var ih=bt(iq,ij);}else{if(id){var iu=id[2],it=id[1],ic=it,id=iu;continue;}var ih=id;}}else{if(id){var ig=id[2],ie=id[1],ic=ie,id=ig;continue;}var ih=id;}return ih;}}function iE(iy,iw){var ix=[0,iw],iz=h8(iy),iA=iz[1];switch(iA[0]){case 1:if(iA[1][1]===h4){var iC=0,iB=1;}else var iB=0;break;case 2:var iD=iA[1][2];iz[1]=ix;var iC=iv(iD,ix),iB=1;break;default:var iB=0;}if(!iB)var iC=bc(av);return iC;}function iH(iF,iG){if(iF instanceof Array?0:1)return iG;if(iG instanceof Array?0:1)return iF;return [2,iF,iG];}function iL(iI){if(iI instanceof Array)switch(iI[0]){case 0:var iJ=iI[1][1],iK=iJ?iI:iJ;return iK;case 2:var iM=iL(iI[2]);return iH(iL(iI[1]),iM);default:}return iI;}function iX(iN,iP){var iO=ia(iN),iQ=ia(iP),iR=iO[1];{if(2===iR[0]){if(iO===iQ)return 0;var iS=iR[1],iT=iQ[1];{if(2===iT[0]){var iU=iT[1];iQ[1]=[3,iO];iS[1][1]=iU[1][1];var iV=iH(iS[2],iU[2]),iW=iS[3]+iU[3]|0;if(h_<iW){iS[3]=0;iS[2]=iL(iV);return 0;}iS[3]=iW;iS[2]=iV;return 0;}iO[1]=iT;return iv(iS[2],iT);}}return bc(aw);}}function i3(iY,i1){var iZ=ia(iY),i0=iZ[1];{if(2===i0[0]){var i2=i0[1][2];iZ[1]=i1;return iv(i2,i1);}return bc(ax);}}function i5(i4){return [0,[0,i4]];}function i7(i6){return [0,[1,i6]];}function i9(i8){return [0,[2,[0,i8,0,0]]];}var jk=[0,function(i_){return 0;}];function jj(ji){var i$=[],jh=0,jg=0;caml_update_dummy(i$,[0,[2,[0,[0,function(jf){var ja=h8(i$),jb=ja[1];if(2===jb[0]){var jd=jb[1][2],jc=[1,[0,h4]];ja[1]=jc;var je=iv(jd,jc);}else var je=0;return je;}],jg,jh]]]);return [0,i$,i$];}function jo(jl,jm){var jn=(jl[2] instanceof Array?0:1)?[1,jm]:[2,[1,jm],jl[2]];jl[2]=jn;return 0;}function jy(jp,ju){var jq=ia(jp)[1];switch(jq[0]){case 1:return i7(jq[1]);case 2:var jr=jq[1],js=i9(jr[1]);jo(jr,function(jt){switch(jt[0]){case 0:try {var jv=bt(ju,jt[1]),jw=jv;}catch(jx){var jw=i7(jx);}return iX(js,jw);case 1:return i3(js,[1,jt[1]]);default:throw [0,c,az];}});return js;case 3:throw [0,c,ay];default:return bt(ju,jq[1]);}}function jL(jz,jE){try {var jA=bt(jz,0),jB=jA;}catch(jC){var jB=i7(jC);}var jD=ia(jB)[1];switch(jD[0]){case 1:return bt(jE,jD[1]);case 2:var jF=jD[1],jG=i9(jF[1]);jo(jF,function(jH){switch(jH[0]){case 0:return i3(jG,jH);case 1:try {var jI=bt(jE,jH[1]),jJ=jI;}catch(jK){var jJ=i7(jK);}return iX(jG,jJ);default:throw [0,c,aB];}});return jG;case 3:throw [0,c,aA];default:return jB;}}h1(0);var jM=null,jR=undefined;function jQ(jN,jP){var jO=1-(jN==jM?1:0);if(jO)return bt(jP,jN);return jO;}var jS=false,jW=true,jV=Array;function jU(jT){return jT;}function jY(jX){return jX;}function j1(jZ,j0){jZ.appendChild(j0);return 0;}function j4(j2,j3){j2.removeChild(j3);return 0;}var j9=caml_js_on_ie(0)|0;function j8(j7){return jY(caml_js_wrap_callback(function(j5){var j6=j5===jR?event:j5;return bt(j7,j6);}));}function ka(j_,j$){if(j_)return bt(j$,j_[1]);return j_;}function kd(kc,kb){return kc.createElement(kb.toString());}function kg(kf,ke){return kd(kf,ke);}function ki(kh){return kg(kh,ao);}function kk(kj){return kg(kj,ap);}function km(kl){return kg(kl,ar);}var kn=window;function kp(ko){return ActiveXObject;}function ku(kr){var kq=jj(0),kt=kr*1000;kn.setTimeout(caml_js_wrap_callback(function(ks){return iE(kq[2],0);}),kt);return kq[1];}function kw(kv){return kv.toString();}var kx=kn.document;function kA(kz,ky){return j1(kz,kx.createTextNode(kw(ky)));}function kF(kC,kE){function kD(kB){return j4(kC,kB);}jQ(kC.firstChild,kD);return j1(kC,kE);}var kG=kw(i),kI=kw(h),kH=[0,g];function kK(kJ){switch(kJ){case 1:return kw(t);case 2:return kw(s);case 3:return kw(r);case 4:return kw(q);case 5:return kw(p);case 6:return kw(o);case 7:return kw(n);case 8:return kw(m);default:return kw(u);}}function kQ(kL,kO,kM,kN){caml_array_set(caml_array_get(kL[1],kM),kO,kN);var kP=caml_array_get(caml_array_get(kL[2],kM),kO);return kP.src=kK(kN);}function k6(kS){var kR=[0,0],kT=kS[1].length-1-2|0,kU=1;if(kU<=kT){var kV=kT;for(;;){var kW=1,kX=caml_array_get(kS[1],kV).length-1-2|0;if(kW<=kX){var kY=kW;for(;;){var kZ=6===caml_array_get(caml_array_get(kS[1],kV+1|0),kY)?1:0,k0=kZ?3===caml_array_get(caml_array_get(kS[1],kV),kY)?1:0:kZ,k1=0===caml_array_get(caml_array_get(kS[1],kV),kY)?3===caml_array_get(caml_array_get(kS[1],kV-1|0),kY)?(kQ(kS,kY,kV-1|0,0),(kQ(kS,kY,kV,3),(kR[1]=1,1))):0:0;k1;var k2=0===caml_array_get(caml_array_get(kS[1],kV),kY)?0===caml_array_get(caml_array_get(kS[1],kV-1|0),kY)?3===caml_array_get(caml_array_get(kS[1],kV),kY-1|0)?3===caml_array_get(caml_array_get(kS[1],kV-1|0),kY-1|0)?(kQ(kS,kY-1|0,kV-1|0,0),(kQ(kS,kY,kV,3),(kR[1]=1,1))):0:0:0:0;k2;var k3=0===caml_array_get(caml_array_get(kS[1],kV),kY)?0===caml_array_get(caml_array_get(kS[1],kV-1|0),kY)?3===caml_array_get(caml_array_get(kS[1],kV),kY+1|0)?3===caml_array_get(caml_array_get(kS[1],kV-1|0),kY+1|0)?(kQ(kS,kY+1|0,kV-1|0,0),(kQ(kS,kY,kV,3),(kR[1]=1,1))):0:0:0:0;k3;if(!k0&&6===caml_array_get(caml_array_get(kS[1],kV+1|0),kY)&&3===caml_array_get(caml_array_get(kS[1],kV),kY)){kQ(kS,kY,kV+1|0,8);throw [0,kH];}var k4=kY+1|0;if(kX!==kY){var kY=k4;continue;}break;}}var k5=kV-1|0;if(kU!==kV){var kV=k5;continue;}break;}}if(kR[1]){var k8=function(k7){return k6(kS);};return jy(ku(0.05),k8);}return i5(0);}function lY(la,lZ,k9){var k_=k9[3];function ne(nc){var k$=0,lb=la[1].length-1-1|0;if(k$<=lb){var lc=k$;for(;;){var ld=0,le=caml_array_get(la[1],lc).length-1-1|0;if(ld<=le){var lf=ld;for(;;){caml_array_get(caml_array_get(la[2],lc),lf).onmouseover=jM;caml_array_get(caml_array_get(la[2],lc),lf).onmouseout=jM;caml_array_get(caml_array_get(la[2],lc),lf).onclick=jM;var lg=lf+1|0;if(le!==lf){var lf=lg;continue;}break;}}var lh=lc+1|0;if(lb!==lc){var lc=lh;continue;}break;}}function lm(lj,ll){if(!la[8]){la[8]=1;var lk=function(li){la[8]=0;return i5(0);};jy(bt(lj,0),lk);}return jS;}function ls(lp,ln,lr){function lq(lo){la[9][1]=[0,ln];return i5(0);}return jy(bt(lp,0),lq);}function lw(lu,lv){var lt=la[9][1];if(lt){bt(lt[1],0);la[9][1]=0;return bt(lu,0);}return bt(lu,0);}function me(lx,mc,lJ,lz,lA,lB){var ly=lx,lC=lB,lI=lz,lN=lA;for(;;){var lD=ly[2],lE=ly[1],lF=caml_array_get(caml_array_get(la[1],lD),lE);if(5===lF||!(3<=lF))var lG=0;else{var lH=0,lG=1;}if(!lG)var lH=1;if(lH){var lM=caml_array_get(caml_array_get(la[2],lD),lE).src,lL=function(lI,lD,lE){return function(lK){caml_array_get(caml_array_get(la[2],lD),lE).src=lJ;return bt(lI,0);};}(lI,lD,lE),lP=function(lN,lD,lE,lM){return function(lO){caml_array_get(caml_array_get(la[2],lD),lE).src=lM;return bt(lN,0);};}(lN,lD,lE,lM),l8=function(lC,lD,lE){return function(lW){function lV(lU){if(2===caml_array_get(caml_array_get(la[1],lD),lE))la[5]=la[5]-1|0;kQ(la,lE,lD,6);function lT(lS){function lR(lQ){kQ(la,lE,lD,0);return i5(0);}return jy(k6(la),lR);}return jy(ku(0.05),lT);}return jy(bt(lC,0),lV);};}(lC,lD,lE),l_=function(lC,lD,lE){return function(l7){var lX=la[3];kQ(la,lX[1],lX[2],0);function l2(l0){return lY(la,lZ,k9);}function l6(l1){if(l1[1]===kH){la[6]=1;return i5(0);}return i7(l1);}return jy(jL(function(l5){function l4(l3){if(2===caml_array_get(caml_array_get(la[1],lD),lE))la[5]=la[5]-1|0;kQ(la,lE,lD,6);la[3]=[0,lE,lD];return k6(la);}return jy(bt(lC,0),l4);},l6),l2);};}(lC,lD,lE),l9=caml_array_get(caml_array_get(la[2],lD),lE);l9.onmouseover=j8(bt(lm,d4(ls,bt(lw,lL),lP)));var ma=caml_array_get(caml_array_get(la[2],lD),lE);ma.onmouseout=j8(bt(lm,bt(lw,function(l$){return i5(0);})));var mb=caml_array_get(caml_array_get(la[2],lD),lE);mb.onclick=j8(bt(lm,bt(lw,l_)));if(5===caml_array_get(caml_array_get(la[1],lD),lE))return 0;var md=bt(mc,[0,lE,lD]),ly=md,lC=l8,lI=lL,lN=lP;continue;}return 0;}}function mN(mf,mi,mv,mu){var mg=mf[2],mh=mf[1],mj=bt(mi,mf),mk=mj[2],ml=mj[1],mm=bt(mi,mj),mn=mm[2],mo=mm[1];try {var mp=3===caml_array_get(caml_array_get(la[1],mk),ml)?1:0,mq=mp?0===caml_array_get(caml_array_get(la[1],mn),mo)?1:0:mp,mr=mq;}catch(ms){if(ms[1]===b&&!caml_string_notequal(ms[2],Y)){var mr=0,mt=1;}else var mt=0;if(!mt)throw ms;}if(mr){var mA=function(mw){caml_array_get(caml_array_get(la[2],mg),mh).src=mu;caml_array_get(caml_array_get(la[2],mk),ml).src=mv;return i5(0);},mH=function(mz){var mx=caml_array_get(caml_array_get(la[2],mg),mh);mx.src=kw(_);var my=caml_array_get(caml_array_get(la[2],mk),ml);return my.src=kw(Z);},mJ=function(mG){kQ(la,mh,mg,0);kQ(la,ml,mk,6);la[3]=mj;kQ(la,mo,mn,3);function mD(mB){return lY(la,lZ,k9);}function mF(mC){if(mC[1]===kH){la[6]=1;return i5(0);}return i7(mC);}return jy(jL(function(mE){return k6(la);},mF),mD);},mI=caml_array_get(caml_array_get(la[2],mk),ml);mI.onmouseover=j8(bt(lm,d4(ls,bt(lw,mA),mH)));var mL=caml_array_get(caml_array_get(la[2],mk),ml);mL.onmouseout=j8(bt(lm,bt(lw,function(mK){return i5(0);})));var mM=caml_array_get(caml_array_get(la[2],mk),ml);return mM.onclick=j8(bt(lm,bt(lw,mJ)));}return 0;}if(caml_equal(la[3],la[4])){bt(k_,0);kn.alert(kw(X));}else if(la[6]){bt(k_,0);kn.alert(kw(W));}else{if(0===la[5]){var mO=la[4],mP=mO[2],mQ=mO[1],mR=caml_array_get(caml_array_get(la[2],mP),mQ);mR.src=kw(V);caml_array_set(caml_array_get(la[1],mP),mQ,5);}var mT=function(mS){return [0,mS[1]+1|0,mS[2]];},mV=function(mU){return [0,mU[1]-1|0,mU[2]];},mX=function(mW){return [0,mW[1],mW[2]-1|0];},mZ=function(mY){return [0,mY[1],mY[2]+1|0];},m1=function(m0){return 0;},m3=function(m2){return i5(0);},m4=kw(U);me(mT(la[3]),mT,m4,m3,m1,m3);var m5=kw(T);me(mV(la[3]),mV,m5,m3,m1,m3);var m6=kw(S);me(mX(la[3]),mX,m6,m3,m1,m3);var m7=kw(R);me(mZ(la[3]),mZ,m7,m3,m1,m3);var m8=kw(Q);mN(la[3],mT,kw(P),m8);var m9=kw(O);mN(la[3],mV,kw(N),m9);bt(lZ,la[5]);}var m_=la[7];if(m_[1])if(h3(m_[2]))m_[1]=0;else{var m$=m_[2],nb=0;if(h3(m$))throw [0,hY];var na=m$[2];hX(na);iE(na[3],nb);}return i5(0);}var nd=la[7];if(nd[1]){var nf=jj(0),ng=nf[1],nh=nd[2],ni=[0,nh[1],nh,nf[2],1];nh[1][2]=ni;nh[1]=ni;var nk=function(nj){return hX(ni);},nl=ia(ng)[1];switch(nl[0]){case 1:var nm=nl[1][1]===h4?(nk(0),1):0;break;case 2:jo(nl[1],function(nn){if(1===nn[0]&&nn[1][1]===h4){try {var no=nk(0);}catch(np){return 0;}return no;}return 0;});var nm=1;break;default:var nm=0;}nm;var nq=ng;}else{nd[1]=1;var nq=i5(0);}return jy(nq,ne);}function nu(ns,nr){if(nr){var nt=nr[1];return ns.style.cssText=nt;}return 0;}kn.onload=j8(function(pU){var nv=kx.getElementById(kw(x));if(nv==jM)throw [0,c,y];var nw=kk(kx),nx=[0,caml_sys_time(0)],ny=kk(kx);ny.style.cssText=kG;kA(ny,k);var nz=[0,1];function nD(nG){var nA=caml_sys_time(0)-nx[1];if(!nz[1]){var nB=nA|0;kF(ny,kx.createTextNode(kw(nC(hT,l,caml_div(nB,3600),caml_mod(caml_div(nB,60),60),caml_mod(nB,60)))));}function nF(nE){return nD(0);}return jy(ku(1),nF);}nD(0);function nJ(nH){nz[1]=1;return 0;}var nK=[0,ny,function(nI){nx[1]=caml_sys_time(0);nz[1]=0;return 0;},nJ],oa=nK[2],n$=nK[1];function n_(nS,nO){var nL=kk(kx);nL.style.cssText=kI;kA(nL,j);j1(nv,nL);function nR(nN){function nP(nM){j4(nv,nL);return i5(nM);}return jy(bt(nO,nN),nP);}var nQ=[0,[2,[0,jk,0,0]]],n2=kw(nS);try {var nT=new XMLHttpRequest,nU=nT;}catch(n9){try {var nV=new (kp(0))(ac.toString()),nU=nV;}catch(n0){try {var nW=new (kp(0))(ab.toString()),nU=nW;}catch(nZ){try {var nX=new (kp(0))(aa.toString());}catch(nY){throw [0,c,$];}var nU=nX;}}}var n1=jM===jM?ae:ad;nU.open(n1.toString(),n2,jW);jQ(jM,function(n3){return nU.setRequestHeader(af.toString(),ag.toString());});nU.onreadystatechange=jY(function(n8){var n4=4===nU.readyState?1:0;if(n4){var n5=0===nU.status?1:0;if(n5)var n6=n5;else{var n7=200===nU.status?1:0,n6=n7?n7:304===nU.status?1:0;}}else var n6=n4;if(n6)return iE(nQ,new MlWrappedString(nU.responseText));return n6;});nU.send(jM);return jy(nQ,nR);}var ob=kk(kx);ob.style.cssText=kG;kA(ob,w);function ps(oc){return kF(ob,kx.createTextNode(bk(oc).toString()));}function pT(or){var od=nv.style;od.cssText=kw(E);var oe=kg(kx,aq);kA(oe,D);j1(nv,oe);var of=kk(kx);kA(of,C);j1(of,n$);kA(of,B);j1(of,ob);kA(of,A);var og=0,oh=0;if(0===oh&&0===og){var oi=kd(kx,d),oj=1;}else var oj=0;if(!oj)if(j9){var ok=new jV;ok.push(aj.toString(),d.toString());ka(oh,function(ol){ok.push(ak.toString(),caml_js_html_escape(ol),al.toString());return 0;});ka(og,function(om){ok.push(am.toString(),caml_js_html_escape(om),an.toString());return 0;});ok.push(ai.toString());var oi=kx.createElement(ok.join(ah.toString()));}else{var on=kd(kx,d);ka(oh,function(oo){return on.type=oo;});ka(og,function(op){return on.name=op;});var oi=on;}var oq=ki(kx);kA(oq,z);j1(oi,oq);var os=or;for(;;){if(os){var ov=os[2],ou=os[1][2],ot=ki(kx);kA(ot,ou);j1(oi,ot);var os=ov;continue;}oi.onchange=j8(function(pv){var ow=oi.selectedIndex-1|0;if(0<=ow){var ox=0,oy=or;for(;;){if(oy){var oA=oy[2],oz=ox+1|0,ox=oz,oy=oA;continue;}if(ow<ox){if(0<=ow){var oB=or,oC=ow;for(;;){if(oB){var oF=oB[2],oD=oB[1];if(0!==oC){var oG=oC-1|0,oB=oF,oC=oG;continue;}var oE=oD;}else var oE=f(a6);var oH=oE;break;}}else var oH=bc(a5);var pt=oH[1];n_(pt,function(oL){var oI=[0,0],oJ=[0,0],oK=0,oM=oL.getLen()-1|0;if(oK<=oM){var oN=oK;for(;;){var oO=oL.safeGet(oN);if(47<=oO)if(83<=oO)if(89<=oO)var oP=0;else{switch(oO-83|0){case 0:oJ[1]=[0,6,oJ[1]];var oQ=1;break;case 4:oJ[1]=[0,6,oJ[1]];var oQ=1;break;case 5:oJ[1]=[0,3,oJ[1]];var oQ=1;break;default:var oP=0,oQ=0;}if(oQ)var oP=1;}else var oP=69===oO?(oJ[1]=[0,4,oJ[1]],1):0;else if(10===oO){var oR=oI[1];oI[1]=[0,bS(oJ[1]),oR];oJ[1]=0;var oP=1;}else if(32<=oO){switch(oO-32|0){case 0:oJ[1]=[0,0,oJ[1]];var oS=1;break;case 3:oJ[1]=[0,7,oJ[1]];var oS=1;break;case 11:oJ[1]=[0,2,oJ[1]];var oS=1;break;case 14:oJ[1]=[0,1,oJ[1]];var oS=1;break;default:var oP=0,oS=0;}if(oS)var oP=1;}else var oP=0;if(!oP)f(H);var oT=oN+1|0;if(oM!==oN){var oN=oT;continue;}break;}}var oU=bM(bW(bM,bS(oI[1]))),oX=bz(bt(bz,function(oW){var oV=kg(kx,as);oV.src=kK(oW);return oV;}),oU),oY=[0,0],oZ=[0,0],o0=[0,0],o1=[0,0],o2=[0,0],o3=kw(G),o5=[0,kw(F)],o4=kg(kx,at);nu(o4,[0,o3]);var o6=0,o7=oX.length-1-1|0,o_=0;if(o6<=o7){var o8=o6;for(;;){var o9=o4.insertRow(-1);nu(o9,o_);var o$=0,pa=caml_array_get(oX,o8).length-1-1|0;if(o$<=pa){var pb=o$;for(;;){var pc=o9.insertCell(-1);nu(pc,o5);var pd=caml_array_get(caml_array_get(oX,o8),pb);switch(caml_array_get(caml_array_get(oU,o8),pb)){case 2:o2[1]+=1;break;case 4:o0[1]=pb;o1[1]=o8;break;case 6:oY[1]=pb;oZ[1]=o8;break;default:}j1(pc,pd);j1(o9,pc);var pe=pb+1|0;if(pa!==pb){var pb=pe;continue;}break;}}j1(o4,o9);var pf=o8+1|0;if(o7!==o8){var o8=pf;continue;}break;}}kF(nw,o4);function pr(pq){var pg=caml_sys_time(0);function pk(pn){var ph=caml_sys_time(0);if(1<=ph-pg){var pi=o4.style;pi.opacity=jU(kw(I));return i5(0);}function pm(pl){var pj=o4.style;pj.opacity=jU(kw(d4(hT,J,ph-pg)));return pk(0);}return jy(ku(0.05),pm);}function pp(po){bt(oa,0);return i5(0);}return jy(pk(0),pp);}return jy(lY([0,oU,oX,[0,oY[1],oZ[1]],[0,o0[1],o1[1]],o2[1],0,[0,0,h1(0)],0,[0,0]],ps,nK),pr);});var pu=1;}else var pu=0;break;}}else var pu=0;pu;return jS;});j1(of,oi);j1(of,km(kx));j1(of,km(kx));j1(of,nw);j1(nv,of);return i5(0);}}jy(n_(v,function(pw){function pG(py){var px=pw.getLen(),pz=py;for(;;){if(px<=pz)var pA=f(L);else{if(34!==pw.safeGet(pz)){var pF=pz+1|0,pz=pF;continue;}var pB=pz+1|0,pC=pz+2|0;for(;;){if(px<=pB)var pD=f(K);else{if(34!==pw.safeGet(pC)){var pE=pC+1|0,pC=pE;continue;}var pD=[0,b6(pw,pB,pC-pB|0),pC+1|0];}var pA=pD;break;}}return pA;}}var pH=0,pI=0;for(;;){try {var pJ=pG(pI),pL=pJ[1],pK=pG(pJ[2]),pM=[0,[0,[0,pL,pK[1]],pK[2]]],pN=pM;}catch(pO){if(pO[1]===a&&!caml_string_notequal(pO[2],M)){var pN=0,pP=1;}else var pP=0;if(!pP)throw pO;}if(pN){var pQ=pN[1],pR=pQ[2],pS=[0,pQ[1],pH],pH=pS,pI=pR;continue;}return i5(bS(pH));}}),pT);return jS;});bp(0);return;}());
