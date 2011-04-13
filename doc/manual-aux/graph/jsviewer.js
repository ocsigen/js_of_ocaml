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
function caml_compare (a, b) { return caml_compare_val (a, b, true); }
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
function caml_get_public_method (obj, tag) {
  var meths = obj[1];
  var li = 3, hi = meths[1] * 2 + 1, mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < meths[mi+1]) hi = mi-2;
    else li = mi;
  }
  return (tag == meths[li+1] ? meths[li] : 0);
}
function caml_greaterequal (x, y) { return +(caml_compare(x,y,false) >= 0); }
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
function caml_js_call(f, o, args) { return f.apply(o, args.slice(1)); }
function caml_js_from_byte_string (s) {return s.getFullBytes();}
function caml_js_get_console () {
  var c = window.console?window.console:{};
  var m = ["log", "debug", "info", "warn", "error", "assert", "dir", "dirxml",
           "trace", "group", "groupCollapsed", "groupEnd", "time", "timeEnd"];
  function f () {}
  for (i = 0; i < m.length; i++) if (!c[m[i]]) c[m[i]]=f;
  return c;
}
function caml_js_to_byte_string (s) {return new MlString (s);}
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    return caml_call_gen(f, args);
  }
}
function caml_lessequal (x, y) { return +(caml_compare(x,y,false) <= 0); }
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
function caml_obj_block (tag, size) {
  var o = [tag];
  for (var i = 1; i <= size; i++) o[i] = 0;
  return o;
}
function caml_register_global (n, v) { caml_global_data[n] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_string_notequal(s1, s2) { return +!s1.equal(s2); }
function caml_sys_get_config (e) { return [0, "Unix", 32]; }
function caml_update_dummy (x, y) {
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}
(function()
  {function t2(wF,wG,wH,wI,wJ,wK,wL,wM,wN)
    {return wF.length==
            8?wF(wG,wH,wI,wJ,wK,wL,wM,wN):caml_call_gen
                                           (wF,[wG,wH,wI,wJ,wK,wL,wM,wN]);}
   function ud(wx,wy,wz,wA,wB,wC,wD,wE)
    {return wx.length==
            7?wx(wy,wz,wA,wB,wC,wD,wE):caml_call_gen
                                        (wx,[wy,wz,wA,wB,wC,wD,wE]);}
   function ky(wq,wr,ws,wt,wu,wv,ww)
    {return wq.length==
            6?wq(wr,ws,wt,wu,wv,ww):caml_call_gen(wq,[wr,ws,wt,wu,wv,ww]);}
   function ee(wm,wn,wo,wp)
    {return wm.length==3?wm(wn,wo,wp):caml_call_gen(wm,[wn,wo,wp]);}
   function bK(wj,wk,wl)
    {return wj.length==2?wj(wk,wl):caml_call_gen(wj,[wk,wl]);}
   function bV(wh,wi){return wh.length==1?wh(wi):caml_call_gen(wh,[wi]);}
   var a=[0,new MlString("Failure")],b=[0,new MlString("Invalid_argument")],
    c=[0,new MlString("Not_found")],d=[0,new MlString("Assert_failure")],
    e=new MlString("&"),f=[0,0,0,0,0],g=new MlString("scene.json"),
    h=
     [0,new MlString("_value"),new MlString("_lower"),new MlString("_upper"),
      new MlString("_step_incr"),new MlString("_page_incr"),
      new MlString("_page_size")],
    i=
     [0,new MlString("value"),new MlString("upper"),
      new MlString("step_increment"),new MlString("set_value"),
      new MlString("set_bounds"),new MlString("page_size"),
      new MlString("page_increment"),new MlString("lower")],
    j=
     [0,new MlString("set_value"),new MlString("set_bounds"),
      new MlString("step_increment"),new MlString("page_increment"),
      new MlString("lower"),new MlString("page_size"),new MlString("upper"),
      new MlString("value")];
   caml_register_global(5,[0,new MlString("Division_by_zero")]);
   caml_register_global(3,b);caml_register_global(2,a);
   var bj=new MlString("%.12g"),bi=new MlString("."),bh=new MlString("%d"),
    bg=new MlString("true"),bf=new MlString("false"),
    be=new MlString("Pervasives.do_at_exit"),bd=new MlString("Array.blit"),
    bc=new MlString("\\b"),bb=new MlString("\\t"),ba=new MlString("\\n"),
    a$=new MlString("\\r"),a_=new MlString("\\\\"),a9=new MlString("\\'"),
    a8=new MlString(""),a7=new MlString("String.blit"),
    a6=new MlString("String.sub"),a5=new MlString("Map.remove_min_elt"),
    a4=new MlString("Map.bal"),a3=new MlString("Map.bal"),
    a2=new MlString("Map.bal"),a1=new MlString("Map.bal"),
    a0=new MlString("Buffer.add: cannot grow buffer"),aZ=new MlString("%"),
    aY=new MlString(""),aX=new MlString(""),aW=new MlString("\""),
    aV=new MlString("\""),aU=new MlString("'"),aT=new MlString("'"),
    aS=new MlString("."),aR=new MlString("nan"),
    aQ=new MlString("printf: bad positional specification (0)."),
    aP=new MlString("%_"),aO=[0,new MlString("printf.ml"),143,8],
    aN=new MlString("''"),
    aM=new MlString("Printf: premature end of format string ``"),
    aL=new MlString("''"),aK=new MlString(" in format string ``"),
    aJ=new MlString(", at char number "),
    aI=new MlString("Printf: bad conversion %"),
    aH=new MlString("Sformat.index_of_int: negative argument "),
    aG=new MlString(""),aF=[0,new MlString("src/core/lwt.ml"),315,20],
    aE=[0,new MlString("src/core/lwt.ml"),318,8],
    aD=new MlString("Lwt.fast_connect"),aC=new MlString("Lwt.connect"),
    aB=new MlString("Lwt.wakeup"),aA=new MlString("Lwt.Canceled"),
    az=new MlString("return;"),ay=new MlString("onmousewheel"),
    ax=new MlString("function"),aw=new MlString("onmousewheel"),
    av=new MlString("canvas"),au=new MlString("p"),at=new MlString("div"),
    as=new MlString("on"),ar=new MlString("mouseup"),
    aq=new MlString("mousemove"),ap=new MlString("mousewheel"),
    ao=new MlString("DOMMouseScroll"),an=new MlString("2d"),
    am=new MlString("Dom_html.Canvas_not_available"),al=new MlString("POST"),
    ak=
     [0,new MlString("POST"),
      [0,new MlString("application/x-www-form-urlencoded")]],
    aj=new MlString("GET"),ai=new MlString("?"),
    ah=new MlString("Content-type"),ag=new MlString("="),
    af=new MlString("Msxml2.XMLHTTP"),ae=new MlString("Msxml3.XMLHTTP"),
    ad=new MlString("Microsoft.XMLHTTP"),
    ac=[0,new MlString("xmlHttpRequest.ml"),56,2],ab=[0,0,0],
    aa=[0,new MlString("viewer_common.ml"),242,8],
    $=[0,new MlString("viewer_common.ml"),245,8],
    _=[0,new MlString("viewer_common.ml"),251,6],
    Z=[0,new MlString("viewer_common.ml"),254,6],
    Y=[0,new MlString("viewer_common.ml"),104,63],X=new MlString("keycode:"),
    W=new MlString("%dpx"),V=new MlString("update"),
    U=new MlString("loading"),T=new MlString("parsing"),
    S=new MlString("parsing"),R=new MlString("init"),Q=[0,0],P=[0,0],
    O=[0,1],N=[0,20],M=new MlString("absolute"),L=new MlString("0px"),
    K=new MlString("1px"),J=new MlString("black"),I=new MlString("absolute"),
    H=new MlString("2px solid black"),G=new MlString("1px"),
    F=new MlString("10px"),E=new MlString("10px"),
    D=new MlString("initial drawing"),C=new MlString("initial drawing"),
    B=new MlString("init"),A=new MlString("inline"),z=new MlString("hidden"),
    y=new MlString("hidden"),x=new MlString("0px"),
    w=new MlString("Loading graph..."),v=new MlString("none"),
    u=new MlString("loading"),t=new MlString("default"),
    s=new MlString("move"),r=new MlString("draw"),q=new MlString("draw"),
    p=new MlString("draw"),o=[0,new MlString("viewer_js.ml"),69,26],
    n=new MlString("center"),m=new MlString("white");
   function l(k){throw [0,a,k];}function bl(bk){throw [0,b,bk];}
   function bo(bn,bm){if(caml_lessequal(bn,bm))return bn;return bm;}
   function br(bq,bp){if(caml_greaterequal(bq,bp))return bq;return bp;}
   function bx(bs,bu)
    {var bt=bs.getLen(),bv=bu.getLen(),bw=caml_create_string(bt+bv|0);
     caml_blit_string(bs,0,bw,0,bt);caml_blit_string(bu,0,bw,bt,bv);
     return bw;}
   function bz(by){return caml_format_int(bh,by);}
   function bE(bD)
    {var bA=caml_ml_out_channels_list(0);
     for(;;)
      {if(bA){var bB=bA[2];try {}catch(bC){}var bA=bB;continue;}return 0;}}
   caml_register_named_value(be,bE);
   function bM(bJ,bG)
    {var bF=0,bH=bG.length-1-1|0;
     if(bF<=bH)
      {var bI=bF;
       for(;;)
        {bK(bJ,bI,bG[bI+1]);var bL=bI+1|0;if(bH!==bI){var bI=bL;continue;}
         break;}}
     return 0;}
   function bS(bN)
    {var bO=bN,bP=0;
     for(;;)
      {if(bO){var bQ=bO[2],bR=[0,bO[1],bP],bO=bQ,bP=bR;continue;}return bP;}}
   function bX(bU,bT)
    {if(bT){var bW=bT[2],bY=bV(bU,bT[1]);return [0,bY,bX(bU,bW)];}return 0;}
   function b3(b1,bZ)
    {var b0=bZ;
     for(;;){if(b0){var b2=b0[2];bV(b1,b0[1]);var b0=b2;continue;}return 0;}}
   function b7(b4,b6)
    {var b5=caml_create_string(b4);caml_fill_string(b5,0,b4,b6);return b5;}
   function ca(b_,b8,b9)
    {if(0<=b8&&0<=b9&&((b_.getLen()-b9|0)<b8?0:1))
      {var b$=caml_create_string(b9);caml_blit_string(b_,b8,b$,0,b9);
       return b$;}
     return bl(a6);}
   function cg(cd,cc,cf,ce,cb)
    {if
      (0<=cb&&0<=cc&&cc<=(cd.getLen()-cb|0)&&0<=ce&&
       ((cf.getLen()-cb|0)<ce?0:1))
      return caml_blit_string(cd,cc,cf,ce,cb);
     return bl(a7);}
   var ch=caml_sys_get_config(0)[2],
    ci=caml_mul(caml_div(ch,8),(1<<(ch-10|0))-1|0)-1|0,eY=248;
   function eX(c1)
    {function ck(cj){if(cj)return cj[5];return 0;}
     function cs(cl,cr,cq,cn)
      {var cm=ck(cl),co=ck(cn),cp=co<=cm?cm+1|0:co+1|0;
       return [0,cl,cr,cq,cn,cp];}
     function cS(ct,cD,cC,cv)
      {var cu=ct?ct[5]:0,cw=cv?cv[5]:0;
       if((cw+2|0)<cu)
        {if(ct)
          {var cx=ct[4],cy=ct[3],cz=ct[2],cA=ct[1],cB=ck(cx);
           if(cB<=ck(cA))return cs(cA,cz,cy,cs(cx,cD,cC,cv));
           if(cx)
            {var cG=cx[3],cF=cx[2],cE=cx[1],cH=cs(cx[4],cD,cC,cv);
             return cs(cs(cA,cz,cy,cE),cF,cG,cH);}
           return bl(a4);}
         return bl(a3);}
       if((cu+2|0)<cw)
        {if(cv)
          {var cI=cv[4],cJ=cv[3],cK=cv[2],cL=cv[1],cM=ck(cL);
           if(cM<=ck(cI))return cs(cs(ct,cD,cC,cL),cK,cJ,cI);
           if(cL)
            {var cP=cL[3],cO=cL[2],cN=cL[1],cQ=cs(cL[4],cK,cJ,cI);
             return cs(cs(ct,cD,cC,cN),cO,cP,cQ);}
           return bl(a2);}
         return bl(a1);}
       var cR=cw<=cu?cu+1|0:cw+1|0;return [0,ct,cD,cC,cv,cR];}
     var cU=0;function c6(cT){if(cT)return 0;return 1;}
     function c5(c2,c4,cV)
      {if(cV)
        {var cX=cV[5],cW=cV[4],cY=cV[3],cZ=cV[2],c0=cV[1],c3=bK(c1[1],c2,cZ);
         if(0===c3)return [0,c0,c2,c4,cW,cX];
         if(0<=c3)return cS(c0,cZ,cY,c5(c2,c4,cW));
         return cS(c5(c2,c4,c0),cZ,cY,cW);}
       return [0,0,c2,c4,0,1];}
     function dl(c9,c7)
      {var c8=c7;
       for(;;)
        {if(c8)
          {var db=c8[4],da=c8[3],c$=c8[1],c_=bK(c1[1],c9,c8[2]);
           if(0===c_)return da;var dc=0<=c_?db:c$,c8=dc;continue;}
         throw [0,c];}}
     function dt(df,dd)
      {var de=dd;
       for(;;)
        {if(de)
          {var di=de[4],dh=de[1],dg=bK(c1[1],df,de[2]),dj=0===dg?1:0;
           if(dj)return dj;var dk=0<=dg?di:dh,de=dk;continue;}
         return 0;}}
     function dp(dm)
      {if(dm)
        {var dn=dm[1];
         if(dn){var ds=dm[4],dr=dm[3],dq=dm[2];return cS(dp(dn),dq,dr,ds);}
         return dm[4];}
       return bl(a5);}
     function dG(dz,du)
      {if(du)
        {var dv=du[4],dw=du[3],dx=du[2],dy=du[1],dA=bK(c1[1],dz,dx);
         if(0===dA)
          {if(dy)
            if(dv)
             {var dB=dv;
              for(;;)
               {if(!dB)throw [0,c];var dC=dB[1];if(dC){var dB=dC;continue;}
                var dE=dB[3],dD=dB[2],dF=cS(dy,dD,dE,dp(dv));break;}}
            else var dF=dy;
           else var dF=dv;return dF;}
         if(0<=dA)return cS(dy,dx,dw,dG(dz,dv));
         return cS(dG(dz,dy),dx,dw,dv);}
       return 0;}
     function dJ(dK,dH)
      {var dI=dH;
       for(;;)
        {if(dI)
          {var dN=dI[4],dM=dI[3],dL=dI[2];dJ(dK,dI[1]);bK(dK,dL,dM);
           var dI=dN;continue;}
         return 0;}}
     function dP(dQ,dO)
      {if(dO)
        {var dU=dO[5],dT=dO[3],dS=dO[2],dR=dO[1],dV=dP(dQ,dO[4]),
          dW=bV(dQ,dT);
         return [0,dP(dQ,dR),dS,dW,dV,dU];}
       return 0;}
     function d2(d3,dX)
      {if(dX)
        {var d1=dX[5],d0=dX[4],dZ=dX[3],dY=dX[2],d4=dX[1],d5=d2(d3,d0),
          d6=bK(d3,dY,dZ);
         return [0,d2(d3,d4),dY,d6,d5,d1];}
       return 0;}
     function d$(ea,d7,d9)
      {var d8=d7,d_=d9;
       for(;;)
        {if(d8)
          {var ed=d8[4],ec=d8[3],eb=d8[2],ef=ee(ea,eb,ec,d$(ea,d8[1],d_)),
            d8=ed,d_=ef;
           continue;}
         return d_;}}
     function em(eg,ei)
      {var eh=eg,ej=ei;
       for(;;)
        {if(eh)
          {var ek=eh[1],el=[0,eh[2],eh[3],eh[4],ej],eh=ek,ej=el;continue;}
         return ej;}}
     function eW(ez,eo,en)
      {var ep=em(en,0),eq=em(eo,0),er=ep;
       for(;;)
        {if(eq)
          if(er)
           {var ey=er[4],ex=er[3],ew=er[2],ev=eq[4],eu=eq[3],et=eq[2],
             es=bK(c1[1],eq[1],er[1]);
            if(0===es)
             {var eA=bK(ez,et,ew);
              if(0===eA){var eB=em(ex,ey),eC=em(eu,ev),eq=eC,er=eB;continue;}
              var eD=eA;}
            else var eD=es;}
          else var eD=1;
         else var eD=er?-1:0;return eD;}}
     return [0,cU,c6,c5,dl,dG,dt,dJ,dP,d2,d$,eW,
             function(eQ,eF,eE)
              {var eG=em(eE,0),eH=em(eF,0),eI=eG;
               for(;;)
                {if(eH)
                  if(eI)
                   {var eO=eI[4],eN=eI[3],eM=eI[2],eL=eH[4],eK=eH[3],
                     eJ=eH[2],eP=0===bK(c1[1],eH[1],eI[1])?1:0;
                    if(eP)
                     {var eR=bK(eQ,eJ,eM);
                      if(eR)
                       {var eS=em(eN,eO),eT=em(eK,eL),eH=eT,eI=eS;continue;}
                      var eU=eR;}
                    else var eU=eP;var eV=eU;}
                  else var eV=0;
                 else var eV=eI?0:1;return eV;}}];}
   function e3(eZ)
    {var e0=1<=eZ?eZ:1,e1=ci<e0?ci:e0,e2=caml_create_string(e1);
     return [0,e2,0,e1,e2];}
   function e5(e4){return ca(e4[1],0,e4[2]);}
   function e_(e6,e8)
    {var e7=[0,e6[3]];
     for(;;)
      {if(e7[1]<(e6[2]+e8|0)){e7[1]=caml_mul(2,e7[1]);continue;}
       if(ci<e7[1])if((e6[2]+e8|0)<=ci)e7[1]=ci;else l(a0);
       var e9=caml_create_string(e7[1]);cg(e6[1],0,e9,0,e6[2]);e6[1]=e9;
       e6[3]=e7[1];return 0;}}
   function fc(e$,fb)
    {var fa=e$[2];if(e$[3]<=fa)e_(e$,1);e$[1].safeSet(fa,fb);e$[2]=fa+1|0;
     return 0;}
   function fh(ff,fd)
    {var fe=fd.getLen(),fg=ff[2]+fe|0;if(ff[3]<fg)e_(ff,fe);
     cg(fd,0,ff[1],ff[2],fe);ff[2]=fg;return 0;}
   function fj(fi){if(0<=fi)return fi;return l(bx(aH,bz(fi)));}
   function fm(fk,fl){return fj(fk+fl|0);}var fn=bV(fm,1);
   function fp(fo){return ca(fo,0,fo.getLen());}
   function fv(fq,fr,ft)
    {var fs=bx(aK,bx(fq,aL)),fu=bx(aJ,bx(bz(fr),fs));
     return bl(bx(aI,bx(b7(1,ft),fu)));}
   function fz(fw,fy,fx){return fv(fp(fw),fy,fx);}
   function fB(fA){return bl(bx(aM,bx(fp(fA),aN)));}
   function fW(fC,fK,fM,fO)
    {function fJ(fD)
      {if((fC.safeGet(fD)-48|0)<0||9<(fC.safeGet(fD)-48|0))return fD;
       var fE=fD+1|0;
       for(;;)
        {var fF=fC.safeGet(fE);
         if(48<=fF){if(fF<58){var fH=fE+1|0,fE=fH;continue;}var fG=0;}else
          if(36===fF){var fI=fE+1|0,fG=1;}else var fG=0;
         if(!fG)var fI=fD;return fI;}}
     var fL=fJ(fK+1|0),fN=e3((fM-fL|0)+10|0);fc(fN,37);
     var fQ=bS(fO),fP=fL,fR=fQ;
     for(;;)
      {if(fP<=fM)
        {var fS=fC.safeGet(fP);
         if(42===fS)
          {if(fR)
            {var fT=fR[2];fh(fN,bz(fR[1]));var fU=fJ(fP+1|0),fP=fU,fR=fT;
             continue;}
           throw [0,d,aO];}
         fc(fN,fS);var fV=fP+1|0,fP=fV;continue;}
       return e5(fN);}}
   function f3(f2,f0,fZ,fY,fX)
    {var f1=fW(f0,fZ,fY,fX);if(78!==f2&&110!==f2)return f1;
     f1.safeSet(f1.getLen()-1|0,117);return f1;}
   function go(f_,gi,gm,f4,gl)
    {var f5=f4.getLen();
     function gj(f6,gh)
      {var f7=40===f6?41:125;
       function gg(f8)
        {var f9=f8;
         for(;;)
          {if(f5<=f9)return bV(f_,f4);
           if(37===f4.safeGet(f9))
            {var f$=f9+1|0;
             if(f5<=f$)var ga=bV(f_,f4);else
              {var gb=f4.safeGet(f$),gc=gb-40|0;
               if(gc<0||1<gc)
                {var gd=gc-83|0;
                 if(gd<0||2<gd)var ge=1;else
                  switch(gd){case 1:var ge=1;break;case 2:
                    var gf=1,ge=0;break;
                   default:var gf=0,ge=0;}
                 if(ge){var ga=gg(f$+1|0),gf=2;}}
               else var gf=0===gc?0:1;
               switch(gf){case 1:var ga=gb===f7?f$+1|0:ee(gi,f4,gh,gb);break;
                case 2:break;default:var ga=gg(gj(gb,f$+1|0)+1|0);}}
             return ga;}
           var gk=f9+1|0,f9=gk;continue;}}
       return gg(gh);}
     return gj(gm,gl);}
   function gp(gn){return ee(go,fB,fz,gn);}
   function gT(gq,gB,gL)
    {var gr=gq.getLen()-1|0;
     function gM(gs)
      {var gt=gs;a:
       for(;;)
        {if(gt<gr)
          {if(37===gq.safeGet(gt))
            {var gu=0,gv=gt+1|0;
             for(;;)
              {if(gr<gv)var gw=fB(gq);else
                {var gx=gq.safeGet(gv);
                 if(58<=gx)
                  {if(95===gx){var gz=gv+1|0,gy=1,gu=gy,gv=gz;continue;}}
                 else
                  if(32<=gx)
                   switch(gx-32|0){case 1:case 2:case 4:case 5:case 6:
                    case 7:case 8:case 9:case 12:case 15:break;case 0:
                    case 3:case 11:case 13:var gA=gv+1|0,gv=gA;continue;
                    case 10:var gC=ee(gB,gu,gv,105),gv=gC;continue;default:
                     var gD=gv+1|0,gv=gD;continue;
                    }
                 var gE=gv;c:
                 for(;;)
                  {if(gr<gE)var gF=fB(gq);else
                    {var gG=gq.safeGet(gE);
                     if(126<=gG)var gH=0;else
                      switch(gG){case 78:case 88:case 100:case 105:case 111:
                       case 117:case 120:var gF=ee(gB,gu,gE,105),gH=1;break;
                       case 69:case 70:case 71:case 101:case 102:case 103:
                        var gF=ee(gB,gu,gE,102),gH=1;break;
                       case 33:case 37:case 44:var gF=gE+1|0,gH=1;break;
                       case 83:case 91:case 115:
                        var gF=ee(gB,gu,gE,115),gH=1;break;
                       case 97:case 114:case 116:
                        var gF=ee(gB,gu,gE,gG),gH=1;break;
                       case 76:case 108:case 110:
                        var gI=gE+1|0;
                        if(gr<gI){var gF=ee(gB,gu,gE,105),gH=1;}else
                         {var gJ=gq.safeGet(gI)-88|0;
                          if(gJ<0||32<gJ)var gK=1;else
                           switch(gJ){case 0:case 12:case 17:case 23:
                            case 29:case 32:
                             var gF=bK(gL,ee(gB,gu,gE,gG),105),gH=1,gK=0;
                             break;
                            default:var gK=1;}
                          if(gK){var gF=ee(gB,gu,gE,105),gH=1;}}
                        break;
                       case 67:case 99:var gF=ee(gB,gu,gE,99),gH=1;break;
                       case 66:case 98:var gF=ee(gB,gu,gE,66),gH=1;break;
                       case 41:case 125:var gF=ee(gB,gu,gE,gG),gH=1;break;
                       case 40:var gF=gM(ee(gB,gu,gE,gG)),gH=1;break;
                       case 123:
                        var gN=ee(gB,gu,gE,gG),gO=ee(gp,gG,gq,gN),gP=gN;
                        for(;;)
                         {if(gP<(gO-2|0))
                           {var gQ=bK(gL,gP,gq.safeGet(gP)),gP=gQ;continue;}
                          var gR=gO-1|0,gE=gR;continue c;}
                       default:var gH=0;}
                     if(!gH)var gF=fz(gq,gE,gG);}
                   var gw=gF;break;}}
               var gt=gw;continue a;}}
           var gS=gt+1|0,gt=gS;continue;}
         return gt;}}
     gM(0);return 0;}
   function g5(g4)
    {var gU=[0,0,0,0];
     function g3(gZ,g0,gV)
      {var gW=41!==gV?1:0,gX=gW?125!==gV?1:0:gW;
       if(gX)
        {var gY=97===gV?2:1;if(114===gV)gU[3]=gU[3]+1|0;
         if(gZ)gU[2]=gU[2]+gY|0;else gU[1]=gU[1]+gY|0;}
       return g0+1|0;}
     gT(g4,g3,function(g1,g2){return g1+1|0;});return gU[1];}
   function hg(g6,g9,hf,g7)
    {var g8=g6.safeGet(g7);if((g8-48|0)<0||9<(g8-48|0))return bK(g9,0,g7);
     var g_=g8-48|0,g$=g7+1|0;
     for(;;)
      {var ha=g6.safeGet(g$);
       if(48<=ha)
        {if(ha<58)
          {var hd=g$+1|0,hc=caml_mul(10,g_)+(ha-48|0)|0,g_=hc,g$=hd;
           continue;}
         var hb=0;}
       else
        if(36===ha)
         if(0===g_){var he=l(aQ),hb=1;}else
          {var he=bK(g9,[0,fj(g_-1|0)],g$+1|0),hb=1;}
        else var hb=0;
       if(!hb)var he=bK(g9,0,g7);return he;}}
   function hj(hh,hi){if(hh)return hi;return bV(fn,hi);}
   function hm(hk,hl){if(hk)return hk[1];return hl;}
   function kp(jq,ho,jC,jr,i7,jI,hn)
    {var hp=bV(ho,hn);
     function i6(hu,jH,hq,hy)
      {var ht=hq.getLen();
       function i3(jz,hr)
        {var hs=hr;
         for(;;)
          {if(ht<=hs)return bV(hu,hp);var hv=hq.safeGet(hs);
           if(37===hv)
            {var hz=function(hx,hw){return caml_array_get(hy,hm(hx,hw));},
              hH=
               function(hJ,hD,hF,hA)
                {var hB=hA;
                 for(;;)
                  {var hC=hq.safeGet(hB)-32|0;
                   if(0<=hC&&hC<=25)
                    switch(hC){case 1:case 2:case 4:case 5:case 6:case 7:
                     case 8:case 9:case 12:case 15:break;case 10:
                      return hg
                              (hq,
                               function(hE,hI)
                                {var hG=[0,hz(hE,hD),hF];
                                 return hH(hJ,hj(hE,hD),hG,hI);},
                               hD,hB+1|0);
                     default:var hK=hB+1|0,hB=hK;continue;}
                   var hL=hq.safeGet(hB);
                   if(124<=hL)var hM=0;else
                    switch(hL){case 78:case 88:case 100:case 105:case 111:
                     case 117:case 120:
                      var hN=hz(hJ,hD),
                       hO=caml_format_int(f3(hL,hq,hs,hB,hF),hN),
                       hQ=hP(hj(hJ,hD),hO,hB+1|0),hM=1;
                      break;
                     case 69:case 71:case 101:case 102:case 103:
                      var hR=hz(hJ,hD),
                       hS=caml_format_float(fW(hq,hs,hB,hF),hR),
                       hQ=hP(hj(hJ,hD),hS,hB+1|0),hM=1;
                      break;
                     case 76:case 108:case 110:
                      var hT=hq.safeGet(hB+1|0)-88|0;
                      if(hT<0||32<hT)var hU=1;else
                       switch(hT){case 0:case 12:case 17:case 23:case 29:
                        case 32:
                         var hV=hB+1|0,hW=hL-108|0;
                         if(hW<0||2<hW)var hX=0;else
                          {switch(hW){case 1:var hX=0,hY=0;break;case 2:
                             var hZ=hz(hJ,hD),
                              h0=caml_format_int(fW(hq,hs,hV,hF),hZ),
                              hY=1;
                             break;
                            default:
                             var h1=hz(hJ,hD),
                              h0=caml_format_int(fW(hq,hs,hV,hF),h1),
                              hY=1;
                            }
                           if(hY){var h2=h0,hX=1;}}
                         if(!hX)
                          {var h3=hz(hJ,hD),
                            h2=caml_int64_format(fW(hq,hs,hV,hF),h3);}
                         var hQ=hP(hj(hJ,hD),h2,hV+1|0),hM=1,hU=0;break;
                        default:var hU=1;}
                      if(hU)
                       {var h4=hz(hJ,hD),
                         h5=caml_format_int(f3(110,hq,hs,hB,hF),h4),
                         hQ=hP(hj(hJ,hD),h5,hB+1|0),hM=1;}
                      break;
                     case 83:case 115:
                      var h6=hz(hJ,hD);
                      if(115===hL)var h7=h6;else
                       {var h8=[0,0],h9=0,h_=h6.getLen()-1|0;
                        if(h9<=h_)
                         {var h$=h9;
                          for(;;)
                           {var ia=h6.safeGet(h$),
                             ib=14<=ia?34===ia?1:92===ia?1:0:11<=ia?13<=
                              ia?1:0:8<=ia?1:0,
                             ic=ib?2:caml_is_printable(ia)?1:4;
                            h8[1]=h8[1]+ic|0;var id=h$+1|0;
                            if(h_!==h$){var h$=id;continue;}break;}}
                        if(h8[1]===h6.getLen())var ie=h6;else
                         {var ig=caml_create_string(h8[1]);h8[1]=0;
                          var ih=0,ii=h6.getLen()-1|0;
                          if(ih<=ii)
                           {var ij=ih;
                            for(;;)
                             {var ik=h6.safeGet(ij),il=ik-34|0;
                              if(il<0||58<il)
                               if(-20<=il)var im=1;else
                                {switch(il+34|0){case 8:
                                   ig.safeSet(h8[1],92);h8[1]+=1;
                                   ig.safeSet(h8[1],98);var io=1;break;
                                  case 9:
                                   ig.safeSet(h8[1],92);h8[1]+=1;
                                   ig.safeSet(h8[1],116);var io=1;break;
                                  case 10:
                                   ig.safeSet(h8[1],92);h8[1]+=1;
                                   ig.safeSet(h8[1],110);var io=1;break;
                                  case 13:
                                   ig.safeSet(h8[1],92);h8[1]+=1;
                                   ig.safeSet(h8[1],114);var io=1;break;
                                  default:var im=1,io=0;}
                                 if(io)var im=0;}
                              else
                               var im=(il-1|0)<0||56<
                                (il-1|0)?(ig.safeSet(h8[1],92),
                                          (h8[1]+=1,(ig.safeSet(h8[1],ik),0))):1;
                              if(im)
                               if(caml_is_printable(ik))ig.safeSet(h8[1],ik);
                               else
                                {ig.safeSet(h8[1],92);h8[1]+=1;
                                 ig.safeSet(h8[1],48+caml_div(ik,100)|0);
                                 h8[1]+=1;
                                 ig.safeSet
                                  (h8[1],48+caml_mod(caml_div(ik,10),10)|0);
                                 h8[1]+=1;
                                 ig.safeSet(h8[1],48+caml_mod(ik,10)|0);}
                              h8[1]+=1;var ip=ij+1|0;
                              if(ii!==ij){var ij=ip;continue;}break;}}
                          var ie=ig;}
                        var h7=bx(aV,bx(ie,aW));}
                      if(hB===(hs+1|0))var iq=h7;else
                       {var ir=fW(hq,hs,hB,hF);
                        try
                         {var is=0,it=1;
                          for(;;)
                           {if(ir.getLen()<=it)var iu=[0,0,is];else
                             {var iv=ir.safeGet(it);
                              if(49<=iv)
                               if(58<=iv)var iw=0;else
                                {var
                                  iu=
                                   [0,
                                    caml_int_of_string
                                     (ca(ir,it,(ir.getLen()-it|0)-1|0)),
                                    is],
                                  iw=1;}
                              else
                               {if(45===iv)
                                 {var iy=it+1|0,ix=1,is=ix,it=iy;continue;}
                                var iw=0;}
                              if(!iw){var iz=it+1|0,it=iz;continue;}}
                            var iA=iu;break;}}
                        catch(iB)
                         {if(iB[1]===a?0:1)throw iB;var iA=fv(ir,0,115);}
                        var iD=iA[2],iC=iA[1],iE=h7.getLen(),iF=0,iI=32;
                        if(iC===iE&&0===iF){var iH=h7,iG=1;}else var iG=0;
                        if(!iG)
                         if(iC<=iE)var iH=ca(h7,iF,iE);else
                          {var iJ=b7(iC,iI);
                           if(iD)cg(h7,iF,iJ,0,iE);else
                            cg(h7,iF,iJ,iC-iE|0,iE);
                           var iH=iJ;}
                        var iq=iH;}
                      var hQ=hP(hj(hJ,hD),iq,hB+1|0),hM=1;break;
                     case 67:case 99:
                      var iK=hz(hJ,hD);
                      if(99===hL)var iL=b7(1,iK);else
                       {if(39===iK)var iM=a9;else
                         if(92===iK)var iM=a_;else
                          {if(14<=iK)var iN=0;else
                            switch(iK){case 8:var iM=bc,iN=1;break;case 9:
                              var iM=bb,iN=1;break;
                             case 10:var iM=ba,iN=1;break;case 13:
                              var iM=a$,iN=1;break;
                             default:var iN=0;}
                           if(!iN)
                            if(caml_is_printable(iK))
                             {var iO=caml_create_string(1);iO.safeSet(0,iK);
                              var iM=iO;}
                            else
                             {var iP=caml_create_string(4);iP.safeSet(0,92);
                              iP.safeSet(1,48+caml_div(iK,100)|0);
                              iP.safeSet(2,48+caml_mod(caml_div(iK,10),10)|0);
                              iP.safeSet(3,48+caml_mod(iK,10)|0);var iM=iP;}}
                        var iL=bx(aT,bx(iM,aU));}
                      var hQ=hP(hj(hJ,hD),iL,hB+1|0),hM=1;break;
                     case 66:case 98:
                      var iR=hB+1|0,iQ=hz(hJ,hD)?bg:bf,
                       hQ=hP(hj(hJ,hD),iQ,iR),hM=1;
                      break;
                     case 40:case 123:
                      var iS=hz(hJ,hD),iT=ee(gp,hL,hq,hB+1|0);
                      if(123===hL)
                       {var iU=e3(iS.getLen()),
                         iX=function(iW,iV){fc(iU,iV);return iW+1|0;};
                        gT
                         (iS,
                          function(iY,i0,iZ)
                           {if(iY)fh(iU,aP);else fc(iU,37);return iX(i0,iZ);},
                          iX);
                        var i1=e5(iU),hQ=hP(hj(hJ,hD),i1,iT),hM=1;}
                      else
                       {var i2=hj(hJ,hD),i4=fm(g5(iS),i2),
                         hQ=i6(function(i5){return i3(i4,iT);},i2,iS,hy),
                         hM=1;}
                      break;
                     case 33:bV(i7,hp);var hQ=i3(hD,hB+1|0),hM=1;break;
                     case 37:var hQ=hP(hD,aZ,hB+1|0),hM=1;break;case 41:
                      var hQ=hP(hD,aY,hB+1|0),hM=1;break;
                     case 44:var hQ=hP(hD,aX,hB+1|0),hM=1;break;case 70:
                      var i8=hz(hJ,hD);
                      if(0===hF)
                       {var i9=caml_format_float(bj,i8),i_=0,i$=i9.getLen();
                        for(;;)
                         {if(i$<=i_)var ja=bx(i9,bi);else
                           {var jb=i9.safeGet(i_),
                             jc=48<=jb?58<=jb?0:1:45===jb?1:0;
                            if(jc){var jd=i_+1|0,i_=jd;continue;}var ja=i9;}
                          var je=ja;break;}}
                      else
                       {var jf=fW(hq,hs,hB,hF);
                        if(70===hL)jf.safeSet(jf.getLen()-1|0,103);
                        var jg=caml_format_float(jf,i8);
                        if(3<=caml_classify_float(i8))var jh=jg;else
                         {var ji=jg.getLen();
                          if(0===ji)var jj=aR;else
                           {var jk=0;
                            for(;;)
                             {if(ji<=jk)var jl=bx(jg,aS);else
                               {if(46!==jg.safeGet(jk))
                                 {var jm=jk+1|0,jk=jm;continue;}
                                var jl=jg;}
                              var jj=jl;break;}}
                          var jh=jj;}
                        var je=jh;}
                      var hQ=hP(hj(hJ,hD),je,hB+1|0),hM=1;break;
                     case 97:
                      var jn=hz(hJ,hD),jo=bV(fn,hm(hJ,hD)),jp=hz(0,jo),
                       jt=hB+1|0,js=hj(hJ,jo);
                      if(jq)bK(jr,hp,bK(jn,0,jp));else bK(jn,hp,jp);
                      var hQ=i3(js,jt),hM=1;break;
                     case 116:
                      var ju=hz(hJ,hD),jw=hB+1|0,jv=hj(hJ,hD);
                      if(jq)bK(jr,hp,bV(ju,0));else bV(ju,hp);
                      var hQ=i3(jv,jw),hM=1;break;
                     default:var hM=0;}
                   if(!hM)var hQ=fz(hq,hB,hL);return hQ;}},
              jB=hs+1|0,jy=0;
             return hg(hq,function(jA,jx){return hH(jA,jz,jy,jx);},jz,jB);}
           bK(jC,hp,hv);var jD=hs+1|0,hs=jD;continue;}}
       function hP(jG,jE,jF){bK(jr,hp,jE);return i3(jG,jF);}return i3(jH,0);}
     var jJ=bK(i6,jI,fj(0)),jK=g5(hn);
     if(jK<0||6<jK)
      {var
        jX=
         function(jL,jR)
          {if(jK<=jL)
            {var jM=caml_make_vect(jK,0),
              jP=function(jN,jO){return caml_array_set(jM,(jK-jN|0)-1|0,jO);},
              jQ=0,jS=jR;
             for(;;)
              {if(jS)
                {var jT=jS[2],jU=jS[1];
                 if(jT){jP(jQ,jU);var jV=jQ+1|0,jQ=jV,jS=jT;continue;}
                 jP(jQ,jU);}
               return bK(jJ,hn,jM);}}
           return function(jW){return jX(jL+1|0,[0,jW,jR]);};},
        jY=jX(0,0);}
     else
      switch(jK){case 1:
        var jY=
         function(j0)
          {var jZ=caml_make_vect(1,0);caml_array_set(jZ,0,j0);
           return bK(jJ,hn,jZ);};
        break;
       case 2:
        var jY=
         function(j2,j3)
          {var j1=caml_make_vect(2,0);caml_array_set(j1,0,j2);
           caml_array_set(j1,1,j3);return bK(jJ,hn,j1);};
        break;
       case 3:
        var jY=
         function(j5,j6,j7)
          {var j4=caml_make_vect(3,0);caml_array_set(j4,0,j5);
           caml_array_set(j4,1,j6);caml_array_set(j4,2,j7);
           return bK(jJ,hn,j4);};
        break;
       case 4:
        var jY=
         function(j9,j_,j$,ka)
          {var j8=caml_make_vect(4,0);caml_array_set(j8,0,j9);
           caml_array_set(j8,1,j_);caml_array_set(j8,2,j$);
           caml_array_set(j8,3,ka);return bK(jJ,hn,j8);};
        break;
       case 5:
        var jY=
         function(kc,kd,ke,kf,kg)
          {var kb=caml_make_vect(5,0);caml_array_set(kb,0,kc);
           caml_array_set(kb,1,kd);caml_array_set(kb,2,ke);
           caml_array_set(kb,3,kf);caml_array_set(kb,4,kg);
           return bK(jJ,hn,kb);};
        break;
       case 6:
        var jY=
         function(ki,kj,kk,kl,km,kn)
          {var kh=caml_make_vect(6,0);caml_array_set(kh,0,ki);
           caml_array_set(kh,1,kj);caml_array_set(kh,2,kk);
           caml_array_set(kh,3,kl);caml_array_set(kh,4,km);
           caml_array_set(kh,5,kn);return bK(jJ,hn,kh);};
        break;
       default:var jY=bK(jJ,hn,[0]);}
     return jY;}
   function kt(ko){return e3(caml_mul(2,ko.getLen()));}
   function kv(ks,kq){var kr=e5(kq);kq[2]=0;return bV(ks,kr);}
   function kB(ku)
    {var kx=bV(kv,ku);return ky(kp,1,kt,fc,fh,function(kw){return 0;},kx);}
   function kC(kA){return bK(kB,function(kz){return kz;},kA);}32===ch;
   var kD=[0,0],kM=2;
   function kL(kG)
    {var kE=[0,0],kF=0,kH=kG.getLen()-1|0;
     if(kF<=kH)
      {var kI=kF;
       for(;;)
        {kE[1]=caml_mul(223,kE[1])+kG.safeGet(kI)|0;var kJ=kI+1|0;
         if(kH!==kI){var kI=kJ;continue;}break;}}
     kE[1]=kE[1]&((1<<31)-1|0);var kK=1073741823<kE[1]?kE[1]-(1<<31)|0:kE[1];
     return kK;}
   var kP=eX([0,function(kO,kN){return caml_compare(kO,kN);}]),
    kS=eX([0,function(kR,kQ){return caml_compare(kR,kQ);}]),
    kV=eX([0,function(kU,kT){return caml_compare(kU,kT);}]),
    kW=caml_obj_block(0,0),kZ=[0,0];
   function kY(kX)
    {if(2<kX)return caml_mul(kY(caml_div(kX+1|0,2)),2);return kX;}
   function k7(k0)
    {kZ[1]+=1;var k1=k0.length-1,k2=caml_make_vect(caml_mul(k1,2)+2|0,kW);
     caml_array_set(k2,0,k1);
     caml_array_set(k2,1,caml_div(caml_mul(kY(k1),ch),8)-1|0);
     var k3=0,k4=k1-1|0;
     if(k3<=k4)
      {var k5=k3;
       for(;;)
        {caml_array_set(k2,caml_mul(k5,2)+3|0,caml_array_get(k0,k5));
         var k6=k5+1|0;if(k4!==k5){var k5=k6;continue;}break;}}
     return [0,kM,k2,kS[1],kV[1],0,0,kP[1],0];}
   function lo(k8,k_)
    {var k9=k8[2].length-1,k$=k9<k_?1:0;
     if(k$)
      {var la=caml_make_vect(k_,kW),lb=0,lc=0,ld=k8[2];
       if
        (0<=k9&&0<=lc&&
         !((ld.length-1-k9|0)<lc||!(0<=lb&&!((la.length-1-k9|0)<lb))))
        if(lc<lb)
         {var lf=k9-1|0,lg=0;
          if(lg<=lf)
           {var lh=lf;
            for(;;)
             {la[(lb+lh|0)+1]=ld[(lc+lh|0)+1];var li=lh-1|0;
              if(lg!==lh){var lh=li;continue;}break;}}
          var le=1;}
        else
         {var lj=0,lk=k9-1|0;
          if(lj<=lk)
           {var ll=lj;
            for(;;)
             {la[(lb+ll|0)+1]=ld[(lc+ll|0)+1];var lm=ll+1|0;
              if(lk!==ll){var ll=lm;continue;}break;}}
          var le=1;}
       else var le=0;if(!le)bl(bd);k8[2]=la;var ln=0;}
     else var ln=k$;return ln;}
   var lp=[0,0],lt=[0,0];
   function ls(lq){var lr=lq[2].length-1;lo(lq,lr+1|0);return lr;}
   function ly(lu)
    {var lv=ls(lu);
     if
      (0===caml_mod(lv,2)||
       (2+caml_div(caml_mul(caml_array_get(lu[2],1),16),ch)|0)<lv)
      var lw=0;
     else{var lx=ls(lu),lw=1;}if(!lw)var lx=lv;caml_array_set(lu[2],lx,0);
     return lx;}
   var lz=[0,aA],lF=42;
   function lD(lA)
    {var lB=lA[1];
     {if(3===lB[0])
       {var lC=lB[1],lE=lD(lC);if(lE!==lC)lA[1]=[3,lE];return lE;}
      return lA;}}
   function lH(lG){return lD(lG);}
   function l0(lI,lM)
    {var lJ=lI,lK=0;
     for(;;)
      {if(typeof lJ==="number")
        {if(lK){var lZ=lK[2],lY=lK[1],lJ=lY,lK=lZ;continue;}var lP=lK;}
       else
        switch(lJ[0]){case 1:
          var lL=lJ[1];
          if(lK){bV(lL,lM);var lO=lK[2],lN=lK[1],lJ=lN,lK=lO;continue;}
          var lP=bV(lL,lM);break;
         case 2:var lR=[0,lJ[2],lK],lQ=lJ[1],lJ=lQ,lK=lR;continue;default:
          var lS=lJ[1][1];
          if(lS)
           {var lT=lS[1];
            if(lK){bV(lT,lM);var lV=lK[2],lU=lK[1],lJ=lU,lK=lV;continue;}
            var lP=bV(lT,lM);}
          else{if(lK){var lX=lK[2],lW=lK[1],lJ=lW,lK=lX;continue;}var lP=lK;}
         }
       return lP;}}
   function l9(l3,l1)
    {var l2=[0,l1],l4=lD(l3),l5=l4[1];
     switch(l5[0]){case 1:
       if(l5[1][1]===lz){var l7=0,l6=1;}else var l6=0;break;
      case 2:var l8=l5[1][2];l4[1]=l2;var l7=l0(l8,l2),l6=1;break;default:
       var l6=0;
      }
     if(!l6)var l7=bl(aB);return l7;}
   function ma(l_,l$)
    {if(typeof l_==="number")return l$;if(typeof l$==="number")return l_;
     return [2,l_,l$];}
   function me(mb)
    {if(typeof mb!=="number")
      switch(mb[0]){case 0:var mc=mb[1][1],md=mc?mb:mc;return md;case 2:
        var mf=me(mb[2]);return ma(me(mb[1]),mf);
       default:}
     return mb;}
   function mh(mg){return [0,[0,mg]];}function mj(mi){return [0,[1,mi]];}
   var mw=[0,function(mk){return 0;}];
   function mv(mu)
    {var ml=[],mt=0,ms=0;
     caml_update_dummy
      (ml,
       [0,
        [2,
         [0,
          [0,
           function(mr)
            {var mm=lD(ml),mn=mm[1];
             if(2===mn[0])
              {var mp=mn[1][2],mo=[1,[0,lz]];mm[1]=mo;var mq=l0(mp,mo);}
             else var mq=0;return mq;}],
          ms,mt]]]);
     return [0,ml,ml];}
   function mA(mx,my)
    {var mz=typeof mx[2]==="number"?[1,my]:[2,[1,my],mx[2]];mx[2]=mz;
     return 0;}
   function mH(mB,mD)
    {var mC=lH(mB)[1];
     switch(mC[0]){case 1:if(mC[1][1]===lz)return bV(mD,0);break;case 2:
       return mA
               (mC[1],
                function(mE)
                 {if(1===mE[0]&&mE[1][1]===lz)
                   {try {var mF=bV(mD,0);}catch(mG){return 0;}return mF;}
                  return 0;});
      default:}
     return 0;}
   function m5(mI,mN)
    {var mJ=lH(mI)[1];
     switch(mJ[0]){case 1:return mj(mJ[1]);case 2:
       var mK=mJ[1],mL=[0,[2,[0,mK[1],0,0]]];
       mA
        (mK,
         function(mM)
          {switch(mM[0]){case 0:
             try {var mO=bV(mN,mM[1]),mP=mO;}catch(mQ){var mP=mj(mQ);}
             var mR=lH(mL),mS=lH(mP),mT=mR[1];
             if(2===mT[0])
              if(mR===mS)var mU=0;else
               {var mV=mT[1],mW=mS[1];
                if(2===mW[0])
                 {var mX=mW[1];mS[1]=[3,mR];mV[1][1]=mX[1][1];
                  var mY=ma(mV[2],mX[2]),mZ=mV[3]+mX[3]|0,
                   mU=lF<
                    mZ?(mV[3]=0,(mV[2]=me(mY),0)):(mV[3]=mZ,(mV[2]=mY,0));}
                else{mR[1]=mW;var mU=l0(mV[2],mW);}}
             else var mU=bl(aC);return mU;
            case 1:
             var m0=[1,mM[1]],m1=lH(mL),m2=m1[1];
             if(2===m2[0]){var m3=m2[1][2];m1[1]=m0;var m4=l0(m3,m0);}else
              var m4=bl(aD);
             return m4;
            default:throw [0,d,aF];}});
       return mL;
      case 3:throw [0,d,aE];default:return bV(mN,mJ[1]);}}
   var m6=[];caml_update_dummy(m6,[0,m6,m6]);var m7=null,m8=undefined;
   function m$(m9,m_){if(m9===m8)return bV(m_,0);return m9;}
   var na=true,nb=false,nc=Date;function ne(nd){return nd;}
   function nh(nf,ng){nf.appendChild(ng);return 0;}
   function no(nk)
    {return ne
             (caml_js_wrap_callback
               (function(ni)
                 {if(ni===m8)
                   {var nj=event,nl=bV(nk,nj);nj.returnValue=nl;var nm=nl;}
                  else
                   {var nn=bV(nk,ni);if(1-(nn|0))ni.preventDefault();
                    var nm=nn;}
                  return nm;}));}
   var nG=ar.toString(),nF=aq.toString(),nE=ap.toString(),nD=ao.toString();
   function nC(np,nq,nt,nA)
    {if(np.addEventListener===m8)
      {var nr=as.toString().concat(nq),
        ny=
         function(ns)
          {var nx=[0,nt,ns,[0]];
           return bV(function(nw,nv,nu){return caml_js_call(nw,nv,nu);},nx);};
       np.attachEvent(nr,ny);
       return function(nz){return np.detachEvent(nr,ny);};}
     np.addEventListener(nq,nt,nA);
     return function(nB){return np.removeEventListener(nq,nt,nA);};}
   function nI(nH){return bV(nH,0);}var nJ=an.toString();
   function nM(nL,nK){return nL.createElement(nK.toString());}
   function nO(nN){return nM(nN,at);}var nR=[0,am];
   function nS(nP)
    {var nQ=nM(nP,av);if(1-(1-(nQ.getContext==m7?1:0)))throw [0,nR];
     return nQ;}
   var nT=window,nU=nT.document;
   function n4(nV)
    {var nW=nV.getBoundingClientRect(),nX=nU.body,nY=nU.documentElement,
      nZ=nY.clientTop,n0=nX.clientTop,n1=((nW.top|0)-n0|0)-nZ|0,
      n2=nY.clientLeft,n3=nX.clientLeft;
     return [0,((nW.left|0)-n3|0)-n2|0,n1];}
   function n6(n5){return ActiveXObject;}
   function n8(n7)
    {return caml_js_to_byte_string(escape(caml_js_from_byte_string(n7)));}
   function ok(n$)
    {var oa=
      bX(function(n9){var n_=bx(ag,n8(n9[2]));return bx(n8(n9[1]),n_);},n$);
     if(oa)
      {var oc=oa[2],ob=oa[1],od=[0,0],oe=[0,0];
       b3(function(of){od[1]+=1;oe[1]=oe[1]+of.getLen()|0;return 0;},oa);
       var og=caml_create_string(oe[1]+caml_mul(e.getLen(),od[1]-1|0)|0);
       caml_blit_string(ob,0,og,0,ob.getLen());var oh=[0,ob.getLen()];
       b3
        (function(oi)
          {caml_blit_string(e,0,og,oh[1],e.getLen());
           oh[1]=oh[1]+e.getLen()|0;
           caml_blit_string(oi,0,og,oh[1],oi.getLen());
           oh[1]=oh[1]+oi.getLen()|0;return 0;},
         oc);
       var oj=og;}
     else var oj=a8;return oj;}
   function os(on)
    {var ol=mv(0),om=ol[1],op=on*1000,
      oq=
       nT.setTimeout
        (caml_js_wrap_callback(function(oo){return l9(ol[2],0);}),op);
     mH(om,function(or){return nT.clearTimeout(oq);});return om;}
   var ot=caml_js_get_console(0),ox=4*Math.atan(1),ow=m.toString();
   function ov(ou){return ou.save();}function oz(oy){return oy.restore();}
   function oD(oA,oC,oB){return oA.scale(oC,oB);}
   function oH(oE,oG,oF){return oE.translate(oG,oF);}
   function oJ(oI){return oI.beginPath();}
   function oN(oK,oM,oL){return oK.moveTo(oM,oL);}
   function oQ(oO,oP){oO.fillStyle=oP;return oO.fill();}
   function oT(oR){var oS=oR.getContext(nJ);oS.lineWidth=2;return [0,oR,oS];}
   function oV(oU){return oU;}
   function o4(oX,o1,o0,o3,o2,oZ,oY,oW)
    {return oX[2].drawImage(oW[1],o3,o2,oZ,oY,o1,o0,oZ,oY);}
   function o9(o5){throw [0,d,o];}
   function o8(o6){var o7=o6[1];if(o7)return o7[1];throw [0,d,Y];}
   function pb(o$,o_,pa)
    {if(o_)oQ(o$,o_[1]);if(pa){o$.strokeStyle=pa[1];return o$.stroke();}
     return pa;}
   function pU(pc,pl,pk,pn,pj,pi,ph,pg)
    {var pd=o8(pc[8])[2];ov(pd);
     var pe=0===pc[1].length-1?1:0,pf=pe?0<pc[2].length-1?1:0:pe;
     if(pf)pc[1]=bK(o9,pd,pc[2]);oJ(pd);pd.rect(pj,pi,ph,pg);oQ(pd,ow);
     pd.clip();var pm=pk/pl,po=pn/pl;oD(pd,pl,pl);oH(pd,-pc[4]-pm,-pc[5]-po);
     var pp=pc[4]+pm+pj/pl,pq=pc[5]+po+pi/pl,pt=pq+pc[8][3]/pl,
      ps=pp+pc[8][2]/pl,pr=0,pu=pc[2].length-1-1|0;
     if(pr<=pu)
      {var pv=pr;
       for(;;)
        {var pw=caml_array_get(pc[1],pv),px=caml_array_get(pc[2],pv),
          py=pw[1]<=ps?1:0;
         if(py)
          {var pz=pw[2]<=pt?1:0;
           if(pz){var pA=pp<=pw[3]?1:0,pB=pA?pq<=pw[4]?1:0:pA;}else
            var pB=pz;}
         else var pB=py;
         if(pB)
          {oJ(pd);
           switch(px[0]){case 1:
             var pG=px[1];
             bM
              (function(pF,pC)
                {var pD=pC[2],pE=pC[1];if(0===pF)return oN(pd,pE,pD);
                 return pd.lineTo(pE,pD);},
               pG);
             pd.closePath();pb(pd,px[2],px[3]);break;
            case 2:
             ov(pd);oH(pd,px[1],px[2]);oD(pd,px[3],px[4]);
             pd.arc(0,0,1,0,2*ox,na);oz(pd);pb(pd,px[5],px[6]);break;
            case 3:
             var pH=px[6],pI=px[5],pK=px[4],pJ=px[3],pL=px[2],pM=px[1];
             pd.font=pK;pd.textAlign=n.toString();
             if(pI){pd.fillStyle=pI[1];pd.fillText(pJ,pM,pL);}
             if(pH){pd.strokeStyle=pH[1];pd.strokeText(pJ,pM,pL);}break;
            default:
             var pN=px[1],pO=0,pP=pN.length-1-1|0;
             if(pO<=pP)
              {var pQ=pO;
               for(;;)
                {var pR=pN[pQ+1];
                 if(0===pR[0])oN(pd,pR[1],pR[2]);else
                  pd.bezierCurveTo(pR[1],pR[2],pR[3],pR[4],pR[5],pR[6]);
                 var pS=pQ+1|0;if(pP!==pQ){var pQ=pS;continue;}break;}}
             pb(pd,px[2],px[3]);
            }}
         var pT=pv+1|0;if(pu!==pv){var pv=pT;continue;}break;}}
     return oz(pd);}
   function qS(p0,qc,qe,qi,pV)
    {var pW=pV.width,pX=pV.height;ot.time(r.toString());
     var pY=0,pZ=0,p1=p0[8],p2=br(pW,p1[2]),p3=br(pX,p1[3]),p4=p1[2]<p2?1:0,
      p5=p4?p4:p1[3]<p3?1:0;
     if(p5)
      {var p6=p1[1],p7=nS(nU);p7.width=p2;p7.height=p3;
       var p8=oT(p7),p9=p1[4];
       if(p6){var qa=p6[1],p$=p9[4],p_=p9[3];o4(oV(p8),0,0,0,0,p_,p$,qa);}
       p1[1]=[0,p8];p1[2]=p2;p1[3]=p3;}
     function qd(qb){return qb*qc+0.5|0;}
     var qg=qd(qe),qf=qd((pW/qc-p0[6])/2),qh=0<qf?-qf|0:qg,qk=qd(qi),
      qj=qd((pX/qc-p0[7])/2),ql=0<qj?-qj|0:qk,qm=p1[4][1]-qh|0,
      qn=p1[4][2]-ql|0,qo=0<qm?(p1[4][3]+qm|0)<pW?1:0:0;
     if(qo)var qp=0;else
      {if(0<qn&&(p1[4][4]+qn|0)<pX){var qp=0,qq=0;}else var qq=1;
       if(qq)
        {var qr=p1[4],qs=0===qr[3]?1:0,qt=qs?qs:0===qr[4]?1:0;
         if(1-qt)
          {var qu=o8(p1),qv=p1[4],qw=0!==qm?1:0,qx=qw?qw:0!==qn?1:0;
           if(qx){var qz=qv[4],qy=qv[3];o4(oV(qu),qm,qn,0,0,qy,qz,qu);}
           var
            qE=
             function(qA,qC,qB,qD)
              {if(0<((qA+qB|0)+qC|0))
                {if(0<=(qA+qB|0))
                  {if(qD<=(qA+qB|0))return [0,qD,0];
                   if(qD<((qA+qB|0)+qC|0))return [0,qA+qB|0,(qD-qA|0)-qB|0];
                   return [0,qA+qB|0,qC];}
                 return [0,0,(qC+qA|0)+qB|0];}
               return ab;},
            qF=qE(0,qv[3],qm,p1[2]),qG=qF[2],qH=qF[1],
            qI=qE(0,qv[4],qn,p1[3]),qJ=qI[2],qK=qI[1];
           if(0<qJ)
            if(0<qH)
             {if(pW<=(qH+qG|0)?0:1)throw [0,d,aa];
              pU(p0,qc,qh,ql,0,qK,qH,qJ);}
            else
             {if(0!==qH)throw [0,d,$];
              if(qG<pW)pU(p0,qc,qh,ql,qG,qK,pW-qG|0,qJ);}
           if(0<qK)
            {if(pX<=(qK+qJ|0)?0:1)throw [0,d,_];pU(p0,qc,qh,ql,0,0,pW,qK);}
           else
            {if(0!==qK)throw [0,d,Z];
             if(qJ<pX)pU(p0,qc,qh,ql,0,qJ,pW,pX-qJ|0);}
           p1[4]=[0,qh,ql,pW,pX];var qp=1;}
         else var qp=1;}}
     if(!qp)p1[4]=f;var qL=p1[4],qM=pZ<0?1:0;
     if(qM)var qN=qM;else
      {var qO=pY<0?1:0;
       if(qO)var qN=qO;else
        {var qP=qL[3]<(pZ+pW|0)?1:0,qN=qP?qP:qL[4]<(pY+pX|0)?1:0;}}
     if(qN){pU(p0,qc,qh,ql,0,0,pW,pX);p1[4]=[0,qh,ql,pW,pX];}var qQ=o8(p1);
     o4(oT(pV),pZ,pY,pZ,pY,pW,pX,qQ);
     try {pV.getContext(nJ).getImageData(0,0,1,1);}catch(qR){}
     ot.timeEnd(q.toString());return ot.log(p.toString(),nc.now());}
   var q5=JSON;
   if(j===0)var qT=k7([0]);else
    {var qU=j.length-1;
     if(0===qU)var qV=[0];else
      {var qW=caml_make_vect(qU,kL(j[0+1])),qX=1,qY=qU-1|0;
       if(qX<=qY)
        {var qZ=qX;
         for(;;)
          {qW[qZ+1]=kL(j[qZ+1]);var q0=qZ+1|0;
           if(qY!==qZ){var qZ=q0;continue;}break;}}
       var qV=qW;}
     var q1=k7(qV);
     bM
      (function(q2,q4)
        {var q3=caml_mul(q2,2)+2|0;q1[3]=ee(kS[3],q4,q3,q1[3]);
         q1[4]=ee(kV[3],q3,1,q1[4]);return 0;},
       j);
     var qT=q1;}
   var q6=caml_equal(i,0)?[0]:i,q7=q6.length-1,q8=h.length-1,
    q9=caml_make_vect(q7+q8|0,0),q_=0,q$=q7-1|0;
   if(q_<=q$)
    {var ra=q_;
     for(;;)
      {var rb=caml_array_get(q6,ra);
       try {var rc=bK(kS[4],rb,qT[3]),rd=rc;}
       catch(re)
        {if(re[1]===c?0:1)throw re;var rf=ls(qT);qT[3]=ee(kS[3],rb,rf,qT[3]);
         qT[4]=ee(kV[3],rf,1,qT[4]);var rd=rf;}
       caml_array_set(q9,ra,rd);var rg=ra+1|0;
       if(q$!==ra){var ra=rg;continue;}break;}}
   var rh=0,ri=q8-1|0;
   if(rh<=ri)
    {var rj=rh;
     for(;;)
      {var rk=caml_array_get(h,rj);
       try {var rl=bK(kP[4],rk,qT[7]),rm=rl;}
       catch(rn)
        {if(rn[1]===c?0:1)throw rn;var ro=qT[1];qT[1]=ro+1|0;
         if(caml_string_notequal(rk,aG))qT[7]=ee(kP[3],rk,ro,qT[7]);
         var rm=ro;}
       caml_array_set(q9,rj+q7|0,rm);var rp=rj+1|0;
       if(ri!==rj){var rj=rp;continue;}break;}}
   var ry=q9[1],rx=q9[2],rw=q9[3],rv=q9[4],ru=q9[5],rt=q9[6],rs=q9[7],
    rr=q9[8],rq=q9[9],rz=q9[10],rA=q9[11],rB=q9[12],rC=q9[13],rD=q9[14],
    rM=
     [0,ry,1,rq,rr,1,rz,rx,1,rA,rw,1,rB,rs,1,rC,rt,1,rD,rv,4,rq,ru,
      function(rF,rE,rG,rH,rI,rJ,rL)
       {if(rE)rF[rz+1]=rE[1];if(rG)rF[rA+1]=rG[1];if(rH)rF[rB+1]=rH[1];
        if(rI)rF[rC+1]=rI[1];var rK=rJ?(rF[rD+1]=rJ[1],0):rJ;return rK;}],
    rN=[0,0],rO=rM.length-1;
   for(;;)
    {if(rN[1]<rO)
      {var rP=caml_array_get(rM,rN[1]),
        rR=function(rQ){rN[1]+=1;return caml_array_get(rM,rN[1]);},rS=rR(0);
       if(typeof rS==="number")
        switch(rS){case 1:
          var rT=rR(0),
           rV=function(rT){return function(rU){return rU[rT+1];};}(rT);
          break;
         case 2:
          var rW=rR(0),rX=rR(0),
           rV=
            function(rW,rX){return function(rY){return rY[rW+1][rX+1];};}
             (rW,rX);
          break;
         case 3:
          var rZ=rR(0),
           rV=
            function(rZ){return function(r0){return bV(r0[1][rZ+1],r0);};}
             (rZ);
          break;
         case 4:
          var r1=rR(0),
           rV=
            function(r1){return function(r2,r3){r2[r1+1]=r3;return 0;};}(r1);
          break;
         case 5:
          var r4=rR(0),r5=rR(0),
           rV=function(r4,r5){return function(r6){return bV(r4,r5);};}(r4,r5);
          break;
         case 6:
          var r7=rR(0),r8=rR(0),
           rV=
            function(r7,r8){return function(r9){return bV(r7,r9[r8+1]);};}
             (r7,r8);
          break;
         case 7:
          var r_=rR(0),r$=rR(0),sa=rR(0),
           rV=
            function(r_,r$,sa)
             {return function(sb){return bV(r_,sb[r$+1][sa+1]);};}(r_,r$,sa);
          break;
         case 8:
          var sc=rR(0),sd=rR(0),
           rV=
            function(sc,sd)
             {return function(se){return bV(sc,bV(se[1][sd+1],se));};}
             (sc,sd);
          break;
         case 9:
          var sf=rR(0),sg=rR(0),sh=rR(0),
           rV=
            function(sf,sg,sh){return function(si){return bK(sf,sg,sh);};}
             (sf,sg,sh);
          break;
         case 10:
          var sj=rR(0),sk=rR(0),sl=rR(0),
           rV=
            function(sj,sk,sl)
             {return function(sm){return bK(sj,sk,sm[sl+1]);};}(sj,sk,sl);
          break;
         case 11:
          var sn=rR(0),so=rR(0),sp=rR(0),sq=rR(0),
           rV=
            function(sn,so,sp,sq)
             {return function(sr){return bK(sn,so,sr[sp+1][sq+1]);};}
             (sn,so,sp,sq);
          break;
         case 12:
          var ss=rR(0),st=rR(0),su=rR(0),
           rV=
            function(ss,st,su)
             {return function(sv){return bK(ss,st,bV(sv[1][su+1],sv));};}
             (ss,st,su);
          break;
         case 13:
          var sw=rR(0),sx=rR(0),sy=rR(0),
           rV=
            function(sw,sx,sy)
             {return function(sz){return bK(sw,sz[sx+1],sy);};}(sw,sx,sy);
          break;
         case 14:
          var sA=rR(0),sB=rR(0),sC=rR(0),sD=rR(0),
           rV=
            function(sA,sB,sC,sD)
             {return function(sE){return bK(sA,sE[sB+1][sC+1],sD);};}
             (sA,sB,sC,sD);
          break;
         case 15:
          var sF=rR(0),sG=rR(0),sH=rR(0),
           rV=
            function(sF,sG,sH)
             {return function(sI){return bK(sF,bV(sI[1][sG+1],sI),sH);};}
             (sF,sG,sH);
          break;
         case 16:
          var sJ=rR(0),sK=rR(0),
           rV=
            function(sJ,sK)
             {return function(sL){return bK(sL[1][sJ+1],sL,sK);};}(sJ,sK);
          break;
         case 17:
          var sM=rR(0),sN=rR(0),
           rV=
            function(sM,sN)
             {return function(sO){return bK(sO[1][sM+1],sO,sO[sN+1]);};}
             (sM,sN);
          break;
         case 18:
          var sP=rR(0),sQ=rR(0),sR=rR(0),
           rV=
            function(sP,sQ,sR)
             {return function(sS){return bK(sS[1][sP+1],sS,sS[sQ+1][sR+1]);};}
             (sP,sQ,sR);
          break;
         case 19:
          var sT=rR(0),sU=rR(0),
           rV=
            function(sT,sU)
             {return function(sV)
               {return bK(sV[1][sT+1],sV,bV(sV[1][sU+1],sV));};}
             (sT,sU);
          break;
         case 20:
          var sX=rR(0),sW=rR(0);ly(qT);
          var rV=
           function(sX,sW)
            {return function(sY)
              {return bV(caml_get_public_method(sW,sX),sW);};}
            (sX,sW);
          break;
         case 21:
          var sZ=rR(0),s0=rR(0);ly(qT);
          var rV=
           function(sZ,s0)
            {return function(s1)
              {var s2=s1[s0+1];return bV(caml_get_public_method(s2,sZ),s2);};}
            (sZ,s0);
          break;
         case 22:
          var s3=rR(0),s4=rR(0),s5=rR(0);ly(qT);
          var rV=
           function(s3,s4,s5)
            {return function(s6)
              {var s7=s6[s4+1][s5+1];
               return bV(caml_get_public_method(s7,s3),s7);};}
            (s3,s4,s5);
          break;
         case 23:
          var s8=rR(0),s9=rR(0);ly(qT);
          var rV=
           function(s8,s9)
            {return function(s_)
              {var s$=bV(s_[1][s9+1],s_);
               return bV(caml_get_public_method(s$,s8),s$);};}
            (s8,s9);
          break;
         default:
          var ta=rR(0),rV=function(ta){return function(tb){return ta;};}(ta);
         }
       else var rV=rS;lt[1]+=1;
       if(bK(kV[4],rP,qT[4])){lo(qT,rP+1|0);caml_array_set(qT[2],rP,rV);}else
        qT[6]=[0,[0,rP,rV],qT[6]];
       rN[1]+=1;continue;}
     var tu=
      function(tt,to,tc)
       {var td=tc?tc[1]:0;
        return function(te)
         {var tf=te?te[1]:0;
          return function(tg)
           {var th=tg?tg[1]:100;
            return function(ti)
             {var tj=ti?ti[1]:1;
              return function(tk)
               {var tl=tk?tk[1]:10;
                return function(tm)
                 {var tn=tm?tm[1]:10;
                  return function(ts)
                   {if(to)var tp=to;else
                     {var tq=caml_obj_block(eY,qT[1]);tq[0+1]=qT[2];
                      var tr=kD[1];tq[1+1]=tr;kD[1]=tr+1|0;var tp=tq;}
                    tp[rq+1]=td;tp[rz+1]=tf;tp[rA+1]=th;tp[rB+1]=tj;
                    tp[rC+1]=tl;tp[rD+1]=tn;return tp;};};};};};};};
     lp[1]=(lp[1]+qT[1]|0)-1|0;qT[8]=bS(qT[8]);
     lo(qT,3+caml_div(caml_mul(caml_array_get(qT[2],1),16),ch)|0);
     var tv=bV(tu,0),
      tK=
       function(tz,tF)
        {var tw=[0,0],tx=[0,0];
         return tz.onmousedown=
                no
                 (function(ty)
                   {tw[1]=ty.clientX;tx[1]=ty.clientY;
                    tz.style.cursor=s.toString();
                    var
                     tH=
                      nC
                       (nU,nF,
                        no
                         (function(tA)
                           {var tB=tA.clientX,tC=tA.clientY,tE=tw[1],
                             tD=tx[1];
                            tw[1]=tB;tx[1]=tC;bK(tF,tB-tE|0,tC-tD|0);
                            return na;}),
                        na),
                     tG=[0,m7];
                    tG[1]=
                    ne
                     (nC
                       (nU,nG,
                        no
                         (function(tJ)
                           {nI(tH);var tI=tG[1];if(1-(tI==m7?1:0))nI(tI);
                            tz.style.cursor=t.toString();return na;}),
                        na));
                    return na;});};
     nT.onload=
     no
      (function(wg)
        {var tL=nU.documentElement;tL.style.overflow=z.toString();
         nU.body.style.overflow=y.toString();
         nU.body.style.margin=x.toString();var tM=[0,0],tN=nM(nU,au);
         tN.innerHTML=w.toString();tN.style.display=v.toString();
         nh(nU.body,tN);
         function tP(tO)
          {if(1-tM[1])tN.style.display=A.toString();return mh(0);}
         m5(os(0.5),tP);ot.time(u.toString());
         function vN(tQ)
          {ot.timeEnd(U.toString());ot.time(T.toString());
           var tR=q5.parse(tQ.toString()),tS=tR[1],tT=tS[2],tU=tS[1];
           ot.timeEnd(S.toString());ot.time(R.toString());tM[1]=1;
           nU.body.removeChild(tN);
           var tV=[0,tR[2],tR[3],1/20,tU,tT,tS[3]-tU,tS[4]-tT,[0,0,0,0,f]],
            tW=tL.clientHeight,tY=tL.clientWidth,tX=nS(nU);
           tX.width=tY;tX.height=tW;nh(nU.body,tX);
           function t1(t0){var tZ=tX.height;return [0,0,0,tX.width,tZ];}
           var t3=t2(tv,0,0,0,0,0,0,0,0),t4=t2(tv,0,0,0,0,0,0,0,0),
            t5=t2(tv,0,0,0,N,O,P,Q,0),t6=8;
           function t9(t8)
            {var t7=tV[3];
             return Math.pow
                     (2,bV(caml_get_public_method(t5,834174833),t5)/t6)/
                    t7;}
           var t_=[0,0];
           function up(ug)
            {ot.log(V.toString(),nc.now());
             var t$=t1(0),ua=t9(0),ub=Math.ceil(t$[3]/ua),
              uc=Math.ceil(t$[4]/ua);
             ud
              (caml_get_public_method(t3,-635267918),t3,0,[0,tV[6]],
               [0,ub/20],[0,ub/2],[0,bo(ub,tV[6])],0);
             var ue=tV[6]-bV(caml_get_public_method(t3,307110897),t3);
             if(bV(caml_get_public_method(t3,834174833),t3)<0)
              bK(caml_get_public_method(t3,-659372076),t3,0);
             if(ue<bV(caml_get_public_method(t3,834174833),t3))
              bK(caml_get_public_method(t3,-659372076),t3,ue);
             ud
              (caml_get_public_method(t4,-635267918),t4,0,[0,tV[7]],
               [0,uc/20],[0,uc/2],[0,bo(uc,tV[7])],0);
             var uf=tV[7]-bV(caml_get_public_method(t4,307110897),t4);
             if(bV(caml_get_public_method(t4,834174833),t4)<0)
              bK(caml_get_public_method(t4,-659372076),t4,0);
             if(uf<bV(caml_get_public_method(t4,834174833),t4))
              bK(caml_get_public_method(t4,-659372076),t4,uf);
             if(ug)
              {var uh=bV(caml_get_public_method(t4,834174833),t4),
                ui=bV(caml_get_public_method(t3,834174833),t3);
               return qS(tV,t9(0),ui,uh,tX);}
             var uj=1-t_[1];
             if(uj)
              {t_[1]=1;
               var un=
                function(um)
                 {t_[1]=0;
                  var uk=bV(caml_get_public_method(t4,834174833),t4),
                   ul=bV(caml_get_public_method(t3,834174833),t3);
                  qS(tV,t9(0),ul,uk,tX);return mh(0);};
               m5(os(0),un);var uo=0;}
             else var uo=uj;return uo;}
           var uq=t1(0),
            ur=
             Math.ceil(Math.log(br(tV[6]/uq[3],tV[7]/uq[4]))/Math.log(2)*t6);
           ud(caml_get_public_method(t5,-635267918),t5,0,[0,ur],0,0,0,0);
           tV[3]=Math.pow(2,ur/t6);var us=[0,t9(0)];
           function uz(uv,ux)
            {var ut=t9(0),uu=1-us[1]/ut,
              uw=bV(caml_get_public_method(t3,307110897),t3)*uu*uv;
             bK
              (caml_get_public_method(t3,-659372076),t3,
               bV(caml_get_public_method(t3,834174833),t3)+uw);
             var uy=bV(caml_get_public_method(t4,307110897),t4)*uu*ux;
             bK
              (caml_get_public_method(t4,-659372076),t4,
               bV(caml_get_public_method(t4,834174833),t4)+uy);
             us[1]=ut;tV[8][4]=f;return up(0);}
           var uA=16,uB=300-uA|0;
           function uD(uC){return bK(kC,W,uC).toString();}
           var uE=uD(uA),uF=[0,uB],uG=nO(nU),uH=uG.style;
           uH.position=M.toString();uH.width=uE;uH.height=uE;
           uH.top=uD(uF[1]);uH.left=L.toString();uH.margin=K.toString();
           uH.backgroundColor=J.toString();var uI=nO(nU),uJ=uI.style;
           uJ.position=I.toString();uJ.width=uE;uJ.height=uD(uB+uA|0);
           uJ.border=H.toString();uJ.padding=G.toString();
           uJ.top=F.toString();uJ.left=E.toString();nh(uI,uG);nh(nU.body,uI);
           function uN(uK)
            {var uL=uK!==uF[1]?1:0;
             if(uL)
              {var uM=uG.style;uM.top=uD(uK);uF[1]=uK;
               bK
                (caml_get_public_method(t5,-659372076),t5,(uB-uK|0)*
                 bV(caml_get_public_method(t5,675223906),t5)/uB);
               return uz(0.5,0.5);}
             return uL;}
           tK(uG,function(uP,uO){return uN(bo(uB,br(0,uF[1]+uO|0)));});
           uI.onmousedown=
           no
            (function(uQ)
              {var uR=uQ.clientY;
               uN(br(0,bo(uB,(uR-n4(uI)[2]|0)-caml_div(uA,2)|0)));return nb;});
           nT.onresize=
           no
            (function(uT)
              {var uS=nU.documentElement;tX.width=uS.clientWidth;
               tX.height=uS.clientHeight;up(1);return na;});
           tK
            (tX,
             function(u0,u1)
              {var uX=t9(0);
               function uZ(uU,uW)
                {var uV=bV(caml_get_public_method(uU,307110897),uU),
                  uY=bV(caml_get_public_method(uU,675223906),uU)-uV;
                 return bK
                         (caml_get_public_method(uU,-659372076),uU,
                          bo
                           (bV(caml_get_public_method(uU,834174833),uU)-uW/
                            uX,uY));}
               uZ(t3,u0);uZ(t4,u1);return up(1);});
           function ve(u3,u5,u8)
            {var u2=t1(0),u4=u3/u2[3],u6=u5/u2[4],
              u7=bV(caml_get_public_method(t5,834174833),t5),
              u9=u7+u8*bV(caml_get_public_method(t5,-292814788),t5),
              u_=br(bV(caml_get_public_method(t5,-117442047),t5),u9),
              u$=bo(bV(caml_get_public_method(t5,675223906),t5),u_);
             if(u$!=u7)
              {bK(caml_get_public_method(t5,-659372076),t5,u$);
               var va=bV(caml_get_public_method(t5,675223906),t5),
                vb=uB-
                 (bV(caml_get_public_method(t5,834174833),t5)*uB/va+0.5|0)|0,
                vc=uG.style;
               vc.top=uD(vb);uF[1]=vb;
               var vd=0<=u4?u4<=1?0<=u6?u6<=1?(uz(u4,u6),1):0:0:0:0;
               if(!vd)uz(0.5,0.5);}
             return nb;}
           function vn(vg,vm,vl)
            {var vf=n4(tX),vh=vf[1],vi=vg.clientX-vh|0,vj=vf[2],
              vk=vg.clientY-vj|0;
             if(0<=vl){if(0<vl)return ve(vi,vk,-1);return nb;}
             return ve(vi,vk,1);}
           var vo=nO(nU);vo.setAttribute(ay.toString(),az.toString());
           if(typeof vo[aw.toString()]===ax.toString())
            nC
             (tX,nE,
              no
               (function(vr)
                 {var vq=40;function vs(vp){return 0;}
                  var vv=caml_div(-m$(vr.wheelDeltaX,vs)|0,vq),vu=40;
                  function vw(vt){return vr.wheelDelta;}
                  return vn(vr,vv,caml_div(-m$(vr.wheelDeltaY,vw)|0,vu));}),
              na);
           else
            nC
             (tX,nD,
              no
               (function(vx)
                 {var vy=vx.detail,vz=vx.HORIZONTAL;
                  if(vx.axis===vz)return vn(vx,vy,0);return vn(vx,0,vy);}),
              na);
           function vG(vA)
            {var vB=vA.keyCode-37|0;
             if(vB<0||3<vB){ot.log(X.toString(),vA.keyCode);return na;}
             switch(vB){case 1:
               var vC=bV(caml_get_public_method(t4,-292814788),t4);
               bK
                (caml_get_public_method(t4,-659372076),t4,
                 bV(caml_get_public_method(t4,834174833),t4)-vC);
               up(0);return nb;
              case 2:
               var vD=bV(caml_get_public_method(t3,-292814788),t3);
               bK
                (caml_get_public_method(t3,-659372076),t3,
                 bV(caml_get_public_method(t3,834174833),t3)+vD);
               up(0);return nb;
              case 3:
               var vE=bV(caml_get_public_method(t4,-292814788),t4);
               bK
                (caml_get_public_method(t4,-659372076),t4,
                 bV(caml_get_public_method(t4,834174833),t4)+vE);
               up(0);return nb;
              default:
               var vF=bV(caml_get_public_method(t3,-292814788),t3);
               bK
                (caml_get_public_method(t3,-659372076),t3,
                 bV(caml_get_public_method(t3,834174833),t3)-vF);
               up(0);return nb;
              }}
           var vH=[0,-1];
           nU.onkeydown=no(function(vI){vH[1]=vI.keyCode;return vG(vI);});
           nU.onkeypress=
           no
            (function(vK)
              {var vJ=vH[1];vH[1]=-1;if(vK.keyCode===vJ)return na;
               return vG(vK);});
           ot.time(D.toString());up(1);ot.timeEnd(C.toString());
           ot.timeEnd(B.toString());return mh(0);}
         function vP(vL)
          {var vM=vL[1];if(0!==vM&&200!==vM)return [0,[2,[0,mw,0,0]]];
           return mh(vL[3]);}
         var vO=0,vQ=0,vR=0,vS=0,vT=vS?vS[1]:vS,vU=vO?vO[1]:vO,
          vV=vQ?vR?[0,al,vR]:ak:[0,aj,vR],vW=vV[2],
          vX=vU?bx(g,bx(ai,ok(vU))):g,vY=mv(0),vZ=vY[1];
         try {var v0=new XMLHttpRequest,v1=v0;}
         catch(wf)
          {try {var v2=new (n6(0))(af.toString()),v1=v2;}
           catch(v7)
            {try {var v3=new (n6(0))(ae.toString()),v1=v3;}
             catch(v6)
              {try {var v4=new (n6(0))(ad.toString());}
               catch(v5){throw [0,d,ac];}var v1=v4;}}}
         v1.open(vV[1].toString(),vX.toString(),na);
         if(vW)v1.setRequestHeader(ah.toString(),vW[1].toString());
         b3
          (function(v8)
            {return v1.setRequestHeader(v8[1].toString(),v8[2].toString());},
           vT);
         v1.onreadystatechange=
         ne
          (function(wd)
            {var v9=4===v1.readyState?1:0;
             if(v9)
              {var wb=new MlWrappedString(v1.responseText),
                wc=
                 function(v_)
                  {var v$=v1.getResponseHeader(caml_js_from_byte_string(v_)),
                    wa=v$==m7?0:[0,new MlWrappedString(v$)];
                   return wa;};
               return l9(vY[2],[0,v1.status,wc,wb]);}
             return v9;});
         if(vQ)v1.send(ne(ok(vQ[1]).toString()));else v1.send(m7);
         mH(vZ,function(we){return v1.abort();});m5(m5(vZ,vP),vN);return nb;});
     bE(0);return;}}
  ());
