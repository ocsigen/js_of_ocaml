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
function caml_sys_get_config (e) { return [0, "Unix", 32]; }
function caml_update_dummy (x, y) {
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}
(function()
  {function g7(o0,o1,o2,o3,o4,o5,o6)
    {return o0.length==
            6?o0(o1,o2,o3,o4,o5,o6):caml_call_gen(o0,[o1,o2,o3,o4,o5,o6]);}
   function cR(oW,oX,oY,oZ)
    {return oW.length==3?oW(oX,oY,oZ):caml_call_gen(oW,[oX,oY,oZ]);}
   function dj(oT,oU,oV)
    {return oT.length==2?oT(oU,oV):caml_call_gen(oT,[oU,oV]);}
   function a6(oR,oS){return oR.length==1?oR(oS):caml_call_gen(oR,[oS]);}
   var a=[0,new MlString("Failure")],b=[0,new MlString("Invalid_argument")],
    c=[0,new MlString("Assert_failure")];
   caml_register_global(5,[0,new MlString("Division_by_zero")]);
   caml_register_global(3,b);caml_register_global(2,a);
   var aN=new MlString("%.12g"),aM=new MlString("."),aL=new MlString("%d"),
    aK=new MlString("true"),aJ=new MlString("false"),
    aI=new MlString("Pervasives.do_at_exit"),aH=new MlString("\\b"),
    aG=new MlString("\\t"),aF=new MlString("\\n"),aE=new MlString("\\r"),
    aD=new MlString("\\\\"),aC=new MlString("\\'"),
    aB=new MlString("String.blit"),aA=new MlString("String.sub"),
    az=new MlString("Buffer.add: cannot grow buffer"),ay=new MlString("%"),
    ax=new MlString(""),aw=new MlString(""),av=new MlString("\""),
    au=new MlString("\""),at=new MlString("'"),as=new MlString("'"),
    ar=new MlString("."),aq=new MlString("nan"),
    ap=new MlString("printf: bad positional specification (0)."),
    ao=new MlString("%_"),an=[0,new MlString("printf.ml"),143,8],
    am=new MlString("''"),
    al=new MlString("Printf: premature end of format string ``"),
    ak=new MlString("''"),aj=new MlString(" in format string ``"),
    ai=new MlString(", at char number "),
    ah=new MlString("Printf: bad conversion %"),
    ag=new MlString("Sformat.index_of_int: negative argument "),
    af=[0,new MlString("src/core/lwt.ml"),397,20],
    ae=[0,new MlString("src/core/lwt.ml"),399,8],
    ad=[0,new MlString("src/core/lwt.ml"),315,20],
    ac=[0,new MlString("src/core/lwt.ml"),318,8],
    ab=new MlString("Lwt.fast_connect"),aa=new MlString("Lwt.connect"),
    $=new MlString("Lwt.wakeup"),_=new MlString("Lwt.Canceled"),
    Z=new MlString("canvas"),Y=new MlString("img"),X=new MlString("br"),
    W=new MlString("p"),V=new MlString("div"),U=new MlString("label"),
    T=new MlString("input"),S=new MlString("select"),
    R=new MlString("option"),Q=new MlString("\""),P=new MlString(" name=\""),
    O=new MlString("\""),N=new MlString(" type=\""),M=new MlString("<"),
    L=new MlString(">"),K=new MlString(""),J=new MlString("on"),
    I=new MlString("mouseup"),H=new MlString("mousemove"),
    G=new MlString("2d"),F=new MlString("Dom_html.Canvas_not_available"),
    E=new MlString("lighter"),D=new MlString("copy"),C=new MlString("% 2.f"),
    B=new MlString(""),A=new MlString("controls"),
    z=new MlString("Click and drag mouse to rotate."),
    y=new MlString("Resume"),x=new MlString("Pause"),
    w=new MlString("Fixed position"),v=new MlString("Follow rotation"),
    u=new MlString("Reset orientation"),t=new MlString("Date:"),
    s=
     [0,new MlString("December solstice"),
      [0,new MlString("Equinox"),[0,new MlString("June solstice"),0]]],
    r=new MlString("Lighting"),q=new MlString("Clip"),
    p=new MlString("Frames per second: "),
    o=
     new MlString
      ("Credit: <a href='http://visibleearth.nasa.gov/'>Visual Earth</a>, Nasa"),
    n=[0,new MlString("planet.ml"),415,0],
    m=[0,new MlString("planet.ml"),416,0],
    l=[0,new MlString("planet.ml"),417,0],
    k=[0,new MlString("planet.ml"),418,0],j=new MlString("copy"),
    i=new MlString("checkbox"),h=new MlString("button"),g=[254,0,0,1],
    f=new MlString("../planet/texture.jpg");
   function e(d){throw [0,a,d];}function aP(aO){throw [0,b,aO];}
   function aV(aQ,aS)
    {var aR=aQ.getLen(),aT=aS.getLen(),aU=caml_create_string(aR+aT|0);
     caml_blit_string(aQ,0,aU,0,aR);caml_blit_string(aS,0,aU,aR,aT);
     return aU;}
   function aX(aW){return caml_format_int(aL,aW);}
   function a2(a1)
    {var aY=caml_ml_out_channels_list(0);
     for(;;)
      {if(aY){var aZ=aY[2];try {}catch(a0){}var aY=aZ;continue;}return 0;}}
   caml_register_named_value(aI,a2);
   function ba(a5,a3)
    {var a4=a3.length-1;if(0===a4)return [0];
     var a7=caml_make_vect(a4,a6(a5,a3[0+1])),a8=1,a9=a4-1|0;
     if(a8<=a9)
      {var a_=a8;
       for(;;)
        {a7[a_+1]=a6(a5,a3[a_+1]);var a$=a_+1|0;
         if(a9!==a_){var a_=a$;continue;}break;}}
     return a7;}
   function be(bb,bd)
    {var bc=caml_create_string(bb);caml_fill_string(bc,0,bb,bd);return bc;}
   function bj(bh,bf,bg)
    {if(0<=bf&&0<=bg&&((bh.getLen()-bg|0)<bf?0:1))
      {var bi=caml_create_string(bg);caml_blit_string(bh,bf,bi,0,bg);
       return bi;}
     return aP(aA);}
   function bp(bm,bl,bo,bn,bk)
    {if
      (0<=bk&&0<=bl&&bl<=(bm.getLen()-bk|0)&&0<=bn&&
       ((bo.getLen()-bk|0)<bn?0:1))
      return caml_blit_string(bm,bl,bo,bn,bk);
     return aP(aB);}
   var bq=caml_sys_get_config(0)[2],
    br=caml_mul(caml_div(bq,8),(1<<(bq-10|0))-1|0)-1|0;
   function bw(bs)
    {var bt=1<=bs?bs:1,bu=br<bt?br:bt,bv=caml_create_string(bu);
     return [0,bv,0,bu,bv];}
   function by(bx){return bj(bx[1],0,bx[2]);}
   function bD(bz,bB)
    {var bA=[0,bz[3]];
     for(;;)
      {if(bA[1]<(bz[2]+bB|0)){bA[1]=caml_mul(2,bA[1]);continue;}
       if(br<bA[1])if((bz[2]+bB|0)<=br)bA[1]=br;else e(az);
       var bC=caml_create_string(bA[1]);bp(bz[1],0,bC,0,bz[2]);bz[1]=bC;
       bz[3]=bA[1];return 0;}}
   function bH(bE,bG)
    {var bF=bE[2];if(bE[3]<=bF)bD(bE,1);bE[1].safeSet(bF,bG);bE[2]=bF+1|0;
     return 0;}
   function bM(bK,bI)
    {var bJ=bI.getLen(),bL=bK[2]+bJ|0;if(bK[3]<bL)bD(bK,bJ);
     bp(bI,0,bK[1],bK[2],bJ);bK[2]=bL;return 0;}
   function bO(bN){if(0<=bN)return bN;return e(aV(ag,aX(bN)));}
   function bR(bP,bQ){return bO(bP+bQ|0);}var bS=a6(bR,1);
   function bU(bT){return bj(bT,0,bT.getLen());}
   function b0(bV,bW,bY)
    {var bX=aV(aj,aV(bV,ak)),bZ=aV(ai,aV(aX(bW),bX));
     return aP(aV(ah,aV(be(1,bY),bZ)));}
   function b4(b1,b3,b2){return b0(bU(b1),b3,b2);}
   function b6(b5){return aP(aV(al,aV(bU(b5),am)));}
   function cs(b7,cd,cf,ch)
    {function cc(b8)
      {if((b7.safeGet(b8)-48|0)<0||9<(b7.safeGet(b8)-48|0))return b8;
       var b9=b8+1|0;
       for(;;)
        {var b_=b7.safeGet(b9);
         if(48<=b_){if(b_<58){var ca=b9+1|0,b9=ca;continue;}var b$=0;}else
          if(36===b_){var cb=b9+1|0,b$=1;}else var b$=0;
         if(!b$)var cb=b8;return cb;}}
     var ce=cc(cd+1|0),cg=bw((cf-ce|0)+10|0);bH(cg,37);var ci=ch,cj=0;
     for(;;)
      {if(ci){var ck=ci[2],cl=[0,ci[1],cj],ci=ck,cj=cl;continue;}
       var cm=ce,cn=cj;
       for(;;)
        {if(cm<=cf)
          {var co=b7.safeGet(cm);
           if(42===co)
            {if(cn)
              {var cp=cn[2];bM(cg,aX(cn[1]));var cq=cc(cm+1|0),cm=cq,cn=cp;
               continue;}
             throw [0,c,an];}
           bH(cg,co);var cr=cm+1|0,cm=cr;continue;}
         return by(cg);}}}
   function cz(cy,cw,cv,cu,ct)
    {var cx=cs(cw,cv,cu,ct);if(78!==cy&&110!==cy)return cx;
     cx.safeSet(cx.getLen()-1|0,117);return cx;}
   function cX(cG,cQ,cV,cA,cU)
    {var cB=cA.getLen();
     function cS(cC,cP)
      {var cD=40===cC?41:125;
       function cO(cE)
        {var cF=cE;
         for(;;)
          {if(cB<=cF)return a6(cG,cA);
           if(37===cA.safeGet(cF))
            {var cH=cF+1|0;
             if(cB<=cH)var cI=a6(cG,cA);else
              {var cJ=cA.safeGet(cH),cK=cJ-40|0;
               if(cK<0||1<cK)
                {var cL=cK-83|0;
                 if(cL<0||2<cL)var cM=1;else
                  switch(cL){case 1:var cM=1;break;case 2:
                    var cN=1,cM=0;break;
                   default:var cN=0,cM=0;}
                 if(cM){var cI=cO(cH+1|0),cN=2;}}
               else var cN=0===cK?0:1;
               switch(cN){case 1:var cI=cJ===cD?cH+1|0:cR(cQ,cA,cP,cJ);break;
                case 2:break;default:var cI=cO(cS(cJ,cH+1|0)+1|0);}}
             return cI;}
           var cT=cF+1|0,cF=cT;continue;}}
       return cO(cP);}
     return cS(cV,cU);}
   function cY(cW){return cR(cX,b6,b4,cW);}
   function ds(cZ,c_,di)
    {var c0=cZ.getLen()-1|0;
     function dk(c1)
      {var c2=c1;a:
       for(;;)
        {if(c2<c0)
          {if(37===cZ.safeGet(c2))
            {var c3=0,c4=c2+1|0;
             for(;;)
              {if(c0<c4)var c5=b6(cZ);else
                {var c6=cZ.safeGet(c4);
                 if(58<=c6)
                  {if(95===c6){var c8=c4+1|0,c7=1,c3=c7,c4=c8;continue;}}
                 else
                  if(32<=c6)
                   switch(c6-32|0){case 1:case 2:case 4:case 5:case 6:
                    case 7:case 8:case 9:case 12:case 15:break;case 0:
                    case 3:case 11:case 13:var c9=c4+1|0,c4=c9;continue;
                    case 10:var c$=cR(c_,c3,c4,105),c4=c$;continue;default:
                     var da=c4+1|0,c4=da;continue;
                    }
                 var db=c4;c:
                 for(;;)
                  {if(c0<db)var dc=b6(cZ);else
                    {var dd=cZ.safeGet(db);
                     if(126<=dd)var de=0;else
                      switch(dd){case 78:case 88:case 100:case 105:case 111:
                       case 117:case 120:var dc=cR(c_,c3,db,105),de=1;break;
                       case 69:case 70:case 71:case 101:case 102:case 103:
                        var dc=cR(c_,c3,db,102),de=1;break;
                       case 33:case 37:case 44:var dc=db+1|0,de=1;break;
                       case 83:case 91:case 115:
                        var dc=cR(c_,c3,db,115),de=1;break;
                       case 97:case 114:case 116:
                        var dc=cR(c_,c3,db,dd),de=1;break;
                       case 76:case 108:case 110:
                        var df=db+1|0;
                        if(c0<df){var dc=cR(c_,c3,db,105),de=1;}else
                         {var dg=cZ.safeGet(df)-88|0;
                          if(dg<0||32<dg)var dh=1;else
                           switch(dg){case 0:case 12:case 17:case 23:
                            case 29:case 32:
                             var dc=dj(di,cR(c_,c3,db,dd),105),de=1,dh=0;
                             break;
                            default:var dh=1;}
                          if(dh){var dc=cR(c_,c3,db,105),de=1;}}
                        break;
                       case 67:case 99:var dc=cR(c_,c3,db,99),de=1;break;
                       case 66:case 98:var dc=cR(c_,c3,db,66),de=1;break;
                       case 41:case 125:var dc=cR(c_,c3,db,dd),de=1;break;
                       case 40:var dc=dk(cR(c_,c3,db,dd)),de=1;break;
                       case 123:
                        var dl=cR(c_,c3,db,dd),dm=cR(cY,dd,cZ,dl),dn=dl;
                        for(;;)
                         {if(dn<(dm-2|0))
                           {var dp=dj(di,dn,cZ.safeGet(dn)),dn=dp;continue;}
                          var dq=dm-1|0,db=dq;continue c;}
                       default:var de=0;}
                     if(!de)var dc=b4(cZ,db,dd);}
                   var c5=dc;break;}}
               var c2=c5;continue a;}}
           var dr=c2+1|0,c2=dr;continue;}
         return c2;}}
     dk(0);return 0;}
   function dE(dD)
    {var dt=[0,0,0,0];
     function dC(dy,dz,du)
      {var dv=41!==du?1:0,dw=dv?125!==du?1:0:dv;
       if(dw)
        {var dx=97===du?2:1;if(114===du)dt[3]=dt[3]+1|0;
         if(dy)dt[2]=dt[2]+dx|0;else dt[1]=dt[1]+dx|0;}
       return dz+1|0;}
     ds(dD,dC,function(dA,dB){return dA+1|0;});return dt[1];}
   function dR(dF,dI,dQ,dG)
    {var dH=dF.safeGet(dG);if((dH-48|0)<0||9<(dH-48|0))return dj(dI,0,dG);
     var dJ=dH-48|0,dK=dG+1|0;
     for(;;)
      {var dL=dF.safeGet(dK);
       if(48<=dL)
        {if(dL<58)
          {var dO=dK+1|0,dN=caml_mul(10,dJ)+(dL-48|0)|0,dJ=dN,dK=dO;
           continue;}
         var dM=0;}
       else
        if(36===dL)
         if(0===dJ){var dP=e(ap),dM=1;}else
          {var dP=dj(dI,[0,bO(dJ-1|0)],dK+1|0),dM=1;}
        else var dM=0;
       if(!dM)var dP=dj(dI,0,dG);return dP;}}
   function dU(dS,dT){if(dS)return dT;return a6(bS,dT);}
   function dX(dV,dW){if(dV)return dV[1];return dW;}
   function gY(fZ,dZ,f$,f0,fE,gf,dY)
    {var d0=a6(dZ,dY);
     function fD(d5,ge,d1,d9)
      {var d4=d1.getLen();
       function fA(f8,d2)
        {var d3=d2;
         for(;;)
          {if(d4<=d3)return a6(d5,d0);var d6=d1.safeGet(d3);
           if(37===d6)
            {var d_=function(d8,d7){return caml_array_get(d9,dX(d8,d7));},
              eg=
               function(ei,ec,ee,d$)
                {var ea=d$;
                 for(;;)
                  {var eb=d1.safeGet(ea)-32|0;
                   if(0<=eb&&eb<=25)
                    switch(eb){case 1:case 2:case 4:case 5:case 6:case 7:
                     case 8:case 9:case 12:case 15:break;case 10:
                      return dR
                              (d1,
                               function(ed,eh)
                                {var ef=[0,d_(ed,ec),ee];
                                 return eg(ei,dU(ed,ec),ef,eh);},
                               ec,ea+1|0);
                     default:var ej=ea+1|0,ea=ej;continue;}
                   var ek=d1.safeGet(ea);
                   if(124<=ek)var el=0;else
                    switch(ek){case 78:case 88:case 100:case 105:case 111:
                     case 117:case 120:
                      var em=d_(ei,ec),
                       en=caml_format_int(cz(ek,d1,d3,ea,ee),em),
                       ep=eo(dU(ei,ec),en,ea+1|0),el=1;
                      break;
                     case 69:case 71:case 101:case 102:case 103:
                      var eq=d_(ei,ec),
                       er=caml_format_float(cs(d1,d3,ea,ee),eq),
                       ep=eo(dU(ei,ec),er,ea+1|0),el=1;
                      break;
                     case 76:case 108:case 110:
                      var es=d1.safeGet(ea+1|0)-88|0;
                      if(es<0||32<es)var et=1;else
                       switch(es){case 0:case 12:case 17:case 23:case 29:
                        case 32:
                         var eu=ea+1|0,ev=ek-108|0;
                         if(ev<0||2<ev)var ew=0;else
                          {switch(ev){case 1:var ew=0,ex=0;break;case 2:
                             var ey=d_(ei,ec),
                              ez=caml_format_int(cs(d1,d3,eu,ee),ey),
                              ex=1;
                             break;
                            default:
                             var eA=d_(ei,ec),
                              ez=caml_format_int(cs(d1,d3,eu,ee),eA),
                              ex=1;
                            }
                           if(ex){var eB=ez,ew=1;}}
                         if(!ew)
                          {var eC=d_(ei,ec),
                            eB=caml_int64_format(cs(d1,d3,eu,ee),eC);}
                         var ep=eo(dU(ei,ec),eB,eu+1|0),el=1,et=0;break;
                        default:var et=1;}
                      if(et)
                       {var eD=d_(ei,ec),
                         eE=caml_format_int(cz(110,d1,d3,ea,ee),eD),
                         ep=eo(dU(ei,ec),eE,ea+1|0),el=1;}
                      break;
                     case 83:case 115:
                      var eF=d_(ei,ec);
                      if(115===ek)var eG=eF;else
                       {var eH=[0,0],eI=0,eJ=eF.getLen()-1|0;
                        if(eI<=eJ)
                         {var eK=eI;
                          for(;;)
                           {var eL=eF.safeGet(eK),
                             eM=14<=eL?34===eL?1:92===eL?1:0:11<=eL?13<=
                              eL?1:0:8<=eL?1:0,
                             eN=eM?2:caml_is_printable(eL)?1:4;
                            eH[1]=eH[1]+eN|0;var eO=eK+1|0;
                            if(eJ!==eK){var eK=eO;continue;}break;}}
                        if(eH[1]===eF.getLen())var eP=eF;else
                         {var eQ=caml_create_string(eH[1]);eH[1]=0;
                          var eR=0,eS=eF.getLen()-1|0;
                          if(eR<=eS)
                           {var eT=eR;
                            for(;;)
                             {var eU=eF.safeGet(eT),eV=eU-34|0;
                              if(eV<0||58<eV)
                               if(-20<=eV)var eW=1;else
                                {switch(eV+34|0){case 8:
                                   eQ.safeSet(eH[1],92);eH[1]+=1;
                                   eQ.safeSet(eH[1],98);var eX=1;break;
                                  case 9:
                                   eQ.safeSet(eH[1],92);eH[1]+=1;
                                   eQ.safeSet(eH[1],116);var eX=1;break;
                                  case 10:
                                   eQ.safeSet(eH[1],92);eH[1]+=1;
                                   eQ.safeSet(eH[1],110);var eX=1;break;
                                  case 13:
                                   eQ.safeSet(eH[1],92);eH[1]+=1;
                                   eQ.safeSet(eH[1],114);var eX=1;break;
                                  default:var eW=1,eX=0;}
                                 if(eX)var eW=0;}
                              else
                               var eW=(eV-1|0)<0||56<
                                (eV-1|0)?(eQ.safeSet(eH[1],92),
                                          (eH[1]+=1,(eQ.safeSet(eH[1],eU),0))):1;
                              if(eW)
                               if(caml_is_printable(eU))eQ.safeSet(eH[1],eU);
                               else
                                {eQ.safeSet(eH[1],92);eH[1]+=1;
                                 eQ.safeSet(eH[1],48+caml_div(eU,100)|0);
                                 eH[1]+=1;
                                 eQ.safeSet
                                  (eH[1],48+caml_mod(caml_div(eU,10),10)|0);
                                 eH[1]+=1;
                                 eQ.safeSet(eH[1],48+caml_mod(eU,10)|0);}
                              eH[1]+=1;var eY=eT+1|0;
                              if(eS!==eT){var eT=eY;continue;}break;}}
                          var eP=eQ;}
                        var eG=aV(au,aV(eP,av));}
                      if(ea===(d3+1|0))var eZ=eG;else
                       {var e0=cs(d1,d3,ea,ee);
                        try
                         {var e1=0,e2=1;
                          for(;;)
                           {if(e0.getLen()<=e2)var e3=[0,0,e1];else
                             {var e4=e0.safeGet(e2);
                              if(49<=e4)
                               if(58<=e4)var e5=0;else
                                {var
                                  e3=
                                   [0,
                                    caml_int_of_string
                                     (bj(e0,e2,(e0.getLen()-e2|0)-1|0)),
                                    e1],
                                  e5=1;}
                              else
                               {if(45===e4)
                                 {var e7=e2+1|0,e6=1,e1=e6,e2=e7;continue;}
                                var e5=0;}
                              if(!e5){var e8=e2+1|0,e2=e8;continue;}}
                            var e9=e3;break;}}
                        catch(e_)
                         {if(e_[1]===a?0:1)throw e_;var e9=b0(e0,0,115);}
                        var fa=e9[2],e$=e9[1],fb=eG.getLen(),fc=0,ff=32;
                        if(e$===fb&&0===fc){var fe=eG,fd=1;}else var fd=0;
                        if(!fd)
                         if(e$<=fb)var fe=bj(eG,fc,fb);else
                          {var fg=be(e$,ff);
                           if(fa)bp(eG,fc,fg,0,fb);else
                            bp(eG,fc,fg,e$-fb|0,fb);
                           var fe=fg;}
                        var eZ=fe;}
                      var ep=eo(dU(ei,ec),eZ,ea+1|0),el=1;break;
                     case 67:case 99:
                      var fh=d_(ei,ec);
                      if(99===ek)var fi=be(1,fh);else
                       {if(39===fh)var fj=aC;else
                         if(92===fh)var fj=aD;else
                          {if(14<=fh)var fk=0;else
                            switch(fh){case 8:var fj=aH,fk=1;break;case 9:
                              var fj=aG,fk=1;break;
                             case 10:var fj=aF,fk=1;break;case 13:
                              var fj=aE,fk=1;break;
                             default:var fk=0;}
                           if(!fk)
                            if(caml_is_printable(fh))
                             {var fl=caml_create_string(1);fl.safeSet(0,fh);
                              var fj=fl;}
                            else
                             {var fm=caml_create_string(4);fm.safeSet(0,92);
                              fm.safeSet(1,48+caml_div(fh,100)|0);
                              fm.safeSet(2,48+caml_mod(caml_div(fh,10),10)|0);
                              fm.safeSet(3,48+caml_mod(fh,10)|0);var fj=fm;}}
                        var fi=aV(as,aV(fj,at));}
                      var ep=eo(dU(ei,ec),fi,ea+1|0),el=1;break;
                     case 66:case 98:
                      var fo=ea+1|0,fn=d_(ei,ec)?aK:aJ,
                       ep=eo(dU(ei,ec),fn,fo),el=1;
                      break;
                     case 40:case 123:
                      var fp=d_(ei,ec),fq=cR(cY,ek,d1,ea+1|0);
                      if(123===ek)
                       {var fr=bw(fp.getLen()),
                         fu=function(ft,fs){bH(fr,fs);return ft+1|0;};
                        ds
                         (fp,
                          function(fv,fx,fw)
                           {if(fv)bM(fr,ao);else bH(fr,37);return fu(fx,fw);},
                          fu);
                        var fy=by(fr),ep=eo(dU(ei,ec),fy,fq),el=1;}
                      else
                       {var fz=dU(ei,ec),fB=bR(dE(fp),fz),
                         ep=fD(function(fC){return fA(fB,fq);},fz,fp,d9),
                         el=1;}
                      break;
                     case 33:a6(fE,d0);var ep=fA(ec,ea+1|0),el=1;break;
                     case 37:var ep=eo(ec,ay,ea+1|0),el=1;break;case 41:
                      var ep=eo(ec,ax,ea+1|0),el=1;break;
                     case 44:var ep=eo(ec,aw,ea+1|0),el=1;break;case 70:
                      var fF=d_(ei,ec);
                      if(0===ee)
                       {var fG=caml_format_float(aN,fF),fH=0,fI=fG.getLen();
                        for(;;)
                         {if(fI<=fH)var fJ=aV(fG,aM);else
                           {var fK=fG.safeGet(fH),
                             fL=48<=fK?58<=fK?0:1:45===fK?1:0;
                            if(fL){var fM=fH+1|0,fH=fM;continue;}var fJ=fG;}
                          var fN=fJ;break;}}
                      else
                       {var fO=cs(d1,d3,ea,ee);
                        if(70===ek)fO.safeSet(fO.getLen()-1|0,103);
                        var fP=caml_format_float(fO,fF);
                        if(3<=caml_classify_float(fF))var fQ=fP;else
                         {var fR=fP.getLen();
                          if(0===fR)var fS=aq;else
                           {var fT=0;
                            for(;;)
                             {if(fR<=fT)var fU=aV(fP,ar);else
                               {if(46!==fP.safeGet(fT))
                                 {var fV=fT+1|0,fT=fV;continue;}
                                var fU=fP;}
                              var fS=fU;break;}}
                          var fQ=fS;}
                        var fN=fQ;}
                      var ep=eo(dU(ei,ec),fN,ea+1|0),el=1;break;
                     case 97:
                      var fW=d_(ei,ec),fX=a6(bS,dX(ei,ec)),fY=d_(0,fX),
                       f2=ea+1|0,f1=dU(ei,fX);
                      if(fZ)dj(f0,d0,dj(fW,0,fY));else dj(fW,d0,fY);
                      var ep=fA(f1,f2),el=1;break;
                     case 116:
                      var f3=d_(ei,ec),f5=ea+1|0,f4=dU(ei,ec);
                      if(fZ)dj(f0,d0,a6(f3,0));else a6(f3,d0);
                      var ep=fA(f4,f5),el=1;break;
                     default:var el=0;}
                   if(!el)var ep=b4(d1,ea,ek);return ep;}},
              f_=d3+1|0,f7=0;
             return dR(d1,function(f9,f6){return eg(f9,f8,f7,f6);},f8,f_);}
           dj(f$,d0,d6);var ga=d3+1|0,d3=ga;continue;}}
       function eo(gd,gb,gc){dj(f0,d0,gb);return fA(gd,gc);}return fA(ge,0);}
     var gg=dj(fD,gf,bO(0)),gh=dE(dY);
     if(gh<0||6<gh)
      {var
        gu=
         function(gi,go)
          {if(gh<=gi)
            {var gj=caml_make_vect(gh,0),
              gm=function(gk,gl){return caml_array_set(gj,(gh-gk|0)-1|0,gl);},
              gn=0,gp=go;
             for(;;)
              {if(gp)
                {var gq=gp[2],gr=gp[1];
                 if(gq){gm(gn,gr);var gs=gn+1|0,gn=gs,gp=gq;continue;}
                 gm(gn,gr);}
               return dj(gg,dY,gj);}}
           return function(gt){return gu(gi+1|0,[0,gt,go]);};},
        gv=gu(0,0);}
     else
      switch(gh){case 1:
        var gv=
         function(gx)
          {var gw=caml_make_vect(1,0);caml_array_set(gw,0,gx);
           return dj(gg,dY,gw);};
        break;
       case 2:
        var gv=
         function(gz,gA)
          {var gy=caml_make_vect(2,0);caml_array_set(gy,0,gz);
           caml_array_set(gy,1,gA);return dj(gg,dY,gy);};
        break;
       case 3:
        var gv=
         function(gC,gD,gE)
          {var gB=caml_make_vect(3,0);caml_array_set(gB,0,gC);
           caml_array_set(gB,1,gD);caml_array_set(gB,2,gE);
           return dj(gg,dY,gB);};
        break;
       case 4:
        var gv=
         function(gG,gH,gI,gJ)
          {var gF=caml_make_vect(4,0);caml_array_set(gF,0,gG);
           caml_array_set(gF,1,gH);caml_array_set(gF,2,gI);
           caml_array_set(gF,3,gJ);return dj(gg,dY,gF);};
        break;
       case 5:
        var gv=
         function(gL,gM,gN,gO,gP)
          {var gK=caml_make_vect(5,0);caml_array_set(gK,0,gL);
           caml_array_set(gK,1,gM);caml_array_set(gK,2,gN);
           caml_array_set(gK,3,gO);caml_array_set(gK,4,gP);
           return dj(gg,dY,gK);};
        break;
       case 6:
        var gv=
         function(gR,gS,gT,gU,gV,gW)
          {var gQ=caml_make_vect(6,0);caml_array_set(gQ,0,gR);
           caml_array_set(gQ,1,gS);caml_array_set(gQ,2,gT);
           caml_array_set(gQ,3,gU);caml_array_set(gQ,4,gV);
           caml_array_set(gQ,5,gW);return dj(gg,dY,gQ);};
        break;
       default:var gv=dj(gg,dY,[0]);}
     return gv;}
   function g2(gX){return bw(caml_mul(2,gX.getLen()));}
   function g4(g1,gZ){var g0=by(gZ);gZ[2]=0;return a6(g1,g0);}
   function g_(g3)
    {var g6=a6(g4,g3);return g7(gY,1,g2,bH,bM,function(g5){return 0;},g6);}
   function g$(g9){return dj(g_,function(g8){return g8;},g9);}32===bq;
   var ha=[0,_],hg=42;
   function he(hb)
    {var hc=hb[1];
     {if(3===hc[0])
       {var hd=hc[1],hf=he(hd);if(hf!==hd)hb[1]=[3,hf];return hf;}
      return hb;}}
   function hi(hh){return he(hh);}
   function hB(hj,hq)
    {var hk=hj,hl=0;
     for(;;)
      {if(hk instanceof Array)
        switch(hk[0]){case 1:
          var hp=hk[1];
          if(hl){a6(hp,hq);var hs=hl[2],hr=hl[1],hk=hr,hl=hs;continue;}
          var ho=a6(hp,hq);break;
         case 2:var hu=[0,hk[2],hl],ht=hk[1],hk=ht,hl=hu;continue;default:
          var hv=hk[1][1];
          if(hv)
           {var hw=hv[1];
            if(hl){a6(hw,hq);var hy=hl[2],hx=hl[1],hk=hx,hl=hy;continue;}
            var ho=a6(hw,hq);}
          else{if(hl){var hA=hl[2],hz=hl[1],hk=hz,hl=hA;continue;}var ho=hl;}
         }
       else{if(hl){var hn=hl[2],hm=hl[1],hk=hm,hl=hn;continue;}var ho=hl;}
       return ho;}}
   function hK(hE,hC)
    {var hD=[0,hC],hF=he(hE),hG=hF[1];
     switch(hG[0]){case 1:
       if(hG[1][1]===ha){var hI=0,hH=1;}else var hH=0;break;
      case 2:var hJ=hG[1][2];hF[1]=hD;var hI=hB(hJ,hD),hH=1;break;default:
       var hH=0;
      }
     if(!hH)var hI=aP($);return hI;}
   function hN(hL,hM)
    {if(hL instanceof Array?0:1)return hM;
     if(hM instanceof Array?0:1)return hL;return [2,hL,hM];}
   function hR(hO)
    {if(hO instanceof Array)
      switch(hO[0]){case 0:var hP=hO[1][1],hQ=hP?hO:hP;return hQ;case 2:
        var hS=hR(hO[2]);return hN(hR(hO[1]),hS);
       default:}
     return hO;}
   function hU(hT){return [0,[1,hT]];}
   function h5(h4)
    {var hV=[],h3=0,h2=0;
     caml_update_dummy
      (hV,
       [0,
        [2,
         [0,
          [0,
           function(h1)
            {var hW=he(hV),hX=hW[1];
             if(2===hX[0])
              {var hZ=hX[1][2],hY=[1,[0,ha]];hW[1]=hY;var h0=hB(hZ,hY);}
             else var h0=0;return h0;}],
          h2,h3]]]);
     return [0,hV,hV];}
   function h9(h6,h7)
    {var h8=(h6[2] instanceof Array?0:1)?[1,h7]:[2,[1,h7],h6[2]];h6[2]=h8;
     return 0;}
   function ix(h_,id)
    {var h$=hi(h_)[1];
     switch(h$[0]){case 1:return hU(h$[1]);case 2:
       var ia=h$[1],ib=[0,[2,[0,ia[1],0,0]]];
       h9
        (ia,
         function(ic)
          {switch(ic[0]){case 0:
             try {var ie=a6(id,ic[1]),ig=ie;}catch(ih){var ig=hU(ih);}
             var ii=hi(ib),ij=hi(ig),ik=ii[1];
             if(2===ik[0])
              if(ii===ij)var il=0;else
               {var im=ik[1],io=ij[1];
                if(2===io[0])
                 {var ip=io[1];ij[1]=[3,ii];im[1][1]=ip[1][1];
                  var iq=hN(im[2],ip[2]),ir=im[3]+ip[3]|0,
                   il=hg<
                    ir?(im[3]=0,(im[2]=hR(iq),0)):(im[3]=ir,(im[2]=iq,0));}
                else{ii[1]=io;var il=hB(im[2],io);}}
             else var il=aP(aa);return il;
            case 1:
             var is=[1,ic[1]],it=hi(ib),iu=it[1];
             if(2===iu[0]){var iv=iu[1][2];it[1]=is;var iw=hB(iv,is);}else
              var iw=aP(ab);
             return iw;
            default:throw [0,c,ad];}});
       return ib;
      case 3:throw [0,c,ac];default:return a6(id,h$[1]);}}
   var iy=[];caml_update_dummy(iy,[0,iy,iy]);
   var iz=null,iA=undefined,iB=true,iC=false,iD=Date,iG=Array;
   function iF(iE){return iE;}
   function iJ(iH,iI){iH.appendChild(iI);return 0;}var iO=caml_js_on_ie(0)|0;
   function iN(iM)
    {return iF
             (caml_js_wrap_callback
               (function(iK){var iL=iK===iA?event:iK;return a6(iM,iL);}));}
   var i4=I.toString(),i3=H.toString();
   function i2(iP,iQ,iT,i0)
    {if(iP.addEventListener===iA)
      {var iR=J.toString().concat(iQ),
        iY=
         function(iS)
          {var iX=[0,iT,iS,[0]];
           return a6(function(iW,iV,iU){return caml_js_call(iW,iV,iU);},iX);};
       iP.attachEvent(iR,iY);
       return function(iZ){return iP.detachEvent(iR,iY);};}
     iP.addEventListener(iQ,iT,i0);
     return function(i1){return iP.removeEventListener(iQ,iT,i0);};}
   function i6(i5){return a6(i5,0);}var i7=G.toString();
   function i_(i8,i9){if(i8)return a6(i9,i8[1]);return i8;}
   function jb(ja,i$){return ja.createElement(i$.toString());}
   function je(jd,jc){return jb(jd,jc);}
   function jp(jf,jg,ji,jh)
    {if(0===jf&&0===jg)return jb(ji,jh);
     if(iO)
      {var jj=new iG;jj.push(M.toString(),jh.toString());
       i_
        (jf,
         function(jk)
          {jj.push(N.toString(),caml_js_html_escape(jk),O.toString());
           return 0;});
       i_
        (jg,
         function(jl)
          {jj.push(P.toString(),caml_js_html_escape(jl),Q.toString());
           return 0;});
       jj.push(L.toString());return ji.createElement(jj.join(K.toString()));}
     var jm=jb(ji,jh);i_(jf,function(jn){return jm.type=jn;});
     i_(jg,function(jo){return jm.name=jo;});return jm;}
   function jt(js,jr,jq){return jp(js,jr,jq,T);}
   function jv(ju){return je(ju,U);}function jx(jw){return je(jw,V);}
   var jy=window,jz=jy.document,jA=600,jB=4*Math.atan(1),jC=23.5*jB/180,
    jD=2,jE=Math.pow(0.2,jD),jF=h.toString(),jQ=[0,F];
   function jP(jH,jJ,jN)
    {var jG=[0,0],jI=jH.toString(),jL=jJ.toString(),jK=jt([0,jF],0,jz);
     jK.value=jI;
     jK.onclick=
     iN
      (function(jO)
        {jG[1]=1-jG[1];var jM=jG[1]?jL:jI;jK.value=jM;a6(jN,jG[1]);
         return iB;});
     return jK;}
   function jX(jW,jS,jT)
    {var jR=jt([0,i.toString()],0,jz);jR.checked=!!jS;
     jR.onclick=iN(function(jU){a6(jT,jR.checked|0);return iB;});
     var jV=jv(jz);iJ(jV,jR);iJ(jV,jz.createTextNode(jW.toString()));
     return jV;}
   function j1(j0,jZ,jY){return [-1,j0,jZ,jY];}
   function j4(j3,j2){return [-1,j2[1]-j3[1],j2[2]-j3[2],j2[3]-j3[3]];}
   function kb(j9,j5)
    {var j6=j5[3],j7=j5[2],j8=j5[1],j_=j9[3],j$=j9[2],ka=j9[1];
     return [-1,j8*ka[1]+j7*ka[2]+j6*ka[3],j8*j$[1]+j7*j$[2]+j6*j$[3],j8*
             j_[1]+j7*j_[2]+j6*j_[3]];}
   function kg(kc)
    {var kd=kc[3],ke=kc[2],kf=kc[1];
     return [0,[-1,kf[1],ke[1],kd[1]],[-1,kf[2],ke[2],kd[2]],
             [-1,kf[3],ke[3],kd[3]]];}
   function km(kj,kh)
    {var ki=kg(kh),kk=kb(ki,kj[3]),kl=kb(ki,kj[2]);
     return [0,kb(ki,kj[1]),kl,kk];}
   function ks(kn)
    {var ko=Math.cos(kn),kp=Math.sin(kn),kq=j1(-kp,0,ko),kr=j1(0,1,0);
     return [0,j1(ko,0,kp),kr,kq];}
   function ky(kt)
    {var ku=Math.cos(kt),kv=Math.sin(kt),kw=j1(0,0,1),kx=j1(-kv,ku,0);
     return [0,j1(ku,kv,0),kx,kw];}
   var kz=ks(0);function kD(kC,kB,kA){return [0,kC,kB,kA];}
   function kH(kF,kG)
    {var kE=je(jz,Z);if(1-(1-(kE.getContext==iz?1:0)))throw [0,jQ];
     kE.width=kF;kE.height=kG;return kE;}
   function kK(kI,kJ){if(kI<kJ)return kI;return kJ;}
   function kN(kL,kM){if(kL<kM)return kM;return kL;}
   var kO=8,kP=12,kQ=jB/kO,kR=caml_mul(kO,kP),
    kS=caml_make_vect(kR+2|0,j1(0,0,0)),
    kT=caml_make_vect(caml_mul(kR,2),kD(0,0,0)),kU=kR+1|0;
   caml_array_set(kS,kR,j1(0,-1,0));caml_array_set(kS,kU,j1(0,1,0));
   var kV=0,kW=kP-1|0,k1=2*jB/kP,k3=(jB-kQ)/2;
   if(kV<=kW)
    {var kX=kV;
     for(;;)
      {var kY=0,kZ=kO-1|0;
       if(kY<=kZ)
        {var k0=kY;
         for(;;)
          {var k2=kX*k1,k4=k0*kQ-k3,k5=caml_mul(kX,kO)+k0|0;
           caml_array_set
            (kS,k5,
             j1
              (Math.cos(k2)*Math.cos(k4),Math.sin(k4),Math.sin(k2)*
               Math.cos(k4)));
           if(0===k0)
            {caml_array_set(kT,caml_mul(2,k5),kD(kR,k5,caml_mod(k5+kO|0,kR)));
             caml_array_set
              (kT,caml_mul(2,k5)+1|0,
               kD(kU,caml_mod((k5+caml_mul(2,kO)|0)-1|0,kR),(k5+kO|0)-1|0));}
           else
            {caml_array_set
              (kT,caml_mul(2,k5),kD(k5,caml_mod(k5+kO|0,kR),k5-1|0));
             caml_array_set
              (kT,caml_mul(2,k5)+1|0,
               kD(k5-1|0,caml_mod(k5+kO|0,kR),caml_mod((k5+kO|0)-1|0,kR)));}
           var k6=k0+1|0;if(kZ!==k0){var k0=k6;continue;}break;}}
       var k7=kX+1|0;if(kW!==kX){var kX=k7;continue;}break;}}
   var oN=f.toString();
   jy.onload=
   iN
    (function(oQ)
      {function oE(k8)
        {var k9=k8.width,k_=k8.height,k$=kH(k9,k_),la=k$.getContext(i7),
          lb=caml_div(k_,8),lc=caml_div(k9,8),ld=la.getImageData(0,0,lc,lb),
          le=ld.data,lu=1/jD;
         function ly(lf)
          {var lg=0,lh=lb-1|0,lo=Math.cos(lf),ln=-Math.sin(lf);
           if(lg<=lh)
            {var li=lg;
             for(;;)
              {var lj=0,lk=caml_div(lc,2)-1|0;
               if(lj<=lk)
                {var ll=lj;
                 for(;;)
                  {var lm=(li/lb-0.5)*jB,
                    lp=Math.cos(ll/lc*2*jB)*Math.cos(lm)*lo+Math.sin(lm)*ln,
                    ls=4*(ll+li*lc)|0,lr=4*(lc-ll+li*lc-1)|0,
                    lq=0<lp?jE:jE-lp*(1-jE)*1.2,lt=lq<=1?lq:1,
                    lv=255-(255.99*Math.pow(lt,lu)|0)|0;
                   le[ls+3|0]=lv;le[lr+3|0]=lv;var lw=ll+1|0;
                   if(lk!==ll){var ll=lw;continue;}break;}}
               var lx=li+1|0;if(lh!==li){var li=lx;continue;}break;}}
           la.putImageData(ld,0,0);la.globalCompositeOperation=j.toString();
           la.save();la.scale(8*(lc+2|0)/lc,8*(lb+2|0)/lb);
           la.translate(-1,-1);la.drawImage(k$,0,0);return la.restore();}
         ly(jC);
         var lz=k8.width,lA=kH(lz,k8.height),lB=lA.getContext(i7),lC=[0,0],
          lD=kH(jA,jA),lE=kH(jA,jA);
         iJ(jz.body,lD);
         var lF=lD.getContext(i7),lG=lE.getContext(i7),lH=jA/2,lI=k8.width,
          lJ=k8.height,
          lO=
           ba
            (function(lK)
              {var lM=lK[2],
                lL=(lI-Math.atan2(lK[3],lK[1])*((lI/2-0.99)/jB)|0)%lI,
                lN=lJ/2+Math.asin(lM)*((lJ-0.99)/jB)|0;
               if(0<=lL)
                {if(lL<lI)
                  {if(0<=lN){if(lN<lJ)return [0,lL,lN];throw [0,c,k];}
                   throw [0,c,l];}
                 throw [0,c,m];}
               throw [0,c,n];},
             kS),
          mI=
           ba
            (function(lP)
              {var lS=lP[3],lR=lP[2],lQ=caml_array_get(kS,lP[1]),
                lU=caml_array_get(kS,lR),lT=j4(lQ,caml_array_get(kS,lS)),
                lV=j4(lQ,lU),lW=lT[3],lX=lT[2],lY=lT[1],lZ=lV[3],l0=lV[2],
                l1=lV[1];
               return [-1,l0*lW-lX*lZ,lZ*lY-lW*l1,l1*lX-lY*l0];},
             kT),
          mK=
           ba
            (function(l2)
              {var l5=l2[3],l4=l2[2],l3=caml_array_get(lO,l2[1]),l6=l3[2],
                l7=l3[1],l8=caml_array_get(lO,l4),l9=l8[2],l_=l8[1],
                l$=caml_array_get(lO,l5),ma=l$[2],mb=l$[1],mc=lI/2;
               if(l7==0)
                {if(mc<l_||mc<mb)var md=1;else{var me=0,md=0;}
                 if(md){var mf=lI-2,me=1;}}
               else var me=0;if(!me)var mf=l7;
               if(l_==0)
                {if(mc<mf||mc<mb)var mg=1;else{var mh=0,mg=0;}
                 if(mg){var mi=lI-2,mh=1;}}
               else var mh=0;if(!mh)var mi=l_;
               if(mb==0)
                {if(mc<mi||mc<mf)var mj=1;else{var mk=0,mj=0;}
                 if(mj){var ml=lI-2,mk=1;}}
               else var mk=0;if(!mk)var ml=mb;var mm=lJ-2;
               if(l6==0||mm<=l6)var mn=0;else{var mo=mf,mn=1;}
               if(!mn)var mo=(mi+ml)/2;
               if(l9==0||mm<=l9)var mp=0;else{var mq=mi,mp=1;}
               if(!mp)var mq=(mo+ml)/2;
               if(ma==0||mm<=ma)var mr=0;else{var ms=ml,mr=1;}
               if(!mr)var ms=(mq+mo)/2;
               var mt=kN(1,mo),mu=kN(1,mq),mv=kN(1,ms),mw=kN(1,l6),
                mx=kN(1,l9),my=kN(1,ma),mz=mu-mt,mA=mv-mt,mB=mx-mw,mC=my-mw,
                mD=mB*mA-mC*mz,mE=mz*mC-mA*mB,mF=kN(0,kK(mt,kK(mu,mv))-4),
                mG=kN(0,kK(mw,kK(mx,my))-4),mH=kK(lI,kN(mt,kN(mu,mv))+4);
               return [0,mt,mw,mz/mD,mB/mE,mA/mD,mC/mE,mF,mG,mH-mF,
                       kK(lJ,kN(mw,kN(mx,my))+4)-mG];},
             kT),
          mJ=[0,0],mL=[0,0],mM=[0,1],mN=[0,1],mO=[0,jC],mP=[0,ky(-jC)],
          mQ=[0,kz],mR=[0,0],mS=jz.createTextNode(B.toString()),mT=jx(jz);
         mT.className=A.toString();var mU=jx(jz);
         iJ(mU,jz.createTextNode(z.toString()));iJ(mT,mU);var mV=jx(jz);
         function mX(mW){return je(jz,X);}
         iJ(mV,jP(x,y,function(mY){mJ[1]=mY;return 0;}));iJ(mV,mX(0));
         iJ(mV,jP(v,w,function(mZ){mL[1]=mZ;return 0;}));iJ(mV,mX(0));
         var m0=jt([0,jF],0,jz);m0.value=u.toString();
         m0.onclick=
         iN(function(m1){mQ[1]=kz;mR[1]=0;mP[1]=ky(-mO[1]);return iB;});
         iJ(mV,m0);iJ(mV,mX(0));var m2=jv(jz);
         iJ(m2,jz.createTextNode(t.toString()));var m3=jp(0,0,jz,S),m4=s;
         for(;;)
          {if(m4)
            {var m7=m4[2],m6=m4[1],m5=je(jz,R);
             iJ(m5,jz.createTextNode(m6.toString()));m3.add(m5,iz);var m4=m7;
             continue;}
           m3.onchange=
           iN
            (function(m_)
              {var m8=m3.selectedIndex,m9=0===m8?jC:1===m8?0:-jC;ly(m9);
               mO[1]=m9;return iB;});
           iJ(m2,m3);iJ(mV,m2);iJ(mT,mV);var m$=jx(jz);
           iJ(m$,jX(r,1,function(na){mM[1]=na;return 0;}));iJ(m$,mX(0));
           iJ(m$,jX(q,1,function(nb){mN[1]=nb;return 0;}));iJ(m$,mX(0));
           iJ(m$,jz.createTextNode(p.toString()));iJ(m$,mS);iJ(mT,m$);
           iJ(jz.body,mT);var nc=je(jz,W);nc.innerHTML=o.toString();
           iJ(jz.body,nc);var nd=[0,0],ne=[0,0];
           lD.onmousedown=
           iN
            (function(nf)
              {nd[1]=nf.clientX;ne[1]=nf.clientY;
               var
                nt=
                 i2
                  (jz,i3,
                   iN
                    (function(ng)
                      {var nh=ng.clientX,ni=ng.clientY,nj=nh-nd[1]|0,
                        nk=ni-ne[1]|0;
                       if(0!==nk)
                        {var nm=mQ[1],nl=2*nk/jA,nn=Math.cos(nl),
                          no=Math.sin(nl),np=j1(0,-no,nn),nq=j1(0,nn,no);
                         mQ[1]=km([0,j1(1,0,0),nq,np],nm);}
                       if(0!==nj){var nr=mQ[1];mQ[1]=km(ks(2*nj/jA),nr);}
                       nd[1]=nh;ne[1]=ni;return iB;}),
                   iB),
                ns=[0,iz];
               ns[1]=
               iF
                (i2
                  (jz,i4,
                   iN
                    (function(nv)
                      {i6(nt);var nu=ns[1];if(1-(nu==iz?1:0))i6(nu);
                       return iB;}),
                   iB));
               return iC;});
           var nw=[0,iD.now()],nx=[0,0],
            on=
             function(oh,ny)
              {var nA=ks(ny-mR[1]);
               if(mM[1])
                {lC[1]=0;lB.drawImage(k8,0,0);
                 var nz=(2*jB-ny%(2*jB))*lz/2/jB%lz|0;lB.drawImage(k$,nz,0);
                 lB.drawImage(k$,nz-lz,0);}
               else if(!lC[1]){lB.drawImage(k8,0,0);lC[1]=1;}
               var nB=km(mQ[1],km(mP[1],nA)),
                nD=ba(function(nC){return kb(nB,nC);},kS),nE=kb(kg(nB),g);
               lG.clearRect(0,0,jA,jA);lG.save();
               if(mN[1])
                {lG.beginPath();lG.arc(lH,lH,lH*0.95,0,-2*jB,iB);lG.clip();}
               lG.setTransform(lH-2,0,0,lH-2,lH,lH);
               lG.globalCompositeOperation=E.toString();
               var nF=0,nG=kT.length-1-1|0;
               if(nF<=nG)
                {var nH=nF;
                 for(;;)
                  {var nI=kT[nH+1],nL=nI[3],nK=nI[2],
                    nJ=caml_array_get(nD,nI[1]),nM=nJ[2],nN=nJ[1],
                    nO=caml_array_get(nD,nK),nP=nO[2],nQ=nO[1],
                    nR=caml_array_get(nD,nL),nS=nR[2],nT=nR[1],
                    nU=caml_array_get(mI,nH);
                   if(0<=nU[1]*nE[1]+nU[2]*nE[2]+nU[3]*nE[3])
                    {lG.beginPath();lG.moveTo(nN,nM);lG.lineTo(nQ,nP);
                     lG.lineTo(nT,nS);lG.closePath();lG.save();lG.clip();
                     var nV=caml_array_get(mK,nH),nW=nV[10],nX=nV[9],
                      nY=nV[8],nZ=nV[7],n0=nV[6],n1=nV[5],n2=nV[4],n3=nV[3],
                      n4=nV[2],n5=nV[1],n6=nQ-nN,n7=nT-nN,n8=nP-nM,n9=nS-nM,
                      n_=n6*n0-n7*n2,n$=n6*n1-n7*n3,oa=n8*n0-n9*n2,
                      ob=n8*n1-n9*n3;
                     lG.transform(n_,oa,n$,ob,nN-n_*n5-n$*n4,nM-oa*n5-ob*n4);
                     lG.drawImage(lA,nZ,nY,nX,nW,nZ,nY,nX,nW);lG.restore();}
                   var oc=nH+1|0;if(nG!==nH){var nH=oc;continue;}break;}}
               lG.restore();lF.globalCompositeOperation=D.toString();
               lF.drawImage(lE,0,0);
               try {lF.getImageData(0,0,1,1);}catch(oC){}
               var od=iD.now(),oe=1000/(od-nw[1]),
                of=nx[1]==0?oe:0.9*nx[1]+0.1*oe;
               nx[1]=of;mS.data=dj(g$,C,nx[1]).toString();nw[1]=od;
               function oq(oo)
                {var og=iD.now(),oi=og-oh,oj=oi<0?0:1000<oi?0:oi,
                  ok=2*jB*oj/1000/10,ol=mJ[1]?0:mL[1]?(mR[1]=mR[1]+ok,1):0;
                 ol;var om=mJ[1]?ny:ny+ok;return on(og,om);}
               var op=h5(0),or=op[1],ot=0.01*1000,
                ou=
                 jy.setTimeout
                  (caml_js_wrap_callback(function(os){return hK(op[2],0);}),
                   ot);
               function ow(ov){return jy.clearTimeout(ou);}var ox=hi(or)[1];
               switch(ox[0]){case 1:var oy=ox[1][1]===ha?(ow(0),1):0;break;
                case 2:
                 h9
                  (ox[1],
                   function(oz)
                    {if(1===oz[0]&&oz[1][1]===ha)
                      {try {var oA=ow(0);}catch(oB){return 0;}return oA;}
                     return 0;});
                 var oy=1;break;
                default:var oy=0;}
               oy;return ix(or,oq);};
           return on(iD.now(),0);}}
       var oD=je(jz,Y);function oH(oF){return [0,[0,oD]];}
       var oG=h5(0),oJ=oG[2],oK=oG[1];function oL(oI){return hK(oJ,oI);}
       oD.onload=iN(function(oM){oL(0);return iC;});oD.src=oN;
       var oO=hi(ix(ix(oK,oH),oE))[1];
       switch(oO[0]){case 1:throw oO[1];case 2:
         h9
          (oO[1],
           function(oP)
            {switch(oP[0]){case 0:return 0;case 1:throw oP[1];default:
               throw [0,c,af];
              }});
         break;
        case 3:throw [0,c,ae];default:}
       return iC;});
   a2(0);return;}
  ());
