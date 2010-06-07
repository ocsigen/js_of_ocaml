//Provides: MlString
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
  string:null, // JS string
  bytes:null,  // byte string
  len:null,    // length
  last:0,      // last initialized byte
  array:null,  // byte array

  toJsString:function() {
    // assumes !this.string
    return this.string = decodeURIComponent (escape(this.toFullBytes()));
  },

  toBytes:function() {
    // assumes !this.bytes
    if (this.string != null)
      var b = unescape (encodeURIComponent (this.string));
    else {
      var b = "", a = this.array, l = a.length;
      // FIX should benchmark different conversion functions
      for (var i = 0; i < l; i ++) b += String.fromCharCode (a[i]);
    }
    this.bytes = b;
    this.last = this.len = b.length;
    return b;
  },

  toFullBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    return b;
  },

  toArray:function() {
    // assumes !this.array
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

  getLen:function() {
    var len = this.len;
    if (len) return len;
    this.toBytes();
    return this.len;
  },

  getBytes:function() {
    var b = this.bytes;
    if (b != null) return b;
    this.toBytes();
    return this.bytes;
  },

  toString:function() {
    var s = this.string;
    return s?s:this.toJsString();
  },

  valueOf:function() {
    var s = this.string;
    return s?s:this.toJsString();
  },

  blit:function(i1, s2, i2, l) {
    if (l == 0) return 0;
    if (s2.bytes != null && i2 == s2.last && this.len == l && this.last == l) {
      // s2.string and s2.array are null
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
    if (!this.len) this.toBytes ();
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
    var b1 = this.toFullBytes ();
    var b2 = s2.toFullBytes ();
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
    return 0;
  },

  equal:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string == s2.string;
    return this.toFullBytes () == s2.toFullBytes ();
  },
  notEqual:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string != s2.string;
    return this.toFullBytes () != s2.toFullBytes ();
  },
}

// Conversion Javascript -> Caml
function MlWrappedString (s) {
  this.string = s;
}
MlWrappedString.prototype = new MlString();

// Uninitialized Caml string
function MlMakeString (l) {
  this.bytes = "";
  this.len = l;
}
MlMakeString.prototype = new MlString ();
