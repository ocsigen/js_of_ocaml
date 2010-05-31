
function caml_int64_bits_of_float (x) {
  if (!isFinite(x)) {
    if (isNaN(x)) return [255, 1, 0, 0xfff0];
    return (x > 0)?[255,0,0,0x7ff0]:[255,0,0,0xfff0];
  }
  var sign = (x>=0)?0:0x8000;
  if (sign) x = -x;
  var exp = Math.floor(Math.LOG2E*Math.log(x)) + 1023;
  var res = [];
  if (exp <= 0) {
    exp = 0;
    x /= Math.pow(2,-1026);
  } else {
    x /= Math.pow(2,exp-1027);
    if (x < 16) { x *= 2; exp -=1; }
    if (exp == 0) { x /= 2; }
  }
  var k = Math.pow(2,24);
  var r3 = x|0;
  x = (x - r3) * k;
  var r2 = x|0;
  x = (x - r2) * k;
  var r1 = x|0;
  r3 = (r3 &0xf) | sign | exp << 4;
  return [255, r1, r2, r3];
}
function caml_int64_float_of_bits (x) {
  var exp = (x[3] & 0x7fff) >> 4;
  if (exp == 2047) {
      if ((x[1]|x[2]|(x[3]&0xf)) == 0)
        return (x[3] & 0x8000)?(-Infinity):Infinity;
      else
        return NaN;
  }
  var k = Math.pow(2,-24);
  var res = (x[1]*k+x[2])*k+(x[3]&0xf);
  if (exp > 0) {
    res += 16
    res *= Math.pow(2,exp-1027);
  } else
    res *= Math.pow(2,-1026);
  if (x[3] & 0x8000) res = - res;
  return res;
}

function caml_classify_float (x) {
  if (isFinite (x)) {
    if (Math.abs(x) >= 2.2250738585072014e-308) return 0;
    if (x != 0) return 1;
    return 2;
  }
  return isNaN(x)?4:3;
}
function caml_modf_float (x) {
  if (isFinite (x)) {
    var neg = (1/x) < 0;
    x = Math.abs(x);
    var i = Math.floor (x);
    var f = x - i;
    if (neg) { i = -i; f = -f; }
    return [0, f, i];
  }
  if (isNaN (x)) return [0, NaN, NaN];
  return [0, 1/x, x];
}
function caml_ldexp_float (x,exp) {
  exp |= 0;
  if (exp > 1023) {
    exp -= 1023;
    x *= Math.pow(2, 1023);
    if (exp > 1023) {  // in case x is subnormal
      exp -= 1023;
      x *= Math.pow(2, 1023);
    }
  }
  if (exp < -1023) {
    exp += 1023;
    x *= Math.pow(2, -1023);
  }
  x *= Math.pow(2, exp);
  return x;
}
function caml_frexp_float (x) {
  if ((x == 0) || !isFinite(x)) return [0, x, 0];
  var neg = x < 0;
  if (neg) x = - x;
  var exp = Math.floor(Math.LOG2E*Math.log(x)) + 1;
  x *= Math.pow(2,-exp);
  if (x < 0.5) { x *= 2; exp -= 1; }
  if (neg) x = - x;
  return [0, x, exp];
}

//FIX: should use this function (and also in generic compare)
function caml_float_compare (x, y) {
  if (x === y) return 0;
  if (x < y) return -1;
  if (x > y) return 1;
  if (x === x) return 1;
  if (y === y) return -1;
  return 0;
}

// These four functions only give approximate results.
function caml_log10_float (x) { return Math.LOG10E * Math.log(x); }
function caml_cosh_float (x) { return (Math.exp(x) + Math.exp(-x)) / 2; }
function caml_sinh_float (x) { return (Math.exp(x) - Math.exp(-x)) / 2; }
function caml_tanh (x) {
  var y = Math.exp(x), z = Math.exp(-x);
  return (y + z) / (y - z);
}
