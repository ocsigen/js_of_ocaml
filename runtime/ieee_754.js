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

//Provides: jsoo_floor_log2
var log2_ok = Math.log2 && Math.log2(1.1235582092889474E+307) == 1020
function jsoo_floor_log2(x) {
    if(log2_ok) return Math.floor(Math.log2(x))
    var i = 0;
    if (x == 0) return -Infinity;
    if(x>=1) {while (x>=2) {x/=2; i++} }
    else {while (x < 1) {x*=2; i--} };
    return i;
}

//Provides: caml_int64_bits_of_float const
//Requires: jsoo_floor_log2
function caml_int64_bits_of_float (x) {
  if (!isFinite(x)) {
    if (isNaN(x)) return [255, 1, 0, 0xfff0];
    return (x > 0)?[255,0,0,0x7ff0]:[255,0,0,0xfff0];
  }
  var sign = (x==0 && 1/x == -Infinity)?0x8000:(x>=0)?0:0x8000;
  if (sign) x = -x;
  // Int64.bits_of_float 1.1235582092889474E+307 = 0x7fb0000000000000L
  // using Math.LOG2E*Math.log(x) in place of Math.log2 result in precision lost
  var exp = jsoo_floor_log2(x) + 1023;
  if (exp <= 0) {
    exp = 0;
    x /= Math.pow(2,-1026);
  } else {
    x /= Math.pow(2,exp-1027);
    if (x < 16) {
      x *= 2; exp -=1; }
    if (exp == 0) {
      x /= 2; }
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

//Provides: caml_int32_bits_of_float const
//Requires: jsoo_floor_log2
function caml_int32_bits_of_float (x) {
  var float32a = new joo_global_object.Float32Array(1);
  float32a[0] = x;
  var int32a = new joo_global_object.Int32Array(float32a.buffer);
  return int32a[0] | 0;
}

//FP literals can be written using the hexadecimal
//notation 0x<mantissa in hex>p<exponent> from ISO C99.
//https://github.com/dankogai/js-hexfloat/blob/master/hexfloat.js
//Provides: caml_hexstring_of_float const
//Requires: caml_js_to_string, caml_str_repeat
function caml_hexstring_of_float (x, prec, style) {
  if (!isFinite(x)) {
    if (isNaN(x)) return caml_js_to_string("nan");
    return caml_js_to_string ((x > 0)?"infinity":"-infinity");
  }
  var sign = (x==0 && 1/x == -Infinity)?1:(x>=0)?0:1;
  if(sign) x = -x;
  var exp = 0;
  if (x == 0) { }
  else if (x < 1) {
    while (x < 1 && exp > -1022)  { x *= 2; exp-- }
  } else {
    while (x >= 2) { x /= 2; exp++ }
  }
  var exp_sign = exp < 0 ? '' : '+';
  var sign_str = '';
  if (sign) sign_str = '-'
  else {
    switch(style){
    case 43 /* '+' */: sign_str = '+'; break;
    case 32 /* ' ' */: sign_str = ' '; break;
    default: break;
    }
  }
  if (prec >= 0 && prec < 13) {
    /* If a precision is given, and is small, round mantissa accordingly */
      var cst = Math.pow(2,prec * 4);
      x = Math.round(x * cst) / cst;
  }
  var x_str = x.toString(16);
  if(prec >= 0){
      var idx = x_str.indexOf('.');
    if(idx<0) {
      x_str += '.' + caml_str_repeat(prec, '0');
    }
    else {
      var size = idx+1+prec;
      if(x_str.length < size)
        x_str += caml_str_repeat(size - x_str.length, '0');
      else
        x_str = x_str.substr(0,size);
    }
  }
  return caml_js_to_string (sign_str + '0x' + x_str + 'p' + exp_sign + exp.toString(10));
}

//Provides: caml_int64_float_of_bits const
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
    res += 16;
    res *= Math.pow(2,exp-1027);
  } else
    res *= Math.pow(2,-1026);
  if (x[3] & 0x8000) res = - res;
  return res;
}

//Provides: caml_int32_float_of_bits const
function caml_int32_float_of_bits (x) {
  var int32a = new joo_global_object.Int32Array(1);
  int32a[0] = x;
  var float32a = new joo_global_object.Float32Array(int32a.buffer);
  return float32a[0];
}

//Provides: caml_classify_float const
function caml_classify_float (x) {
  if (isFinite (x)) {
    if (Math.abs(x) >= 2.2250738585072014e-308) return 0;
    if (x != 0) return 1;
    return 2;
  }
  return isNaN(x)?4:3;
}
//Provides: caml_modf_float const
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
//Provides: caml_ldexp_float const
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
//Provides: caml_frexp_float const
//Requires: jsoo_floor_log2
function caml_frexp_float (x) {
  if ((x == 0) || !isFinite(x)) return [0, x, 0];
  var neg = x < 0;
  if (neg) x = - x;
  var exp = jsoo_floor_log2(x) + 1;
  x *= Math.pow(2,-exp);
  if (x < 0.5) { x *= 2; exp -= 1; }
  if (neg) x = - x;
  return [0, x, exp];
}

//Provides: caml_float_compare const
function caml_float_compare (x, y) {
  if (x === y) return 0;
  if (x < y) return -1;
  if (x > y) return 1;
  if (x === x) return 1;
  if (y === y) return -1;
  return 0;
}

//Provides: caml_copysign_float const
function caml_copysign_float (x, y) {
  if (y == 0) y = 1 / y;
  x = Math.abs(x);
  return (y < 0)?(-x):x;
}

//Provides: caml_expm1_float const
function caml_expm1_float (x) {
  var y = Math.exp(x), z = y - 1;
  return (Math.abs(x)>1?z:(z==0?x:x*z/Math.log(y)));
}

//Provides: caml_log1p_float const
function caml_log1p_float (x) {
  var y = 1 + x, z = y - 1;
  return (z==0?x:x*Math.log(y)/z);
}

//Provides: caml_hypot_float const
function caml_hypot_float (x, y) {
  var x = Math.abs(x), y = Math.abs(y);
  var a = Math.max(x, y), b = Math.min(x,y) / (a?a:1);
  return (a * Math.sqrt(1 + b*b));
}

// FIX: these five functions only give approximate results.
//Provides: caml_log10_float const
function caml_log10_float (x) { return Math.LOG10E * Math.log(x); }
//Provides: caml_cosh_float const
function caml_cosh_float (x) { return (Math.exp(x) + Math.exp(-x)) / 2; }
//Provides: caml_sinh_float const
function caml_sinh_float (x) { return (Math.exp(x) - Math.exp(-x)) / 2; }
//Provides: caml_tanh_float const
function caml_tanh_float (x) {
  var y = Math.exp(x), z = Math.exp(-x);
  return (y + z) / (y - z);
}
