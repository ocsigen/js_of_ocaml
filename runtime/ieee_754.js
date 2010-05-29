/*
sign: 1 bit
exponent: 11 bits
mantisse: 52 bits
*/

function exponent(x) { return Math.floor(Math.LOG2E*Math.log(x)); }

// x1=ofHex([127,31,255,255,255,255,254,0]);
// x2=ofHex([127,31,255,255,255,255,255,0]);
// Math.LOG2E*Math.log(x1);
// 1010.9999999999999
// Math.LOG2E*Math.log(x2);
// 1011
function toHex(x) {
  var sign = (x>=0)?0:0x80;
  if (sign) x = -x;
  var exp = exponent(x) + 1023;
  var res = [];
  if (exp <= 0) {
    exp = 0;
    x /= Math.pow(2,-1026);
  } else {
    x /= Math.pow(2,exp-1027);
    if (x < 16) { x *= 2; exp -=1; }
    if (exp == 0) { x /= 2; }
  }
  for (i = 1; i<8; i++) {
    res[i] = x|0;
    x = (x-res[i])*256;
  }
  res[1] &= 0xf;
  res[0] = sign | (exp >> 4);
  res[1] = res[1] | ((exp & 0xf) << 4);
  return res;
}
function ofHex(x) {
  var exp = (((x[0] & 0x7f) << 4) | (x[1] >> 4));
  var res = 0;
  for (i = 7; i > 1; i--)
    res = (res + x[i]) / 256;
  res = res + (x[1] & 0xf);
  if (exp > 0) {
    res += 16
    res *= Math.pow(2,exp-1027);
  } else
    res *= Math.pow(2,-1026);
  if (x[0] & 0x80) res = - res;
  return res;
}
