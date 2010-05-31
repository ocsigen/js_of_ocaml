
var caml_int64_offset = Math.pow(2, -24);

function caml_int64_ucompare(x,y) {
  if (x[3] > y[3]) return 1;
  if (x[3] < y[3]) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}

function caml_int64_ult(x,y) { return caml_int64_ucompare(x,y) < 0; }

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

function caml_int64_neg (x) {
  y1 = - x[1];
  y2 = - x[2] + (y1 >> 24);
  y3 = - x[3] + (y2 >> 24);
  return [255, y1 & 0xffffff, y2 & 0xffffff, y3 & 0xffff];
}

function caml_int64_add (x, y) {
  z1 = x[1] + y[1];
  z2 = x[2] + y[2] + (z1 >> 24);
  z3 = x[3] + y[3] + (z2 >> 24);
  return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}

function caml_int64_sub (x, y) {
  z1 = x[1] - y[1];
  z2 = x[2] - y[2] + (z1 >> 24);
  z3 = x[3] - y[3] + (z2 >> 24);
  return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}

function caml_int64_mul(x,y) {
  z1 = x[1] * y[1];
  z2 = ((z1 * caml_int64_offset) | 0) + x[2] * y[1] + x[1] * y[2];
  z3 = ((z2 * caml_int64_offset) | 0) + x[3] * y[1] + x[2] * y[2] + x[1] * y[3];
  return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}

function caml_int64_is_zero(x) {
  return (x[3]|x[2]|x[1]) == 0;
}

function caml_int64_is_negative(x) {
  return (x[3] << 16) < 0;
}

function caml_int64_is_min_int(x) {
  return x[3] == 0x8000 && (x[1]|x[2]) == 0;
}

function caml_int64_is_minus_one(x) {
  return x[3] == 0xffff && (x[1]&x[2]) == 0xffffff;
}

function caml_int64_and (x, y) {
  return [255, x[1]&y[1], x[2]&y[2], x[3]&y[3]];
}

function caml_int64_or (x, y) {
  return [255, x[1]|y[1], x[2]|y[2], x[3]|y[3]];
}

function caml_int64_xor (x, y) {
  return [255, x[1]^y[1], x[2]^y[2], x[3]^y[3]];
}

function caml_int64_lsl (x, s) {
  s = s & 63;
  if (s == 0) return x;
  if (s < 24)
    return [255,
            (x[1] << s) & 0xffffff,
            ((x[2] << s) | (x[1] >> (24 - s))) & 0xffffff,
            ((x[3] << s) | (x[2] >> (24 - s))) & 0xffff];
  if (s < 48)
    return [255, 0,
            (x[1] << (s - 24)) & 0xffffff,
            ((x[2] << (s - 24)) | (x[1] >> (48 - s))) & 0xffff];
  return [255, 0, 0, (x[1] << (s - 48)) & 0xffff];
}

function caml_int64_lsr (x, s) {
  s = s & 63;
  if (s == 0) return x;
  if (s < 24)
    return [255,
            ((x[1] >> s) | (x[2] << (24 - s))) & 0xffffff,
            ((x[2] >> s) | (x[3] << (24 - s))) & 0xffffff,
            (x[3] >> s)];
  if (s < 48)
    return [255,
            ((x[2] >> (s - 24)) | (x[3] << (48 - s))) & 0xffffff,
            (x[3] >> (s - 24)),
            0];
  return [255, (x[3] >> (s - 48)), 0, 0];
}

function caml_int64_asr (x, s) {
  s = s & 63;
  if (s == 0) return x;
  var h = (x[3] << 16) >> 16;
  if (s < 24)
    return [255,
            ((x[1] >> s) | (x[2] << (24 - s))) & 0xffffff,
            ((x[2] >> s) | (h << (24 - s))) & 0xffffff,
            ((x[3] << 16) >> s) >>> 16];
  var sign = (x[3] << 16) >> 31;
  if (s < 48)
    return [255,
            ((x[2] >> (s - 24)) | (x[3] << (48 - s))) & 0xffffff,
            ((x[3] << 16) >> (s - 24) >> 16) & 0xffffff,
            sign & 0xffff];
  return [255,
          ((x[3] << 16) >> (s - 32)) & 0xffffff,
          sign & 0xffffff, sign & 0xffff];
}

function caml_int64_lsl1 (x) {
  x[3] = (x[3] << 1) | (x[2] >> 23);
  x[2] = ((x[2] << 1) | (x[1] >> 23)) & 0xffffff;
  x[1] = x[1] << 1;
}

function caml_int64_lsr1 (x) {
  x[1] = ((x[1] >>> 1) | (x[2] << 23)) & 0xffffff;
  x[2] = ((x[2] >>> 1) | (x[3] << 23)) & 0xffffff;
  x[3] = x[3] >>> 1;
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

function caml_int64_div (x, y)
{
  if (caml_int64_is_zero (y)) caml_raise_zero_divide ();
  var sign = x[3] ^ y[3];
  if (x[3] & 0x8000) x = caml_int64_neg(x);
  if (y[3] & 0x8000) y = caml_int64_neg(y);
  var q = caml_int64_udivmod(x, y)[1];
  if (sign & 0x8000) q = caml_int64_neg(q);
  return q;
}

function caml_int64_mod (x, y)
{
  if (caml_int64_is_zero (y)) caml_raise_zero_divide ();
  var sign = x[3] ^ y[3];
  if (x[3] & 0x8000) x = caml_int64_neg(x);
  if (y[3] & 0x8000) y = caml_int64_neg(y);
  var r = caml_int64_udivmod(x, y)[2];
  if (sign & 0x8000) r = caml_int64_neg(r);
  return r;
}

function caml_int64_of_int32 (x) {
  return [255, x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff]
}

function caml_int64_to_int32 (x) {
  return x[1] | (x[2] << 24);
}

function caml_int64_to_double (x) {
  return ((x[3] << 16) * Math.pow(2, 32) + x[2] * Math.pow(2, 24)) + x[1];
}

function caml_int64_of_double (x) {
  if (x < 0) x = Math.ceil(x);
  return [255,
          x & 0xffffff,
          Math.floor(x * caml_int64_offset) & 0xffffff,
          Math.floor(x * caml_int64_offset * caml_int64_offset) & 0xffff];
}
//FIX: caml_int64_format