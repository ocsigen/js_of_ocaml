
var caml_int64_offset = Math.pow(2, -24);

function caml_int64_ucompare(x,y) {
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  if (x[0] > y[0]) return 1;
  if (x[0] < y[0]) return -1;
  return 0;
}

function caml_int64_ult(x,y) { return caml_int64_ucompare(x,y) < 0; }

function caml_int64_compare(x,y) {
  x2 = x[2] << 16;
  y2 = y[2] << 16;
  if (x2 > y2) return 1;
  if (x2 < y2) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  if (x[0] > y[0]) return 1;
  if (x[0] < y[0]) return -1;
  return 0;
}

function caml_int64_neg (x) {
  y0 = - x[0];
  y1 = - x[1] + (y0 >> 24);
  y2 = - x[2] + (y1 >> 24);
  return [y0 & 0xffffff, y1 & 0xffffff, y2 & 0xffff];
}

function caml_int64_add (x, y) {
  z0 = x[0] + y[0];
  z1 = x[1] + y[1] + (z0 >> 24);
  z2 = x[2] + y[2] + (z1 >> 24);
  return [z0 & 0xffffff, z1 & 0xffffff, z2 & 0xffff];
}

function caml_int64_sub (x, y) {
  z0 = x[0] - y[0];
  z1 = x[1] - y[1] + (z0 >> 24);
  z2 = x[2] - y[2] + (z1 >> 24);
  return [z0 & 0xffffff, z1 & 0xffffff, z2 & 0xffff];
}

function caml_int64_mul(x,y) {
  z0 = x[0] * y[0];
  z1 = ((z0 * caml_int64_offset) | 0) + x[1] * y[0] + x[0] * y[1];
  z2 = ((z1 * caml_int64_offset) | 0) + x[2] * y[0] + x[1] * y[1] + x[0] * y[2];
  return [z0 & 0xffffff, z1 & 0xffffff, z2 & 0xffff];
}

function caml_int64_is_zero(x) {
  return (x[2]|x[1]|x[0]) == 0;
}

function caml_int64_is_negative(x) {
  return (x[2] << 16) < 0;
}

function caml_int64_is_min_int(x) {
  return x[2] == 0x8000 && (x[0]|x[1]) == 0;
}

function caml_int64_is_minus_one(x) {
  return x[2] == 0xffff && (x[0]&x[1]) == 0xffffff;
}

function caml_int64_and (x, y) {
  return [x[0]&y[0], x[1]&y[1], x[2]&y[2]];
}

function caml_int64_or (x, y) {
  return [x[0]|y[0], x[1]|y[1], x[2]|y[2]];
}

function caml_int64_xor (x, y) {
  return [x[0]^y[0], x[1]^y[1], x[2]^y[2]];
}

function caml_int64_lsl (x, s) {
  s = s & 63;
  if (s == 0) return x;
  if (s < 24)
    return [(x[0] << s) & 0xffffff,
            ((x[1] << s) | (x[0] >> (24 - s))) & 0xffffff,
            ((x[2] << s) | (x[1] >> (24 - s))) & 0xffff];
  if (s < 48)
    return [0,
            (x[0] << (s - 24)) & 0xffffff,
            ((x[1] << (s - 24)) | (x[0] >> (48 - s))) & 0xffff];
  return [0, 0, (x[0] << (s - 48)) & 0xffff];
}

function caml_int64_lsr (x, s) {
  s = s & 63;
  if (s == 0) return x;
  if (s < 24)
    return [((x[0] >> s) | (x[1] << (24 - s))) & 0xffffff,
            ((x[1] >> s) | (x[2] << (24 - s))) & 0xffffff,
            (x[2] >> s)];
  if (s < 48)
    return [((x[1] >> (s - 24)) | (x[2] << (48 - s))) & 0xffffff,
            (x[2] >> (s - 24)),
            0]
  return [(x[2] >> (s - 48)), 0, 0];
}

function caml_int64_asr (x, s) {
  s = s & 63;
  if (s == 0) return x;
  var h = (x[2] << 16) >> 16;
  if (s < 24)
    return [((x[0] >> s) | (x[1] << (24 - s))) & 0xffffff,
            ((x[1] >> s) | (h << (24 - s))) & 0xffffff,
            ((x[2] << 16) >> s) >>> 16];
  var sign = (x[2] << 16) >> 31;
  if (s < 48)
    return [((x[1] >> (s - 24)) | (x[2] << (48 - s))) & 0xffffff,
            ((x[2] << 16) >> (s - 24) >> 16) & 0xffffff,
            sign & 0xffff]
  return [((x[2] << 16) >> (s - 32)) & 0xffffff,
          sign & 0xffffff, sign & 0xffff];
}

function caml_int64_lsl1 (x) {
  x[2] = (x[2] << 1) | (x[1] >> 23);
  x[1] = ((x[1] << 1) | (x[0] >> 23)) & 0xffffff;
  x[0] = x[0] << 1;
}

function caml_int64_lsr1 (x) {
  x[0] = ((x[0] >>> 1) | (x[1] << 23)) & 0xffffff;
  x[1] = ((x[1] >>> 1) | (x[2] << 23)) & 0xffffff;
  x[2] = x[2] >>> 1;
}

function caml_int64_udivmod (x, y) {
  var offset = 0;
  var modulus = x.slice ();
  var divisor = y.slice ();
  var quotient = [0, 0, 0];
  while (caml_int64_ucompare (modulus, divisor) > 0) {
    offset++;
    caml_int64_lsl1 (divisor);
  }
  while (offset >= 0) {
    offset --;
    caml_int64_lsl1 (quotient);
    if (caml_int64_ucompare (modulus, divisor) >= 0) {
      quotient[0] ++;
      modulus = caml_int64_sub (modulus, divisor);
    }
    caml_int64_lsr1 (divisor);
  }
  return [quotient, modulus];
}

function caml_int64_div (x, y)
{
  if (caml_int64_is_zero (y)) caml_raise_zero_divide ();
  var sign = x[2] ^ y[2];
  if (x[2] & 0x8000) x = caml_int64_neg(x);
  if (y[2] & 0x8000) y = caml_int64_neg(y);
  var q = caml_int64_udivmod(x, y)[0];
  if (sign & 0x8000) q = caml_int64_neg(q);
  return q;
}

function caml_int64_mod (x, y)
{
  if (caml_int64_is_zero (y)) caml_raise_zero_divide ();
  var sign = x[2] ^ y[2];
  if (x[2] & 0x8000) x = caml_int64_neg(x);
  if (y[2] & 0x8000) y = caml_int64_neg(y);
  var r = caml_int64_udivmod(x, y)[1];
  if (sign & 0x8000) r = caml_int64_neg(r);
  return r;
}

function caml_int64_of_int32 (x) {
  return [x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff]
}

function caml_int64_to_int32 (x) {
  return x[0] | (x[1] << 24);
}

function caml_int64_to_double (x) {
  return ((x[2] << 16) * Math.pow(2, 32) + x[1] * Math.pow(2, 24)) + x[0];
}

function caml_int64_of_double (x) {
  if (x < 0) x = Math.ceil(x);
  return [x & 0xffffff,
          Math.floor(x * caml_int64_offset) & 0xffffff,
          Math.floor(x * caml_int64_offset * caml_int64_offset) & 0xffff];
}
