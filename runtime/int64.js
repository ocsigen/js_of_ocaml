var mask0 = 0xffffff;
var mask1 = 0xffffff;
var mask2 = 0xffff;
var offset = Math.pow(2, -24);

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
  return [y0 & mask0, y1 & mask1, y2 & mask2];
}

function caml_int64_add (x, y) {
  z0 = x[0] + y[0];
  z1 = x[1] + y[1] + (z0 >> 24);
  z2 = x[2] + y[2] + (z1 >> 24);
  return [z0 & mask0, z1 & mask1, z2 & mask2];
}

function caml_int64_sub (x, y) {
  z0 = x[0] - y[0];
  z1 = x[1] - y[1] + (z0 >> 24);
  z2 = x[2] - y[2] + (z1 >> 24);
  return [z0 & mask0, z1 & mask1, z2 & mask2];
}

function caml_int64_mul(x,y) {
  z0 = x[0] * y[0];
  z1 = ((z0 * offset) | 0) + x[1] * y[0] + x[0] * y[1];
  z2 = ((z1 * offset) | 0) + x[2] * y[0] + x[1] * y[1] + x[0] * y[2];
  return [z0 & mask0, z1 & mask1, z2 & mask2];
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
    return [(x[0] << s) & mask0,
            ((x[1] << s) | (x[0] >>> (24 - s))) & mask1,
            ((x[2] << s) | (x[1] >>> (24 - s))) & mask2];
  if (s < 48)
    return [0,
            (x[0] << (s - 24)) & mask1,
            ((x[1] << (s - 24)) | (x[0] >>> (48 - s))) & mask2];
  return [0, 0, (x[0] << (s - 48)) & mask2];
}

function caml_int64_lsr (x, s) {
  s = s & 63;
  if (s == 0) return x;
  if (s < 24)
    return [((x[0] >>> s) | (x[1] << (24 - s))) & mask0,
            ((x[1] >>> s) | (x[2] << (24 - s))) & mask1,
            (x[2] >>> s)];
  if (s < 48)
    return [((x[1] >>> (s - 24)) | (x[2] << (48 - s))) & mask0,
            (x[2] >>> (s - 24)),
            0]
  return [(x[0] >>> (s - 48)), 0, 0];
}

function caml_int64_asr (x, s) {
  s = s & 63;
  if (s == 0) return x;
  if (s < 24)
    return [((x[0] >>> s) | (x[1] << (24 - s))) & mask0,
            ((x[1] >>> s) | (x[2] << (24 - s))) & mask1,
            (((x[2] << 16) >> s) >>> 16)];
  var sign = (x[2] << 16) >>> 31;
  if (s < 48)
    return [((x[1] >>> (s - 24)) | (x[2] << (48 - s))) & mask0,
            (x[2] >>> (s - 24)),
            sign & mask2]
  return [(x[0] >> (s - 48)), sign & mask1, sign & mask2];
}

// shl1,shr1,udivmod,div,mod
// (be careful with min_int...)

function caml_int64_of_int32 (x) {
  return [x & mask0, (x >> 24) & mask1, (x >> 31) & mask2]
}

function caml_int64_to_int32 (x) {
  return x[0] | (x[1] << 24);
}

function caml_int64_to_double (x) {
  return ((x[2] << 16) * Math.pow(2, 32) + x[1] * Math.pow(2, 24)) + x[0];
}

function caml_int64_of_double (x) {
    if (x < 0) x = Math.ceil(x);
    return [x & mask0,
            Math.floor(x * offset) & mask1,
            Math.floor(x * offset * offset) & mask2];
}
