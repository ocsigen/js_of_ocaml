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

//Provides: caml_md5_chan
//Requires: caml_string_of_uint8_array
//Requires: caml_raise_end_of_file, caml_ml_input_block
//Requires: caml_MD5Init, caml_MD5Update, caml_MD5Final
function caml_md5_chan(chanid, toread) {
  var ctx = caml_MD5Init();
  var buffer = new Uint8Array(4096);
  if (toread < 0) {
    while (true) {
      var read = caml_ml_input_block(chanid, buffer, 0, buffer.length);
      if (read === 0) break;
      caml_MD5Update(ctx, buffer.subarray(0, read), read);
    }
  } else {
    while (toread > 0) {
      var read = caml_ml_input_block(
        chanid,
        buffer,
        0,
        toread > buffer.length ? buffer.length : toread,
      );
      if (read === 0) caml_raise_end_of_file();
      caml_MD5Update(ctx, buffer.subarray(0, read), read);
      toread -= read;
    }
  }
  return caml_string_of_uint8_array(caml_MD5Final(ctx));
}

//Provides: caml_md5_string
//Requires: caml_bytes_of_string, caml_md5_bytes
function caml_md5_string(s, ofs, len) {
  return caml_md5_bytes(caml_bytes_of_string(s), ofs, len);
}

//Provides: caml_MD5Transform
var caml_MD5Transform = (function () {
  function add(x, y) {
    return (x + y) | 0;
  }
  function xx(q, a, b, x, s, t) {
    a = add(add(a, q), add(x, t));
    return add((a << s) | (a >>> (32 - s)), b);
  }
  function ff(a, b, c, d, x, s, t) {
    return xx((b & c) | (~b & d), a, b, x, s, t);
  }
  function gg(a, b, c, d, x, s, t) {
    return xx((b & d) | (c & ~d), a, b, x, s, t);
  }
  function hh(a, b, c, d, x, s, t) {
    return xx(b ^ c ^ d, a, b, x, s, t);
  }
  function ii(a, b, c, d, x, s, t) {
    return xx(c ^ (b | ~d), a, b, x, s, t);
  }

  return function (w, buffer) {
    var a = w[0],
      b = w[1],
      c = w[2],
      d = w[3];

    a = ff(a, b, c, d, buffer[0], 7, 0xd76aa478);
    d = ff(d, a, b, c, buffer[1], 12, 0xe8c7b756);
    c = ff(c, d, a, b, buffer[2], 17, 0x242070db);
    b = ff(b, c, d, a, buffer[3], 22, 0xc1bdceee);
    a = ff(a, b, c, d, buffer[4], 7, 0xf57c0faf);
    d = ff(d, a, b, c, buffer[5], 12, 0x4787c62a);
    c = ff(c, d, a, b, buffer[6], 17, 0xa8304613);
    b = ff(b, c, d, a, buffer[7], 22, 0xfd469501);
    a = ff(a, b, c, d, buffer[8], 7, 0x698098d8);
    d = ff(d, a, b, c, buffer[9], 12, 0x8b44f7af);
    c = ff(c, d, a, b, buffer[10], 17, 0xffff5bb1);
    b = ff(b, c, d, a, buffer[11], 22, 0x895cd7be);
    a = ff(a, b, c, d, buffer[12], 7, 0x6b901122);
    d = ff(d, a, b, c, buffer[13], 12, 0xfd987193);
    c = ff(c, d, a, b, buffer[14], 17, 0xa679438e);
    b = ff(b, c, d, a, buffer[15], 22, 0x49b40821);

    a = gg(a, b, c, d, buffer[1], 5, 0xf61e2562);
    d = gg(d, a, b, c, buffer[6], 9, 0xc040b340);
    c = gg(c, d, a, b, buffer[11], 14, 0x265e5a51);
    b = gg(b, c, d, a, buffer[0], 20, 0xe9b6c7aa);
    a = gg(a, b, c, d, buffer[5], 5, 0xd62f105d);
    d = gg(d, a, b, c, buffer[10], 9, 0x02441453);
    c = gg(c, d, a, b, buffer[15], 14, 0xd8a1e681);
    b = gg(b, c, d, a, buffer[4], 20, 0xe7d3fbc8);
    a = gg(a, b, c, d, buffer[9], 5, 0x21e1cde6);
    d = gg(d, a, b, c, buffer[14], 9, 0xc33707d6);
    c = gg(c, d, a, b, buffer[3], 14, 0xf4d50d87);
    b = gg(b, c, d, a, buffer[8], 20, 0x455a14ed);
    a = gg(a, b, c, d, buffer[13], 5, 0xa9e3e905);
    d = gg(d, a, b, c, buffer[2], 9, 0xfcefa3f8);
    c = gg(c, d, a, b, buffer[7], 14, 0x676f02d9);
    b = gg(b, c, d, a, buffer[12], 20, 0x8d2a4c8a);

    a = hh(a, b, c, d, buffer[5], 4, 0xfffa3942);
    d = hh(d, a, b, c, buffer[8], 11, 0x8771f681);
    c = hh(c, d, a, b, buffer[11], 16, 0x6d9d6122);
    b = hh(b, c, d, a, buffer[14], 23, 0xfde5380c);
    a = hh(a, b, c, d, buffer[1], 4, 0xa4beea44);
    d = hh(d, a, b, c, buffer[4], 11, 0x4bdecfa9);
    c = hh(c, d, a, b, buffer[7], 16, 0xf6bb4b60);
    b = hh(b, c, d, a, buffer[10], 23, 0xbebfbc70);
    a = hh(a, b, c, d, buffer[13], 4, 0x289b7ec6);
    d = hh(d, a, b, c, buffer[0], 11, 0xeaa127fa);
    c = hh(c, d, a, b, buffer[3], 16, 0xd4ef3085);
    b = hh(b, c, d, a, buffer[6], 23, 0x04881d05);
    a = hh(a, b, c, d, buffer[9], 4, 0xd9d4d039);
    d = hh(d, a, b, c, buffer[12], 11, 0xe6db99e5);
    c = hh(c, d, a, b, buffer[15], 16, 0x1fa27cf8);
    b = hh(b, c, d, a, buffer[2], 23, 0xc4ac5665);

    a = ii(a, b, c, d, buffer[0], 6, 0xf4292244);
    d = ii(d, a, b, c, buffer[7], 10, 0x432aff97);
    c = ii(c, d, a, b, buffer[14], 15, 0xab9423a7);
    b = ii(b, c, d, a, buffer[5], 21, 0xfc93a039);
    a = ii(a, b, c, d, buffer[12], 6, 0x655b59c3);
    d = ii(d, a, b, c, buffer[3], 10, 0x8f0ccc92);
    c = ii(c, d, a, b, buffer[10], 15, 0xffeff47d);
    b = ii(b, c, d, a, buffer[1], 21, 0x85845dd1);
    a = ii(a, b, c, d, buffer[8], 6, 0x6fa87e4f);
    d = ii(d, a, b, c, buffer[15], 10, 0xfe2ce6e0);
    c = ii(c, d, a, b, buffer[6], 15, 0xa3014314);
    b = ii(b, c, d, a, buffer[13], 21, 0x4e0811a1);
    a = ii(a, b, c, d, buffer[4], 6, 0xf7537e82);
    d = ii(d, a, b, c, buffer[11], 10, 0xbd3af235);
    c = ii(c, d, a, b, buffer[2], 15, 0x2ad7d2bb);
    b = ii(b, c, d, a, buffer[9], 21, 0xeb86d391);

    w[0] = add(a, w[0]);
    w[1] = add(b, w[1]);
    w[2] = add(c, w[2]);
    w[3] = add(d, w[3]);
  };
})();

//Provides: caml_MD5Init
function caml_MD5Init() {
  var buffer = new ArrayBuffer(64);
  var b32 = new Uint32Array(buffer);
  var b8 = new Uint8Array(buffer);
  return {
    len: 0,
    w: new Uint32Array([0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476]),
    b32: b32,
    b8: b8,
  };
}

//Provides: caml_MD5Update
//Requires: caml_MD5Transform
function caml_MD5Update(ctx, input, input_len) {
  var in_buf = ctx.len & 0x3f;
  var input_pos = 0;
  ctx.len += input_len;
  if (in_buf) {
    var missing = 64 - in_buf;
    if (input_len < missing) {
      ctx.b8.set(input.subarray(0, input_len), in_buf);
      return;
    }
    ctx.b8.set(input.subarray(0, missing), in_buf);
    caml_MD5Transform(ctx.w, ctx.b32);
    input_len -= missing;
    input_pos += missing;
  }
  while (input_len >= 64) {
    ctx.b8.set(input.subarray(input_pos, input_pos + 64), 0);
    caml_MD5Transform(ctx.w, ctx.b32);
    input_len -= 64;
    input_pos += 64;
  }
  if (input_len)
    ctx.b8.set(input.subarray(input_pos, input_pos + input_len), 0);
}

//Provides: caml_MD5Final
//Requires: caml_MD5Transform
function caml_MD5Final(ctx) {
  var in_buf = ctx.len & 0x3f;
  ctx.b8[in_buf] = 0x80;
  in_buf++;
  if (in_buf > 56) {
    for (var j = in_buf; j < 64; j++) {
      ctx.b8[j] = 0;
    }
    caml_MD5Transform(ctx.w, ctx.b32);
    for (var j = 0; j < 56; j++) {
      ctx.b8[j] = 0;
    }
  } else {
    for (var j = in_buf; j < 56; j++) {
      ctx.b8[j] = 0;
    }
  }
  ctx.b32[14] = ctx.len << 3;
  ctx.b32[15] = (ctx.len >> 29) & 0x1fffffff;
  caml_MD5Transform(ctx.w, ctx.b32);
  var t = new Uint8Array(16);
  for (var i = 0; i < 4; i++)
    for (var j = 0; j < 4; j++) t[i * 4 + j] = (ctx.w[i] >> (8 * j)) & 0xff;
  return t;
}

//Provides: caml_md5_bytes
//Requires: caml_uint8_array_of_bytes, caml_string_of_uint8_array
//Requires: caml_MD5Init, caml_MD5Update, caml_MD5Final
function caml_md5_bytes(s, ofs, len) {
  var ctx = caml_MD5Init();
  var a = caml_uint8_array_of_bytes(s);
  caml_MD5Update(ctx, a.subarray(ofs, ofs + len), len);
  return caml_string_of_uint8_array(caml_MD5Final(ctx));
}
