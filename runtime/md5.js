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
//Requires: caml_string_of_array
//Requires: caml_raise_end_of_file, caml_ml_input_block
//Requires: caml_MD5Init, caml_MD5Update, caml_MD5Final
function caml_md5_chan(chanid,toread){
  var ctx = caml_MD5Init();
  var buffer = new Uint8Array(4096);
  if(toread < 0){
    while(true){
      var read = caml_ml_input_block(chanid,buffer,0,buffer.length);
      if(read == 0) break;
      caml_MD5Update(ctx,buffer.subarray(0, read), read);
    }
  } else {
    while(toread > 0) {
      var read = caml_ml_input_block(chanid,buffer,0, (toread > buffer.length ? buffer.length : toread));
      if(read == 0) caml_raise_end_of_file();
      caml_MD5Update(ctx,buffer.subarray(0, read), read);
      toread -= read
    }
  }
  return caml_string_of_array(caml_MD5Final(ctx));
}

//Provides: caml_md5_string
//Requires: caml_bytes_of_string, caml_md5_bytes
function caml_md5_string(s, ofs, len) {
  return caml_md5_bytes(caml_bytes_of_string(s),ofs,len);
}

//Provides: caml_MD5Transform
var caml_MD5Transform = (function () {
  function add (x, y) { return (x + y) | 0; }
  function xx(q,a,b,x,s,t) {
    a = add(add(a, q), add(x, t));
    return add((a << s) | (a >>> (32 - s)), b);
  }
  function ff(a,b,c,d,x,s,t) {
    return xx((b & c) | ((~b) & d), a, b, x, s, t);
  }
  function gg(a,b,c,d,x,s,t) {
    return xx((b & d) | (c & (~d)), a, b, x, s, t);
  }
  function hh(a,b,c,d,x,s,t) { return xx(b ^ c ^ d, a, b, x, s, t); }
  function ii(a,b,c,d,x,s,t) { return xx(c ^ (b | (~d)), a, b, x, s, t); }

  return function (w, buffer) {
    var a = w[0], b = w[1], c = w[2], d = w[3];

    a = ff(a, b, c, d, buffer[ 0], 7, 0xD76AA478);
    d = ff(d, a, b, c, buffer[ 1], 12, 0xE8C7B756);
    c = ff(c, d, a, b, buffer[ 2], 17, 0x242070DB);
    b = ff(b, c, d, a, buffer[ 3], 22, 0xC1BDCEEE);
    a = ff(a, b, c, d, buffer[ 4], 7, 0xF57C0FAF);
    d = ff(d, a, b, c, buffer[ 5], 12, 0x4787C62A);
    c = ff(c, d, a, b, buffer[ 6], 17, 0xA8304613);
    b = ff(b, c, d, a, buffer[ 7], 22, 0xFD469501);
    a = ff(a, b, c, d, buffer[ 8], 7, 0x698098D8);
    d = ff(d, a, b, c, buffer[ 9], 12, 0x8B44F7AF);
    c = ff(c, d, a, b, buffer[10], 17, 0xFFFF5BB1);
    b = ff(b, c, d, a, buffer[11], 22, 0x895CD7BE);
    a = ff(a, b, c, d, buffer[12], 7, 0x6B901122);
    d = ff(d, a, b, c, buffer[13], 12, 0xFD987193);
    c = ff(c, d, a, b, buffer[14], 17, 0xA679438E);
    b = ff(b, c, d, a, buffer[15], 22, 0x49B40821);

    a = gg(a, b, c, d, buffer[ 1], 5, 0xF61E2562);
    d = gg(d, a, b, c, buffer[ 6], 9, 0xC040B340);
    c = gg(c, d, a, b, buffer[11], 14, 0x265E5A51);
    b = gg(b, c, d, a, buffer[ 0], 20, 0xE9B6C7AA);
    a = gg(a, b, c, d, buffer[ 5], 5, 0xD62F105D);
    d = gg(d, a, b, c, buffer[10], 9, 0x02441453);
    c = gg(c, d, a, b, buffer[15], 14, 0xD8A1E681);
    b = gg(b, c, d, a, buffer[ 4], 20, 0xE7D3FBC8);
    a = gg(a, b, c, d, buffer[ 9], 5, 0x21E1CDE6);
    d = gg(d, a, b, c, buffer[14], 9, 0xC33707D6);
    c = gg(c, d, a, b, buffer[ 3], 14, 0xF4D50D87);
    b = gg(b, c, d, a, buffer[ 8], 20, 0x455A14ED);
    a = gg(a, b, c, d, buffer[13], 5, 0xA9E3E905);
    d = gg(d, a, b, c, buffer[ 2], 9, 0xFCEFA3F8);
    c = gg(c, d, a, b, buffer[ 7], 14, 0x676F02D9);
    b = gg(b, c, d, a, buffer[12], 20, 0x8D2A4C8A);

    a = hh(a, b, c, d, buffer[ 5], 4, 0xFFFA3942);
    d = hh(d, a, b, c, buffer[ 8], 11, 0x8771F681);
    c = hh(c, d, a, b, buffer[11], 16, 0x6D9D6122);
    b = hh(b, c, d, a, buffer[14], 23, 0xFDE5380C);
    a = hh(a, b, c, d, buffer[ 1], 4, 0xA4BEEA44);
    d = hh(d, a, b, c, buffer[ 4], 11, 0x4BDECFA9);
    c = hh(c, d, a, b, buffer[ 7], 16, 0xF6BB4B60);
    b = hh(b, c, d, a, buffer[10], 23, 0xBEBFBC70);
    a = hh(a, b, c, d, buffer[13], 4, 0x289B7EC6);
    d = hh(d, a, b, c, buffer[ 0], 11, 0xEAA127FA);
    c = hh(c, d, a, b, buffer[ 3], 16, 0xD4EF3085);
    b = hh(b, c, d, a, buffer[ 6], 23, 0x04881D05);
    a = hh(a, b, c, d, buffer[ 9], 4, 0xD9D4D039);
    d = hh(d, a, b, c, buffer[12], 11, 0xE6DB99E5);
    c = hh(c, d, a, b, buffer[15], 16, 0x1FA27CF8);
    b = hh(b, c, d, a, buffer[ 2], 23, 0xC4AC5665);

    a = ii(a, b, c, d, buffer[ 0], 6, 0xF4292244);
    d = ii(d, a, b, c, buffer[ 7], 10, 0x432AFF97);
    c = ii(c, d, a, b, buffer[14], 15, 0xAB9423A7);
    b = ii(b, c, d, a, buffer[ 5], 21, 0xFC93A039);
    a = ii(a, b, c, d, buffer[12], 6, 0x655B59C3);
    d = ii(d, a, b, c, buffer[ 3], 10, 0x8F0CCC92);
    c = ii(c, d, a, b, buffer[10], 15, 0xFFEFF47D);
    b = ii(b, c, d, a, buffer[ 1], 21, 0x85845DD1);
    a = ii(a, b, c, d, buffer[ 8], 6, 0x6FA87E4F);
    d = ii(d, a, b, c, buffer[15], 10, 0xFE2CE6E0);
    c = ii(c, d, a, b, buffer[ 6], 15, 0xA3014314);
    b = ii(b, c, d, a, buffer[13], 21, 0x4E0811A1);
    a = ii(a, b, c, d, buffer[ 4], 6, 0xF7537E82);
    d = ii(d, a, b, c, buffer[11], 10, 0xBD3AF235);
    c = ii(c, d, a, b, buffer[ 2], 15, 0x2AD7D2BB);
    b = ii(b, c, d, a, buffer[ 9], 21, 0xEB86D391);

    w[0] = add(a, w[0]);
    w[1] = add(b, w[1]);
    w[2] = add(c, w[2]);
    w[3] = add(d, w[3]);
  }})()

//Provides: caml_MD5Init
function caml_MD5Init() {
  var buffer = new ArrayBuffer(64);
  var b32 = new Uint32Array(buffer);
  var b8 = new Uint8Array(buffer);
  return {len:0,
          w:new Uint32Array([0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476]),
          b32:b32,
          b8:b8}
}

//Provides: caml_MD5Update
//Requires: caml_MD5Transform
function caml_MD5Update(ctx, input, input_len){
  var in_buf = ctx.len & 0x3f;
  var input_pos = 0;
  ctx.len += input_len;
  if(in_buf){
    var missing = 64 - in_buf;
    if(input_len < missing) {
      ctx.b8.set(input.subarray(0,input_len),in_buf);
      return
    }
    ctx.b8.set(input.subarray(0,missing),in_buf);
    caml_MD5Transform(ctx.w, ctx.b32);
    input_len -= missing;
    input_pos += missing;
  }
  while(input_len >= 64){
    ctx.b8.set(input.subarray(input_pos,input_pos + 64), 0);
    caml_MD5Transform(ctx.w, ctx.b32);
    input_len -= 64;
    input_pos += 64;
  }
  if(input_len)
    ctx.b8.set(input.subarray(input_pos,input_pos + input_len), 0);
}

//Provides: caml_MD5Final
//Requires: caml_MD5Transform
function caml_MD5Final(ctx){
  var in_buf = ctx.len & 0x3f;
  ctx.b8[in_buf] = 0x80;
  in_buf ++;
  if(in_buf > 56) {
    for(var j = in_buf; j < 64; j++){
      ctx.b8[j] = 0;
    }
    caml_MD5Transform(ctx.w, ctx.b32);
    for(var j = 0; j < 56; j++){
      ctx.b8[j] = 0;
    }
  } else {
    for(var j = in_buf; j < 56; j++){
      ctx.b8[j] = 0;
    }
  }
  ctx.b32[14] = ctx.len << 3;
  ctx.b32[15] = (ctx.len >> 29) & 0x1FFFFFFF;
  caml_MD5Transform(ctx.w, ctx.b32);
  var t = new Uint8Array(16);
  for (var i = 0; i < 4; i++)
    for (var j = 0; j < 4; j++)
      t[i * 4 + j] = (ctx.w[i] >> (8 * j)) & 0xFF;
  return t;
}


//Provides: caml_md5_bytes
//Requires: caml_uint8_array_of_bytes, caml_string_of_array
//Requires: caml_MD5Init, caml_MD5Update, caml_MD5Final
function caml_md5_bytes(s, ofs, len) {
  var ctx = caml_MD5Init();
  var a = caml_uint8_array_of_bytes(s);
  caml_MD5Update(ctx,a.subarray(ofs, ofs + len), len);
  return caml_string_of_array(caml_MD5Final(ctx));
}
