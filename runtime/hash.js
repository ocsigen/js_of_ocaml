// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
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

///////////// Hashtbl


//Provides: caml_hash_univ_param mutable
//Requires: caml_is_ml_string, caml_is_ml_bytes
//Requires: caml_ml_bytes_content
//Requires: caml_int64_to_bytes, caml_int64_bits_of_float, caml_custom_ops
//Requires: caml_ml_bytes_length, caml_jsbytes_of_string
//Version: < 4.12
function caml_hash_univ_param (count, limit, obj) {
  var hash_accu = 0;
  function hash_aux (obj) {
    limit --;
    if (count < 0 || limit < 0) return;
    if (obj instanceof Array && obj[0] === (obj[0]|0)) {
      switch (obj[0]) {
      case 248:
        // Object
        count --;
        hash_accu = (hash_accu * 65599 + obj[2]) | 0;
        break;
      case 250:
        // Forward
        limit++; hash_aux(obj); break;
      default:
        count --;
        hash_accu = (hash_accu * 19 + obj[0]) | 0;
        for (var i = obj.length - 1; i > 0; i--) hash_aux (obj[i]);
      }
    } else if (caml_is_ml_bytes(obj)) {
      count --;
      var content = caml_ml_bytes_content(obj);
      if(typeof content === "string") {
        for (var b = content, l = b.length, i = 0; i < l; i++)
          hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
      } else { /* ARRAY */
        for (var a = content, l = a.length, i = 0; i < l; i++)
          hash_accu = (hash_accu * 19 + a[i]) | 0;
      }
    } else if (caml_is_ml_string(obj)) {
      var jsbytes = caml_jsbytes_of_string(obj);
      for (var b = jsbytes, l = jsbytes.length, i = 0; i < l; i++)
        hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
    } else if (typeof obj === "string") {
      for (var b = obj, l = obj.length, i = 0; i < l; i++)
        hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
    } else if (obj === (obj|0)) {
      // Integer
      count --;
      hash_accu = (hash_accu * 65599 + obj) | 0;
    } else if (obj === +obj) {
      // Float
      count--;
      var p = caml_int64_to_bytes (caml_int64_bits_of_float (obj));
      for (var i = 7; i >= 0; i--) hash_accu = (hash_accu * 19 + p[i]) | 0;
    } else if(obj && obj.caml_custom) {
      if(caml_custom_ops[obj.caml_custom] && caml_custom_ops[obj.caml_custom].hash) {
        var h = caml_custom_ops[obj.caml_custom].hash(obj) | 0;
        hash_accu = (hash_accu * 65599 + h) | 0;
      }
    }
  }
  hash_aux (obj);
  return hash_accu & 0x3FFFFFFF;
}

//function ROTL32(x,n) { return ((x << n) | (x >>> (32-n))); }
//Provides: caml_hash_mix_int
//Requires: caml_mul
function caml_hash_mix_int(h,d) {
  d = caml_mul(d, 0xcc9e2d51|0);
  d = ((d << 15) | (d >>> (32-15))); // ROTL32(d, 15);
  d = caml_mul(d, 0x1b873593);
  h ^= d;
  h = ((h << 13) | (h >>> (32-13)));   //ROTL32(h, 13);
  return (((h + (h << 2))|0) + (0xe6546b64|0))|0;
}

//Provides: caml_hash_mix_final
//Requires: caml_mul
function caml_hash_mix_final(h) {
  h ^= h >>> 16;
  h = caml_mul (h, 0x85ebca6b|0);
  h ^= h >>> 13;
  h = caml_mul (h, 0xc2b2ae35|0);
  h ^= h >>> 16;
  return h;
}

//Provides: caml_hash_mix_float
//Requires: caml_int64_bits_of_float, caml_hash_mix_int64
function caml_hash_mix_float (h, v0) {
  return caml_hash_mix_int64(h, caml_int64_bits_of_float (v0));
}
//Provides: caml_hash_mix_int64
//Requires: caml_hash_mix_int
//Requires: caml_int64_lo32, caml_int64_hi32
function caml_hash_mix_int64 (h, v) {
  h = caml_hash_mix_int(h, caml_int64_lo32(v));
  h = caml_hash_mix_int(h, caml_int64_hi32(v));
  return h;
}

//Provides: caml_hash_mix_jsbytes
//Requires: caml_hash_mix_int
function caml_hash_mix_jsbytes(h, s) {
  var len = s.length, i, w;
  for (i = 0; i + 4 <= len; i += 4) {
    w = s.charCodeAt(i)
      | (s.charCodeAt(i+1) << 8)
      | (s.charCodeAt(i+2) << 16)
      | (s.charCodeAt(i+3) << 24);
    h = caml_hash_mix_int(h, w);
  }
  w = 0;
  switch (len & 3) {
  case 3: w  = s.charCodeAt(i+2) << 16;
  case 2: w |= s.charCodeAt(i+1) << 8;
  case 1:
    w |= s.charCodeAt(i);
    h = caml_hash_mix_int(h, w);
  default:
  }
  h ^= len;
  return h;
}

//Provides: caml_hash_mix_bytes_arr
//Requires: caml_hash_mix_int
function caml_hash_mix_bytes_arr(h, s) {
  var len = s.length, i, w;
  for (i = 0; i + 4 <= len; i += 4) {
    w = s[i]
      | (s[i+1] << 8)
      | (s[i+2] << 16)
      | (s[i+3] << 24);
    h = caml_hash_mix_int(h, w);
  }
  w = 0;
  switch (len & 3) {
  case 3: w  = s[i+2] << 16;
  case 2: w |= s[i+1] << 8;
  case 1: w |= s[i];
    h = caml_hash_mix_int(h, w);
  default:
  }
  h ^= len;
  return h;
}

//Provides: caml_hash_mix_bytes
//Requires: caml_ml_bytes_content
//Requires: caml_hash_mix_jsbytes
//Requires: caml_hash_mix_bytes_arr
function caml_hash_mix_bytes(h, v) {
  var content = caml_ml_bytes_content(v);
  if(typeof content === "string")
    return caml_hash_mix_jsbytes(h, content)
  else /* ARRAY */
    return caml_hash_mix_bytes_arr(h, content);
}

//Provides: caml_hash_mix_string
//Requires: caml_hash_mix_jsbytes, caml_jsbytes_of_string
function caml_hash_mix_string(h, v) {
  return caml_hash_mix_jsbytes(h, caml_jsbytes_of_string(v));
}


//Provides: caml_hash mutable
//Requires: caml_is_ml_string, caml_is_ml_bytes
//Requires: caml_hash_mix_int, caml_hash_mix_final
//Requires: caml_hash_mix_float, caml_hash_mix_string, caml_hash_mix_bytes, caml_custom_ops
//Requires: caml_hash_mix_jsbytes
//Requires: caml_is_continuation_tag
function caml_hash (count, limit, seed, obj) {
  var queue, rd, wr, sz, num, h, v, i, len;
  sz = limit;
  if (sz < 0 || sz > 256) sz = 256;
  num = count;
  h = seed;
  queue = [obj]; rd = 0; wr = 1;
  while (rd < wr && num > 0) {
    v = queue[rd++];
    if (v && v.caml_custom){
      if(caml_custom_ops[v.caml_custom] && caml_custom_ops[v.caml_custom].hash) {
        var hh = caml_custom_ops[v.caml_custom].hash(v);
        h = caml_hash_mix_int (h, hh);
        num --;
      }
    }
    else if (v instanceof Array && v[0] === (v[0]|0)) {
      switch (v[0]) {
      case 248:
        // Object
        h = caml_hash_mix_int(h, v[2]);
        num--;
        break;
      case 250:
        // Forward
        queue[--rd] = v[1];
        break;
      default:
        if(caml_is_continuation_tag(v[0])) {
          /* All continuations hash to the same value,
             since we have no idea how to distinguish them. */
          break;
        }
        var tag = ((v.length - 1) << 10) | v[0];
        h = caml_hash_mix_int(h, tag);
        for (i = 1, len = v.length; i < len; i++) {
          if (wr >= sz) break;
          queue[wr++] = v[i];
        }
        break;
      }
    } else if (caml_is_ml_bytes(v)) {
      h = caml_hash_mix_bytes(h,v)
      num--;
    } else if (caml_is_ml_string(v)) {
      h = caml_hash_mix_string(h,v)
      num--;
    } else if (typeof v === "string") {
      h = caml_hash_mix_jsbytes(h,v)
      num--;
    } else if (v === (v|0)) {
      // Integer
      h = caml_hash_mix_int(h, v+v+1);
      num--;
    } else if (typeof v === "number") {
      // Float
      h = caml_hash_mix_float(h,v);
      num--;
    }
  }
  h = caml_hash_mix_final(h);
  return h & 0x3FFFFFFF;
}

//Provides: caml_string_hash
//Requires: caml_hash_mix_final, caml_hash_mix_string
function caml_string_hash(h, v){
  var h = caml_hash_mix_string(h,v);
  var h = caml_hash_mix_final(h);
  return h & 0x3FFFFFFF;
}
