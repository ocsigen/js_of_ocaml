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

//function ROTL32(x,n) { return ((x << n) | (x >>> (32-n))); }
//Provides: caml_hash_mix_int
//Requires: caml_mul
function caml_hash_mix_int(h, d) {
  d = caml_mul(d, 0xcc9e2d51 | 0);
  d = (d << 15) | (d >>> (32 - 15)); // ROTL32(d, 15);
  d = caml_mul(d, 0x1b873593);
  h ^= d;
  h = (h << 13) | (h >>> (32 - 13)); //ROTL32(h, 13);
  return (((h + (h << 2)) | 0) + (0xe6546b64 | 0)) | 0;
}

//Provides: caml_hash_mix_final
//Requires: caml_mul
function caml_hash_mix_final(h) {
  h ^= h >>> 16;
  h = caml_mul(h, 0x85ebca6b | 0);
  h ^= h >>> 13;
  h = caml_mul(h, 0xc2b2ae35 | 0);
  h ^= h >>> 16;
  return h;
}

//Provides: caml_hash_mix_float
//Requires: caml_int64_bits_of_float
//Requires: caml_hash_mix_int
//Requires: caml_int64_lo32, caml_int64_hi32
function caml_hash_mix_float(hash, v0) {
  var i64 = caml_int64_bits_of_float(v0);
  var l = caml_int64_lo32(i64);
  var h = caml_int64_hi32(i64);
  /* Normalize NaNs */
  if ((h & 0x7ff00000) === 0x7ff00000 && (l | (h & 0xfffff)) !== 0) {
    h = 0x7ff00000;
    l = 0x00000001;
  } else if (h === (0x80000000 | 0) && l === 0) {
    /* Normalize -0 into +0 */
    // This code path is not used by caml_hash because 0 and -0 look
    // like integers
    h = 0;
  }
  hash = caml_hash_mix_int(hash, l);
  hash = caml_hash_mix_int(hash, h);
  return hash;
}
//Provides: caml_hash_mix_int64
//Requires: caml_hash_mix_int
//Requires: caml_int64_lo32, caml_int64_hi32
function caml_hash_mix_int64(h, v) {
  h = caml_hash_mix_int(h, caml_int64_lo32(v));
  h = caml_hash_mix_int(h, caml_int64_hi32(v));
  return h;
}

//Provides: caml_hash_mix_jsbytes
//Requires: caml_hash_mix_int
function caml_hash_mix_jsbytes(h, s) {
  var len = s.length,
    i,
    w;
  for (i = 0; i + 4 <= len; i += 4) {
    w =
      s.charCodeAt(i) |
      (s.charCodeAt(i + 1) << 8) |
      (s.charCodeAt(i + 2) << 16) |
      (s.charCodeAt(i + 3) << 24);
    h = caml_hash_mix_int(h, w);
  }
  w = 0;
  switch (len & 3) {
    case 3:
      // biome-ignore lint/suspicious/noFallthroughSwitchClause:
      w = s.charCodeAt(i + 2) << 16;
    // fallthrough
    case 2:
      // biome-ignore lint/suspicious/noFallthroughSwitchClause:
      w |= s.charCodeAt(i + 1) << 8;
    // fallthrough
    case 1:
      w |= s.charCodeAt(i);
      h = caml_hash_mix_int(h, w);
  }
  h ^= len;
  return h;
}

//Provides: caml_hash_mix_bytes_arr
//Requires: caml_hash_mix_int
function caml_hash_mix_bytes_arr(h, s) {
  var len = s.length,
    i,
    w;
  for (i = 0; i + 4 <= len; i += 4) {
    w = s[i] | (s[i + 1] << 8) | (s[i + 2] << 16) | (s[i + 3] << 24);
    h = caml_hash_mix_int(h, w);
  }
  w = 0;
  switch (len & 3) {
    case 3:
      // biome-ignore lint/suspicious/noFallthroughSwitchClause:
      w = s[i + 2] << 16;
    // fallthrough
    case 2:
      // biome-ignore lint/suspicious/noFallthroughSwitchClause:
      w |= s[i + 1] << 8;
    // fallthrough
    case 1:
      w |= s[i];
      h = caml_hash_mix_int(h, w);
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
  if (typeof content === "string") return caml_hash_mix_jsbytes(h, content);
  /* ARRAY */ else return caml_hash_mix_bytes_arr(h, content);
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
function caml_hash(count, limit, seed, obj) {
  var queue, rd, wr, sz, num, h, v, i, len;
  sz = limit;
  if (sz < 0 || sz > 256) sz = 256;
  num = count;
  h = seed;
  queue = [obj];
  rd = 0;
  wr = 1;
  while (rd < wr && num > 0) {
    v = queue[rd++];
    if (v?.caml_custom) {
      if (
        caml_custom_ops[v.caml_custom] &&
        caml_custom_ops[v.caml_custom].hash
      ) {
        var hh = caml_custom_ops[v.caml_custom].hash(v);
        h = caml_hash_mix_int(h, hh);
        num--;
      }
    } else if (Array.isArray(v) && v[0] === (v[0] | 0)) {
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
          if (caml_is_continuation_tag(v[0])) {
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
      h = caml_hash_mix_bytes(h, v);
      num--;
    } else if (caml_is_ml_string(v)) {
      h = caml_hash_mix_string(h, v);
      num--;
    } else if (typeof v === "string") {
      h = caml_hash_mix_jsbytes(h, v);
      num--;
    } else if (v === (v | 0)) {
      // Integer
      h = caml_hash_mix_int(h, v + v + 1);
      num--;
    } else if (typeof v === "number") {
      // Float
      h = caml_hash_mix_float(h, v);
      num--;
    }
  }
  h = caml_hash_mix_final(h);
  return h & 0x3fffffff;
}

//Provides: caml_string_hash
//Requires: caml_hash_mix_final, caml_hash_mix_string
//Version: >= 5.0
function caml_string_hash(h, v) {
  var h = caml_hash_mix_string(h, v);
  var h = caml_hash_mix_final(h);
  return h & 0x3fffffff;
}
