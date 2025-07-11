//Provides: blake2b
//Version: >= 5.2
var blake2b = (function () {
  // Blake2B in pure Javascript
  // Adapted from the reference implementation in RFC7693
  // Ported to Javascript by DC - https://github.com/dcposch

  // 64-bit unsigned addition
  // Sets v[a,a+1] += v[b,b+1]
  // v should be a Uint32Array
  function ADD64AA(v, a, b) {
    const o0 = v[a] + v[b];
    let o1 = v[a + 1] + v[b + 1];
    if (o0 >= 0x100000000) {
      o1++;
    }
    v[a] = o0;
    v[a + 1] = o1;
  }

  // 64-bit unsigned addition
  // Sets v[a,a+1] += b
  // b0 is the low 32 bits of b, b1 represents the high 32 bits
  function ADD64AC(v, a, b0, b1) {
    let o0 = v[a] + b0;
    if (b0 < 0) {
      o0 += 0x100000000;
    }
    let o1 = v[a + 1] + b1;
    if (o0 >= 0x100000000) {
      o1++;
    }
    v[a] = o0;
    v[a + 1] = o1;
  }

  // Little-endian byte access
  function B2B_GET32(arr, i) {
    return arr[i] ^ (arr[i + 1] << 8) ^ (arr[i + 2] << 16) ^ (arr[i + 3] << 24);
  }

  // G Mixing function
  // The ROTRs are inlined for speed
  function B2B_G(a, b, c, d, ix, iy) {
    const x0 = m[ix];
    const x1 = m[ix + 1];
    const y0 = m[iy];
    const y1 = m[iy + 1];

    ADD64AA(v, a, b); // v[a,a+1] += v[b,b+1] ... in JS we must store a uint64 as two uint32s
    ADD64AC(v, a, x0, x1); // v[a, a+1] += x ... x0 is the low 32 bits of x, x1 is the high 32 bits

    // v[d,d+1] = (v[d,d+1] xor v[a,a+1]) rotated to the right by 32 bits
    let xor0 = v[d] ^ v[a];
    let xor1 = v[d + 1] ^ v[a + 1];
    v[d] = xor1;
    v[d + 1] = xor0;

    ADD64AA(v, c, d);

    // v[b,b+1] = (v[b,b+1] xor v[c,c+1]) rotated right by 24 bits
    xor0 = v[b] ^ v[c];
    xor1 = v[b + 1] ^ v[c + 1];
    v[b] = (xor0 >>> 24) ^ (xor1 << 8);
    v[b + 1] = (xor1 >>> 24) ^ (xor0 << 8);

    ADD64AA(v, a, b);
    ADD64AC(v, a, y0, y1);

    // v[d,d+1] = (v[d,d+1] xor v[a,a+1]) rotated right by 16 bits
    xor0 = v[d] ^ v[a];
    xor1 = v[d + 1] ^ v[a + 1];
    v[d] = (xor0 >>> 16) ^ (xor1 << 16);
    v[d + 1] = (xor1 >>> 16) ^ (xor0 << 16);

    ADD64AA(v, c, d);

    // v[b,b+1] = (v[b,b+1] xor v[c,c+1]) rotated right by 63 bits
    xor0 = v[b] ^ v[c];
    xor1 = v[b + 1] ^ v[c + 1];
    v[b] = (xor1 >>> 31) ^ (xor0 << 1);
    v[b + 1] = (xor0 >>> 31) ^ (xor1 << 1);
  }

  // Initialization Vector
  const BLAKE2B_IV32 = new Uint32Array([
    0xf3bcc908, 0x6a09e667, 0x84caa73b, 0xbb67ae85, 0xfe94f82b, 0x3c6ef372,
    0x5f1d36f1, 0xa54ff53a, 0xade682d1, 0x510e527f, 0x2b3e6c1f, 0x9b05688c,
    0xfb41bd6b, 0x1f83d9ab, 0x137e2179, 0x5be0cd19,
  ]);

  const SIGMA8 = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10, 4, 8, 9, 15,
    13, 6, 1, 12, 0, 2, 11, 7, 5, 3, 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6,
    7, 1, 9, 4, 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8, 9, 0, 5,
    7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13, 2, 12, 6, 10, 0, 11, 8, 3, 4,
    13, 7, 5, 15, 14, 1, 9, 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8,
    11, 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10, 6, 15, 14, 9, 11,
    3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5, 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14,
    3, 12, 13, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10,
    4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3,
  ];

  // These are offsets into a uint64 buffer.
  // Multiply them all by 2 to make them offsets into a uint32 buffer,
  // because this is Javascript and we don't have uint64s
  const SIGMA82 = new Uint8Array(
    SIGMA8.map(function (x) {
      return x * 2;
    }),
  );

  // Compression function. 'last' flag indicates last block.
  // Note we're representing 16 uint64s as 32 uint32s
  const v = new Uint32Array(32);
  const m = new Uint32Array(32);
  function blake2bCompress(ctx, last) {
    let i = 0;

    // init work variables
    for (i = 0; i < 16; i++) {
      v[i] = ctx.h[i];
      v[i + 16] = BLAKE2B_IV32[i];
    }

    // low 64 bits of offset
    v[24] = v[24] ^ ctx.t;
    v[25] = v[25] ^ (ctx.t / 0x100000000);
    // high 64 bits not supported, offset may not be higher than 2**53-1

    // last block flag set ?
    if (last) {
      v[28] = ~v[28];
      v[29] = ~v[29];
    }

    // get little-endian words
    for (i = 0; i < 32; i++) {
      m[i] = B2B_GET32(ctx.b, 4 * i);
    }

    // twelve rounds of mixing
    // uncomment the DebugPrint calls to log the computation
    // and match the RFC sample documentation
    for (i = 0; i < 12; i++) {
      B2B_G(0, 8, 16, 24, SIGMA82[i * 16 + 0], SIGMA82[i * 16 + 1]);
      B2B_G(2, 10, 18, 26, SIGMA82[i * 16 + 2], SIGMA82[i * 16 + 3]);
      B2B_G(4, 12, 20, 28, SIGMA82[i * 16 + 4], SIGMA82[i * 16 + 5]);
      B2B_G(6, 14, 22, 30, SIGMA82[i * 16 + 6], SIGMA82[i * 16 + 7]);
      B2B_G(0, 10, 20, 30, SIGMA82[i * 16 + 8], SIGMA82[i * 16 + 9]);
      B2B_G(2, 12, 22, 24, SIGMA82[i * 16 + 10], SIGMA82[i * 16 + 11]);
      B2B_G(4, 14, 16, 26, SIGMA82[i * 16 + 12], SIGMA82[i * 16 + 13]);
      B2B_G(6, 8, 18, 28, SIGMA82[i * 16 + 14], SIGMA82[i * 16 + 15]);
    }

    for (i = 0; i < 16; i++) {
      ctx.h[i] = ctx.h[i] ^ v[i] ^ v[i + 16];
    }
  }

  // reusable parameterBlock
  const parameterBlock = new Uint8Array([
    0,
    0,
    0,
    0, //  0: outlen, keylen, fanout, depth
    0,
    0,
    0,
    0, //  4: leaf length, sequential mode
    0,
    0,
    0,
    0, //  8: node offset
    0,
    0,
    0,
    0, // 12: node offset
    0,
    0,
    0,
    0, // 16: node depth, inner length, rfu
    0,
    0,
    0,
    0, // 20: rfu
    0,
    0,
    0,
    0, // 24: rfu
    0,
    0,
    0,
    0, // 28: rfu
    0,
    0,
    0,
    0, // 32: salt
    0,
    0,
    0,
    0, // 36: salt
    0,
    0,
    0,
    0, // 40: salt
    0,
    0,
    0,
    0, // 44: salt
    0,
    0,
    0,
    0, // 48: personal
    0,
    0,
    0,
    0, // 52: personal
    0,
    0,
    0,
    0, // 56: personal
    0,
    0,
    0,
    0, // 60: personal
  ]);

  // Creates a BLAKE2b hashing context
  // Requires an output length between 1 and 64 bytes
  // Takes an optional Uint8Array key
  function blake2bInit(outlen, key) {
    if (outlen === 0 || outlen > 64) {
      throw new Error("Illegal output length, expected 0 < length <= 64");
    }
    if (key.length > 64) {
      throw new Error("Illegal key, expected Uint8Array with 0 < length <= 64");
    }

    // state, 'param block'
    const ctx = {
      b: new Uint8Array(128),
      h: new Uint32Array(16),
      t: 0, // input count
      c: 0, // pointer within buffer
      outlen: outlen, // output length in bytes
    };

    // initialize parameterBlock before usage
    parameterBlock.fill(0);
    parameterBlock[0] = outlen;
    parameterBlock[1] = key.length;
    parameterBlock[2] = 1; // fanout
    parameterBlock[3] = 1; // depth

    // initialize hash state
    for (let i = 0; i < 16; i++) {
      ctx.h[i] = BLAKE2B_IV32[i] ^ B2B_GET32(parameterBlock, i * 4);
    }

    if (key.length > 0) {
      blake2bUpdate(ctx, key);
      // at the end
      ctx.c = 128;
    }

    return ctx;
  }

  // Updates a BLAKE2b streaming hash
  // Requires hash context and Uint8Array (byte array)
  function blake2bUpdate(ctx, input) {
    for (let i = 0; i < input.length; i++) {
      if (ctx.c === 128) {
        // buffer full ?
        ctx.t += ctx.c; // add counters
        blake2bCompress(ctx, false); // compress (not last)
        ctx.c = 0; // counter to zero
      }
      ctx.b[ctx.c++] = input[i];
    }
  }

  // Completes a BLAKE2b streaming hash
  // Returns a Uint8Array containing the message digest
  function blake2bFinal(ctx) {
    ctx.t += ctx.c; // mark last block offset

    while (ctx.c < 128) {
      // fill up with zeros
      ctx.b[ctx.c++] = 0;
    }
    blake2bCompress(ctx, true); // final block flag = 1

    // little endian convert and store
    const out = new Uint8Array(ctx.outlen);
    for (let i = 0; i < ctx.outlen; i++) {
      out[i] = ctx.h[i >> 2] >> (8 * (i & 3));
    }
    return out;
  }
  return { Init: blake2bInit, Update: blake2bUpdate, Final: blake2bFinal };
})();

//Provides: caml_blake2_create
//Requires: caml_uint8_array_of_string
//Requires: blake2b
//Version: >= 5.2
function caml_blake2_create(hashlen, key) {
  key = caml_uint8_array_of_string(key);
  if (key.length > 64) {
    key.subarray(0, 64);
  }
  return blake2b.Init(hashlen, key);
}

//Provides: caml_blake2_final
//Requires: caml_string_of_uint8_array
//Requires: blake2b
//Version: >= 5.2
function caml_blake2_final(ctx, _hashlen) {
  // ctx.outlen === hashlen
  var r = blake2b.Final(ctx);
  return caml_string_of_uint8_array(r);
}

//Provides: caml_blake2_update
//Requires: blake2b
//Requires: caml_uint8_array_of_string
//Version: >= 5.2, < 5.3
function caml_blake2_update(ctx, buf, ofs, len) {
  var input = caml_uint8_array_of_string(buf);
  input = input.subarray(ofs, ofs + len);
  blake2b.Update(ctx, input);
  return 0;
}

//Provides: caml_blake2_update
//Requires: blake2b
//Requires: caml_uint8_array_of_bytes
//Version: >= 5.3
function caml_blake2_update(ctx, buf, ofs, len) {
  var input = caml_uint8_array_of_bytes(buf);
  input = input.subarray(ofs, ofs + len);
  blake2b.Update(ctx, input);
  return 0;
}

//Provides: caml_blake2_string
//Requires: caml_blake2_create
//Requires: caml_blake2_update
//Requires: caml_blake2_final
//Version: >= 5.2, < 5.3
function caml_blake2_string(hashlen, key, buf, ofs, len) {
  var ctx = caml_blake2_create(hashlen, key);
  caml_blake2_update(ctx, buf, ofs, len);
  return caml_blake2_final(ctx, hashlen);
}

//Provides: caml_blake2_string
//Requires: caml_blake2_create
//Requires: caml_blake2_update
//Requires: caml_blake2_final
//Requires: caml_bytes_of_string
//Version: >= 5.3
function caml_blake2_string(hashlen, key, buf_str, ofs, len) {
  var ctx = caml_blake2_create(hashlen, key);
  var buf = caml_bytes_of_string(buf_str);
  caml_blake2_update(ctx, buf, ofs, len);
  return caml_blake2_final(ctx, hashlen);
}

//Provides: caml_blake2_bytes
//Requires: caml_blake2_create
//Requires: caml_blake2_update
//Requires: caml_blake2_final
//Version: >= 5.3
function caml_blake2_bytes(hashlen, key, buf, ofs, len) {
  var ctx = caml_blake2_create(hashlen, key);
  caml_blake2_update(ctx, buf, ofs, len);
  return caml_blake2_final(ctx, hashlen);
}

//Provides: blake2_js_for_wasm_create
//Requires: caml_blake2_create, caml_string_of_jsbytes
//If: wasm
//Version: >= 5.2
function blake2_js_for_wasm_create(hashlen, key) {
  const key_jsoo_string = caml_string_of_jsbytes(key);
  return caml_blake2_create(hashlen, key_jsoo_string);
}

//Provides: blake2_js_for_wasm_final
//Requires: caml_blake2_final, caml_jsbytes_of_string
//If: wasm
//Version: >= 5.2
function blake2_js_for_wasm_final(ctx, hashlen) {
  return caml_jsbytes_of_string(caml_blake2_final(ctx, hashlen));
}

//Provides: blake2_js_for_wasm_update
//Requires: caml_blake2_update, caml_string_of_jsbytes
//If: wasm
//Version: >= 5.2, < 5.3
function blake2_js_for_wasm_update(ctx, buf, ofs, len) {
  const buf_jsoo_string = caml_string_of_jsbytes(buf);
  return caml_blake2_update(ctx, buf_jsoo_string, ofs, len);
}

//Provides: blake2_js_for_wasm_update
//Requires: caml_blake2_update, caml_bytes_of_jsbytes
//If: wasm
//Version: >= 5.3
function blake2_js_for_wasm_update(ctx, buf, ofs, len) {
  const buf_jsoo_string = caml_bytes_of_jsbytes(buf);
  return caml_blake2_update(ctx, buf_jsoo_string, ofs, len);
}
