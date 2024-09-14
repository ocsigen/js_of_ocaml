//Provides: zstd_decompress
//Version: >= 5.1
const zstd_decompress = (function () {
  // aliases for shorter compressed code (most minifers don't do this)
  const ab = ArrayBuffer;
  const u8 = Uint8Array;
  const u16 = Uint16Array;
  const i16 = Int16Array;
  const u32 = Uint32Array;
  const i32 = Int32Array;
  const slc = function (v, s, e) {
    if (u8.prototype.slice) return u8.prototype.slice.call(v, s, e);
    if (s == null || s < 0) s = 0;
    if (e == null || e > v.length) e = v.length;
    const n = new u8(e - s);
    n.set(v.subarray(s, e));
    return n;
  };
  const fill = function (v, n, s, e) {
    if (u8.prototype.fill) return u8.prototype.fill.call(v, n, s, e);
    if (s == null || s < 0) s = 0;
    if (e == null || e > v.length) e = v.length;
    for (; s < e; ++s) v[s] = n;
    return v;
  };
  const cpw = function (v, t, s, e) {
    if (u8.prototype.copyWithin)
      return u8.prototype.copyWithin.call(v, t, s, e);
    if (s == null || s < 0) s = 0;
    if (e == null || e > v.length) e = v.length;
    while (s < e) {
      v[t++] = v[s++];
    }
  };
  /**
   * Codes for errors generated within this library
   */
  // error codes
  const ec = [
    "invalid zstd data",
    "window size too large (>2046MB)",
    "invalid block type",
    "FSE accuracy too high",
    "match distance too far back",
    "unexpected EOF",
  ];
  const err = function (ind, msg, nt) {
    const e = new Error(msg || ec[ind]);
    e.code = ind;
    if (!nt) throw e;
    return e;
  };
  const rb = function (d, b, n) {
    let i = 0;
    let o = 0;
    for (; i < n; ++i) o |= d[b++] << (i << 3);
    return o;
  };
  const b4 = function (d, b) {
    return (d[b] | (d[b + 1] << 8) | (d[b + 2] << 16) | (d[b + 3] << 24)) >>> 0;
  };
  // read Zstandard frame header
  const rzfh = function (dat, w) {
    const n3 = dat[0] | (dat[1] << 8) | (dat[2] << 16);
    if (n3 == 0x2fb528 && dat[3] == 253) {
      // Zstandard
      const flg = dat[4];
      //    single segment       checksum             dict flag     frame content flag
      const ss = (flg >> 5) & 1;
      const cc = (flg >> 2) & 1;
      const df = flg & 3;
      const fcf = flg >> 6;
      if (flg & 8) err(0);
      // byte
      let bt = 6 - ss;
      // dict bytes
      const db = df == 3 ? 4 : df;
      // dictionary id
      const di = rb(dat, bt, db);
      bt += db;
      // frame size bytes
      const fsb = fcf ? 1 << fcf : ss;
      // frame source size
      const fss = rb(dat, bt, fsb) + (fcf == 1 && 256);
      // window size
      let ws = fss;
      if (!ss) {
        // window descriptor
        const wb = 1 << (10 + (dat[5] >> 3));
        ws = wb + (wb >> 3) * (dat[5] & 7);
      }
      if (ws > 2145386496) err(1);
      const buf = new u8((w == 1 ? fss || ws : w ? 0 : ws) + 12);
      (buf[0] = 1), (buf[4] = 4), (buf[8] = 8);
      return {
        b: bt + fsb,
        y: 0,
        l: 0,
        d: di,
        w: w && w != 1 ? w : buf.subarray(12),
        e: ws,
        o: new i32(buf.buffer, 0, 3),
        u: fss,
        c: cc,
        m: Math.min(131072, ws),
      };
    } else if (((n3 >> 4) | (dat[3] << 20)) == 0x184d2a5) {
      // skippable
      return b4(dat, 4) + 8;
    }
    err(0);
  };
  // most significant bit for nonzero
  const msb = function (val) {
    let bits = 0;
    for (; 1 << bits <= val; ++bits);
    return bits - 1;
  };
  // read finite state entropy
  const rfse = function (dat, bt, mal) {
    // table pos
    let tpos = (bt << 3) + 4;
    // accuracy log
    const al = (dat[bt] & 15) + 5;
    if (al > mal) err(3);
    // size
    const sz = 1 << al;
    // probabilities symbols  repeat   index   high threshold
    let probs = sz;
    let sym = -1;
    let re = -1;
    let i = -1;
    let ht = sz;
    // optimization: single allocation is much faster
    const buf = new ab(512 + (sz << 2));
    const freq = new i16(buf, 0, 256);
    // same view as freq
    const dstate = new u16(buf, 0, 256);
    const nstate = new u16(buf, 512, sz);
    const bb1 = 512 + (sz << 1);
    const syms = new u8(buf, bb1, sz);
    const nbits = new u8(buf, bb1 + sz);
    while (sym < 255 && probs > 0) {
      const bits = msb(probs + 1);
      const cbt = tpos >> 3;
      // mask
      const msk = (1 << (bits + 1)) - 1;
      let val =
        ((dat[cbt] | (dat[cbt + 1] << 8) | (dat[cbt + 2] << 16)) >>
          (tpos & 7)) &
        msk;
      // mask (1 fewer bit)
      const msk1fb = (1 << bits) - 1;
      // max small value
      const msv = msk - probs - 1;
      // small value
      const sval = val & msk1fb;
      if (sval < msv) (tpos += bits), (val = sval);
      else {
        tpos += bits + 1;
        if (val > msk1fb) val -= msv;
      }
      freq[++sym] = --val;
      if (val == -1) {
        probs += val;
        syms[--ht] = sym;
      } else probs -= val;
      if (!val) {
        do {
          // repeat byte
          const rbt = tpos >> 3;
          re = ((dat[rbt] | (dat[rbt + 1] << 8)) >> (tpos & 7)) & 3;
          tpos += 2;
          sym += re;
        } while (re == 3);
      }
    }
    if (sym > 255 || probs) err(0);
    let sympos = 0;
    // sym step (coprime with sz - formula from zstd source)
    const sstep = (sz >> 1) + (sz >> 3) + 3;
    // sym mask
    const smask = sz - 1;
    for (let s = 0; s <= sym; ++s) {
      const sf = freq[s];
      if (sf < 1) {
        dstate[s] = -sf;
        continue;
      }
      // This is split into two loops in zstd to avoid branching, but as JS is higher-level that is unnecessary
      for (i = 0; i < sf; ++i) {
        syms[sympos] = s;
        do {
          sympos = (sympos + sstep) & smask;
        } while (sympos >= ht);
      }
    }
    // After spreading symbols, should be zero again
    if (sympos) err(0);
    for (i = 0; i < sz; ++i) {
      // next state
      const ns = dstate[syms[i]]++;
      // num bits
      const nb = (nbits[i] = al - msb(ns));
      nstate[i] = (ns << nb) - sz;
    }
    return [
      (tpos + 7) >> 3,
      {
        b: al,
        s: syms,
        n: nbits,
        t: nstate,
      },
    ];
  };
  // read huffman
  const rhu = function (dat, bt) {
    //  index  weight count
    let i = 0,
      wc = -1;
    //    buffer             header byte
    const buf = new u8(292);
    const hb = dat[bt];
    // huffman weights
    const hw = buf.subarray(0, 256);
    // rank count
    const rc = buf.subarray(256, 268);
    // rank index
    const ri = new u16(buf.buffer, 268);
    // NOTE: at this point bt is 1 less than expected
    if (hb < 128) {
      // end byte, fse decode table
      const _a = rfse(dat, bt + 1, 6);
      const ebt = _a[0];
      const fdt = _a[1];
      bt += hb;
      const epos = ebt << 3;
      // last byte
      const lb = dat[bt];
      if (!lb) err(0);
      //  state1   state2   state1 bits   state2 bits
      let st1 = 0;
      let st2 = 0;
      let btr1 = fdt.b;
      let btr2 = btr1;
      // fse pos
      // pre-increment to account for original deficit of 1
      let fpos = (++bt << 3) - 8 + msb(lb);
      for (;;) {
        fpos -= btr1;
        if (fpos < epos) break;
        let cbt = fpos >> 3;
        st1 +=
          ((dat[cbt] | (dat[cbt + 1] << 8)) >> (fpos & 7)) & ((1 << btr1) - 1);
        hw[++wc] = fdt.s[st1];
        fpos -= btr2;
        if (fpos < epos) break;
        cbt = fpos >> 3;
        st2 +=
          ((dat[cbt] | (dat[cbt + 1] << 8)) >> (fpos & 7)) & ((1 << btr2) - 1);
        hw[++wc] = fdt.s[st2];
        btr1 = fdt.n[st1];
        st1 = fdt.t[st1];
        btr2 = fdt.n[st2];
        st2 = fdt.t[st2];
      }
      if (++wc > 255) err(0);
    } else {
      wc = hb - 127;
      for (; i < wc; i += 2) {
        const byte = dat[++bt];
        hw[i] = byte >> 4;
        hw[i + 1] = byte & 15;
      }
      ++bt;
    }
    // weight exponential sum
    let wes = 0;
    for (i = 0; i < wc; ++i) {
      const wt = hw[i];
      // bits must be at most 11, same as weight
      if (wt > 11) err(0);
      wes += wt && 1 << (wt - 1);
    }
    // max bits
    const mb = msb(wes) + 1;
    // table size
    const ts = 1 << mb;
    // remaining sum
    const rem = ts - wes;
    // must be power of 2
    if (rem & (rem - 1)) err(0);
    hw[wc++] = msb(rem) + 1;
    for (i = 0; i < wc; ++i) {
      const wt = hw[i];
      ++rc[(hw[i] = wt && mb + 1 - wt)];
    }
    // huf buf
    const hbuf = new u8(ts << 1);
    //    symbols                      num bits
    const syms = hbuf.subarray(0, ts);
    const nb = hbuf.subarray(ts);
    ri[mb] = 0;
    for (i = mb; i > 0; --i) {
      const pv = ri[i];
      fill(nb, i, pv, (ri[i - 1] = pv + rc[i] * (1 << (mb - i))));
    }
    if (ri[0] != ts) err(0);
    for (i = 0; i < wc; ++i) {
      const bits = hw[i];
      if (bits) {
        const code = ri[bits];
        fill(syms, i, code, (ri[bits] = code + (1 << (mb - bits))));
      }
    }
    return [
      bt,
      {
        n: nb,
        b: mb,
        s: syms,
      },
    ];
  };
  // Tables generated using this:
  // https://gist.github.com/101arrowz/a979452d4355992cbf8f257cbffc9edd
  // default literal length table
  const dllt = /*#__PURE__*/ rfse(
    /*#__PURE__*/ new u8([
      81, 16, 99, 140, 49, 198, 24, 99, 12, 33, 196, 24, 99, 102, 102, 134, 70,
      146, 4,
    ]),
    0,
    6,
  )[1];
  // default match length table
  const dmlt = /*#__PURE__*/ rfse(
    /*#__PURE__*/ new u8([
      33, 20, 196, 24, 99, 140, 33, 132, 16, 66, 8, 33, 132, 16, 66, 8, 33, 68,
      68, 68, 68, 68, 68, 68, 68, 36, 9,
    ]),
    0,
    6,
  )[1];
  // default offset code table
  const doct = /*#__PURE__ */ rfse(
    /*#__PURE__*/ new u8([32, 132, 16, 66, 102, 70, 68, 68, 68, 68, 36, 73, 2]),
    0,
    5,
  )[1];
  // bits to baseline
  const b2bl = function (b, s) {
    const len = b.length;
    const bl = new i32(len);
    for (let i = 0; i < len; ++i) {
      bl[i] = s;
      s += 1 << b[i];
    }
    return bl;
  };
  // literal length bits
  const llb = /*#__PURE__ */ new u8(
    /*#__PURE__ */ new i32([
      0, 0, 0, 0, 16843009, 50528770, 134678020, 202050057, 269422093,
    ]).buffer,
    0,
    36,
  );
  // literal length baseline
  const llbl = /*#__PURE__ */ b2bl(llb, 0);
  // match length bits
  const mlb = /*#__PURE__ */ new u8(
    /*#__PURE__ */ new i32([
      0, 0, 0, 0, 0, 0, 0, 0, 16843009, 50528770, 117769220, 185207048,
      252579084, 16,
    ]).buffer,
    0,
    53,
  );
  // match length baseline
  const mlbl = /*#__PURE__ */ b2bl(mlb, 3);
  // decode huffman stream
  const dhu = function (dat, out, hu) {
    const len = dat.length;
    const ss = out.length;
    const lb = dat[len - 1];
    const msk = (1 << hu.b) - 1;
    const eb = -hu.b;
    if (!lb) err(0);
    let st = 0;
    let btr = hu.b;
    let pos = (len << 3) - 8 + msb(lb) - btr;
    let i = -1;
    while (pos > eb && i < ss) {
      const cbt = pos >> 3;
      const val =
        (dat[cbt] | (dat[cbt + 1] << 8) | (dat[cbt + 2] << 16)) >> (pos & 7);
      st = ((st << btr) | val) & msk;
      out[++i] = hu.s[st];
      pos -= btr = hu.n[st];
    }
    if (pos != eb || i + 1 != ss) err(0);
  };
  // decode huffman stream 4x
  // TODO: use workers to parallelize
  const dhu4 = function (dat, out, hu) {
    let bt = 6;
    const ss = out.length;
    const sz1 = (ss + 3) >> 2;
    const sz2 = sz1 << 1;
    const sz3 = sz1 + sz2;
    dhu(
      dat.subarray(bt, (bt += dat[0] | (dat[1] << 8))),
      out.subarray(0, sz1),
      hu,
    );
    dhu(
      dat.subarray(bt, (bt += dat[2] | (dat[3] << 8))),
      out.subarray(sz1, sz2),
      hu,
    );
    dhu(
      dat.subarray(bt, (bt += dat[4] | (dat[5] << 8))),
      out.subarray(sz2, sz3),
      hu,
    );
    dhu(dat.subarray(bt), out.subarray(sz3), hu);
  };
  // read Zstandard block
  const rzb = function (dat, st, out) {
    let _a;
    let bt = st.b;
    //    byte 0        block type
    const b0 = dat[bt];
    const btype = (b0 >> 1) & 3;
    st.l = b0 & 1;
    const sz = (b0 >> 3) | (dat[bt + 1] << 5) | (dat[bt + 2] << 13);
    // end byte for block
    const ebt = (bt += 3) + sz;
    if (btype == 1) {
      if (bt >= dat.length) return;
      st.b = bt + 1;
      if (out) {
        fill(out, dat[bt], st.y, (st.y += sz));
        return out;
      }
      return fill(new u8(sz), dat[bt]);
    }
    if (ebt > dat.length) return;
    if (btype == 0) {
      st.b = ebt;
      if (out) {
        out.set(dat.subarray(bt, ebt), st.y);
        st.y += sz;
        return out;
      }
      return slc(dat, bt, ebt);
    }
    if (btype == 2) {
      //    byte 3        lit btype     size format
      const b3 = dat[bt];
      const lbt = b3 & 3;
      const sf = (b3 >> 2) & 3;
      // lit src size  lit cmp sz 4 streams
      let lss = b3 >> 4;
      let lcs = 0;
      let s4 = 0;
      if (lbt < 2) {
        if (sf & 1) lss |= (dat[++bt] << 4) | (sf & 2 && dat[++bt] << 12);
        else lss = b3 >> 3;
      } else {
        s4 = sf;
        if (sf < 2)
          (lss |= (dat[++bt] & 63) << 4),
            (lcs = (dat[bt] >> 6) | (dat[++bt] << 2));
        else if (sf == 2)
          (lss |= (dat[++bt] << 4) | ((dat[++bt] & 3) << 12)),
            (lcs = (dat[bt] >> 2) | (dat[++bt] << 6));
        else
          (lss |= (dat[++bt] << 4) | ((dat[++bt] & 63) << 12)),
            (lcs = (dat[bt] >> 6) | (dat[++bt] << 2) | (dat[++bt] << 10));
      }
      ++bt;
      // add literals to end - can never overlap with backreferences because unused literals always appended
      let buf = out ? out.subarray(st.y, st.y + st.m) : new u8(st.m);
      // starting point for literals
      let spl = buf.length - lss;
      if (lbt == 0) buf.set(dat.subarray(bt, (bt += lss)), spl);
      else if (lbt == 1) fill(buf, dat[bt++], spl);
      else {
        // huffman table
        let hu = st.h;
        if (lbt == 2) {
          const hud = rhu(dat, bt);
          // subtract description length
          lcs += bt - (bt = hud[0]);
          st.h = hu = hud[1];
        } else if (!hu) err(0);
        (s4 ? dhu4 : dhu)(dat.subarray(bt, (bt += lcs)), buf.subarray(spl), hu);
      }
      // num sequences
      let ns = dat[bt++];
      if (ns) {
        if (ns == 255) ns = (dat[bt++] | (dat[bt++] << 8)) + 0x7f00;
        else if (ns > 127) ns = ((ns - 128) << 8) | dat[bt++];
        // symbol compression modes
        const scm = dat[bt++];
        if (scm & 3) err(0);
        const dts = [dmlt, doct, dllt];
        for (let i = 2; i > -1; --i) {
          const md = (scm >> ((i << 1) + 2)) & 3;
          if (md == 1) {
            // rle buf
            const rbuf = new u8([0, 0, dat[bt++]]);
            dts[i] = {
              s: rbuf.subarray(2, 3),
              n: rbuf.subarray(0, 1),
              t: new u16(rbuf.buffer, 0, 1),
              b: 0,
            };
          } else if (md == 2) {
            // accuracy log 8 for offsets, 9 for others
            (_a = rfse(dat, bt, 9 - (i & 1))), (bt = _a[0]), (dts[i] = _a[1]);
          } else if (md == 3) {
            if (!st.t) err(0);
            dts[i] = st.t[i];
          }
        }
        const _b = (st.t = dts);
        const mlt = _b[0];
        const oct = _b[1];
        const llt = _b[2];
        const lb = dat[ebt - 1];
        if (!lb) err(0);
        let spos = (ebt << 3) - 8 + msb(lb) - llt.b;
        let cbt = spos >> 3;
        let oubt = 0;
        let lst =
          ((dat[cbt] | (dat[cbt + 1] << 8)) >> (spos & 7)) & ((1 << llt.b) - 1);
        cbt = (spos -= oct.b) >> 3;
        let ost =
          ((dat[cbt] | (dat[cbt + 1] << 8)) >> (spos & 7)) & ((1 << oct.b) - 1);
        cbt = (spos -= mlt.b) >> 3;
        let mst =
          ((dat[cbt] | (dat[cbt + 1] << 8)) >> (spos & 7)) & ((1 << mlt.b) - 1);
        for (++ns; --ns; ) {
          const llc = llt.s[lst];
          const lbtr = llt.n[lst];
          const mlc = mlt.s[mst];
          const mbtr = mlt.n[mst];
          const ofc = oct.s[ost];
          const obtr = oct.n[ost];
          cbt = (spos -= ofc) >> 3;
          const ofp = 1 << ofc;
          let off =
            ofp +
            (((dat[cbt] |
              (dat[cbt + 1] << 8) |
              (dat[cbt + 2] << 16) |
              (dat[cbt + 3] << 24)) >>>
              (spos & 7)) &
              (ofp - 1));
          cbt = (spos -= mlb[mlc]) >> 3;
          let ml =
            mlbl[mlc] +
            (((dat[cbt] | (dat[cbt + 1] << 8) | (dat[cbt + 2] << 16)) >>
              (spos & 7)) &
              ((1 << mlb[mlc]) - 1));
          cbt = (spos -= llb[llc]) >> 3;
          const ll =
            llbl[llc] +
            (((dat[cbt] | (dat[cbt + 1] << 8) | (dat[cbt + 2] << 16)) >>
              (spos & 7)) &
              ((1 << llb[llc]) - 1));
          cbt = (spos -= lbtr) >> 3;
          lst =
            llt.t[lst] +
            (((dat[cbt] | (dat[cbt + 1] << 8)) >> (spos & 7)) &
              ((1 << lbtr) - 1));
          cbt = (spos -= mbtr) >> 3;
          mst =
            mlt.t[mst] +
            (((dat[cbt] | (dat[cbt + 1] << 8)) >> (spos & 7)) &
              ((1 << mbtr) - 1));
          cbt = (spos -= obtr) >> 3;
          ost =
            oct.t[ost] +
            (((dat[cbt] | (dat[cbt + 1] << 8)) >> (spos & 7)) &
              ((1 << obtr) - 1));
          if (off > 3) {
            st.o[2] = st.o[1];
            st.o[1] = st.o[0];
            st.o[0] = off -= 3;
          } else {
            const idx = off - (ll != 0);
            if (idx) {
              off = idx == 3 ? st.o[0] - 1 : st.o[idx];
              if (idx > 1) st.o[2] = st.o[1];
              st.o[1] = st.o[0];
              st.o[0] = off;
            } else off = st.o[0];
          }
          for (let i = 0; i < ll; ++i) {
            buf[oubt + i] = buf[spl + i];
          }
          (oubt += ll), (spl += ll);
          let stin = oubt - off;
          if (stin < 0) {
            let len = -stin;
            const bs = st.e + stin;
            if (len > ml) len = ml;
            for (let i = 0; i < len; ++i) {
              buf[oubt + i] = st.w[bs + i];
            }
            (oubt += len), (ml -= len), (stin = 0);
          }
          for (let i = 0; i < ml; ++i) {
            buf[oubt + i] = buf[stin + i];
          }
          oubt += ml;
        }
        if (oubt != spl) {
          while (spl < buf.length) {
            buf[oubt++] = buf[spl++];
          }
        } else oubt = buf.length;
        if (out) st.y += oubt;
        else buf = slc(buf, 0, oubt);
      } else {
        if (out) {
          st.y += lss;
          if (spl) {
            for (let i = 0; i < lss; ++i) {
              buf[i] = buf[spl + i];
            }
          }
        } else if (spl) buf = slc(buf, spl);
      }
      st.b = ebt;
      return buf;
    }
    err(2);
  };
  // concat
  const cct = function (bufs, ol) {
    if (bufs.length == 1) return bufs[0];
    const buf = new u8(ol);
    for (let i = 0, b = 0; i < bufs.length; ++i) {
      const chk = bufs[i];
      buf.set(chk, b);
      b += chk.length;
    }
    return buf;
  };
  /**
   * Decompresses Zstandard data
   * @param dat The input data
   * @param buf The output buffer. If unspecified, the function will allocate
   *            exactly enough memory to fit the decompressed data. If your
   *            data has multiple frames and you know the output size, specifying
   *            it will yield better performance.
   * @returns The decompressed data
   */
  return function decompress(dat, buf) {
    let bt = 0;
    const bufs = [];
    const nb = +!buf;
    let ol = 0;
    while (dat.length) {
      const st = rzfh(dat, nb || buf);
      if (typeof st == "object") {
        if (nb) {
          buf = null;
          if (st.w.length == st.u) {
            bufs.push((buf = st.w));
            ol += st.u;
          }
        } else {
          bufs.push(buf);
          st.e = 0;
        }
        while (!st.l) {
          const blk = rzb(dat, st, buf);
          if (!blk) err(5);
          if (buf) st.e = st.y;
          else {
            bufs.push(blk);
            ol += blk.length;
            cpw(st.w, 0, blk.length);
            st.w.set(blk, st.w.length - blk.length);
          }
        }
        bt = st.b + st.c * 4;
      } else bt = st;
      dat = dat.subarray(bt);
    }
    return cct(bufs, ol);
  };
})();

//Provides: caml_decompress_input
//Version: < 5.1.0
const caml_decompress_input = null;

//Provides: caml_decompress_input
//Version: >= 5.1.0
//Version: < 5.1.1
//Requires: zstd_decompress
const caml_decompress_input = zstd_decompress;

//Provides: caml_decompress_input
//Version: >= 5.1.1
//Version: < 5.2.0
const caml_decompress_input = null;

//Provides: caml_decompress_input
//Version: >= 5.2
//Requires: zstd_decompress
let caml_decompress_input = zstd_decompress;

//Provides: caml_zstd_initialize
//Requires: caml_decompress_input
//Requires: zstd_decompress
//Version: >= 5.1.1
function caml_zstd_initialize(unit) {
  caml_decompress_input = zstd_decompress;
  return 1;
}
