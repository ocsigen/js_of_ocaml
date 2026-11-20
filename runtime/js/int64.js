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

//Provides: caml_int64_offset
var caml_int64_offset = Math.pow(2, -24);

//Provides: MlInt64
//Requires: caml_int64_offset, caml_raise_zero_divide
class MlInt64 {
  constructor(lo, mi, hi) {
    this.lo = lo & 0xffffff;
    this.mi = mi & 0xffffff;
    this.hi = hi & 0xffff;
    this.caml_custom = "_j";
  }

  static UNSIGNED_MAX = new MlInt64(0xffffff, 0xffffff, 0xffff);
  static SIGNED_MAX = new MlInt64(0xffffff, 0xffffff, 0x7fff);
  static SIGNED_MIN = new MlInt64(0x000000, 0x000000, 0x8000);
  static ONE = new MlInt64(1, 0, 0);

  slice() {
    return new MlInt64(this.lo, this.mi, this.hi);
  }

  ucompare(x) {
    if (this.hi > x.hi) return 1;
    if (this.hi < x.hi) return -1;
    if (this.mi > x.mi) return 1;
    if (this.mi < x.mi) return -1;
    if (this.lo > x.lo) return 1;
    if (this.lo < x.lo) return -1;
    return 0;
  }

  equal(x) {
    return this.lo === x.lo && this.mi === x.mi && this.hi === x.hi;
  }

  compare(x) {
    var hi = this.hi << 16;
    var xhi = x.hi << 16;
    if (hi > xhi) return 1;
    if (hi < xhi) return -1;
    if (this.mi > x.mi) return 1;
    if (this.mi < x.mi) return -1;
    if (this.lo > x.lo) return 1;
    if (this.lo < x.lo) return -1;
    return 0;
  }

  neg() {
    var lo = -this.lo;
    var mi = -this.mi + (lo >> 24);
    var hi = -this.hi + (mi >> 24);
    return new MlInt64(lo, mi, hi);
  }

  add(x) {
    var lo = this.lo + x.lo;
    var mi = this.mi + x.mi + (lo >> 24);
    var hi = this.hi + x.hi + (mi >> 24);
    return new MlInt64(lo, mi, hi);
  }

  sub(x) {
    var lo = this.lo - x.lo;
    var mi = this.mi - x.mi + (lo >> 24);
    var hi = this.hi - x.hi + (mi >> 24);
    return new MlInt64(lo, mi, hi);
  }

  mul(x) {
    var lo = this.lo * x.lo;
    var mi = ((lo * caml_int64_offset) | 0) + this.mi * x.lo + this.lo * x.mi;
    var hi =
      ((mi * caml_int64_offset) | 0) +
      this.hi * x.lo +
      this.mi * x.mi +
      this.lo * x.hi;
    return new MlInt64(lo, mi, hi);
  }

  isZero() {
    return (this.lo | this.mi | this.hi) === 0;
  }

  isNeg() {
    return this.hi << 16 < 0;
  }

  and(x) {
    return new MlInt64(this.lo & x.lo, this.mi & x.mi, this.hi & x.hi);
  }

  or(x) {
    return new MlInt64(this.lo | x.lo, this.mi | x.mi, this.hi | x.hi);
  }

  xor(x) {
    return new MlInt64(this.lo ^ x.lo, this.mi ^ x.mi, this.hi ^ x.hi);
  }

  shift_left(s) {
    s = s & 63;
    if (s === 0) return this;
    if (s < 24) {
      return new MlInt64(
        this.lo << s,
        (this.mi << s) | (this.lo >> (24 - s)),
        (this.hi << s) | (this.mi >> (24 - s)),
      );
    }
    if (s < 48)
      return new MlInt64(
        0,
        this.lo << (s - 24),
        (this.mi << (s - 24)) | (this.lo >> (48 - s)),
      );
    return new MlInt64(0, 0, this.lo << (s - 48));
  }

  shift_right_unsigned(s) {
    s = s & 63;
    if (s === 0) return this;
    if (s < 24)
      return new MlInt64(
        (this.lo >> s) | (this.mi << (24 - s)),
        (this.mi >> s) | (this.hi << (24 - s)),
        this.hi >> s,
      );
    if (s < 48)
      return new MlInt64(
        (this.mi >> (s - 24)) | (this.hi << (48 - s)),
        this.hi >> (s - 24),
        0,
      );
    return new MlInt64(this.hi >> (s - 48), 0, 0);
  }

  shift_right(s) {
    s = s & 63;
    if (s === 0) return this;
    var h = (this.hi << 16) >> 16;
    if (s < 24)
      return new MlInt64(
        (this.lo >> s) | (this.mi << (24 - s)),
        (this.mi >> s) | (h << (24 - s)),
        ((this.hi << 16) >> s) >>> 16,
      );
    var sign = (this.hi << 16) >> 31;
    if (s < 48)
      return new MlInt64(
        // [h] is sign-extended: for s > 40 the sign bits reach
        // into the low limb
        (this.mi >> (s - 24)) | (h << (48 - s)),
        ((this.hi << 16) >> (s - 24)) >> 16,
        sign & 0xffff,
      );
    return new MlInt64((this.hi << 16) >> (s - 32), sign, sign);
  }

  // The unsigned value, rounded to a float
  toUnsignedFloat() {
    return this.hi * 0x1000000000000 + this.mi * 0x1000000 + this.lo;
  }

  // Unsigned division by a nonzero divisor
  udivmod(x) {
    var quotient, modulus;
    if (x.hi === 0 && x.mi < 0x20) {
      // Divisor d below 2^29: divide one limb at a time. Each partial
      // dividend r * 2^24 + limb, with r < d, is below 2^53, so the float
      // operations are exact (Math.floor(a / d) is the integer quotient
      // for integers a < 2^53), and each quotient limb is below 2^24.
      var d = x.mi * 0x1000000 + x.lo;
      var qh = Math.floor(this.hi / d);
      var r = (this.hi - qh * d) * 0x1000000 + this.mi;
      var qm = Math.floor(r / d);
      r = (r - qm * d) * 0x1000000 + this.lo;
      var ql = Math.floor(r / d);
      r -= ql * d;
      quotient = new MlInt64(ql, qm, qh);
      modulus = new MlInt64(r, r / 0x1000000, 0);
    } else {
      // The quotient is below 2^35, so the floor of the float quotient
      // (three roundings, with an absolute error below 2^-16) is off by at
      // most one in either direction. Subtract one from it, so that it is
      // never too large and the remainder cannot be negative, then
      // subtract the divisor from the remainder at most twice,
      // incrementing the quotient each time.
      var q = Math.floor(this.toUnsignedFloat() / x.toUnsignedFloat());
      if (q > 0) q--;
      quotient = new MlInt64(q, q / 0x1000000, 0);
      modulus = this.sub(quotient.mul(x));
      while (modulus.ucompare(x) >= 0) {
        modulus = modulus.sub(x);
        quotient = quotient.add(MlInt64.ONE);
      }
    }
    return { quotient: quotient, modulus: modulus };
  }

  div(y) {
    var x = this;
    if (y.isZero()) caml_raise_zero_divide();
    var sign = x.hi ^ y.hi;
    if (x.hi & 0x8000) x = x.neg();
    if (y.hi & 0x8000) y = y.neg();
    var q = x.udivmod(y).quotient;
    if (sign & 0x8000) q = q.neg();
    return q;
  }

  mod(y) {
    var x = this;
    if (y.isZero()) caml_raise_zero_divide();
    var sign = x.hi;
    if (x.hi & 0x8000) x = x.neg();
    if (y.hi & 0x8000) y = y.neg();
    var r = x.udivmod(y).modulus;
    if (sign & 0x8000) r = r.neg();
    return r;
  }

  toInt() {
    return this.lo | (this.mi << 24);
  }

  toFloat() {
    return (
      (this.hi << 16) * Math.pow(2, 32) + this.mi * Math.pow(2, 24) + this.lo
    );
  }

  toArray() {
    return [
      this.hi >> 8,
      this.hi & 0xff,
      this.mi >> 16,
      (this.mi >> 8) & 0xff,
      this.mi & 0xff,
      this.lo >> 16,
      (this.lo >> 8) & 0xff,
      this.lo & 0xff,
    ];
  }

  lo32() {
    return this.lo | ((this.mi & 0xff) << 24);
  }

  hi32() {
    return ((this.mi >>> 8) & 0xffff) | (this.hi << 16);
  }
}

//Provides: caml_int64_ult const
function caml_int64_ult(x, y) {
  return x.ucompare(y) < 0;
}

//Provides: caml_int64_equal const
//Version: >= 5.6
// The comparison operators on int64 values (used with OCaml 5.6 bytecode
// hints, which tell that the arguments of caml_equal, ... are int64)
function caml_int64_equal(x, y) {
  return +x.equal(y);
}

//Provides: caml_int64_notequal const
//Version: >= 5.6
function caml_int64_notequal(x, y) {
  return +!x.equal(y);
}

//Provides: caml_int64_lessthan const
//Version: >= 5.6
function caml_int64_lessthan(x, y) {
  return +(x.compare(y) < 0);
}

//Provides: caml_int64_lessequal const
//Version: >= 5.6
function caml_int64_lessequal(x, y) {
  return +(x.compare(y) <= 0);
}

//Provides: caml_int64_compare const
function caml_int64_compare(x, y, _total) {
  return x.compare(y);
}

//Provides: caml_int64_neg const
function caml_int64_neg(x) {
  return x.neg();
}

//Provides: caml_int64_add const
function caml_int64_add(x, y) {
  return x.add(y);
}

//Provides: caml_int64_sub const
function caml_int64_sub(x, y) {
  return x.sub(y);
}

//Provides: caml_int64_mul const
//Requires: caml_int64_offset
function caml_int64_mul(x, y) {
  return x.mul(y);
}

//Provides: caml_int64_is_zero const
function caml_int64_is_zero(x) {
  return +x.isZero();
}

//Provides: caml_int64_is_negative const
function caml_int64_is_negative(x) {
  return +x.isNeg();
}

//Provides: caml_int64_and const
function caml_int64_and(x, y) {
  return x.and(y);
}

//Provides: caml_int64_or const
function caml_int64_or(x, y) {
  return x.or(y);
}

//Provides: caml_int64_xor const
function caml_int64_xor(x, y) {
  return x.xor(y);
}

//Provides: caml_int64_shift_left const
function caml_int64_shift_left(x, s) {
  return x.shift_left(s);
}

//Provides: caml_int64_shift_right_unsigned const
function caml_int64_shift_right_unsigned(x, s) {
  return x.shift_right_unsigned(s);
}

//Provides: caml_int64_shift_right const
function caml_int64_shift_right(x, s) {
  return x.shift_right(s);
}

//Provides: caml_int64_div
function caml_int64_div(x, y) {
  return x.div(y);
}

//Provides: caml_int64_mod
function caml_int64_mod(x, y) {
  return x.mod(y);
}

//Provides: caml_int64_of_int32 const
//Requires: MlInt64
//Alias: caml_int64_of_int
//Alias: caml_int64_of_nativeint
function caml_int64_of_int32(x) {
  return new MlInt64(x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff);
}

//Provides: caml_int64_to_int32 const
//Alias: caml_int64_to_int
//Alias: caml_int64_to_nativeint
function caml_int64_to_int32(x) {
  return x.toInt();
}

//Provides: caml_checked_int64_to_int
//Requires: caml_int64_of_int32, caml_array_bound_error
//Version: >= 5.2
//If: oxcaml
function caml_checked_int64_to_int(x) {
  var y = x.toInt();
  if (x.compare(caml_int64_of_int32(y)) !== 0) caml_array_bound_error();
  return y;
}

//Provides: caml_array_unsafe_get_indexed_by_int64 mutable (mutable, const)
//Version: >= 5.2
//If: oxcaml
function caml_array_unsafe_get_indexed_by_int64(array, index) {
  return array[index.toInt() + 1];
}

//Provides: caml_array_unsafe_set_indexed_by_int64 (mutable, const, mutable)
//Version: >= 5.2
//If: oxcaml
function caml_array_unsafe_set_indexed_by_int64(array, index, newval) {
  array[index.toInt() + 1] = newval;
  return 0;
}

//Provides: caml_int64_to_float const
function caml_int64_to_float(x) {
  return x.toFloat();
}

//Provides: caml_int64_of_float const
//Requires: caml_int64_offset, MlInt64
function caml_int64_of_float(x) {
  if (x < 0) x = Math.ceil(x);
  return new MlInt64(
    x & 0xffffff,
    Math.floor(x * caml_int64_offset) & 0xffffff,
    Math.floor(x * caml_int64_offset * caml_int64_offset) & 0xffff,
  );
}

//Provides: caml_int64_unsigned_digits const
//Requires: MlInt64, caml_str_repeat
// The digits of [x], seen as unsigned, in base [base]
function caml_int64_unsigned_digits(x, base) {
  // Number.prototype.toString is exact on integers below 2^53
  if (x.hi < 0x20) return x.toUnsignedFloat().toString(base);
  // Otherwise, x = q * base^n + r, with base^n below 2^29, so that q is
  // below 2^53
  var n = 1;
  var k = base;
  while (k * base < 0x20000000) {
    k *= base;
    n++;
  }
  var p = x.udivmod(new MlInt64(k, k / 0x1000000, 0));
  var r = p.modulus.toUnsignedFloat().toString(base);
  return (
    p.quotient.toUnsignedFloat().toString(base) +
    caml_str_repeat(n - r.length, "0") +
    r
  );
}

//Provides: caml_int64_format const
//Requires: caml_parse_format, caml_finish_formatting
//Requires: caml_int64_unsigned_digits, caml_str_repeat
//Requires: caml_jsbytes_of_string, caml_string_of_jsbytes
function caml_int64_format(fmt, x) {
  if (caml_jsbytes_of_string(fmt) === "%d")
    // Int64.to_string
    return caml_string_of_jsbytes(
      x.isNeg()
        ? "-" + caml_int64_unsigned_digits(x.neg(), 10)
        : caml_int64_unsigned_digits(x, 10),
    );
  var f = caml_parse_format(fmt);
  if (f.signedconv && x.isNeg()) {
    f.sign = -1;
    x = x.neg();
  }
  var buffer = caml_int64_unsigned_digits(x, f.base);
  if (f.prec >= 0) {
    f.filler = " ";
    var n = f.prec - buffer.length;
    if (n > 0) buffer = caml_str_repeat(n, "0") + buffer;
  }
  return caml_finish_formatting(f, buffer);
}

//Provides: caml_int64_of_string
//Requires: caml_parse_sign_and_base, caml_failwith, caml_parse_digit
//Requires: caml_int64_of_int32, caml_int64_ult
//Requires: caml_int64_add, caml_int64_mul, caml_int64_neg
//Requires: caml_ml_string_length,caml_string_unsafe_get, MlInt64
function caml_int64_of_string(s) {
  var r = caml_parse_sign_and_base(s);
  var i = r[0],
    sign = r[1],
    base = r[2],
    signedness = r[3];
  var c = caml_string_unsafe_get(s, i);
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("Int64.of_string");
  // Accumulate the digits in a float as long as the value stays below
  // 2^53, so that the computation is exact
  var limit = Math.floor(0x20000000000000 / base) - 1;
  var acc = d;
  for (;;) {
    i++;
    c = caml_string_unsafe_get(s, i);
    if (c === 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base || acc > limit) break;
    acc = acc * base + d;
  }
  var res = new MlInt64(acc, acc / 0x1000000, acc / 0x1000000000000);
  if (d >= 0 && d < base) {
    // Remaining digits, starting with [d]
    var base64 = caml_int64_of_int32(base);
    var threshold = MlInt64.UNSIGNED_MAX.udivmod(base64).quotient;
    for (;;) {
      /* Detect overflow in multiplication base * res */
      if (caml_int64_ult(threshold, res)) caml_failwith("Int64.of_string");
      d = caml_int64_of_int32(d);
      res = caml_int64_add(caml_int64_mul(base64, res), d);
      /* Detect overflow in addition (base * res) + d */
      if (caml_int64_ult(res, d)) caml_failwith("Int64.of_string");
      do {
        i++;
        c = caml_string_unsafe_get(s, i);
      } while (c === 95);
      d = caml_parse_digit(c);
      if (d < 0 || d >= base) break;
    }
  }
  if (i !== caml_ml_string_length(s)) caml_failwith("Int64.of_string");
  if (
    signedness &&
    caml_int64_ult(sign < 0 ? MlInt64.SIGNED_MIN : MlInt64.SIGNED_MAX, res)
  )
    caml_failwith("Int64.of_string");
  if (sign < 0) res = caml_int64_neg(res);
  return res;
}

//Provides: caml_int64_create_lo_mi_hi const
//Requires: MlInt64
function caml_int64_create_lo_mi_hi(lo, mi, hi) {
  return new MlInt64(lo, mi, hi);
}
//Provides: caml_int64_create_lo_hi const
//Requires: MlInt64
function caml_int64_create_lo_hi(lo, hi) {
  return new MlInt64(
    lo & 0xffffff,
    ((lo >>> 24) & 0xff) | ((hi & 0xffff) << 8),
    (hi >>> 16) & 0xffff,
  );
}
//Provides: caml_int64_lo32 const
function caml_int64_lo32(v) {
  return v.lo32();
}

//Provides: caml_int64_hi32 const
function caml_int64_hi32(v) {
  return v.hi32();
}

//Provides: caml_int64_of_bytes const
//Requires: MlInt64
function caml_int64_of_bytes(a) {
  return new MlInt64(
    (a[7] << 0) | (a[6] << 8) | (a[5] << 16),
    (a[4] << 0) | (a[3] << 8) | (a[2] << 16),
    (a[1] << 0) | (a[0] << 8),
  );
}
//Provides: caml_int64_to_bytes const
function caml_int64_to_bytes(x) {
  return x.toArray();
}

//Provides: caml_int64_hash const
function caml_int64_hash(v) {
  return v.lo32() ^ v.hi32();
}

//Provides: caml_reinterpret_unboxed_int64_as_tagged_int63
//Requires: caml_failwith
//Version: >= 5.2
//If: oxcaml
function caml_reinterpret_unboxed_int64_as_tagged_int63(_v) {
  caml_failwith(
    "caml_reinterpret_unboxed_int64_as_tagged_int63 is not supported in javascript.",
  );
}
