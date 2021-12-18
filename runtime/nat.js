//Provides: initialize_nat
//Requires: caml_custom_ops
//Requires: serialize_nat, deserialize_nat, caml_hash_nat
function initialize_nat() {
  caml_custom_ops["_nat"] =
    { deserialize : deserialize_nat,
      serialize : serialize_nat,
      hash : caml_hash_nat
    }
}

//Provides: MlNat
function MlNat(x){
  this.data = new globalThis.Int32Array(x);
  // length_nat isn't external, so we have to make the Obj.size
  // work out right. The +2 to array length seems to work.
  this.length = this.data.length + 2
}

MlNat.prototype.caml_custom = "_nat";

//Provides: caml_hash_nat
//Requires: caml_hash_mix_int, num_digits_nat
function caml_hash_nat(x) {
  var len = num_digits_nat(x, 0, x.data.length);
  var h = 0;
  for (var i = 0; i < len; i++) {
    h = caml_hash_mix_int(h, x.data[i]);
  }
  return h;
}


//Provides: nat_of_array
//Requires: MlNat
function nat_of_array(l){
  return new MlNat(l);
}

//Provides: create_nat
//Requires: MlNat
function create_nat(size) {
  var arr = new MlNat(size);
  for(var i = 0; i < size; i++) {
    arr.data[i] = -1;
  }
  return arr;
}

//Provides: set_to_zero_nat
function set_to_zero_nat(nat, ofs, len) {
  for(var i = 0; i < len; i++) {
    nat.data[ofs+i] = 0;
  }
  return 0;
}

//Provides: blit_nat
function blit_nat(nat1, ofs1, nat2, ofs2, len) {
  for(var i = 0; i < len; i++) {
    nat1.data[ofs1+i] = nat2.data[ofs2+i];
  }
  return 0;
}

//Provides: set_digit_nat
function set_digit_nat(nat, ofs, digit) {
  nat.data[ofs] = digit;
  return 0;
}

//Provides: nth_digit_nat
function nth_digit_nat(nat, ofs) {
  return nat.data[ofs];
}

//Provides: set_digit_nat_native
function set_digit_nat_native(nat, ofs, digit) {
  nat.data[ofs] = digit;
  return 0;
}

//Provides: nth_digit_nat_native
function nth_digit_nat_native(nat, ofs) {
  return nat.data[ofs];
}

//Provides: num_digits_nat
function num_digits_nat(nat, ofs, len) {
  for(var i = len - 1; i >= 0; i--) {
    if(nat.data[ofs+i] != 0) return i+1;
  }
  return 1; // 0 counts as 1 digit
}

//Provides: num_leading_zero_bits_in_digit
function num_leading_zero_bits_in_digit(nat, ofs) {
  var a = nat.data[ofs];
  var b = 0;
  if(a & 0xFFFF0000) { b +=16; a >>>=16; }
  if(a & 0xFF00)     { b += 8; a >>>= 8; }
  if(a & 0xF0)       { b += 4; a >>>= 4; }
  if(a & 12)         { b += 2; a >>>= 2; }
  if(a & 2)          { b += 1; a >>>= 1; }
  if(a & 1)          { b += 1; }
  return 32 - b;
}

//Provides: is_digit_int
function is_digit_int(nat, ofs) {
  if (nat.data[ofs] >= 0) return 1
  return 0;
}

//Provides: is_digit_zero
function is_digit_zero(nat, ofs) {
  if(nat.data[ofs] == 0) return 1;
  return 0;
}

//Provides: is_digit_odd
function is_digit_odd(nat, ofs) {
  if(nat.data[ofs] & 1) return 1;
  return 0;
}

//Provides: incr_nat
function incr_nat(nat, ofs, len, carry_in) {
  var carry = carry_in;
  for(var i = 0; i < len; i++) {
    var x = (nat.data[ofs+i] >>> 0) + carry;
    nat.data[ofs+i] = (x | 0);
    if(x == (x >>> 0)) {
      carry = 0;
      break;
    } else {
      carry = 1;
    }
  }
  return carry;
}

// len1 >= len2
//Provides: add_nat
//Requires: incr_nat
function add_nat(nat1, ofs1, len1, nat2, ofs2, len2, carry_in) {
  var carry = carry_in;
  for(var i = 0; i < len2; i++) {
    var x = (nat1.data[ofs1+i] >>> 0) + (nat2.data[ofs2+i] >>> 0) + carry;
    nat1.data[ofs1+i] = x
    if(x == (x >>> 0)) {
      carry = 0;
    } else {
      carry = 1;
    }
  }
  return incr_nat(nat1, ofs1+len2, len1-len2, carry);
}

//Provides: complement_nat
function complement_nat(nat, ofs, len) {
  for(var i = 0; i < len; i++) {
    nat.data[ofs+i] = (-1 >>> 0) - (nat.data[ofs+i] >>> 0);
  }
}

// ocaml flips carry_in
//Provides: decr_nat
function decr_nat(nat, ofs, len, carry_in) {
  var borrow = (carry_in == 1) ? 0 : 1;
  for(var i = 0; i < len; i++) {
    var x = (nat.data[ofs+i] >>>0) - borrow;
    nat.data[ofs+i] = x;
    if (x >= 0) {
      borrow = 0;
      break;
    } else {
      borrow = 1;
    }
  }
  return (borrow == 1) ? 0 : 1;
}

// ocaml flips carry_in
// len1 >= len2
//Provides: sub_nat
//Requires: decr_nat
function sub_nat(nat1, ofs1, len1, nat2, ofs2, len2, carry_in) {
  var borrow = (carry_in == 1) ? 0 : 1;
  for(var i = 0; i < len2; i++) {
    var x = (nat1.data[ofs1+i] >>> 0) - (nat2.data[ofs2+i] >>> 0) - borrow;
    nat1.data[ofs1+i] = x;
    if (x >= 0) {
      borrow = 0;
    } else {
      borrow = 1;
    }
  }
  return decr_nat(nat1, ofs1+len2, len1-len2, (borrow==1)?0:1);
}

// nat1 += nat2 * nat3[ofs3]
// len1 >= len2
//Provides: mult_digit_nat
//Requires: add_nat, nat_of_array
function mult_digit_nat(nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3) {
  var carry = 0;
  var a = (nat3.data[ofs3] >>> 0);
  for(var i = 0; i < len2; i++) {
    var x1 = (nat1.data[ofs1+i] >>> 0) + (nat2.data[ofs2+i] >>> 0) * (a & 0x0000FFFF) + carry;
    var x2 = (nat2.data[ofs2+i] >>> 0) * (a >>> 16);
    carry = Math.floor(x2/65536);
    var x3 = x1 + (x2 % 65536) * 65536;
    nat1.data[ofs1+i] = x3;
    carry += Math.floor(x3/4294967296);
  }

  if(len2 < len1 && carry) {
    return add_nat(nat1, ofs1+len2, len1-len2, nat_of_array([carry]), 0, 1, 0);
  } else {
    return carry;
  }
}

// nat1 += nat2 * nat3
// len1 >= len2 + len3.
//Provides: mult_nat
//Requires: mult_digit_nat
function mult_nat(nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3, len3) {
  var carry = 0;
  for(var i = 0; i < len3; i++) {
    carry += mult_digit_nat(nat1, ofs1+i, len1-i, nat2, ofs2, len2, nat3, ofs3+i);
  }
  return carry;
}

// nat1 = 2 * nat1 + nat2 * nat2
// len1 >= 2 * len2
//Provides: square_nat
//Requires: mult_nat, add_nat
function square_nat(nat1, ofs1, len1, nat2, ofs2, len2) {
  var carry = 0;
  carry += add_nat(nat1, ofs1, len1, nat1, ofs1, len1, 0);
  carry += mult_nat(nat1, ofs1, len1, nat2, ofs2, len2, nat2, ofs2, len2);
  return carry;
}


// 0 <= shift < 32
//Provides: shift_left_nat
function shift_left_nat(nat1, ofs1, len1, nat2, ofs2, nbits) {
  if(nbits == 0) {
    nat2.data[ofs2] = 0;
    return 0;
  }
  var wrap = 0;
  for(var i = 0; i < len1; i++) {
    var a = (nat1.data[ofs1+i] >>> 0);
    nat1.data[ofs1+i] = (a << nbits) | wrap;
    wrap = a >>> (32 - nbits);
  }
  nat2.data[ofs2] = wrap;
  return 0;
}

// Assuming c > a, returns [quotient, remainder] of (a<<32 + b)/c
//Provides: div_helper
function div_helper(a, b, c) {
  var x = a * 65536 + (b>>>16);
  var y = Math.floor(x/c) * 65536;
  var z = (x % c) * 65536;
  var w = z + (b & 0x0000FFFF);
  return [y + Math.floor(w/c), w % c];
}

// nat1[ofs1+len] < nat2[ofs2]
//Provides: div_digit_nat
//Requires: div_helper
function div_digit_nat(natq, ofsq, natr, ofsr, nat1, ofs1, len, nat2, ofs2) {
  var rem = (nat1.data[ofs1+len-1] >>>0);
  // natq[ofsq+len-1] is guaranteed to be zero (due to the MSD requirement),
  // and should not be written to.
  for(var i = len-2; i >= 0; i--) {
    var x = div_helper(rem, (nat1.data[ofs1+i] >>> 0), (nat2.data[ofs2] >>> 0));
    natq.data[ofsq+i] = x[0];
    rem = x[1];
  }
  natr.data[ofsr] = rem;
  return 0;
}

// nat1[nat2:] := nat1 / nat2
// nat1[:nat2] := nat1 % nat2
// len1 > len2, nat2[ofs2+len2-1] > nat1[ofs1+len1-1]
//Provides: div_nat
//Requires: div_digit_nat, div_helper, num_leading_zero_bits_in_digit, shift_left_nat, shift_right_nat, create_nat, set_to_zero_nat, mult_digit_nat, sub_nat, compare_nat, nat_of_array
function div_nat(nat1, ofs1, len1, nat2, ofs2, len2) {
  if(len2 == 1) {
    div_digit_nat(nat1, ofs1+1, nat1, ofs1, nat1, ofs1, len1, nat2, ofs2);
    return 0;
  }

  var s = num_leading_zero_bits_in_digit(nat2, ofs2+len2-1);
  shift_left_nat(nat2, ofs2, len2, nat_of_array([0]), 0, s);
  shift_left_nat(nat1, ofs1, len1, nat_of_array([0]), 0, s);

  var d = (nat2.data[ofs2+len2-1] >>> 0) + 1;
  var a = create_nat(len2+1);
  for (var i = len1 - 1; i >= len2; i--) {
    // Decent lower bound on quo
    var quo = d == 4294967296 ? (nat1.data[ofs1+i] >>> 0) : div_helper((nat1.data[ofs1+i] >>> 0), (nat1.data[ofs1+i-1] >>>0), d)[0];
    set_to_zero_nat(a, 0, len2+1);
    mult_digit_nat(a, 0, len2+1, nat2, ofs2, len2, nat_of_array([quo]), 0);
    sub_nat(nat1, ofs1+i-len2, len2+1, a, 0, len2+1, 1);

    while (nat1.data[ofs1+i] != 0 || compare_nat(nat1, ofs1+i-len2, len2, nat2, ofs2, len2) >= 0) {
      quo = quo + 1;
      sub_nat(nat1, ofs1+i-len2, len2+1, nat2, ofs2, len2, 1);
    }

    nat1.data[ofs1+i] = quo;
  }

  shift_right_nat(nat1, ofs1, len2, nat_of_array([0]), 0, s); // shift remainder
  shift_right_nat(nat2, ofs2, len2, nat_of_array([0]), 0, s); // restore
  return 0;
}


// 0 <= shift < 32
//Provides: shift_right_nat
function shift_right_nat(nat1, ofs1, len1, nat2, ofs2, nbits) {
  if(nbits == 0) {
    nat2.data[ofs2] = 0;
    return 0;
  }
  var wrap = 0;
  for(var i = len1-1; i >= 0; i--) {
    var a = nat1.data[ofs1+i] >>> 0;
    nat1.data[ofs1+i] = (a >>> nbits) | wrap;
    wrap = a << (32 - nbits);
  }
  nat2.data[ofs2] = wrap;
  return 0;
}

//Provides: compare_digits_nat
function compare_digits_nat(nat1, ofs1, nat2, ofs2) {
  if(nat1.data[ofs1] > nat2.data[ofs2]) return 1;
  if(nat1.data[ofs1] < nat2.data[ofs2]) return -1;
  return 0;
}

//Provides: compare_nat
//Requires: num_digits_nat
function compare_nat(nat1, ofs1, len1, nat2, ofs2, len2) {
  var a = num_digits_nat(nat1, ofs1, len1);
  var b = num_digits_nat(nat2, ofs2, len2);
  if(a > b) return 1;
  if(a < b) return -1;
  for(var i = len1 - 1; i >= 0; i--) {
    if ((nat1.data[ofs1+i] >>> 0) > (nat2.data[ofs2+i] >>> 0)) return 1;
    if ((nat1.data[ofs1+i] >>> 0) < (nat2.data[ofs2+i] >>> 0)) return -1;
  }
  return 0;
}

//Provides: compare_nat_real
//Requires: compare_nat
function compare_nat_real(nat1,nat2){
  return compare_nat(nat1,0,nat1.data.length,nat2,0,nat2.data.length);
}

//Provides: land_digit_nat
function land_digit_nat(nat1, ofs1, nat2, ofs2) {
  nat1.data[ofs1] &= nat2.data[ofs2];
  return 0;
}

//Provides: lor_digit_nat
function lor_digit_nat(nat1, ofs1, nat2, ofs2) {
  nat1.data[ofs1] |= nat2.data[ofs2];
  return 0;
}

//Provides: lxor_digit_nat
function lxor_digit_nat(nat1, ofs1, nat2, ofs2) {
  nat1.data[ofs1] ^= nat2.data[ofs2];
  return 0;
}


//Provides: serialize_nat
function serialize_nat(writer, nat, sz){
  var len = nat.data.length;
  writer.write(32, len);
  for(var i = 0; i < len; i++){
    writer.write(32, nat.data[i]);
  }
  sz[0] = len * 4;
  sz[1] = len * 8;
}

//Provides: deserialize_nat
//Requires: MlNat
function deserialize_nat(reader, sz){
  var len = reader.read32s();
  var nat = new MlNat(len);
  for(var i = 0; i < len; i++){
    nat.data[i] = reader.read32s();
  }
  sz[0] = len * 4;
  return nat;
}
