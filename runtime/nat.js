// Requires that Sys.word_size == 32, which it is for js_of_ocaml
// This issue is not avoidable without rolling a custom nums.cma
"use strict";

//Provides: initialize_nat
function initialize_nat() {
	return undefined;
}

//Provides: create_nat
function create_nat(size) {
	// length_nat isn't external, so we have to make the Obj.size
	// work out right. The +2 to array length seems to work.
	var arr = [-1, -1];
	for(var i = 2; i < size+2; i++) {
		arr[i] = -1;
	}
	return arr;
}

//Provides: set_to_zero_nat
function set_to_zero_nat(nat, ofs, len) {
	for(var i = 0; i < len; i++) {
		nat[ofs+i] = 0;
	}
	return undefined;
}

//Provides: blit_nat
function blit_nat(nat1, ofs1, nat2, ofs2, len) {
	for(var i = 0; i < len; i++) {
		nat1[ofs1+i] = nat2[ofs2+i];
	}
	return undefined;
}

//Provides: set_digit_nat
function set_digit_nat(nat, ofs, digit) {
	nat[ofs] = digit;
	if(nat[ofs] < 0) nat[ofs] += 4294967296;
	return undefined;
}

//Provides: nth_digit_nat
function nth_digit_nat(nat, ofs) {
	return nat[ofs];
}

//Provides: set_digit_nat_native
function set_digit_nat_native(nat, ofs, digit) {
	nat[ofs] = digit;
	if(nat[ofs] < 0) nat[ofs] += 4294967296;
	return undefined;
}

//Provides: nth_digit_nat_native
function nth_digit_nat_native(nat, ofs) {
	return nat[ofs];
}

//Provides: num_digits_nat
function num_digits_nat(nat, ofs, len) {
	for(var i = len - 1; i >= 0; i--) {
		if(nat[ofs+i] != 0) return i+1;
	}
	return 1; // 0 counts as 1 digit
}

//Provides: num_leading_zero_bits_in_digit
function num_leading_zero_bits_in_digit(nat, ofs) {
	var a = nat[ofs];
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
	return nat[ofs] < 4294967296 / 4;
}

//Provides: is_digit_zero
function is_digit_zero(nat, ofs) {
	return nat[ofs] == 0;
}

//Provides: is_digit_odd
function is_digit_odd(nat, ofs) {
	return nat[ofs] & 1 == 1;
}

//Provides: incr_nat
function incr_nat(nat, ofs, len, carry_in) {
	var carry = carry_in;
	for(var i = 0; i < len; i++) {
		nat[ofs+i] += carry;
		if(nat[ofs+i] < 4294967296) {
			carry = 0;
			break;
		} else {
			nat[ofs+i] -= 4294967296;
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
		nat1[ofs1+i] += nat2[ofs2+i] + carry;
		if(nat1[ofs1+i] < 4294967296) {
			carry = 0;
		} else {
			nat1[ofs1+i] -= 4294967296;
			carry = 1;
		}
	}
	return incr_nat(nat1, ofs1+len2, len1-len2, carry);
}

//Provides: complement_nat
function complement_nat(nat, ofs, len) {
	for(var i = 0; i < len; i++) {
		nat[ofs+i] = 4294967296 - 1 - nat[ofs+i];
	}
}

// ocaml flips carry_in
//Provides: decr_nat
function decr_nat(nat, ofs, len, carry_in) {
	var borrow = (carry_in == 1) ? 0 : 1;
	for(var i = 0; i < len; i++) {
		nat[ofs+i] -= borrow;
		if (nat[ofs+i] >= 0) {
			borrow = 0;
			break;
		} else {
			nat[ofs+i] += 4294967296;
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
		nat1[ofs1+i] -= nat2[ofs2+i] + borrow;
		if (nat1[ofs1+i] >= 0) {
			borrow = 0;
		} else {
			nat1[ofs1+i] += 4294967296;
			borrow = 1;
		}
	}
	return decr_nat(nat1, ofs1+len2, len1-len2, (borrow==1)?0:1);
}

// nat1 += nat2 * nat3[ofs3]
// len1 >= len2
//Provides: mult_digit_nat
//Requires: add_nat
function mult_digit_nat(nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3) {
	var carry = 0;
	var a = nat3[ofs3];
	for(var i = 0; i < len2; i++) {
		nat1[ofs1+i] += nat2[ofs2+i] * (a & 0x0000FFFF) + carry;
		var x = nat2[ofs2+i] * (a >>> 16);
		carry = Math.floor(x/65536);
		nat1[ofs1+i] += (x % 65536) * 65536;
		carry += Math.floor(nat1[ofs1+i]/4294967296);
		nat1[ofs1+i] %= 4294967296;
	}

	if(len2 < len1 && carry) {
		return add_nat(nat1, ofs1+len2, len1-len2, [carry], 0, 1, 0);
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
		nat2[ofs2] = 0;
		return undefined;
	}

	var wrap = 0;
	for(var i = 0; i < len1; i++) {
			var a = nat1[ofs1+i];
			nat1[ofs1+i] = (a << nbits) | wrap;
			if(nat1[ofs1+i] < 0) nat1[ofs1+i] += 4294967296;
			wrap = a >>> (32 - nbits);
	}

	nat2[ofs2] = wrap;
	if(nat2[ofs2] < 0) nat2[ofs2] += 4294967296;
	return undefined;
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
	var rem = nat1[ofs1+len-1];
	// natq[ofsq+len-1] is guaranteed to be zero (due to the MSD requirement),
	// and should not be written to.
	for(var i = len-2; i >= 0; i--) {
			var x = div_helper(rem, nat1[ofs1+i], nat2[ofs2]);
			natq[ofsq+i] = x[0];
			rem = x[1];
	}
	natr[ofsr] = rem;
	return undefined;
}

// nat1[nat2:] := nat1 / nat2
// nat1[:nat2] := nat1 % nat2
// len1 > len2, nat2[ofs2+len2-1] > nat1[ofs1+len1-1]
//Provides: div_nat
//Requires: div_digit_nat, div_helper, num_leading_zero_bits_in_digit, shift_left_nat, shift_right_nat, create_nat, set_to_zero_nat, mult_digit_nat, sub_nat, compare_nat
function div_nat(nat1, ofs1, len1, nat2, ofs2, len2) {
	if(len2 == 1) {
		div_digit_nat(nat1, ofs1+1, nat1, ofs1, nat1, ofs1, len1, nat2, ofs2);
		return undefined;
	}

	var s = num_leading_zero_bits_in_digit(nat2, ofs2+len2-1);
	shift_left_nat(nat2, ofs2, len2, [0], 0, s);
	shift_left_nat(nat1, ofs1, len1, [0], 0, s);

	var d = nat2[ofs2+len2-1] + 1;
	var a = create_nat(len2+1);
	for (var i = len1 - 1; i >= len2; i--) {
		// Decent lower bound on quo
		var quo = d == 4294967296 ? nat1[ofs1+i] : div_helper(nat1[ofs1+i], nat1[ofs1+i-1], d)[0];
		set_to_zero_nat(a, 0, len2+1);
		mult_digit_nat(a, 0, len2+1, nat2, ofs2, len2, [quo], 0);
		sub_nat(nat1, ofs1+i-len2, len2+1, a, 0, len2+1, 1);

		while (nat1[ofs1+i] != 0 || compare_nat(nat1, ofs1+i-len2, len2, nat2, ofs2, len2) >= 0) {
			quo = quo + 1;
			sub_nat(nat1, ofs1+i-len2, len2+1, nat2, ofs2, len2, 1);
		}

		nat1[ofs1+i] = quo;
	}

	shift_right_nat(nat1, ofs1, len2, [0], 0, s); // shift remainder
	shift_right_nat(nat2, ofs2, len2, [0], 0, s); // restore
	return undefined;
}


// 0 <= shift < 32
//Provides: shift_right_nat
function shift_right_nat(nat1, ofs1, len1, nat2, ofs2, nbits) {
	if(nbits == 0) {
		nat2[ofs2] = 0;
		return undefined;
	}

	var wrap = 0;
	for(var i = len1-1; i >= 0; i--) {
			var a = nat1[ofs1+i];
			nat1[ofs1+i] = (a >>> nbits) | wrap;
			if(nat1[ofs1+i] < 0) nat1[ofs1+i] += 4294967296;
			wrap = a << (32 - nbits);
	}

	nat2[ofs2] = wrap;
	if(nat2[ofs2] < 0) nat2[ofs2] += 4294967296;
	return undefined;
}

//Provides: compare_digits_nat
function compare_digits_nat(nat1, ofs1, nat2, ofs2) {
	if(nat1[ofs1] > nat2[ofs2]) return 1;
	if(nat1[ofs1] < nat2[ofs2]) return -1;
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
		if (nat1[ofs1+i] > nat2[ofs2+i]) return 1;
		if (nat1[ofs1+i] < nat2[ofs2+i]) return -1;
	}
	return 0;
}

//Provides: land_digit_nat
function land_digit_nat(nat1, ofs1, nat2, ofs2) {
	nat1[ofs1] &= nat2[ofs2];
	if(nat1[ofs1] < 0) nat1[ofs1] += 4294967296;
	return undefined;
}

//Provides: lor_digit_nat
function lor_digit_nat(nat1, ofs1, nat2, ofs2) {
	nat1[ofs1] |= nat2[ofs2];
	if(nat1[ofs1] < 0) nat1[ofs1] += 4294967296;
	return undefined;
}

//Provides: lxor_digit_nat
function lxor_digit_nat(nat1, ofs1, nat2, ofs2) {
	nat1[ofs1] ^= nat2[ofs2];
	if(nat1[ofs1] < 0) nat1[ofs1] += 4294967296;
	return undefined;
}
