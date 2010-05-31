/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

function md5 (message) {
    
    function add (x, y) {
	var lsw = (x & 0xFFFF) + (y & 0xFFFF);
	var msw = (x >>> 16) + (y >>> 16) + (lsw >>> 16);
	return (msw << 16) | (lsw & 0xFFFF);
    }
    
    function rol (x, y){
	return (x << y) | (x >>> (32 - y));
    }
    
    function xx(q,a,b,x,s,ac) {
        return add(rol(add(add(a, q), add(x, ac)), s),b);
    }
    function ff(a,b,c,d,x,s,ac) {
        return xx((b & c) | ((~b) & d), a, b, x, s, ac);
    }
    function gg(a,b,c,d,x,s,ac) {
        return xx((b & d) | (c & (~d)), a, b, x, s, ac);
    }
    function hh(a,b,c,d,x,s,ac) {
        return xx(b ^ c ^ d, a, b, x, s, ac);
    }
    function ii(a,b,c,d,x,s,ac) {
        return xx(c ^ (b | (~d)), a, b, x, s, ac);
    }

    var buffer = [];
    for(var i = 0; i < message.length;i++)
	buffer[i >> 2] |= message[i] << (8 * (i & 3));
    buffer[i >> 2] |= 0x80 << (8 * (i & 3));
    for (i = (i & ~0x3) + 4;(i & 0x3F) < 56 ;i += 4)
	buffer[i >> 2] = 0;
    buffer[i >> 2] = message.length << 3;
    i += 4;
    buffer[i >> 2] = (message.length >> 29) & 0x1FFFFFFF;

    var a = 0x67452301, b = 0xEFCDAB89, c = 0x98BADCFE, d = 0x10325476;
    
    for(i = 0; i < buffer.length; i += 16) {
	var temp_a = a;
	var temp_b = b;
	var temp_c = c;
	var temp_d = d;
	
	a = ff(a, b, c, d, buffer[i+ 0], 7, 0xD76AA478);
	d = ff(d, a, b, c, buffer[i+ 1], 12, 0xE8C7B756);
	c = ff(c, d, a, b, buffer[i+ 2], 17, 0x242070DB);
	b = ff(b, c, d, a, buffer[i+ 3], 22, 0xC1BDCEEE);
	a = ff(a, b, c, d, buffer[i+ 4], 7, 0xF57C0FAF);
	d = ff(d, a, b, c, buffer[i+ 5], 12, 0x4787C62A);
	c = ff(c, d, a, b, buffer[i+ 6], 17, 0xA8304613);
	b = ff(b, c, d, a, buffer[i+ 7], 22, 0xFD469501);
	a = ff(a, b, c, d, buffer[i+ 8], 7, 0x698098D8);
	d = ff(d, a, b, c, buffer[i+ 9], 12, 0x8B44F7AF);
	c = ff(c, d, a, b, buffer[i+10], 17, 0xFFFF5BB1);
	b = ff(b, c, d, a, buffer[i+11], 22, 0x895CD7BE);
	a = ff(a, b, c, d, buffer[i+12], 7, 0x6B901122);
	d = ff(d, a, b, c, buffer[i+13], 12, 0xFD987193);
	c = ff(c, d, a, b, buffer[i+14], 17, 0xA679438E);
	b = ff(b, c, d, a, buffer[i+15], 22, 0x49B40821);
	
	a = gg(a, b, c, d, buffer[i+ 1], 5, 0xF61E2562);
	d = gg(d, a, b, c, buffer[i+ 6], 9, 0xC040B340);
	c = gg(c, d, a, b, buffer[i+11], 14, 0x265E5A51);
	b = gg(b, c, d, a, buffer[i+ 0], 20, 0xE9B6C7AA);
	a = gg(a, b, c, d, buffer[i+ 5], 5, 0xD62F105D);
	d = gg(d, a, b, c, buffer[i+10], 9, 0x02441453);
	c = gg(c, d, a, b, buffer[i+15], 14, 0xD8A1E681);
	b = gg(b, c, d, a, buffer[i+ 4], 20, 0xE7D3FBC8);
	a = gg(a, b, c, d, buffer[i+ 9], 5, 0x21E1CDE6);
	d = gg(d, a, b, c, buffer[i+14], 9, 0xC33707D6);
	c = gg(c, d, a, b, buffer[i+ 3], 14, 0xF4D50D87);
	b = gg(b, c, d, a, buffer[i+ 8], 20, 0x455A14ED);
	a = gg(a, b, c, d, buffer[i+13], 5, 0xA9E3E905);
	d = gg(d, a, b, c, buffer[i+ 2], 9, 0xFCEFA3F8);
	c = gg(c, d, a, b, buffer[i+ 7], 14, 0x676F02D9);
	b = gg(b, c, d, a, buffer[i+12], 20, 0x8D2A4C8A);

	a = hh(a, b, c, d, buffer[i+ 5], 4, 0xFFFA3942);
	d = hh(d, a, b, c, buffer[i+ 8], 11, 0x8771F681);
	c = hh(c, d, a, b, buffer[i+11], 16, 0x6D9D6122);
	b = hh(b, c, d, a, buffer[i+14], 23, 0xFDE5380C);
	a = hh(a, b, c, d, buffer[i+ 1], 4, 0xA4BEEA44);
	d = hh(d, a, b, c, buffer[i+ 4], 11, 0x4BDECFA9);
	c = hh(c, d, a, b, buffer[i+ 7], 16, 0xF6BB4B60);
	b = hh(b, c, d, a, buffer[i+10], 23, 0xBEBFBC70);
	a = hh(a, b, c, d, buffer[i+13], 4, 0x289B7EC6);
	d = hh(d, a, b, c, buffer[i+ 0], 11, 0xEAA127FA);
	c = hh(c, d, a, b, buffer[i+ 3], 16, 0xD4EF3085);
	b = hh(b, c, d, a, buffer[i+ 6], 23, 0x04881D05);
	a = hh(a, b, c, d, buffer[i+ 9], 4, 0xD9D4D039);
	d = hh(d, a, b, c, buffer[i+12], 11, 0xE6DB99E5);
	c = hh(c, d, a, b, buffer[i+15], 16, 0x1FA27CF8);
	b = hh(b, c, d, a, buffer[i+ 2], 23, 0xC4AC5665);

	a = ii(a, b, c, d, buffer[i+ 0], 6, 0xF4292244);
	d = ii(d, a, b, c, buffer[i+ 7], 10, 0x432AFF97);
	c = ii(c, d, a, b, buffer[i+14], 15, 0xAB9423A7);
	b = ii(b, c, d, a, buffer[i+ 5], 21, 0xFC93A039);
	a = ii(a, b, c, d, buffer[i+12], 6, 0x655B59C3);
	d = ii(d, a, b, c, buffer[i+ 3], 10, 0x8F0CCC92);
	c = ii(c, d, a, b, buffer[i+10], 15, 0xFFEFF47D);
	b = ii(b, c, d, a, buffer[i+ 1], 21, 0x85845DD1);
	a = ii(a, b, c, d, buffer[i+ 8], 6, 0x6FA87E4F);
	d = ii(d, a, b, c, buffer[i+15], 10, 0xFE2CE6E0);
	c = ii(c, d, a, b, buffer[i+ 6], 15, 0xA3014314);
	b = ii(b, c, d, a, buffer[i+13], 21, 0x4E0811A1);
	a = ii(a, b, c, d, buffer[i+ 4], 6, 0xF7537E82);
	d = ii(d, a, b, c, buffer[i+11], 10, 0xBD3AF235);
	c = ii(c, d, a, b, buffer[i+ 2], 15, 0x2AD7D2BB);
	b = ii(b, c, d, a, buffer[i+ 9], 21, 0xEB86D391);

	a = add(a, temp_a);
	b = add(b, temp_b);
	c = add(c, temp_c);
	d = add(d, temp_d);
    }

    var w = [a, b, c, d];
    var t = [];		
    for (var i = 0; i < 4; i++)
	for (var j = 0; j < 32; j += 8)
	    t[i * 4 + j / 8] = (w[i] >> j) & 0xFF;
    return t;
}

//alert (md5 ([]) + "== d41d8cd98f00b204e9800998ecf8427e");
//alert (md5 ([65]) + " == 7fc56270e7a70fa81a5935b72eacbe29");

// Caml name: unsafe_string
// Type:      string -> int -> int -> t

///////////// Digest

function caml_md5_string (v, ofs, len) {
    var s = [];
    for (var i = 0;i < len;i++)
	s[i] = v.get (ofs + i);
    var h = md5(s);
    var res = new MlString(16);
    for (var j = 0;j < 16;j++)
	res.set(j, h[j]);
    return res;
}

