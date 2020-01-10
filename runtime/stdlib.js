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

///////////// Core

//Provides: raw_array_sub
function raw_array_sub (a,i,l) {
  return a.slice(i, i + l);
}

//Provides: raw_array_copy
function raw_array_copy (a) {
  return a.slice();
}

//Provides: raw_array_cons
function raw_array_cons (a,x) {
  var l = a.length;
  var b = new Array(l+1);
  b[0]=x;
  for(var i = 1; i <= l; i++ ) b[i] = a[i-1];
  return b
}

//Provides: caml_call_gen (const, shallow)
//Weakdef
function caml_call_gen(f, args) {
  var args_copied = false

  while (true) {
    if (f.fun) {
      f = f.fun;
      continue;
    }

    var n = f.length;
    var argsLen = args.length;
    var d = n - argsLen;

    if (d == 0) {
      return f.apply(null, args);
    }
    else if (d < 0) {
      if (!args_copied) {
        args = args.slice();
        args_copied = true;
      }

      var before = args;
      var after = before.splice(n);

      f = f.apply(null, before);
      args = after;
    }
    else {
      switch (d) {
      case 1: return function (a1) {
        return f.apply(null, args.concat([a1]));
      }
      case 2: return function (a1, a2) {
        return f.apply(null, args.concat([a1, a2]));
      }
      case 3: return function (a1, a2, a3) {
        return f.apply(null, args.concat([a1, a2, a3]));
      }
      case 4: return function (a1, a2, a3, a4) {
        return f.apply(null, args.concat([a1, a2, a3, a4]));
      }
      case 5: return function (a1, a2, a3, a4, a5) {
        return f.apply(null, args.concat([a1, a2, a3, a4, a5]));
      }
      case 6: return function (a1, a2, a3, a4, a5, a6) {
        return f.apply(null, args.concat([a1, a2, a3, a4, a5, a6]));
      }
      case 7: return function (a1, a2, a3, a4, a5, a6, a7) {
        return f.apply(null, args.concat([a1, a2, a3, a4, a5, a6, a7]));
      }
      default:
        return function (a1, a2, a3, a4, a5, a6, a7, a8) {
          return caml_call_gen(f, args.concat([a1, a2, a3, a4, a5, a6, a7, a8]));
        };
      }
    }
  }
}

//Provides: caml_named_values
var caml_named_values = {};

//Provides: caml_register_named_value (const,const)
//Requires: caml_named_values, caml_jsbytes_of_string
function caml_register_named_value(nm,v) {
  caml_named_values[caml_jsbytes_of_string(nm)] = v;
  return 0;
}

//Provides: caml_named_value
//Requires: caml_named_values
function caml_named_value(nm) {
  return caml_named_values[nm]
}

//Provides: caml_global_data
var caml_global_data = [0];

//Provides: caml_register_global (const, shallow, const)
//Requires: caml_global_data
function caml_register_global (n, v, name_opt) {
  if(name_opt && joo_global_object.toplevelReloc)
    n = joo_global_object.toplevelReloc(name_opt);
  caml_global_data[n + 1] = v;
  if(name_opt) caml_global_data[name_opt] = v;
}

//Provides: caml_get_global_data mutable
//Requires: caml_global_data
function caml_get_global_data () { return caml_global_data; }

//Raise exception


//Provides: caml_raise_constant (const)
function caml_raise_constant (tag) { throw tag; }

//Provides: caml_return_exn_constant (const)
function caml_return_exn_constant (tag) { return tag; }

//Provides: caml_raise_with_arg (const, const)
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }

//Provides: caml_raise_with_string (const, const)
//Requires: caml_raise_with_arg,caml_new_string
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, caml_new_string (msg));
}

//Provides: caml_raise_sys_error (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
  caml_raise_with_string(caml_global_data.Sys_error, msg);
}

//Provides: caml_failwith (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data.Failure, msg);
}

//Provides: caml_wrap_exception const (const)
//Requires: caml_global_data,caml_js_to_string,caml_named_value
//Requires: caml_return_exn_constant
function caml_wrap_exception(e) {
  if(e instanceof Array) return e;
  //Stack_overflow: chrome, safari
  if(joo_global_object.RangeError
     && e instanceof joo_global_object.RangeError
     && e.message
     && e.message.match(/maximum call stack/i))
    return caml_return_exn_constant(caml_global_data.Stack_overflow);
  //Stack_overflow: firefox
  if(joo_global_object.InternalError
     && e instanceof joo_global_object.InternalError
     && e.message
     && e.message.match(/too much recursion/i))
    return caml_return_exn_constant(caml_global_data.Stack_overflow);
  //Wrap Error in Js.Error exception
  if(e instanceof joo_global_object.Error && caml_named_value("jsError"))
    return [0,caml_named_value("jsError"),e];
  //fallback: wrapped in Failure
  return [0,caml_global_data.Failure,caml_js_to_string (String(e))];
}

// Experimental
//Provides: caml_exn_with_js_backtrace
//Requires: caml_global_data
function caml_exn_with_js_backtrace(exn, force) {
  //never reraise for constant exn
  if(!exn.js_error || force || exn[0] == 248) exn.js_error = new joo_global_object.Error("Js exception containing backtrace");
  return exn;
}
//Provides: caml_js_error_of_exception
function caml_js_error_of_exception(exn) {
  if(exn.js_error) { return exn.js_error; }
  return null;
}

//Provides: caml_invalid_argument (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data.Invalid_argument, msg);
}

//Provides: caml_raise_end_of_file
//Requires: caml_raise_constant, caml_global_data
function caml_raise_end_of_file () {
  caml_raise_constant(caml_global_data.End_of_file);
}

//Provides: caml_raise_zero_divide
//Requires: caml_raise_constant, caml_global_data
function caml_raise_zero_divide () {
  caml_raise_constant(caml_global_data.Division_by_zero);
}

//Provides: caml_raise_not_found
//Requires: caml_raise_constant, caml_global_data
function caml_raise_not_found () {
  caml_raise_constant(caml_global_data.Not_found); }


//Provides: caml_array_bound_error
//Requires: caml_invalid_argument
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_update_dummy
function caml_update_dummy (x, y) {
  if( typeof y==="function" ) { x.fun = y; return 0; }
  if( y.fun ) { x.fun = y.fun; return 0; }
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}

//Provides: caml_obj_is_block const (const)
function caml_obj_is_block (x) { return +(x instanceof Array); }


//Provides: caml_obj_tag
//Requires: MlBytes
function caml_obj_tag (x) {
  if ((x instanceof Array) && x[0] == (x[0] >>> 0))
    return x[0]
  else if (x instanceof MlBytes)
    return 252
  else if ((x instanceof Function) || typeof x == "function")
    return 247
  else if (x && x.caml_custom)
    return 255
  else
    return 1000
}

//Provides: caml_obj_set_tag (mutable, const)
function caml_obj_set_tag (x, tag) { x[0] = tag; return 0; }
//Provides: caml_obj_block const (const,const)
function caml_obj_block (tag, size) {
  var o = new Array(size+1);
  o[0]=tag;
  for (var i = 1; i <= size; i++) o[i] = 0;
  return o;
}

//Provides: caml_obj_with_tag
function caml_obj_with_tag(tag,x) {
  var l = x.length;
  var a = new Array(l);
  a[0] = tag;
  for(var i = 1; i < l; i++ ) a[i] = x[i];
  return a;
}

//Provides: caml_obj_dup mutable (const)
function caml_obj_dup (x) {
  var l = x.length;
  var a = new Array(l);
  for(var i = 0; i < l; i++ ) a[i] = x[i];
  return a;
}

//Provides: caml_obj_truncate (mutable, const)
//Requires: caml_invalid_argument
function caml_obj_truncate (x, s) {
  if (s<=0 || s + 1 > x.length)
    caml_invalid_argument ("Obj.truncate");
  if (x.length != s + 1) x.length = s + 1;
  return 0;
}

//Provides: caml_obj_make_forward
function caml_obj_make_forward (b,v) {
  b[0]=250;
  b[1]=v;
  return 0
}

//Provides: caml_lazy_make_forward const (const)
function caml_lazy_make_forward (v) { return [250, v]; }

//Provides: caml_mul const
function caml_mul(a,b){
  return Math.imul(a,b);
}

//Provides: caml_div
//Requires: caml_raise_zero_divide
function caml_div(x,y) {
  if (y == 0) caml_raise_zero_divide ();
  return (x/y)|0;
}

//Provides: caml_mod
//Requires: caml_raise_zero_divide
function caml_mod(x,y) {
  if (y == 0) caml_raise_zero_divide ();
  return x%y;
}

///////////// Pervasive
//Provides: caml_array_set (mutable, const, const)
//Requires: caml_array_bound_error
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}

//Provides: caml_array_get mutable (const, const)
//Requires: caml_array_bound_error
function caml_array_get (array, index) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  return array[index+1];
}

//Provides: caml_array_fill
function caml_array_fill(array, ofs, len, v){
  for(var i = 0; i < len; i++){
    array[ofs+i+1] = v;
  }
  return 0;
}

//Provides: caml_check_bound (const, const)
//Requires: caml_array_bound_error
function caml_check_bound (array, index) {
  if (index >>> 0 >= array.length - 1) caml_array_bound_error();
  return array;
}

//Provides: caml_make_vect const (const, const)
//Requires: caml_array_bound_error
function caml_make_vect (len, init) {
  if (len < 0) caml_array_bound_error();
  var len = len + 1 | 0;
  var b = new Array(len);
  b[0]=0;
  for (var i = 1; i < len; i++) b[i] = init;
  return b;
}

//Provides: caml_make_float_vect const (const)
//Requires: caml_array_bound_error
function caml_make_float_vect(len){
  if (len < 0) caml_array_bound_error();
  var len = len + 1 | 0;
  var b = new Array(len);
  b[0]=254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b
}
//Provides: caml_floatarray_create const (const)
//Requires: caml_array_bound_error
function caml_floatarray_create(len){
  if (len < 0) caml_array_bound_error();
  var len = len + 1 | 0;
  var b = new Array(len);
  b[0]=254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b
}

//Provides: caml_compare_val_tag
//Requires: MlBytes
function caml_compare_val_tag(a){
  if (typeof a === "number") return 1000; // int_tag (we use it for all numbers)
  else if (a instanceof MlBytes) return 252; // string_tag
  else if (a instanceof Array && a[0] === (a[0]>>>0) && a[0] <= 255) {
    // Look like an ocaml block
    var tag = a[0] | 0;
    // ignore double_array_tag because we cannot accurately set
    // this tag when we create an array of float.
    return (tag == 254)?0:tag
  }
  else if (a instanceof String) return 1252; // javascript string, like string_tag (252)
  else if (typeof a == "string") return 1252; // javascript string, like string_tag (252)
  else if (a instanceof Number) return 1000; // int_tag (we use it for all numbers)
  else if (a && a.caml_custom) return 1255; // like custom_tag (255)
  else if (a && a.compare) return 1256; // like custom_tag (255)
  else if (typeof a == "function") return 1247; // like closure_tag (247)
  else if (typeof a == "symbol") return 1251;
  return 1001; //out_of_heap_tag
}

//Provides: caml_compare_val_get_custom
//Requires: caml_custom_ops
function caml_compare_val_get_custom(a){
  return caml_custom_ops[a.caml_custom] && caml_custom_ops[a.caml_custom].compare;
}

//Provides: caml_compare_val_number_custom
//Requires: caml_compare_val_get_custom
function caml_compare_val_number_custom(num, custom, swap, total) {
  var comp = caml_compare_val_get_custom(custom);
  if(comp) {
    var x = (swap > 0)?comp(custom,num,total):comp(num,custom,total);
    if(total && x != x) return swap; // total && nan
    if(+x != +x) return +x; // nan
    if((x | 0) != 0) return (x | 0); // !nan
  }
  return swap
}

//Provides: caml_compare_val (const, const, const)
//Requires: caml_int_compare, caml_string_compare
//Requires: caml_invalid_argument, caml_compare_val_get_custom, caml_compare_val_tag
//Requires: caml_compare_val_number_custom
function caml_compare_val (a, b, total) {
  var stack = [];
  for(;;) {
    if (!(total && a === b)) {
      var tag_a = caml_compare_val_tag(a);
      // forward_tag ?
      if(tag_a == 250) { a = a[1]; continue }

      var tag_b = caml_compare_val_tag(b);
      // forward_tag ?
      if(tag_b == 250) { b = b[1]; continue }

      // tags are different
      if(tag_a !== tag_b) {
        if(tag_a == 1000) {
          if(tag_b == 1255) { //immediate can compare against custom
            return caml_compare_val_number_custom(a, b, -1, total);
          }
          return -1
        }
        if(tag_b == 1000) {
          if(tag_a == 1255) { //immediate can compare against custom
            return caml_compare_val_number_custom(b, a, 1, total);
          }
          return 1
        }
        return (tag_a < tag_b)?-1:1;
      }
      switch(tag_a){
        // 246: Lazy_tag handled bellow
      case 247: // Closure_tag
        // Cannot happen
        caml_invalid_argument("compare: functional value");
        break
      case 248: // Object
        var x = caml_int_compare(a[2], b[2]);
        if (x != 0) return (x | 0);
        break;
      case 249: // Infix
        // Cannot happen
        caml_invalid_argument("compare: functional value");
        break
      case 250: // Forward tag
        // Cannot happen, handled above
        caml_invalid_argument("equal: got Forward_tag, should not happen");
        break;
      case 251: //Abstract
        caml_invalid_argument("equal: abstract value");
        break;
      case 252: // OCaml string
        if (a !== b) {
          var x = caml_string_compare(a, b);
          if (x != 0) return (x | 0);
        };
        break;
      case 253: // Double_tag
        // Cannot happen
        caml_invalid_argument("equal: got Double_tag, should not happen");
        break;
      case 254: // Double_array_tag
        // Cannot happen, handled above
        caml_invalid_argument("equal: got Double_array_tag, should not happen");
        break
      case 255: // Custom_tag
        caml_invalid_argument("equal: got Custom_tag, should not happen");
        break;
      case 1247: // Function
        caml_invalid_argument("compare: functional value");
        break;
      case 1255: // Custom
        var comp = caml_compare_val_get_custom(a);
        if(comp != caml_compare_val_get_custom(b)){
          return (a.caml_custom<b.caml_custom)?-1:1;
        }
        if(!comp)
          caml_invalid_argument("compare: abstract value");
        var x = comp(a,b,total);
        if(x != x){ // Protect against invalid UNORDERED
          return total?-1:x;
        }
        if(x !== (x|0)){ // Protect against invalid return value
          return -1
        }
        if (x != 0) return (x | 0);
        break;
      case 1256: // compare function
        var x = a.compare(b,total);
        if(x != x) { // Protect against invalid UNORDERED
          return total?-1:x;
        }
        if(x !== (x|0)){ // Protect against invalid return value
          return -1
        }
        if (x != 0) return (x | 0);
        break;
      case 1000: // Number
        a = +a;
        b = +b;
        if (a < b) return -1;
        if (a > b) return 1;
        if (a != b) {
          if (!total) return NaN;
          if (a == a) return 1;
          if (b == b) return -1;
        }
        break;
      case 1001: // The rest
        // Here we can be in the following cases:
        // 1. JavaScript primitive types
        // 2. JavaScript object that can be coerced to primitive types
        // 3. JavaScript object than cannot be coerced to primitive types
        //
        // (3) will raise a [TypeError]
        // (2) will coerce to primitive types using [valueOf] or [toString]
        // (2) and (3), after eventual coercion
        // - if a and b are strings, apply lexicographic comparison
        // - if a or b are not strings, convert a and b to number
        //   and apply standard comparison
        //
        // Exception: `!=` will not coerce/convert if both a and b are objects
        if (a < b) return -1;
        if (a > b) return 1;
        if (a != b) {
          if (!total) return NaN;
          if (a == a) return 1;
          if (b == b) return -1;
        }
        break;
      case 1251: // JavaScript Symbol, no ordering.
        if(a !== b) {
          if (!total) return NaN;
          return 1;
        }
        break;
      case 1252: // javascript strings
        var a = a.toString();
        var b = b.toString();
        if(a !== b) {
          if(a < b) return -1;
          if(a > b) return 1;
        }
        break;
      case 246: // Lazy_tag
      case 254: // Double_array
      default: // Block with other tag
        if (a.length != b.length) return (a.length < b.length)?-1:1;
        if (a.length > 1) stack.push(a, b, 1);
        break;
      }
    }
    if (stack.length == 0) return 0;
    var i = stack.pop();
    b = stack.pop();
    a = stack.pop();
    if (i + 1 < a.length) stack.push(a, b, i + 1);
    a = a[i];
    b = b[i];
  }
}
//Provides: caml_compare (const, const)
//Requires: caml_compare_val
function caml_compare (a, b) { return caml_compare_val (a, b, true); }
//Provides: caml_int_compare mutable (const, const)
function caml_int_compare (a, b) {
  if (a < b) return (-1); if (a == b) return 0; return 1;
}
//Provides: caml_equal mutable (const, const)
//Requires: caml_compare_val
function caml_equal (x, y) { return +(caml_compare_val(x,y,false) == 0); }
//Provides: caml_notequal mutable (const, const)
//Requires: caml_compare_val
function caml_notequal (x, y) { return +(caml_compare_val(x,y,false) != 0); }
//Provides: caml_greaterequal mutable (const, const)
//Requires: caml_compare_val
function caml_greaterequal (x, y) { return +(caml_compare_val(x,y,false) >= 0); }
//Provides: caml_greaterthan mutable (const, const)
//Requires: caml_compare_val
function caml_greaterthan (x, y) { return +(caml_compare_val(x,y,false) > 0); }
//Provides: caml_lessequal mutable (const, const)
//Requires: caml_compare_val
function caml_lessequal (x, y) { return +(caml_compare_val(x,y,false) <= 0); }
//Provides: caml_lessthan mutable (const, const)
//Requires: caml_compare_val
function caml_lessthan (x, y) { return +(caml_compare_val(x,y,false) < 0); }

//Provides: caml_parse_sign_and_base
//Requires: caml_string_unsafe_get, caml_ml_string_length
function caml_parse_sign_and_base (s) {
  var i = 0, len = caml_ml_string_length(s), base = 10, sign = 1;
  if (len > 0) {
    switch (caml_string_unsafe_get(s,i)) {
    case 45: i++; sign = -1; break;
    case 43: i++; sign = 1; break;
    }
  }
  if (i + 1 < len && caml_string_unsafe_get(s, i) == 48)
    switch (caml_string_unsafe_get(s, i + 1)) {
    case 120: case 88: base = 16; i += 2; break;
    case 111: case 79: base =  8; i += 2; break;
    case  98: case 66: base =  2; i += 2; break;
    case 117: case 85: sign =  0; i += 2; break;
    }
  return [i, sign, base];
}

//Provides: caml_parse_digit
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57)  return c - 48;
  if (c >= 65 && c <= 90)  return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}

//Provides: caml_int_of_string (const)
//Requires: caml_ml_string_length, caml_string_unsafe_get
//Requires: caml_parse_sign_and_base, caml_parse_digit, caml_failwith
function caml_int_of_string (s) {
  var r = caml_parse_sign_and_base (s);
  var i = r[0], sign = r[1], base = r[2];
  var len = caml_ml_string_length(s);
  var threshold = -1 >>> 0;
  var c = (i < len)?caml_string_unsafe_get(s, i):0;
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("int_of_string");
  var res = d;
  for (i++;i<len;i++) {
    c = caml_string_unsafe_get(s, i);
    if (c == 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
    if (res > threshold) caml_failwith("int_of_string");
  }
  if (i != len) caml_failwith("int_of_string");
  // For base different from 10, we expect an unsigned representation,
  // hence any value of 'res' (less than 'threshold') is acceptable.
  // But we have to convert the result back to a signed integer.
  res = sign * res;
  if ((base == 10) && ((res | 0) != res))
    /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
    caml_failwith("int_of_string");
  return res | 0;
}

//Provides: caml_float_of_string (const)
//Requires: caml_failwith, caml_jsbytes_of_string
function caml_float_of_string(s) {
  var res;
  s = caml_jsbytes_of_string (s);
  res = +s;
  if ((s.length > 0) && (res === res)) return res;
  s = s.replace(/_/g,"");
  res = +s;
  if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) return res;
  var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)p([+-]?[0-9]+)/i.exec(s);
  //          1        2             3           4
  if(m){
    var m3 = m[3].replace(/0+$/,'');
    var mantissa = parseInt(m[1] + m[2] + m3, 16);
    var exponent = (m[4]|0) - 4*m3.length;
    res = mantissa * Math.pow(2, exponent);
    return res;
  }
  if(/^\+?inf(inity)?$/i.test(s)) return Infinity;
  if(/^-inf(inity)?$/i.test(s)) return -Infinity;
  caml_failwith("float_of_string");
}

//Provides: caml_is_printable const (const)
function caml_is_printable(c) { return +(c > 31 && c < 127); }

///////////// Format
//Provides: caml_parse_format
//Requires: caml_jsbytes_of_string, caml_invalid_argument
function caml_parse_format (fmt) {
  fmt = caml_jsbytes_of_string(fmt);
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f =
      { justify:'+', signstyle:'-', filler:' ', alternate:false,
        base:0, signedconv:false, width:0, uppercase:false,
        sign:1, prec:-1, conv:'f' };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
    case '-':
      f.justify = '-'; break;
    case '+': case ' ':
      f.signstyle = c; break;
    case '0':
      f.filler = '0'; break;
    case '#':
      f.alternate = true; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      f.width = 0;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.width = f.width * 10 + c; i++
      }
      i--;
      break;
    case '.':
      f.prec = 0;
      i++;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.prec = f.prec * 10 + c; i++
      }
      i--;
    case 'd': case 'i':
      f.signedconv = true; /* fallthrough */
    case 'u':
      f.base = 10; break;
    case 'x':
      f.base = 16; break;
    case 'X':
      f.base = 16; f.uppercase = true; break;
    case 'o':
      f.base = 8; break;
    case 'e': case 'f': case 'g':
      f.signedconv = true; f.conv = c; break;
    case 'E': case 'F': case 'G':
      f.signedconv = true; f.uppercase = true;
      f.conv = c.toLowerCase (); break;
    }
  }
  return f;
}

//Provides: caml_finish_formatting
//Requires: caml_new_string
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  /* Adjust len to reflect additional chars (sign, etc) */
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  /* Do the formatting */
  var buffer = "";
  if (f.justify == '+' && f.filler == ' ')
    for (var i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (var i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (var i = len; i < f.width; i++) buffer += ' ';
  return caml_new_string (buffer);
}

//Provides: caml_format_int const (const, const)
//Requires: caml_parse_format, caml_finish_formatting, caml_str_repeat
//Requires: caml_new_string, caml_jsbytes_of_string
function caml_format_int(fmt, i) {
  if (caml_jsbytes_of_string(fmt) == "%d") return caml_new_string(""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - s.length;
    if (n > 0) s = caml_str_repeat (n, '0') + s;
  }
  return caml_finish_formatting(f, s);
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float (fmt, x) {
  function toFixed(x,dp) {
    if (Math.abs(x) < 1.0) {
      return x.toFixed(dp);
    } else {
      var e = parseInt(x.toString().split('+')[1]);
      if (e > 20) {
        e -= 20;
        x /= Math.pow(10,e);
        x += (new Array(e+1)).join('0');
        if(dp > 0) {
          x = x + '.' + (new Array(dp+1)).join('0');
        }
        return x;
      }
      else return x.toFixed(dp)
    }
  }
  var s, f = caml_parse_format(fmt);
  var prec = (f.prec < 0)?6:f.prec;
  if (x < 0 || (x == 0 && 1/x == -Infinity)) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = toFixed(x, prec); break;
    case 'g':
      prec = prec?prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
        // remove trailing zeroes
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          // remove trailing zeroes
          var i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}

///////////// Hashtbl
//Provides: caml_hash_univ_param mutable
//Requires: MlBytes, caml_convert_string_to_bytes
//Requires: caml_int64_to_bytes, caml_int64_bits_of_float, caml_custom_ops
//Requires: caml_ml_bytes_length
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
    } else if (obj instanceof MlBytes) {
      count --;
      switch (obj.t & 6) {
      default: /* PARTIAL */
        caml_convert_string_to_bytes(obj);
      case 0: /* BYTES */
        for (var b = obj.c, l = caml_ml_bytes_length(obj), i = 0; i < l; i++)
          hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
        break;
      case 2: /* ARRAY */
        for (var a = obj.c, l = caml_ml_bytes_length(obj), i = 0; i < l; i++)
          hash_accu = (hash_accu * 19 + a[i]) | 0;
      }
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

//Provides: caml_hash_mix_string_str
//Requires: caml_hash_mix_int
function caml_hash_mix_string_str(h, s) {
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

//Provides: caml_hash_mix_string_arr
//Requires: caml_hash_mix_int
function caml_hash_mix_string_arr(h, s) {
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

//Provides: caml_hash_mix_string
//Requires: caml_convert_string_to_bytes
//Requires: caml_hash_mix_string_str
//Requires: caml_hash_mix_string_arr
function caml_hash_mix_string(h, v) {
  switch (v.t & 6) {
  default:
    caml_convert_string_to_bytes (v);
  case 0: /* BYTES */
    h = caml_hash_mix_string_str(h, v.c);
    break;
  case 2: /* ARRAY */
    h = caml_hash_mix_string_arr(h, v.c);
  }
  return h
}


//Provides: caml_hash mutable
//Requires: MlBytes
//Requires: caml_hash_mix_int, caml_hash_mix_final
//Requires: caml_hash_mix_float, caml_hash_mix_string, caml_custom_ops
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
        var tag = ((v.length - 1) << 10) | v[0];
        h = caml_hash_mix_int(h, tag);
        for (i = 1, len = v.length; i < len; i++) {
          if (wr >= sz) break;
          queue[wr++] = v[i];
        }
        break;
      }
    } else if (v instanceof MlBytes) {
      h = caml_hash_mix_string(h,v)
      num--;
    } else if (v === (v|0)) {
      // Integer
      h = caml_hash_mix_int(h, v+v+1);
      num--;
    } else if (v === +v) {
      // Float
      h = caml_hash_mix_float(h,v);
      num--;
    }
  }
  h = caml_hash_mix_final(h);
  return h & 0x3FFFFFFF;
}

///////////// Sys
//Provides: caml_sys_time mutable
var caml_initial_time = (new Date()).getTime() * 0.001;
function caml_sys_time () {
  var now = (new Date()).getTime();
  return now * 0.001 - caml_initial_time;
}

//Provides: caml_sys_get_config const
//Requires: caml_new_string
function caml_sys_get_config () {
  return [0, caml_new_string("Unix"), 32, 0];
}

//Provides: caml_sys_const_backend_type const
//Requires: caml_new_string
function caml_sys_const_backend_type () {
  return [0, caml_new_string("js_of_ocaml")];
}

//Provides: caml_sys_random_seed mutable
//The function needs to return an array since OCaml 4.0...
function caml_sys_random_seed () {
  var now = (new Date()).getTime();
  var x = now^0xffffffff*Math.random();
  return [0,x];
}



//Provides: caml_sys_const_big_endian const
function caml_sys_const_big_endian () { return 0; }
//Provides: caml_sys_const_word_size const
function caml_sys_const_word_size () { return 32; }
//Provides: caml_sys_const_int_size const
function caml_sys_const_int_size () { return 32; }

//Provides: caml_sys_const_max_wosize const
// max_int / 4 so that the following does not overflow
//let max_string_length = word_size / 8 * max_array_length - 1;;
function caml_sys_const_max_wosize () { return (0x7FFFFFFF/4) | 0;}

//Provides: caml_sys_const_ostype_cygwin const
function caml_sys_const_ostype_cygwin () { return 0; }
//Provides: caml_sys_const_ostype_unix const
function caml_sys_const_ostype_unix () { return 1; }
//Provides: caml_sys_const_ostype_win32 const
function caml_sys_const_ostype_win32 () { return 0; }

//Provides: caml_sys_system_command
function caml_sys_system_command(cmd){
  var cmd = cmd.toString();
  joo_global_object.console.log(cmd);
  if (typeof require != "undefined"
      && require('child_process')
      && require('child_process').execSync) {
    try {require('child_process').execSync(cmd); return 0}
    catch (e) {return 1}
  }
  else return 127;
}

///////////// Array
//Provides: caml_array_sub mutable
function caml_array_sub (a, i, len) {
  var a2 = new Array(len+1);
  a2[0]=0;
  for(var i2 = 1, i1= i+1; i2 <= len; i2++,i1++ ){
    a2[i2]=a[i1];
  }
  return a2;
}

//Provides: caml_array_append mutable
function caml_array_append(a1, a2) {
  var l1 = a1.length, l2 = a2.length;
  var l = l1+l2-1
  var a = new Array(l);
  a[0] = 0;
  var i = 1,j = 1;
  for(;i<l1;i++) a[i]=a1[i];
  for(;i<l;i++,j++) a[i]=a2[j];
  return a;
}

//Provides: caml_array_concat mutable
function caml_array_concat(l) {
  var a = [0];
  while (l !== 0) {
    var b = l[1];
    for (var i = 1; i < b.length; i++) a.push(b[i]);
    l = l[2];
  }
  return a;
}

//Provides: caml_array_blit
function caml_array_blit(a1, i1, a2, i2, len) {
  if (i2 <= i1) {
    for (var j = 1; j <= len; j++) a2[i2 + j] = a1[i1 + j];
  } else {
    for (var j = len; j >= 1; j--) a2[i2 + j] = a1[i1 + j];
  };
  return 0;
}

///////////// CamlinternalOO
//Provides: caml_get_public_method const
var caml_method_cache = [];
function caml_get_public_method (obj, tag, cacheid) {
  var meths = obj[1];
  var ofs = caml_method_cache[cacheid];
  if (ofs === null) {
    // Make sure the array is not sparse
    for (var i = caml_method_cache.length; i < cacheid; i++)
      caml_method_cache[i] = 0;
  } else if (meths[ofs] === tag) {
    return meths[ofs - 1];
  }
  var li = 3, hi = meths[1] * 2 + 1, mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < meths[mi+1]) hi = mi-2;
    else li = mi;
  }
  caml_method_cache[cacheid] = li + 1;
  /* return 0 if tag is not there */
  return (tag == meths[li+1] ? meths[li] : 0);
}

//Provides: caml_final_register const
function caml_final_register () { return 0; }
//Provides: caml_final_register_called_without_value const
function caml_final_register_called_without_value () { return 0; }
//Provides: caml_final_release const
function caml_final_release () { return 0; }
//Provides: caml_backtrace_status const
function caml_backtrace_status () { return 0; }
//Provides: caml_get_exception_backtrace const
function caml_get_exception_backtrace () { return 0; }
//Provides: caml_get_exception_raw_backtrace const
function caml_get_exception_raw_backtrace () { return [0]; }
//Provides: caml_record_backtrace
function caml_record_backtrace () { return 0; }
//Provides: caml_convert_raw_backtrace const
function caml_convert_raw_backtrace () { return [0]; }
//Provides: caml_raw_backtrace_length
function caml_raw_backtrace_length() { return 0; }
//Provides: caml_raw_backtrace_next_slot
function caml_raw_backtrace_next_slot() { return 0 }
//Provides: caml_raw_backtrace_slot
//Requires: caml_invalid_argument
function caml_raw_backtrace_slot () {
  caml_invalid_argument("Printexc.get_raw_backtrace_slot: index out of bounds");
}
//Provides: caml_restore_raw_backtrace
function caml_restore_raw_backtrace(exn, bt) { return 0 }
//Provides: caml_get_current_callstack const
function caml_get_current_callstack () { return [0]; }

//Provides: caml_set_static_env
function caml_set_static_env(k,v){
  if(!joo_global_object.jsoo_static_env)
    joo_global_object.jsoo_static_env = {}
  joo_global_object.jsoo_static_env[k] = v;
  return 0;
}
//Provides: caml_sys_getenv (const)
//Requires: caml_raise_not_found
//Requires: caml_js_to_string
function caml_sys_getenv (name) {
  var g = joo_global_object;
  var n = name.toString();
  //nodejs env
  if(g.process
     && g.process.env
     && g.process.env[n] != undefined)
    return caml_js_to_string(g.process.env[n]);
  if(joo_global_object.jsoo_static_env
     && joo_global_object.jsoo_static_env[n])
    return caml_js_to_string(joo_global_object.jsoo_static_env[n])
  caml_raise_not_found ();
}
//Provides: caml_sys_exit
//Requires: caml_invalid_argument
function caml_sys_exit (code) {
  var g = joo_global_object;
  if(g.quit) g.quit(code);
  //nodejs
  if(g.process && g.process.exit)
    g.process.exit(code);
  caml_invalid_argument("Function 'exit' not implemented");
}

//Provides: caml_argv
//Requires: caml_js_to_string
//Requires: raw_array_sub
var caml_argv = ((function () {
  var g = joo_global_object;
  var main = "a.out";
  var args = []

  if(g.process
     && g.process.argv
     && g.process.argv.length > 1) {
    var argv = g.process.argv
    //nodejs
    main = argv[1];
    args = raw_array_sub(argv,2,argv.length - 2);
  }

  var p = caml_js_to_string(main);
  var args2 = [0, p];
  for(var i = 0; i < args.length; i++)
    args2.push(caml_js_to_string(args[i]));
  return args2;
})())

//Provides: caml_executable_name
//Requires: caml_argv
var caml_executable_name = caml_argv[1]

//Provides: caml_sys_get_argv
//Requires: caml_argv
function caml_sys_get_argv (a) {
  return [0, caml_argv[1], caml_argv];
}

//Provides: caml_sys_argv
//Requires: caml_argv
function caml_sys_argv (a) {
  return caml_argv;
}

//Provides: caml_sys_modify_argv
//Requires: caml_argv
function caml_sys_modify_argv(arg){
  caml_argv = arg;
  return 0;
}

//Provides: caml_sys_executable_name const
//Requires: caml_executable_name
function caml_sys_executable_name(a){
  return caml_executable_name
}

//Provides: unix_inet_addr_of_string
function unix_inet_addr_of_string () {return 0;}

//Provides: caml_oo_last_id
var caml_oo_last_id = 0;

//Provides: caml_set_oo_id
//Requires: caml_oo_last_id
function caml_set_oo_id (b) {
  b[2]=caml_oo_last_id++;
  return b;
}

//Provides: caml_fresh_oo_id
//Requires: caml_oo_last_id
function caml_fresh_oo_id() {
  return caml_oo_last_id++;
}

//Provides: caml_install_signal_handler const
function caml_install_signal_handler(){return 0}


//Provides: caml_convert_raw_backtrace_slot
//Requires: caml_failwith
function caml_convert_raw_backtrace_slot(){
  caml_failwith("caml_convert_raw_backtrace_slot");
}

//Provides: caml_bswap16
function caml_bswap16(x) {
  return ((((x & 0x00FF) << 8) |
           ((x & 0xFF00) >> 8)));
}
//Provides: caml_int32_bswap
function caml_int32_bswap(x) {
  return (((x & 0x000000FF) << 24) |
          ((x & 0x0000FF00) << 8) |
          ((x & 0x00FF0000) >>> 8) |
          ((x & 0xFF000000) >>> 24));
}
//Provides: caml_int64_bswap
//Requires: caml_int64_to_bytes, caml_int64_of_bytes
function caml_int64_bswap(x) {
  var y = caml_int64_to_bytes(x);
  return caml_int64_of_bytes([y[7], y[6], y[5], y[4], y[3], y[2], y[1], y[0]]);
}

//Provides: caml_list_of_js_array const (const)
function caml_list_of_js_array(a){
  var l = 0;
  for(var i=a.length - 1; i>=0; i--){
    var e = a[i];
    l = [0,e,l];
  }
  return l
}

//Provides: caml_list_to_js_array const (const)
function caml_list_to_js_array(l){
  var a = [];
  for(; l !== 0; l = l[2]) {
    a.push(l[1]);
  }
  return a;
}

//Provides: caml_runtime_warnings
var caml_runtime_warnings = 0;

//Provides: caml_ml_enable_runtime_warnings
//Requires: caml_runtime_warnings
function caml_ml_enable_runtime_warnings (bool) {
  caml_runtime_warnings = bool;
  return 0;
}

//Provides: caml_ml_runtime_warnings_enabled
//Requires: caml_runtime_warnings
function caml_ml_runtime_warnings_enabled (_unit) {
  return caml_runtime_warnings;
}

//Provides: caml_runtime_variant
//Requires: caml_new_string
function caml_runtime_variant(_unit) {
  return caml_new_string("");
}
//Provides: caml_runtime_parameters
//Requires: caml_new_string
function caml_runtime_parameters(_unit) {
  return caml_new_string("");
}


//Provides: caml_sys_isatty
function caml_sys_isatty(_chan) {
  return 0;
}

//Provides: caml_spacetime_enabled const (const)
function caml_spacetime_enabled(_unit) {
  return 0;
}

//Provides: caml_register_channel_for_spacetime const (const)
function caml_register_channel_for_spacetime(_channel) {
  return 0;
}

//Provides: caml_spacetime_only_works_for_native_code
//Requires: caml_failwith
function caml_spacetime_only_works_for_native_code() {
  caml_failwith("Spacetime profiling only works for native code");
}


//Provides: caml_is_js
function caml_is_js() {
  return 1;
}
