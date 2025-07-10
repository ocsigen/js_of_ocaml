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

//Provides: caml_compare_val_tag
//Requires: caml_is_ml_string, caml_is_ml_bytes
function caml_compare_val_tag(a) {
  if (a === null) return 1010; // null_tag
  if (typeof a === "number")
    return 1000; // int_tag (we use it for all numbers)
  else if (caml_is_ml_bytes(a))
    return 252; // string_tag
  else if (caml_is_ml_string(a))
    return 1252; // ocaml string (if different from bytes)
  else if (Array.isArray(a) && a[0] === a[0] >>> 0 && a[0] <= 255) {
    // Look like an ocaml block
    var tag = a[0] | 0;
    // ignore double_array_tag because we cannot accurately set
    // this tag when we create an array of float.
    return tag === 254 ? 0 : tag;
  } else if (a instanceof String)
    return 12520; // javascript string, like string_tag (252)
  else if (typeof a === "string")
    return 12520; // javascript string, like string_tag (252)
  else if (a instanceof Number)
    return 1000; // int_tag (we use it for all numbers)
  else if (a?.caml_custom)
    return 1255; // like custom_tag (255)
  else if (a?.compare)
    return 1256; // like custom_tag (255)
  else if (typeof a === "function")
    return 1247; // like closure_tag (247)
  else if (typeof a === "symbol") return 1251;
  return 1001; //out_of_heap_tag
}

//Provides: caml_compare_val_get_custom
//Requires: caml_custom_ops
function caml_compare_val_get_custom(a) {
  return (
    caml_custom_ops[a.caml_custom] && caml_custom_ops[a.caml_custom].compare
  );
}

//Provides: caml_compare_val_number_custom
//Requires: caml_compare_val_get_custom
function caml_compare_val_number_custom(num, custom, swap, total) {
  var comp = caml_compare_val_get_custom(custom);
  if (comp) {
    var x = swap > 0 ? comp(custom, num, total) : comp(num, custom, total);
    if (total && Number.isNaN(x)) return swap; // total && nan
    if (Number.isNaN(+x)) return +x; // nan
    if ((x | 0) !== 0) return x | 0; // !nan
  }
  return swap;
}

//Provides: caml_compare_val (const, const, const)
//Requires: caml_int_compare, caml_string_compare, caml_bytes_compare
//Requires: caml_invalid_argument, caml_compare_val_get_custom, caml_compare_val_tag
//Requires: caml_compare_val_number_custom
//Requires: caml_jsbytes_of_string
//Requires: caml_is_continuation_tag
function caml_compare_val(a, b, total) {
  var stack = [];
  for (;;) {
    if (!(total && a === b)) {
      var tag_a = caml_compare_val_tag(a);
      // forward_tag ?
      if (tag_a === 250) {
        a = a[1];
        continue;
      }

      var tag_b = caml_compare_val_tag(b);
      // forward_tag ?
      if (tag_b === 250) {
        b = b[1];
        continue;
      }

      // tags are different
      if (tag_a !== tag_b) {
        if (tag_a === 1010) {
          // Null is less than anything else
          return -1;
        }
        if (tag_b === 1010) {
          return 1;
        }
        if (tag_a === 1000) {
          if (tag_b === 1255) {
            //immediate can compare against custom
            return caml_compare_val_number_custom(a, b, -1, total);
          }
          return -1;
        }
        if (tag_b === 1000) {
          if (tag_a === 1255) {
            //immediate can compare against custom
            return caml_compare_val_number_custom(b, a, 1, total);
          }
          return 1;
        }
        return tag_a < tag_b ? -1 : 1;
      }
      // tag_a = tag_b
      switch (tag_a) {
        // 246: Lazy_tag handled bellow
        case 247: // Closure_tag
          // Cannot happen
          caml_invalid_argument("compare: functional value");
          break;
        case 248: // Object
          var x = caml_int_compare(a[2], b[2]) | 0;
          if (x !== 0) return x;
          break;
        case 249: // Infix
          // Cannot happen
          caml_invalid_argument("compare: functional value");
          break;
        case 250: // Forward tag
          // Cannot happen, handled above
          caml_invalid_argument("equal: got Forward_tag, should not happen");
          break;
        case 251: //Abstract
          caml_invalid_argument("equal: abstract value");
          break;
        case 252: // OCaml bytes
          if (a !== b) {
            var x = caml_bytes_compare(a, b) | 0;
            if (x !== 0) return x;
          }
          break;
        case 253: // Double_tag
          // Cannot happen
          caml_invalid_argument("equal: got Double_tag, should not happen");
          break;
        case 254: // Double_array_tag
          // Cannot happen, handled in caml_compare_val_tag
          caml_invalid_argument(
            "equal: got Double_array_tag, should not happen",
          );
          break;
        case 255: // Custom_tag
          caml_invalid_argument("equal: got Custom_tag, should not happen");
          break;
        case 1247: // Function
          caml_invalid_argument("compare: functional value");
          break;
        case 1255: // Custom
          var comp = caml_compare_val_get_custom(a);
          if (comp !== caml_compare_val_get_custom(b)) {
            return a.caml_custom < b.caml_custom ? -1 : 1;
          }
          if (!comp) caml_invalid_argument("compare: abstract value");
          var x = comp(a, b, total);
          if (Number.isNaN(x)) {
            // Protect against invalid UNORDERED
            return total ? -1 : x;
          }
          if (x !== (x | 0)) {
            // Protect against invalid return value
            return -1;
          }
          if (x !== 0) return x | 0;
          break;
        case 1256: // compare function
          var x = a.compare(b, total);
          if (Number.isNaN(x)) {
            // Protect against invalid UNORDERED
            return total ? -1 : x;
          }
          if (x !== (x | 0)) {
            // Protect against invalid return value
            return -1;
          }
          if (x !== 0) return x | 0;
          break;
        case 1000: // Number
          a = +a;
          b = +b;
          if (a < b) return -1;
          if (a > b) return 1;
          if (a !== b) {
            if (!total) return Number.NaN;
            if (!Number.isNaN(a)) return 1;
            if (!Number.isNaN(b)) return -1;
          }
          break;
        case 1010: // Null pointer
          return 0;
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
          if (a < b) return -1;
          if (a > b) return 1;
          if (a !== b) {
            return total ? 1 : Number.NaN;
          }
          break;
        case 1251: // JavaScript Symbol, no ordering.
          if (a !== b) {
            return total ? 1 : Number.NaN;
          }
          break;
        case 1252: // ocaml strings
          var a = caml_jsbytes_of_string(a);
          var b = caml_jsbytes_of_string(b);
          if (a !== b) {
            if (a < b) return -1;
            if (a > b) return 1;
          }
          break;
        case 12520: // javascript strings
          var a = a.toString();
          var b = b.toString();
          if (a !== b) {
            if (a < b) return -1;
            if (a > b) return 1;
          }
          break;
        default: // Lazy_tag or Block with other tag
          if (caml_is_continuation_tag(tag_a)) {
            caml_invalid_argument("compare: continuation value");
            break;
          }
          if (a.length !== b.length) return a.length < b.length ? -1 : 1;
          if (a.length > 1) stack.push(a, b, 1);
          break;
      }
    }
    if (stack.length === 0) return 0;
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
function caml_compare(a, b) {
  return caml_compare_val(a, b, true);
}

//Provides: caml_int_compare const
//Alias: caml_int32_compare
//Alias: caml_nativeint_compare
function caml_int_compare(a, b) {
  if (a < b) return -1;
  if (a === b) return 0;
  return 1;
}
//Provides: caml_equal mutable (const, const)
//Requires: caml_compare_val
function caml_equal(x, y) {
  return +(caml_compare_val(x, y, false) === 0);
}
//Provides: caml_notequal mutable (const, const)
//Requires: caml_compare_val
function caml_notequal(x, y) {
  return +(caml_compare_val(x, y, false) !== 0);
}
//Provides: caml_greaterequal mutable (const, const)
//Requires: caml_compare_val
function caml_greaterequal(x, y) {
  return +(caml_compare_val(x, y, false) >= 0);
}
//Provides: caml_greaterthan mutable (const, const)
//Requires: caml_compare_val
function caml_greaterthan(x, y) {
  return +(caml_compare_val(x, y, false) > 0);
}
//Provides: caml_lessequal mutable (const, const)
//Requires: caml_compare_val
function caml_lessequal(x, y) {
  return +(caml_compare_val(x, y, false) <= 0);
}
//Provides: caml_lessthan mutable (const, const)
//Requires: caml_compare_val
function caml_lessthan(x, y) {
  return +(caml_compare_val(x, y, false) < 0);
}
