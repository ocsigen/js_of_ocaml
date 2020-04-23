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

//Provides: caml_call_gen (const, shallow)
//Weakdef
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  //FIXME, can happen with too many arguments
  if(typeof f !== "function") return f;
  var n = f.length | 0;
  if(n === 0) return f.apply(null,args);
  var argsLen = args.length | 0;
  var d = n - argsLen | 0;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0) {
    return caml_call_gen(f.apply(null,args.slice(0,n)),args.slice(n));
  }
  else {
    return function (){
      var extra_args = (arguments.length == 0)?1:arguments.length;
      var nargs = new Array(args.length+extra_args);
      for(var i = 0; i < args.length; i++ ) nargs[i] = args[i];
      for(var i = 0; i < arguments.length; i++ ) nargs[args.length+i] = arguments[i];
      return caml_call_gen(f, nargs)
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
//Requires: caml_raise_with_arg, caml_string_of_jsbytes
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, caml_string_of_jsbytes(msg));
}

//Provides: caml_failwith (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data.Failure, msg);
}

//Provides: caml_wrap_exception const (const)
//Requires: caml_global_data,caml_string_of_jsstring,caml_named_value
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
  return [0,caml_global_data.Failure,caml_string_of_jsstring (String(e))];
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
//Requires: caml_string_of_jsbytes
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
  return caml_string_of_jsbytes(buffer);
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
