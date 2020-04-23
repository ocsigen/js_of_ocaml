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

//Provides: caml_call_gen_cb (const, shallow)
function caml_call_gen_cb(f, args) {
  if(f.fun) return caml_call_gen_cb(f.fun, args);
  //This can happen with too many arguments
  if(typeof f !== "function") return f;
  var n = f.length | 0;
  if(n === 0) return f.apply(null,args);
  var argsLen = args.length | 0;
  var d = n - argsLen | 0;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0) {
    return caml_call_gen_cb(f.apply(null,args.slice(0,n)),args.slice(n));
  }
  else {
    return function (){
      var extra_args = (arguments.length == 0)?1:arguments.length;
      var nargs = new Array(args.length+extra_args);
      for(var i = 0; i < args.length; i++ ) nargs[i] = args[i];
      for(var i = 0; i < arguments.length; i++ ) nargs[args.length+i] = arguments[i];
      return caml_call_gen_cb(f, nargs)
    }
  }
}

//Provides: caml_call_gen (const, shallow)
//Requires: caml_invalid_argument
//Weakdef
// args is a non-empty array
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  if(typeof f !== "function") caml_invalid_argument("caml_call_gen: too many arguments");
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
    switch(d) {
    case 1: return function (a1) {
      return f.apply(null,args.concat([a1]))
    }
    case 2: return function (a1,a2) {
      return f.apply(null,args.concat([a1,a2]))
    }
    case 3: return function (a1,a2,a3) {
      return f.apply(null,args.concat([a1,a2,a3]))
    }
    case 4: return function (a1,a2,a3,a4) {
      return f.apply(null,args.concat([a1,a2,a3,a4]))
    }
    case 5: return function (a1,a2,a3,a4,a5) {
      return f.apply(null,args.concat([a1,a2,a3,a4,a5]))
    }
    case 6: return function (a1,a2,a3,a4,a5,a6) {
      return f.apply(null,args.concat([a1,a2,a3,a4,a5,a6]))
    }
    case 7: return function (a1,a2,a3,a4,a5,a6,a7) {
      return f.apply(null,args.concat([a1,a2,a3,a4,a5,a6,a7]))
    }
    default: return function (a1,a2,a3,a4,a5,a6,a7,a8) {
      return caml_call_gen(f,args.concat([a1,a2,a3,a4,a5,a6,a7,a8]))
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

//Provides: caml_is_printable const (const)
function caml_is_printable(c) { return +(c > 31 && c < 127); }
