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
//If: !effects
//Weakdef
function caml_call_gen(f, args) {
  var n = (f.l >= 0)?f.l:(f.l = f.length);
  var argsLen = args.length;
  var d = n - argsLen;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0) {
    var g = f.apply(null,args.slice(0,n));
    if(typeof g !== "function") return g;
    return caml_call_gen(g,args.slice(n));
  }
  else {
    switch (d) {
    case 1: {
      var g = function (x){
        var nargs = new Array(argsLen + 1);
        for(var i = 0; i < argsLen; i++ ) nargs[i] = args[i];
        nargs[argsLen] = x;
        return f.apply(null, nargs)
      };
      break;
    }
    case 2: {
      var g = function (x, y){
        var nargs = new Array(argsLen + 2);
        for(var i = 0; i < argsLen; i++ ) nargs[i] = args[i];
        nargs[argsLen] = x;
        nargs[argsLen + 1] = y;
        return f.apply(null, nargs)
      };
      break;
    }
    default: {
      var g = function (){
        var extra_args = (arguments.length == 0)?1:arguments.length;
        var nargs = new Array(args.length+extra_args);
        for(var i = 0; i < args.length; i++ ) nargs[i] = args[i];
        for(var i = 0; i < arguments.length; i++ ) nargs[args.length+i] = arguments[i];
        return caml_call_gen(f, nargs)
      };
    }}
    g.l = d;
    return g;
  }
}

//Provides: caml_call_gen (const, shallow)
//If: effects
//Weakdef
function caml_call_gen(f, args) {
  var n = (f.l >= 0)?f.l:(f.l = f.length);
  var argsLen = args.length;
  var d = n - argsLen;
  if (d == 0) {
    return f.apply(null, args);
  } else if (d < 0) {
    var rest = args.slice(n - 1);
    var k = args [argsLen - 1];
    args = args.slice(0, n);
    args[n - 1] = function (g) {
      if (typeof g !== "function") return k(g);
      var args = rest.slice();
      args[args.length - 1] = k;
      return caml_call_gen(g, args); };
    return f.apply(null, args);
  } else {
    argsLen--;
    var k = args [argsLen];
    switch (d) {
    case 1: {
      var g = function (x, y){
        var nargs = new Array(argsLen + 2);
        for(var i = 0; i < argsLen; i++ ) nargs[i] = args[i];
        nargs[argsLen] = x;
        nargs[argsLen + 1] = y;
        return f.apply(null, nargs)
      };
      break;
    }
    case 2: {
      var g = function (x, y, z){
        var nargs = new Array(argsLen + 3);
        for(var i = 0; i < argsLen; i++ ) nargs[i] = args[i];
        nargs[argsLen] = x;
        nargs[argsLen + 1] = y;
        nargs[argsLen + 2] = z;
        return f.apply(null, nargs)
      };
      break;
    }
    default: {
      var g = function (){
        var extra_args = (arguments.length == 0)?1:arguments.length;
        var nargs = new Array(argsLen + extra_args);
        for(var i = 0; i < argsLen; i++ ) nargs[i] = args[i];
        for(var i = 0; i < arguments.length; i++ )
          nargs[argsLen + i] = arguments[i];
        return caml_call_gen(f, nargs)
      };
    }}
    g.l = d + 1;
    return k(g);
  }
}

//Provides: caml_named_values
var caml_named_values = {};

//Provides: caml_register_named_value (const,mutable)
//Requires: caml_named_values, caml_jsbytes_of_string
function caml_register_named_value(nm,v) {
  nm = caml_jsbytes_of_string(nm);
  if (!caml_named_values.hasOwnProperty(nm)) caml_named_values[nm] = v;
  return 0;
}

//Provides: caml_unregister_named_value (const)
//Requires: caml_named_values, caml_jsbytes_of_string
function caml_unregister_named_value(nm) {
  nm = caml_jsbytes_of_string(nm);
  delete caml_named_values[nm];
  return 0;
}

//Provides: caml_named_value
//Requires: caml_named_values
function caml_named_value(nm) {
  return caml_named_values[nm]
}

//Provides: caml_global_data
var caml_global_data = [0];

//Provides: caml_build_symbols
//Requires: caml_jsstring_of_string
function caml_build_symbols(toc) {
  var symb;
  while(toc) {
    if(caml_jsstring_of_string(toc[1][1]) == "SYJS") {
      symb = toc[1][2];
      break;
    }
    else toc = toc[2]
  }
  var r = {};
  if(symb) {
    for(var i = 1; i < symb.length; i++){
      r[caml_jsstring_of_string(symb[i][1])] = symb[i][2]
    }
  }
  return r;
}

//Provides: caml_register_global (const, shallow, const)
//Requires: caml_global_data, caml_callback, caml_build_symbols
//Requires: caml_failwith
function caml_register_global (n, v, name_opt) {
  if (name_opt) {
    var name = name_opt;
    if(globalThis.toplevelReloc) {
      n = caml_callback(globalThis.toplevelReloc, [name]);
    }
    else if (caml_global_data.toc) {
      if(!caml_global_data.symbols) {
        caml_global_data.symbols = caml_build_symbols(caml_global_data.toc)
      }
      var nid = caml_global_data.symbols[name]
      if(nid >= 0)
        n = nid
      else {
        caml_failwith("caml_register_global: cannot locate " + name);
      }
    }
  }
  caml_global_data[n + 1] = v;
  if(name_opt) caml_global_data[name_opt] = v;
}

//Provides: caml_get_global_data mutable
//Requires: caml_global_data
function caml_get_global_data () { return caml_global_data; }

//Provides: caml_is_printable const (const)
function caml_is_printable(c) { return +(c > 31 && c < 127); }

//Provides: caml_maybe_print_stats
function caml_maybe_print_stats(unit) { return 0 }
