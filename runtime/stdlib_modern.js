// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
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
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  //FIXME, can happen with too many arguments
  if(typeof f !== "function") return f;
  var n = (f.l >= 0)?f.l:(f.l = f.length);
  if(n === 0) return f(...args);
  var argsLen = args.length | 0;
  var d = n - argsLen | 0;
  if (d == 0)
    return f(...args);
  else if (d < 0) {
    return caml_call_gen(f(...args.slice(0,n)),args.slice(n));
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
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  //FIXME, can happen with too many arguments
  if(typeof f !== "function") return args[args.length-1](f);
  var n = (f.l >= 0)?f.l:(f.l = f.length);
  if(n === 0) return f(...args);
  var argsLen = args.length | 0;
  var d = n - argsLen | 0;
  if (d == 0)
    return f(...args);
  else if (d < 0) {
    var rest = args.slice(n - 1);
    var k = args [argsLen - 1];
    args = args.slice(0, n);
    args[n - 1] = function (g) {
      var args = rest.slice();
      args[args.length - 1] = k;
      return caml_call_gen(g, args); };
    return f(...args);
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
