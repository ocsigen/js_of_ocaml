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
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  //FIXME, can happen with too many arguments
  if(typeof f !== "function") return f;
  var n = f.length | 0;
  if(n === 0) return f(...args);
  var argsLen = args.length | 0;
  var d = n - argsLen | 0;
  if (d == 0)
    return f(...args);
  else if (d < 0) {
    return caml_call_gen(f(...args.slice(0,n)),args.slice(n));
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
