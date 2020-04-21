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
  var args_copied = false;

  while (true) {
    if (f.fun) {
      f = f.fun;
      continue;
    }
    // TODO: This can happen with over-application. Should we fail here ?
    if (typeof f !== "function") return f;
    var n = f.length | 0;
    var argsLen = args.length | 0;
    var d = (n - argsLen) | 0;

    if (d === 0) {
      return f(...args);
    }
    else if (d < 0) {
      if (!args_copied) {
        args = args.slice();
        args_copied = true;
      }

      var before = args;
      var after = before.splice(n);
      f = f(...before);
      args = after;
    }
    else {
      switch (d) {
      case 1: return function (a1) {
        return f(...args, a1);
      }
      case 2: return function (a1, a2) {
        return f(...args, a1, a2);
      }
      case 3: return function (a1, a2, a3) {
        return f(...args, a1, a2, a3);
      }
      case 4: return function (a1, a2, a3, a4) {
        return f(...args, a1, a2, a3, a4);
      }
      case 5: return function (a1, a2, a3, a4, a5) {
        return f(...args, a1, a2, a3, a4, a5);
      }
      case 6: return function (a1, a2, a3, a4, a5, a6) {
        return f(...args, a1, a2, a3, a4, a5, a6);
      }
      case 7: return function (a1, a2, a3, a4, a5, a6, a7) {
        return f(...args, a1, a2, a3, a4, a5, a6, a7);
      }
      default:
        return function (a1, a2, a3, a4, a5, a6, a7, a8) {
          return caml_call_gen(f, args.concat([a1, a2, a3, a4, a5, a6, a7, a8]));
        };
      }
    }
  }
}
