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
  const n = f.l >= 0 ? f.l : (f.l = f.length);
  const argsLen = args.length;
  const d = n - argsLen;
  if (d == 0) return f(...args);
  else if (d < 0) {
    const g = f(...args.slice(0, n));
    if (typeof g !== "function") return g;
    return caml_call_gen(g, args.slice(n));
  } else {
    let g;
    switch (d) {
      case 1: {
        g = function (x) {
          const nargs = new Array(argsLen + 1);
          for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          return f.apply(null, nargs);
        };
        break;
      }
      case 2: {
        g = function (x, y) {
          const nargs = new Array(argsLen + 2);
          for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          nargs[argsLen + 1] = y;
          return f.apply(null, nargs);
        };
        break;
      }
      default: {
        g = function () {
          const extra_args = arguments.length == 0 ? 1 : arguments.length;
          const nargs = new Array(args.length + extra_args);
          for (let i = 0; i < args.length; i++) nargs[i] = args[i];
          for (let i = 0; i < arguments.length; i++)
            nargs[args.length + i] = arguments[i];
          return caml_call_gen(f, nargs);
        };
      }
    }
    g.l = d;
    return g;
  }
}

//Provides: caml_call_gen (const, shallow)
//If: effects
function caml_call_gen(f, args) {
  const n = f.l >= 0 ? f.l : (f.l = f.length);
  let argsLen = args.length;
  const d = n - argsLen;
  if (d == 0) return f(...args);
  else if (d < 0) {
    const rest = args.slice(n - 1);
    const k = args[argsLen - 1];
    args = args.slice(0, n);
    args[n - 1] = function (g) {
      if (typeof g !== "function") return k(g);
      const args = rest.slice();
      args[args.length - 1] = k;
      return caml_call_gen(g, args);
    };
    return f(...args);
  } else {
    argsLen--;
    const k = args[argsLen];
    let g;
    switch (d) {
      case 1: {
        g = function (x, y) {
          const nargs = new Array(argsLen + 2);
          for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          nargs[argsLen + 1] = y;
          return f.apply(null, nargs);
        };
        break;
      }
      case 2: {
        g = function (x, y, z) {
          const nargs = new Array(argsLen + 3);
          for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          nargs[argsLen + 1] = y;
          nargs[argsLen + 2] = z;
          return f.apply(null, nargs);
        };
        break;
      }
      default: {
        g = function () {
          const extra_args = arguments.length == 0 ? 1 : arguments.length;
          const nargs = new Array(argsLen + extra_args);
          for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
          for (let i = 0; i < arguments.length; i++)
            nargs[argsLen + i] = arguments[i];
          return caml_call_gen(f, nargs);
        };
      }
    }
    g.l = d + 1;
    return k(g);
  }
}
