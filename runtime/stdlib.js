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
  const n = f.l >= 0 ? f.l : (f.l = f.length);
  const argsLen = args.length;
  const d = n - argsLen;
  if (d === 0) return f.apply(null, args);
  if (d < 0) {
    const g = f.apply(null, args.slice(0, n));
    if (typeof g !== "function") return g;
    return caml_call_gen(g, args.slice(n));
  }
  let g;
  switch (d) {
    case 1: {
      g = (x) => {
        const nargs = new Array(argsLen + 1);
        for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
        nargs[argsLen] = x;
        return f.apply(null, nargs);
      };
      break;
    }
    case 2: {
      g = (x, y) => {
        const nargs = new Array(argsLen + 2);
        for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
        nargs[argsLen] = x;
        nargs[argsLen + 1] = y;
        return f.apply(null, nargs);
      };
      break;
    }
    default: {
      g = (...extra_args) => {
        const nargs = new Array(args.length + extra_args.length);
        for (let i = 0; i < args.length; i++) nargs[i] = args[i];
        for (let i = 0; i < extra_args.length; i++)
          nargs[args.length + i] = extra_args[i];
        return caml_call_gen(f, nargs);
      };
    }
  }
  g.l = d;
  return g;
}

//Provides: caml_call_gen (const, shallow)
//If: effects
//Weakdef
function caml_call_gen(f, args) {
  const n = f.l >= 0 ? f.l : (f.l = f.length);
  let argsLen = args.length;
  const d = n - argsLen;
  if (d === 0) {
    return f.apply(null, args);
  }
  if (d < 0) {
    const rest = args.slice(n - 1);
    const k = args[argsLen - 1];
    args = args.slice(0, n);
    args[n - 1] = (g) => {
      if (typeof g !== "function") return k(g);
      const args = rest.slice();
      args[args.length - 1] = k;
      return caml_call_gen(g, args);
    };
    return f.apply(null, args);
  }
  argsLen--;
  const k = args[argsLen];
  let g;
  switch (d) {
    case 1: {
      g = (x, y) => {
        const nargs = new Array(argsLen + 2);
        for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
        nargs[argsLen] = x;
        nargs[argsLen + 1] = y;
        return f.apply(null, nargs);
      };
      break;
    }
    case 2: {
      g = (x, y, z) => {
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
      g = (...extra_args) => {
        const nargs = new Array(argsLen + extra_args.length);
        for (let i = 0; i < argsLen; i++) nargs[i] = args[i];
        for (let i = 0; i < extra_args.length; i++)
          nargs[argsLen + i] = extra_args[i];
        return caml_call_gen(f, nargs);
      };
    }
  }
  g.l = d + 1;
  return k(g);
}

//Provides: caml_named_values
const caml_named_values = {};

//Provides: caml_register_named_value (const,mutable)
//Requires: caml_named_values, caml_jsbytes_of_string
function caml_register_named_value(nm, v) {
  caml_named_values[caml_jsbytes_of_string(nm)] = v;
  return 0;
}

//Provides: caml_named_value
//Requires: caml_named_values
function caml_named_value(nm) {
  return caml_named_values[nm];
}

//Provides: caml_global_data
const caml_global_data = [0];

//Provides: caml_build_symbols
//Requires: caml_jsstring_of_string
function caml_build_symbols(symb) {
  const r = {};
  if (symb) {
    for (let i = 1; i < symb.length; i++) {
      r[caml_jsstring_of_string(symb[i][1])] = symb[i][2];
    }
  }
  return r;
}

//Provides: caml_register_global (const, shallow, const)
//Requires: caml_global_data, caml_callback, caml_build_symbols
//Requires: caml_failwith
function caml_register_global(n, v, name_opt) {
  if (name_opt) {
    const name = name_opt;
    if (globalThis.toplevelReloc) {
      n = caml_callback(globalThis.toplevelReloc, [name]);
    } else if (caml_global_data.symbols) {
      if (!caml_global_data.symidx) {
        caml_global_data.symidx = caml_build_symbols(caml_global_data.symbols);
      }
      const nid = caml_global_data.symidx[name];
      if (nid >= 0) n = nid;
      else {
        caml_failwith(`caml_register_global: cannot locate ${name}`);
      }
    }
  }
  caml_global_data[n + 1] = v;
  if (name_opt) caml_global_data[name_opt] = v;
}

//Provides: caml_get_global_data mutable
//Requires: caml_global_data
function caml_get_global_data() {
  return caml_global_data;
}

//Provides: caml_is_printable const (const)
function caml_is_printable(c) {
  return +(c > 31 && c < 127);
}

//Provides: caml_maybe_print_stats
function caml_maybe_print_stats(unit) {
  return 0;
}
