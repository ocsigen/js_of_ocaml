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
  var n = f.l >= 0 ? f.l : (f.l = f.length);
  var argsLen = args.length;
  var d = n - argsLen;
  if (d === 0) return f(...args);
  else if (d < 0) {
    var g = f(...args.slice(0, n));
    if (typeof g !== "function") return g;
    return caml_call_gen(g, args.slice(n));
  } else {
    switch (d) {
      case 1: {
        var g = function (x) {
          var nargs = new Array(argsLen + 1);
          for (var i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          return f(...nargs);
        };
        break;
      }
      case 2: {
        var g = function (x, y) {
          var nargs = new Array(argsLen + 2);
          for (var i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          nargs[argsLen + 1] = y;
          return f(...nargs);
        };
        break;
      }
      default: {
        var g = function (...extra_args) {
          if (extra_args.length === 0) extra_args = [undefined];
          return caml_call_gen(f, args.concat(extra_args));
        };
      }
    }
    g.l = d;
    return g;
  }
}

//Provides: caml_call_gen (const, shallow)
//If: effects
//If: !doubletranslate
//Weakdef
function caml_call_gen(f, args) {
  var n = f.l >= 0 ? f.l : (f.l = f.length);
  var argsLen = args.length;
  var d = n - argsLen;
  if (d === 0) {
    return f(...args);
  } else if (d < 0) {
    var rest = args.slice(n - 1);
    var k = args[argsLen - 1];
    args = args.slice(0, n);
    args[n - 1] = function (g) {
      if (typeof g !== "function") return k(g);
      var args = rest.slice();
      args[args.length - 1] = k;
      return caml_call_gen(g, args);
    };
    return f(...args);
  } else {
    argsLen--;
    var k = args[argsLen];
    switch (d) {
      case 1: {
        var g = function (x, y) {
          var nargs = new Array(argsLen + 2);
          for (var i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          nargs[argsLen + 1] = y;
          return f(...nargs);
        };
        break;
      }
      case 2: {
        var g = function (x, y, z) {
          var nargs = new Array(argsLen + 3);
          for (var i = 0; i < argsLen; i++) nargs[i] = args[i];
          nargs[argsLen] = x;
          nargs[argsLen + 1] = y;
          nargs[argsLen + 2] = z;
          return f(...nargs);
        };
        break;
      }
      default: {
        args.length = argsLen;
        var g = function (...extra_args) {
          if (extra_args.length === 0) extra_args = [undefined];
          return caml_call_gen(f, args.concat(extra_args));
        };
      }
    }
    g.l = d + 1;
    return k(g);
  }
}

//Provides: caml_call_gen_cps
//Requires: caml_call_gen
//If: effects
//If: !doubletranslate
//Weakdef
var caml_call_gen_cps = caml_call_gen;

//Provides: caml_call_gen_tuple (const, shallow)
//Requires: caml_cps_closure
//If: effects
//If: doubletranslate
//Weakdef
var caml_call_gen_tuple = (function () {
  function caml_call_gen_direct(f, args) {
    var n = f.l >= 0 ? f.l : (f.l = f.length);
    var argsLen = args.length;
    var d = n - argsLen;
    if (d === 0) {
      return f.apply(null, args);
    } else if (d < 0) {
      var g = f(...args.slice(0, n));
      if (typeof g !== "function") return g;
      return caml_call_gen_direct(g, args.slice(n));
    } else {
      // FIXME: Restore the optimization of handling specially d = 1 or 2
      var args_ = args.slice();
      args_.length = argsLen;
      var ret = caml_cps_closure(
        function (...extra_args) {
          if (extra_args.length === 0) extra_args = [undefined];
          return caml_call_gen_direct(f, args.concat(extra_args));
        },
        function (...extra_args) {
          if (extra_args.length === 0) extra_args = [undefined];
          return caml_call_gen_cps(f, args_.concat(extra_args));
        },
      );
      ret.l = d;
      ret.cps.l = d + 1;
      return ret;
    }
  }
  function caml_call_gen_cps(f, args) {
    if (!f.cps) {
      var k = args.pop();
      return k(caml_call_gen_direct(f, args));
    }
    var n = f.cps.l >= 0 ? f.cps.l : (f.cps.l = f.cps.length);
    var argsLen = args.length;
    var d = n - argsLen;
    if (d === 0) {
      return f.cps.apply(null, args);
    } else if (d < 0) {
      var rest = args.slice(n - 1);
      var k = args[argsLen - 1];
      args = args.slice(0, n);
      args[n - 1] = function (g) {
        var args = rest.slice();
        args[args.length - 1] = k;
        return caml_call_gen_cps(g, args);
      };
      return f.cps.apply(null, args);
    } else {
      argsLen--;
      var args_ = args.slice();
      args_.length = argsLen;
      var cont = caml_cps_closure(
        function (...extra_args) {
          if (extra_args.length === 0) extra_args = [undefined];
          return caml_call_gen_direct(f, args_.concat(extra_args));
        },
        function (...extra_args) {
          if (extra_args.length === 0) extra_args = [undefined];
          return caml_call_gen_cps(f, args_.concat(extra_args));
        },
      );
      var k = args[argsLen];
      cont.l = d;
      cont.cps.l = d + 1;
      return k(cont);
    }
  }
  return [caml_call_gen_direct, caml_call_gen_cps];
})();

//Provides: caml_call_gen
//Requires: caml_call_gen_tuple
//If: effects
//If: doubletranslate
//Weakdef
var caml_call_gen = caml_call_gen_tuple[0];

//Provides: caml_call_gen_cps
//Requires: caml_call_gen_tuple
//If: effects
//If: doubletranslate
//Weakdef
var caml_call_gen_cps = caml_call_gen_tuple[1];

//Provides: caml_named_values
var caml_named_values = {};

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
var caml_global_data = [0];

//Provides: caml_link_info
var caml_link_info = {};

//Provides: caml_set_link_info
//Requires: caml_link_info
function caml_set_link_info(info) {
  caml_link_info.sections = info[1];
  caml_link_info.symbols = info[2];
  caml_link_info.prim_count = info[3];
  caml_link_info.aliases = info[4];
}

//Provides: caml_build_symbols
//Requires: caml_jsstring_of_string
function caml_build_symbols(symb) {
  var r = {};
  var max = -1;
  if (symb) {
    for (var i = 1; i < symb.length; i++) {
      var gn = symb[i][1];
      var is_predef = gn[0];
      var name = caml_jsstring_of_string(gn[1]);
      var idx = symb[i][2];
      max = Math.max(max, idx);
      r[is_predef ? "predef:" + name : name] = idx;
    }
  }
  r.next_idx = max + 1;
  return r;
}

//Provides: jsoo_toplevel_reloc
var jsoo_toplevel_reloc;

//Provides: caml_register_global_by_index (shallow, const)
//Requires: caml_global_data
function caml_register_global_by_index(v, idx) {
  caml_global_data[idx + 1] = v;
}

//Provides: caml_register_global (shallow, const)
//Requires: caml_global_data, caml_callback, caml_build_symbols
//Requires: caml_link_info
//Requires: jsoo_toplevel_reloc
function caml_register_global(v, name) {
  if (jsoo_toplevel_reloc) {
    var n = caml_callback(jsoo_toplevel_reloc, [[0, name]]);
    caml_global_data[n + 1] = v;
  } else if (caml_link_info.symbols) {
    if (!caml_link_info.symidx) {
      caml_link_info.symidx = caml_build_symbols(caml_link_info.symbols);
    }
    var nid = caml_link_info.symidx[name];
    if (nid === undefined) {
      // The unit is unknown, this can happen when dynlinking a precompiled js,
      // let's allocate a fresh idx.
      nid = caml_link_info.symidx.next_idx++;
      caml_link_info.symidx[name] = nid;
    }
    caml_global_data[nid + 1] = v;
  }
  caml_global_data[name] = v;
}

//Provides: caml_register_global_predef (shallow, const)
//Requires: caml_global_data, caml_callback, caml_build_symbols
//Requires: caml_link_info
//Requires: jsoo_toplevel_reloc
function caml_register_global_predef(v, name) {
  var key = "predef:" + name;
  if (jsoo_toplevel_reloc) {
    var n = caml_callback(jsoo_toplevel_reloc, [[1, name]]);
    caml_global_data[n + 1] = v;
  } else if (caml_link_info.symbols) {
    if (!caml_link_info.symidx) {
      caml_link_info.symidx = caml_build_symbols(caml_link_info.symbols);
    }
    var nid = caml_link_info.symidx[key];
    if (nid === undefined) {
      nid = caml_link_info.symidx.next_idx++;
      caml_link_info.symidx[key] = nid;
    }
    caml_global_data[nid + 1] = v;
  }
  caml_global_data[key] = v;
}

//Provides: caml_get_global (mutable)
//Requires: caml_global_data
function caml_get_global(name) {
  return caml_global_data[name];
}

//Provides: caml_get_global_predef (mutable)
//Requires: caml_global_data
function caml_get_global_predef(name) {
  return caml_global_data["predef:" + name];
}

//Provides: caml_get_global_data mutable
//Requires: caml_global_data
function caml_get_global_data(_unit) {
  return caml_global_data;
}

//Provides: caml_is_printable const (const)
function caml_is_printable(c) {
  return +(c > 31 && c < 127);
}

//Provides: caml_maybe_print_stats
function caml_maybe_print_stats(_unit) {
  return 0;
}
