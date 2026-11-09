// Js_of_ocaml
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

// Calling a function stored in a block (typically a module field):
// js_of_ocaml used to emit [M[1].call(null, x, y)] rather than
// [M[1](x, y)], so that the call is a plain function call and not a method
// call with the block as receiver (see README.md and generate.ml,
// [apply_directly]).

const { main } = require("../common.js");

// A module block with 8 functions, as produced for a compilation unit
function make_module() {
  const M = [0];
  for (let k = 0; k < 8; k++)
    M.push(
      new Function("x", "y", `"use strict"; return (x * ${k + 3} + y) | 0;`),
    );
  return M;
}

const ITERS = 2e7;

const variants = [
  {
    name: "method-mono", // M[1](x, y)
    make: () => {
      const M = make_module();
      return (n) => {
        let acc = 0;
        for (let i = 0; i < n; i++) acc = M[1](i, acc);
        return acc;
      };
    },
  },
  {
    name: "call-null-mono", // M[1].call(null, x, y)   (js_of_ocaml before #2613)
    make: () => {
      const M = make_module();
      return (n) => {
        let acc = 0;
        for (let i = 0; i < n; i++) acc = M[1].call(null, i, acc);
        return acc;
      };
    },
  },
  {
    name: "comma-mono", // (0, M[1])(x, y)
    make: () => {
      const M = make_module();
      return (n) => {
        let acc = 0;
        for (let i = 0; i < n; i++) acc = (0, M[1])(i, acc);
        return acc;
      };
    },
  },
  {
    name: "method-poly", // M[k](x, y), 8 different callees at one site
    make: () => {
      const M = make_module();
      return (n) => {
        let acc = 0;
        for (let i = 0; i < n; i++) acc = M[1 + (i & 7)](i, acc);
        return acc;
      };
    },
  },
  {
    name: "call-null-poly", // M[k].call(null, x, y)
    make: () => {
      const M = make_module();
      return (n) => {
        let acc = 0;
        for (let i = 0; i < n; i++) acc = M[1 + (i & 7)].call(null, i, acc);
        return acc;
      };
    },
  },
].map((v) => ({ iters: ITERS, ...v }));

main(variants, __filename);
