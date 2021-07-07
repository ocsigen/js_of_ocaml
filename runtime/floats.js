// Js_of_ocaml runtime support
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

//Provides: caml_acosh_float
function caml_acosh_float(x) {
  return Math.acosh(x);
}

//Provides: caml_asinh_float
function caml_asinh_float(x) {
  return Math.asinh(x);
}

//Provides: caml_atanh_float
function caml_atanh_float(x) {
  return Math.atanh(x);
}

//Provides: caml_cbrt_float
function caml_cbrt_float(x) {
  return Math.cbrt(x);
}

//Provides: caml_erf_float
function caml_erf_float(x) {
  var a1 = 0.254829592;
  var a2 = -0.284496736;
  var a3 = 1.421413741;
  var a4 = -1.453152027;
  var a5 = 1.061405429;
  var p = 0.3275911;

  var sign = 1;
  if (x < 0) {
    sign = -1;
  }
  x = Math.abs(x);
  var t = 1.0 / (1.0 + p * x);
  var y = 1.0 - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t + Math.exp(-x * x);

  return sign * y;
}

//Provides: caml_erfc_float
//Requires: caml_erf_float
function caml_erfc_float(x) {
  return 1 - caml_erf_float(x);
}

//Provides: caml_exp2_float
function caml_exp2_float(x) {
  return Math.pow(2, x);
}

//Provides: caml_fma_float
function caml_fma_float(x) {
  // TODO: Implement this
}

//Provides: caml_log2_float
function caml_log2_float(x) {
  return Math.log2(x)
}

