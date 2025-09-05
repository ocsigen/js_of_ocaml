(***************************************************************************)
(*  Copyright (C) 2000-2013 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published       *)
(* by the Free Software Foundation, either version 3 of the License,       *)
(* or (at your option) any later version.                                  *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.   *)
(***************************************************************************)

let p, a1, a2, a3, a4, a5 = 0.3275911, 0.254829592, (-0.284496736), 1.421413741, (-1.453152027), 1.061405429
let erf x =
  let t = 1. /. (1. +. p *. x) in
  let t2 = t *. t in
  let t3 = t *. t2 in
  let t4 = t *.t3 in
  let t5 = t *. t4 in
  1. -. (a1 *. t +. a2 *. t2 +. a3 *. t3 +. a4 *. t4 +. a5 *. t5) *. exp (-.x *. x)

(** Unit gaussian CDF.  *)
let ugaussian_P x =
  let u = x /. sqrt 2. in
  let erf = if u < 0. then -.erf (-.u) else erf u in
  0.5 *. (1. +. erf)

module Rootfinder = struct
  type error =
    | NotbracketedBelow
    | NotbracketedAbove

  let string_of_error = function
    | NotbracketedBelow -> "Interval borders have both negative values."
    | NotbracketedAbove -> "Interval borders have both positive values."

  exception Rootfinder of error

  let brent a b eval eps =
    if a > b then invalid_arg "Math.brent: arguments should verify xlo <= xhi.";
    let fa, fb = eval a, eval b in
    if 0. < fa *. fb  then raise(Rootfinder(if fa < 0. then NotbracketedBelow else NotbracketedAbove));
    assert (0. <= eps);
    let maxit = 10_000 in

    let a, fa, b, fb = if abs_float fa < abs_float fb then b, fb, a, fa else a, fa, b, fb in

    (* The root is between a and b, such that |f(b)| < |f(a)| *)
    let rec iter i ~a ~b ~c ~d ~fa ~fb ~fc mflag =
      if abs_float (b -. a) < eps || fb = 0. || maxit < i then b (* stop condition *)
      else
        let s =
          if fa <> fc && fb <> fc then
            a *. fb *. fc /. ((fa -. fb) *. (fa -. fc)) +. b *. fa *. fc /. ((fb -. fa) *. (fb -. fc)) +. c *. fa *. fb /. ((fc -. fa) *. (fc -. fb)) (* inverse quadratic interpolation *)
          else
            b -. fb *. (b -. a) /. (fb -. fa) (* secant rule *)
        in
        let s, mflag =
          if
            (4. *. s < 3. *. a +. b || b < s) ||
            (mflag && 2. *. abs_float (s -. b) >= abs_float (b -. c)) ||
            (not mflag && 2. *. abs_float (s -. b) >= abs_float (c -. d)) ||
            (mflag && abs_float (b -. c) < eps) ||
            (not mflag && abs_float (c -. d) < eps)
          then 0.5 *. (a +. b), true else s, false
        in
        let fs = eval s in
        (* d <- c; c <- b; *)
        if fa *. fs < 0. then (* in this case, b <- s *)
          if abs_float fa < abs_float fs then iter (i + 1) ~a: s ~b: a ~c: b ~d: c ~fa: fs ~fb: fa ~fc: fb mflag (* switch a, b *)
          else iter (i + 1) ~a ~b: s ~c: b ~d: c ~fa ~fb: fs ~fc: fb mflag
        else (* in this case, a <- s *)
          if abs_float fs < abs_float fb then iter (i + 1) ~a: b ~b: s ~c: b ~d: c ~fa: fb ~fb: fs ~fc: fb mflag (* switch a, b *)
          else iter (i + 1) ~a: s ~b ~c: b ~d: c ~fa: fs ~fb ~fc: fb mflag
    in
    iter 0 ~a ~b ~c: a ~d: nan ~fa ~fb ~fc: fa true

end

module Gaussian_quadrature =
  struct
    let gauss_hermite_coefficients =
      [|
       0.; 0.6568095668820999044613; -0.6568095668820997934390; -1.3265570844949334805563;  1.3265570844949330364670;  2.0259480158257567872226;
       -2.0259480158257558990442; -2.7832900997816496513337;  2.7832900997816474308877;  3.6684708465595856630159; -3.6684708465595838866591
      |],
      [|
       0.6547592869145917315876; 0.6609604194409607336169; 0.6609604194409606225946; 0.6812118810666693002887; 0.6812118810666689672217; 0.7219536247283847574252;
       0.7219536247283852015144; 0.8025168688510405656800; 0.8025168688510396775015; 1.0065267861723647957461; 1.0065267861723774522886
      |]
  end
