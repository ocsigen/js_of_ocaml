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

(* A 2-factor interest-rate model. Reference book: Brigo and Mercurio "Interest Rate Models" 2005 *)

open Date

let pi = 2. *. asin 1.

type extended_swaption =
    {
     maturity: date;
     strike: float;
     swap_schedule: (date * date) list;
    }

type swap_frequency =
  | Freq_3M
  | Freq_6M
  | Freq_1Y

type swaption =
    {
     swaption_maturity_in_year: int;
     swap_frequency: swap_frequency;
     swap_term_in_year: int;
    }

let extended_swaption_of_swaption ~today ~zc {swaption_maturity_in_year; swap_frequency; swap_term_in_year} =
  let maturity = add_years today swaption_maturity_in_year in
  let frq =
    match swap_frequency with
    | Freq_3M -> 3
    | Freq_6M -> 6
    | Freq_1Y -> 12
  in
  let nschedule = 12 * swap_term_in_year / frq in
  let swap_schedule = Array.init nschedule (fun i -> add_months maturity (i * frq), add_months maturity ((i + 1) * frq)) in
  let swap_schedule = Array.to_list swap_schedule in
  let lvl, t0, tn =
    List.fold_left
      (fun (acc_lvl, acc_t0, acc_tn) (tf, tp) ->
        acc_lvl +. zc tp *. (act_365 tp tf), min acc_t0 tf, max acc_tn tp
      ) (0., max_date, min_date) swap_schedule
  in
  let strike = (zc t0 -. zc tn) /. lvl in (* atm strike *)
  {maturity; swap_schedule; strike}


type parameters =
    {
     g_a : float;
     g_b : float;
     g_sigma : float;
     g_nu : float;(* nu is eta in Brigo and Mercurio *)
     g_rho : float;
    }

(* Places with ** comments should take account of sigma=0 case *)

let b_fun z tau = (* ** *)
  (1. -. exp (-. z *. tau)) /. z

exception Sdomain

(* \var(\int_t^T [x(u)+y(u)]du) *)
let bigv {g_a; g_b; g_rho; g_nu; g_sigma} tau  = (* 4.10 p 135 *)
  let t sigma x =
    let expxtau = exp (-. x *. tau) in (* ** *)
    let exp2xtau = expxtau *. expxtau in (* ** *)
    sigma *. sigma /. (x *. x) *.
      (tau +. 2. /. x *. expxtau -. 1. /. (2. *. x) *. exp2xtau -. 3. /. (2. *. x))
  in
  let t1 = t g_sigma g_a in
  let t2 = t g_nu g_b in
  let ba = b_fun g_a tau in
  let bb = b_fun g_b tau in
  let t3 = (* ** *)
    2. *. g_rho *. g_nu *. g_sigma /. (g_a *. g_b) *.
      (tau -. ba -. bb +. b_fun (g_a +. g_b) tau)
  in
  t1 +. t2 +. t3, ba, bb

(* x drift term in tmat-forward measure *)
let bigmx {g_a; g_b; g_rho; g_nu; g_sigma} today tmat s t =
  let ts = act_365 t s in
  let tmatt = act_365 tmat t in
  let tmat0  = act_365 tmat today in
  let tmats = act_365 tmat s in
  let t0 = act_365 t today in
  let s0 = act_365 s today in
  (g_sigma *. g_sigma /. (g_a *. g_a) +. g_sigma *. g_rho *. g_nu /. (g_a *. g_b)) *. (1. -. exp (-. g_a *. ts)) -.
    (g_sigma *. g_sigma /. (2. *. g_a *. g_a) *. (exp (-. g_a *. tmatt) -. exp (-. g_a *. (tmats +. ts)))) -.
    g_rho *. g_sigma *. g_nu /. (g_b *. (g_a +. g_b)) *. (exp (-. g_b *. tmatt) -. exp (-. g_b *. tmat0 -. g_a *. t0 +. (g_a +. g_b) *. s0))

(* y drift term in tmat-forward measure *)
let bigmy {g_a; g_b; g_rho; g_nu; g_sigma} today tmat s t =
  let ts = act_365 t s in
  let tmatt = act_365 tmat t in
  let tmat0  = act_365 tmat today in
  let tmats = act_365 tmat s in
  let t0 = act_365 t today in
  let s0 = act_365 s today in
  (g_nu *. g_nu /. (g_b *. g_b) +. g_sigma *. g_rho *. g_nu /. (g_a *. g_b)) *. (1. -. exp (-. g_b *. ts)) -.
    (g_nu *. g_nu /. (2. *. g_b *. g_b) *. (exp (-. g_b *. tmatt) -. exp (-. g_b *. (tmats +. ts)))) -.
    g_rho *. g_sigma *. g_nu /. (g_a *. (g_a +. g_b)) *. (exp (-. g_a *. tmatt) -. exp (-. g_a *. tmat0 -. g_b *. t0 +. (g_a +. g_b) *. s0))

let x_quadrature, w_quadrature = Math.Gaussian_quadrature.gauss_hermite_coefficients
let nquadrature = Array.length x_quadrature

let pricer_of_swaption ~today ~zc swaption =
  let swaption = extended_swaption_of_swaption ~today ~zc swaption in
  let maturity = swaption.maturity in
  let strike = swaption.strike in
  let tmat0 = act_365 maturity today in
  let schedulei = Array.of_list swaption.swap_schedule in
  let lastindex = Array.length schedulei - 1 in
  let taui = Array.map (fun (start_date, end_date) -> act_365 end_date start_date) schedulei in
  let ci =
    Array.mapi
      (fun i tau  -> if i = lastindex then 1. +. tau *. strike else tau *. strike)
      taui
  in
  let n_schedi = Array.length schedulei in
  let bai = Array.make n_schedi 0. in
  let bbi = Array.make n_schedi 0. in
  let aici = Array.make n_schedi 0. in
  let log_aici = Array.make n_schedi 0. in
  let scales = Array.make n_schedi 0. in
  let t1_cst = Array.make n_schedi 0. in
  let scale = Array.make n_schedi 0. in
  fun params ->
    let {g_a; g_b; g_rho; g_nu; g_sigma} = params in
    let v0_mat, _, _ = bigv params (act_365 maturity today) in
    let zc_mat = zc maturity in
    let a_fun end_date = (* defined top p138 *)
      let v0_end, _, _ = bigv params (act_365 end_date today) in
      let vt_end, ba, bb = bigv params (act_365 end_date maturity) in
      zc end_date /. zc_mat *. exp (0.5 *. (vt_end -. v0_end +. v0_mat)), ba, bb
    in
    let sigmax = g_sigma *. sqrt (b_fun (2. *. g_a) tmat0) in
    let sigmay = g_nu    *. sqrt (b_fun (2. *. g_b) tmat0) in
    let rhoxy  = g_rho *. g_sigma *. g_nu /. (sigmax *. sigmay) *. (b_fun (g_a +. g_b) tmat0) in
    let rhoxyc = 1. -. rhoxy *. rhoxy in
    let rhoxycs = sqrt rhoxyc in
    let t2 = rhoxy  /. (sigmax *. rhoxycs) in
    let sigmay_rhoxycs = sigmay *. rhoxycs in
    let t4 = rhoxy *. sigmay /. sigmax in
    let mux = -. bigmx params today maturity today maturity in
    let muy = -. bigmy params today maturity today maturity in
    for i = 0 to n_schedi - 1 do
      let a, ba, bb = a_fun (snd schedulei.(i)) in
      let x = ci.(i) *. a in
      let log_ac = log x in
      aici.(i) <- x;
      log_aici.(i) <- log_ac;
      bai.(i) <- ba;
      bbi.(i) <- bb;

      let t3 = muy -. 0.5 *. rhoxyc *. sigmay *. sigmay *. bb in
      let cst = bb *. (mux *. t4 -. t3) in
      t1_cst.(i) <- x *. exp cst;
      scale.(i) <- -. (ba +. bb *. t4);
    done;

    let k = (-3.71901648545568) in (* ugaussian_Pinv 1e-4 *)
    let exact_yhat x =
      (* y_lo x <= yhat x <= y_up x*)
      let lo = ref neg_infinity in
      let up = ref 0. in
      for i = 0 to n_schedi - 1 do
        let baix = bai.(i) *. x in
        lo := max !lo ((log_aici.(i) -. baix) /. bbi.(i));
        up := !up +. aici.(i) *. exp (-. baix)
      done;
      let lo = !lo and s_up = !up in
      if n_schedi = 1 then lo
      else
        let open Math.Rootfinder in
        let up =
          let log_s = log s_up in
          let tmp = log_s /. bbi.(n_schedi - 1) in
          if tmp <= 0. then tmp
          else let tmp = log_s /. bbi.(0) in if 0. <= tmp then tmp
              (* This case happens when all ai * ci are too close to 0. or x to high => to_solve x y is close to -1 for all y,
                 thus the root is reached for y negative with high absolute value (tosolve x is a decreasing function of y) *)
          else neg_infinity
        in
        let yl = lo -. 1e-5 in
        let yu = up +. 1e-5 in
        (* y01 x = y0, y1 / phi(h_i(x, y0)) <= epsilon, 1 - phi(h_i(x, y1)) <= epsilon *)
        let y0 = sigmay *. (rhoxy *. (x -. mux) /. sigmax +. k *. rhoxycs) -. rhoxyc /. g_b +. muy in
        let y1 = sigmay *. (rhoxy *. (x -. mux) /. sigmax -. k *. rhoxycs) +. muy in
        if y1 <= yl then y1 +. 1. (* yhat is greater than y1 => 1 - phi(h_i(x, yhat)) < epsilon *)
        else if yu <= y0 then y0 -. 1. (* yhat is lower than y0 => phi(h_i(x, yhat)) < epsilon *)
        else try
          for i = 0 to n_schedi - 1 do
            scales.(i) <- aici.(i) *. exp (-. bai.(i) *. x);
          done;
          let to_solve yhat = (* eqn at bottom p148 *)
            let sum = ref (-1.) in
            for i = 0 to n_schedi - 1 do
              sum := !sum +. scales.(i) *. exp (-. bbi.(i) *. yhat);
            done;
            assert(!sum = !sum);
            !sum
          in
          brent (max yl y0) (min yu y1) to_solve 1e-4
        with
        | Rootfinder NotbracketedBelow -> y0 -. 1.
        | Rootfinder NotbracketedAbove -> y1 +. 1.
    in
    let yhat =
      let eps = 0.5 *. sigmax in
      let f = exact_yhat mux in
      let df = 0.5 *. (exact_yhat (mux +. eps) -. exact_yhat (mux -. eps)) /. eps in
      fun x -> f +. df *. (x -. mux)
    in

    let integrand x =
      let t1 = exp (-. 0.5 *. ((x -. mux) /. sigmax) ** 2.) in
      let yhat = yhat x in
      let h1 =
        let t1 = (yhat -. muy) /. sigmay_rhoxycs in
        t1 -. t2 *. (x -. mux)
      in
      let t2 = Math.ugaussian_P (-.h1) in
      let acc = ref 0. in
      for i = 0 to n_schedi - 1 do
        let h2 = h1 +. bbi.(i) *. sigmay_rhoxycs in
        acc := !acc +. t1_cst.(i) *. exp (scale.(i) *. x) *. Math.ugaussian_P (-.h2)
      done;
      t1 *. (t2 -. !acc)
    in
    let integral =
      let sqrt2sigmax = sqrt 2. *. sigmax in
      let sum = ref 0. in
      for i = 0 to nquadrature - 1 do
        sum := !sum +. w_quadrature.(i) *. integrand (sqrt2sigmax *. x_quadrature.(i) +. mux)
      done;
      !sum /. (sqrt pi)
    in
    zc_mat *. integral

let black_price ~today ~zc swaption vol =
  let swaption = extended_swaption_of_swaption ~today ~zc swaption in
  let {swap_schedule; strike; maturity; _} = swaption in
  let sqrtt = act_365 maturity today in
  let lvl, t0, tn =
    List.fold_left
      (fun (acc_lvl, acc_t0, acc_tn) (tf, tp) ->
        acc_lvl +. zc tp *. (act_365 tp tf), min acc_t0 tf, max acc_tn tp
      ) (0., max_date, min_date) swap_schedule
  in
  let s0 = (zc t0 -. zc tn) /. lvl in
  let d1 = log (s0 /. strike) /. (vol *. sqrtt) +. 0.5 *. vol *. sqrtt in
  let d2 = d1 -. vol *. sqrtt in
  lvl *. (s0 *. Math.ugaussian_P d1 -. strike *. Math.ugaussian_P d2)

let calibrate ?feedback ~max_global ~today ~swaption_quotes ~variables ~zc () =
  assert(Array.length variables = 5);
  let quotes = Array.map (fun (sw, q) -> black_price ~today ~zc sw q) swaption_quotes in
  let parameters_of_vector x =
    let g_a = x.(0) in
    let g_b = x.(1) in
    let g_sigma = x.(2) in
    let g_nu = x.(3) in
    let g_rho = x.(4) in
    {g_a; g_b; g_sigma; g_nu; g_rho}
  in
  let pricers = Array.map (fun (sw, _) -> pricer_of_swaption ~today ~zc sw) swaption_quotes in

  let pricer params h =
    try
      for i = 0 to Array.length pricers - 1 do
        h.(i) <- pricers.(i) params
      done
    with Sdomain -> raise Optimization.Infeasible
  in
  Optimization.least_squares ?feedback ~max_global ~parameters_of_vector ~pricer ~variables quotes
