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

(* Optimizations algorithm. *)

type termination =
    {
     maxit: int;
     maxf: int;
     target: float;
    }

type status =
  | Maxit_reached
  | Maxf_reached
  | Target_reached

type result =
    {
     x0: float array;
     f: float;
     nb_feval: int;
     status: status;
    }

let ( ** ) (x, f) r = match x with | None -> r  | Some x -> f r x

exception Infeasible

let f_incr f ncalls x =
  let f =
    try
      f x
    with
    | Infeasible -> infinity
  in
  incr ncalls;
  f

module Rand = Random.State

module DE =
  struct
    type t =
        {
         np: int; (* Population size *)
         cr: float; (* Crossover probability, in [0; 1] *)
        }

    let default_parameters n = {np = min (10 * n) 40; cr = 0.9;}

    let parameters ?np ?cr n =
      (cr, fun r cr -> {r with cr}) **
        (np, fun r np -> {r with np}) **
        default_parameters n

    let optimize {np; cr;} ~f ~lower_bounds ~upper_bounds ~call_back ~termination =
      let n = Array.length lower_bounds in
      let rng = Rand.make [| 0 |] in
      let ncalls = ref 0 in
      let f = f_incr f ncalls in
      let {maxit; maxf; target;} = termination in
      let fx = Array.make np 0. in
      let fx0 = ref infinity in (* current min *)
      let v = Array.make_matrix np n 0. in (* potential mutations *)
      let best_idx = ref 0 in (* index of x0 in x *)

      (* initialize current population *)
      let x =
        Array.init np
          (fun i ->
            let x_i = Array.init n
                (fun j ->
                  let blo = lower_bounds.(j) in
                  let db = upper_bounds.(j) -. blo in
                  blo +. db *. Rand.float rng 1.)
            in
            let f = f x_i in
            fx.(i) <- f;
            if f < !fx0 then
              begin
                best_idx := i;
                fx0 := f;
                call_back !ncalls f
              end;
            x_i)
      in

      let x0 = Array.copy x.(!best_idx) in
      let mutation difw i x_i v_i =
        (* draw successively _different_ random integers in [0; np - 1] \ {i} *)
        let drawn = ref [i] in
        let range = ref (np - 1) in
        let rand () =
          let res = ref 0 in
          let rec adjust v = function
            | drawn :: others when drawn <= v -> drawn :: adjust (v + 1) others
            | drawn -> res := v; v :: drawn
          in
          drawn := adjust (Rand.int rng !range) !drawn;
          decr range;
          x.(!res)
        in
        let x_r1 = if Rand.float rng 1. <= 0.5 then x.(!best_idx) else rand () in
        let x_r2 = rand () in
        let x_r3 = rand () in
        let j0 = Rand.int rng n in
        for j = 0 to n - 1 do
          let v =
            let aux = x_r1.(j) +. difw *. (x_r2.(j) -. x_r3.(j)) in
            if (j = j0 || Rand.float rng 1. <= cr) && lower_bounds.(j) <= aux && aux <= upper_bounds.(j) then
              aux
            else
              x_i.(j)
          in
          v_i.(j) <- v
        done
      in
      let recombination () =
        Array.iteri
          (fun i v_i ->
            let f = f v_i in
            if f < fx.(i) then
              begin
                fx.(i) <- f;
                Array.blit v_i 0 x.(i) 0 n;
                if f < !fx0 then
                  begin
                    best_idx := i;
                    fx0 := f;
                    call_back !ncalls f;
                    Array.blit v_i 0 x0 0 n
                  end;
              end;
          )
          v
      in
      let rec iter nb_it =
        let differential_weight = 0.5 +. Rand.float rng 0.5 in (* As recommanded on http://www.icsi.berkeley.edu/~storn/code.html#prac *)
        Array.iteri (fun i x_i -> mutation differential_weight i x_i v.(i)) x;
        recombination ();
        let res status = {x0; f = !fx0; nb_feval = !ncalls; status} in
        if !fx0 <= target then res Target_reached
        else if maxf < !ncalls then res Maxf_reached
        else if nb_it = 0 then res Maxit_reached
        else iter (nb_it - 1)
      in
      iter maxit
  end

type range =
    {
     lower_bound: float;
     upper_bound: float;
     initial_value: float;
    }

type optimization_variable =
  | Fixed_value of float
  | Optimize_value of range

type 'a calibration_result =
    {
     cr_parameters: 'a;
     cr_root_mean_squared_error: float;    (* Calibrated rms. *)
     cr_quoted_prices: float array;      (* i.e. quoted_prices. *)
     cr_calibrated_prices: float array; (* Closed-form prices obtained with calibrated parameters. *)
    }

let least_squares
    ?absolute
    ?feedback
    ~max_global
    ~parameters_of_vector
    ~pricer
    ~variables
    quotes
    =
  begin
    match feedback with
    | Some feedback -> feedback "Calibrating"
    | _ -> ()
  end;
  let free_var_index_to_var_index, strict_subset, x, lower_bounds, upper_bounds =
    let _i, free_vars_to_vars, strict_subset, x, lo, up =
      Array.fold_left
        (fun (i, free_vars_to_vars, strict_subset, x, lo, up) -> function
          | Fixed_value _ -> i + 1, free_vars_to_vars, true, x, lo, up
          | Optimize_value{initial_value; lower_bound; upper_bound} -> i + 1, i :: free_vars_to_vars, strict_subset, initial_value :: x, lower_bound :: lo, upper_bound :: up
        )
        (0, [], false, [], [], [])
        variables
    in
    Array.of_list(List.rev free_vars_to_vars), strict_subset, x, Array.of_list(List.rev lo), Array.of_list(List.rev up)
  in
  let parameters_of_active_vars = match strict_subset with
  | false -> parameters_of_vector
  | true ->
      let all_vars = Array.map (function | Fixed_value x -> x | Optimize_value _ -> nan) variables in
      fun x ->
        Array.iteri (fun i x -> all_vars.(free_var_index_to_var_index.(i)) <- x) x;
        parameters_of_vector all_vars
  in
  let m = Array.length quotes in
  let prices = Array.make m 0. in
  let norm = match absolute with
  | None -> fun acc price quote -> let rel = (price -. quote) /. quote in acc +. rel *. rel
  | Some() -> fun acc price quote -> let dif = price -. quote in acc +. dif *. dif
  in
  let quotes_idx = Array.(init (length quotes) (fun i -> i)) in
  let distance prices = Array.fold_left (fun acc i -> norm acc prices.(i) quotes.(i)) 0. quotes_idx in (* objective is L_2 norm *)
  let rms_of_error =
    let div = 10000. /. float_of_int m in
    fun err -> sqrt (err *. div) in
  (* Initial guess either from global optimization or argument of the calibration routine. *)
  let x =
    if max_global > 0 then
      let call_back =
        match feedback with
        | None -> fun _ _ -> ()
        | Some f -> (fun evals dist -> Printf.ksprintf f "Global Optimizer   Evals: %5i   RMS: %12.8g%%" evals (rms_of_error dist))
      in
      let f x =
        pricer (parameters_of_active_vars x) prices;
        distance prices
      in
      let {x0; _} = DE.optimize (DE.default_parameters (Array.length lower_bounds)) ~f ~lower_bounds ~upper_bounds ~call_back ~termination: {maxit = max_int; maxf = max_global; target = 0.;} in
      x0
    else
      Array.of_list(List.rev x)
  in
  let cr_quoted_prices = quotes in
  let cr_parameters = parameters_of_active_vars x in
  pricer cr_parameters prices;
  {
   cr_parameters;
   cr_root_mean_squared_error = rms_of_error (distance prices);
   cr_quoted_prices;
   cr_calibrated_prices = prices;
  }
