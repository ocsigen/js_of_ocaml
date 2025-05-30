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

module DE:
    (** Implementation based on Differential Evolution - A Simple and Efficient Heuristic for Global Optimization over Continuous Spaces,
        Rainer Storn and Kenneth Price.
        Implemented srategies is a mixture of DE/rand/1/bin and DE/best/1/bin. The mutatant is lambda *. DE/rand/1/bin +. (1. -. lambda) *. DE/best/1/bin
        where lambda is randomly choosen for each generation.
     *)
    sig
      type t

      val default_parameters: int -> t

      val parameters: ?np: int -> ?cr: float -> int -> t
    end

(** {2 Least Square Minimization} *)

exception Infeasible
(** To be raised by optimization callback functions to indicate that a point is infeasible. *)

type 'a calibration_result =
  {
    cr_parameters: 'a;
    cr_root_mean_squared_error: float;
    cr_quoted_prices: float array;
    cr_calibrated_prices: float array;
  }
  (** General calibration result. It contains the model parameters of type ['a]
  and some optional additional informations:

   - [cr_parameters]. The optimal model parameter set

   - [cr_root_mean_squared_error]. Root-mean-squared error of optimal fit

   - [cr_quoted_prices]. Quotes to be fitted

   - [cr_calibrated_prices]. Optimal model price of each quote
   *)

type range = {
  lower_bound: float;
  upper_bound: float;
  initial_value: float;
}

type optimization_variable =
  | Fixed_value of float
  | Optimize_value of range

val least_squares:
  ?absolute: unit ->
    ?feedback: (string -> unit) ->
      max_global:int ->
        parameters_of_vector:(float array -> 'parameters) ->
          pricer:('parameters -> float array -> unit) ->
            variables:optimization_variable array ->
              float array ->
                'parameters calibration_result
(** [least_squares ~pricer ~lo ~up quotes] outputs a vector of calibrated parameters. These calibrated parameters
correspond to the least square fitting of quotes from the [quotes] list and results of applying the [price] function.
This optimization is done first using Direct as a global optimizer then using Minpack as a local optimizer.
The input are:

  - [pricer] is the pricer function; it takes as input a vector of parameters (to be optimized) and a price vector.
  The price vector is filled by this function in order to be compared to quotes from the [quotes] list.

  - [x] is the original value for the parameters. This is only used if no global optimization is performed, i.e. if [max_global] is 0.

  - [lo] contains lower bounds for each parameters.

  - [up] contains upper bounds for each parameters.

  - [quotes] is the list of quotes to be matched by the least-square algorithm.

  Optional parameters are:

    - [absolute] is an optional parameter. If set, the distance is the absolute difference, not the default relative one.

    - [feedback] is a feedback function that can be used to print the current RMS.

    - [max_global] is the maximum number of steps performed in the global optimization (DE).

    - [max_local] is the maximum number of steps performed in the local optimization (Minpack).

    Note that [x], [lo], and [up] must have the same size. This size is the number of parameters, it is also equal to the size of
    the vector in the output and should be equal to the size of the vector used as the first argument of the [price] function.
    The size of the second argument of the [price] function should be equal to the length of the [quotes] list.
*)
