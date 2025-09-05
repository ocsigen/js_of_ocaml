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

val ugaussian_P : float -> float
(** The cumulative distribution function for a normal variate with mean 0 and
   variance 1. [ugaussian_P x] returns the probability that a variate takes a
   value less than or equal to [x]. *)

module Rootfinder: sig
  type error =
    | NotbracketedBelow (** The function is below zero on both the given brackets. *)
    | NotbracketedAbove (** The function is above zero on both the given brackets. *)

  val string_of_error: error -> string
      (** Pretty-printing of error. *)

  exception Rootfinder of error

  val brent: float -> float -> (float -> float) ->  float -> float
(** [brent xlo xhi f eps] finds a root of function [f] given that
    it is bracketed by [xlo, xhi]. [eps] is the absolute error on the
    returned root. Raises {!Rootfinder.Rootfinder} exception when an error is encountered.
    The root is found using brent's method (http://en.wikipedia.org/wiki/Brent's_method).
    Requires [xlo < xhi]. *)
end

module Gaussian_quadrature:
    sig
      val gauss_hermite_coefficients: float array * float array
          (** [gauss_hermite_coefficients nb_points] returns two arrays [x] and [w] such that
              for all [f], the integral of [f] over \]-infty; infty\[ is approximated by \sum w_i f(x_i).
              the approximation is good if [f(x)] is close to [exp(-x^2) * P(x)] with [P] a polynomial, and is exact
              if [P] is a polynomial of degree less or equal to [2 * nb_points - 1]
           *)
    end
