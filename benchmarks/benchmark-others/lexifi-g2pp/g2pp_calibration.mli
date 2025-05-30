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

(** G2++ 2-factors affine interest rate model.  See Brigo and Mercurio
    (2006), chapter 4.2 p142 for more details. *)

open Date

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

type parameters =
    {
     g_a : float;
     g_b : float;
     g_sigma : float;
     g_nu : float;
     g_rho : float;
    }
(** A point in the calibration space of the G2++ model.
    The model parameters of the G2++ model, with mean reversion parameters
    [a] and [b], volatility parameters [sigma] and [nu] and correlation
    coefficient [rho]. *)

exception Sdomain
(** This exception is raised by [swaption] if the numerical integration used in the
    price calculation does not converge (indicating that the model parameters are
    malformed). *)

val pricer_of_swaption: today: date -> zc:(date -> float) -> swaption -> (parameters -> float)
val black_price: today: date -> zc: (date -> float) -> swaption -> (float -> float)

val calibrate:
  ?feedback:(string -> unit) ->
    max_global:int ->
      today: date ->
        swaption_quotes: (swaption * float) array ->
          variables:Optimization.optimization_variable array ->
            zc: (date -> float) ->
              unit -> parameters Optimization.calibration_result
