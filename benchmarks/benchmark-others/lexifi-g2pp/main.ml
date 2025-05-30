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

open G2pp_calibration
open Date

let swaption_quotes =
  [|
   {swaption_maturity_in_year = 1; swap_term_in_year = 1; swap_frequency = Freq_6M}, 1.052;
   {swaption_maturity_in_year = 2; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.81485;
   {swaption_maturity_in_year = 3; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.6165;
   {swaption_maturity_in_year = 4; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.46995;
   {swaption_maturity_in_year = 5; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.38295;
   {swaption_maturity_in_year = 6; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.3325;
   {swaption_maturity_in_year = 7; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.3016;
   {swaption_maturity_in_year = 8; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.2815;
   {swaption_maturity_in_year = 9; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.26435;
   {swaption_maturity_in_year = 10; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.2496;
   {swaption_maturity_in_year = 15; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.2516;
   {swaption_maturity_in_year = 20; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.28835;
   {swaption_maturity_in_year = 25; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.27155;
   {swaption_maturity_in_year = 30; swap_term_in_year = 1; swap_frequency = Freq_6M}, 0.23465;
   {swaption_maturity_in_year = 1; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.61445;
   {swaption_maturity_in_year = 2; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.54805;
   {swaption_maturity_in_year = 3; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.46795;
   {swaption_maturity_in_year = 4; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.3919;
   {swaption_maturity_in_year = 5; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.3434;
   {swaption_maturity_in_year = 6; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.3083;
   {swaption_maturity_in_year = 7; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.28655;
   {swaption_maturity_in_year = 8; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.2697;
   {swaption_maturity_in_year = 9; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.25775;
   {swaption_maturity_in_year = 10; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.2443;
   {swaption_maturity_in_year = 15; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.26495;
   {swaption_maturity_in_year = 20; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.28195;
   {swaption_maturity_in_year = 25; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.26845;
   {swaption_maturity_in_year = 30; swap_term_in_year = 2; swap_frequency = Freq_6M}, 0.20995;
   {swaption_maturity_in_year = 1; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.5835;
   {swaption_maturity_in_year = 2; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.49255;
   {swaption_maturity_in_year = 3; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.42825;
   {swaption_maturity_in_year = 4; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.3695;
   {swaption_maturity_in_year = 5; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.329;
   {swaption_maturity_in_year = 6; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.3022;
   {swaption_maturity_in_year = 7; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.28165;
   {swaption_maturity_in_year = 8; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.26615;
   {swaption_maturity_in_year = 9; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.25485;
   {swaption_maturity_in_year = 10; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.24375;
   {swaption_maturity_in_year = 15; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.2718;
   {swaption_maturity_in_year = 20; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.28135;
   {swaption_maturity_in_year = 25; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.26865;
   {swaption_maturity_in_year = 30; swap_term_in_year = 3; swap_frequency = Freq_6M}, 0.2131;
   {swaption_maturity_in_year = 1; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.5415;
   {swaption_maturity_in_year = 2; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.46235;
   {swaption_maturity_in_year = 3; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.403;
   {swaption_maturity_in_year = 4; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.3559;
   {swaption_maturity_in_year = 5; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.3232;
   {swaption_maturity_in_year = 6; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.29675;
   {swaption_maturity_in_year = 7; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.27715;
   {swaption_maturity_in_year = 8; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.26385;
   {swaption_maturity_in_year = 9; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.254;
   {swaption_maturity_in_year = 10; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.2454;
   {swaption_maturity_in_year = 15; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.27845;
   {swaption_maturity_in_year = 20; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.2821;
   {swaption_maturity_in_year = 25; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.2678;
   {swaption_maturity_in_year = 30; swap_term_in_year = 4; swap_frequency = Freq_6M}, 0.2131;
   {swaption_maturity_in_year = 1; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.517;
   {swaption_maturity_in_year = 2; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.446;
   {swaption_maturity_in_year = 3; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.3903;
   {swaption_maturity_in_year = 4; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.34755;
   {swaption_maturity_in_year = 5; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.3166;
   {swaption_maturity_in_year = 6; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.29305;
   {swaption_maturity_in_year = 7; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.2745;
   {swaption_maturity_in_year = 8; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.2639;
   {swaption_maturity_in_year = 9; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.2534;
   {swaption_maturity_in_year = 10; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.2499;
   {swaption_maturity_in_year = 15; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.28315;
   {swaption_maturity_in_year = 20; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.2825;
   {swaption_maturity_in_year = 25; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.277;
   {swaption_maturity_in_year = 30; swap_term_in_year = 5; swap_frequency = Freq_6M}, 0.21175;
   {swaption_maturity_in_year = 1; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.478;
   {swaption_maturity_in_year = 2; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.42105;
   {swaption_maturity_in_year = 3; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.37715;
   {swaption_maturity_in_year = 4; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.3378;
   {swaption_maturity_in_year = 5; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.311;
   {swaption_maturity_in_year = 6; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.2895;
   {swaption_maturity_in_year = 7; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.2745;
   {swaption_maturity_in_year = 8; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.264;
   {swaption_maturity_in_year = 9; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.2573;
   {swaption_maturity_in_year = 10; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.25475;
   {swaption_maturity_in_year = 15; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.28815;
   {swaption_maturity_in_year = 20; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.28195;
   {swaption_maturity_in_year = 25; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.26015;
   {swaption_maturity_in_year = 30; swap_term_in_year = 6; swap_frequency = Freq_6M}, 0.2097;
   {swaption_maturity_in_year = 1; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.452;
   {swaption_maturity_in_year = 2; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.4074;
   {swaption_maturity_in_year = 3; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.368;
   {swaption_maturity_in_year = 4; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.3307;
   {swaption_maturity_in_year = 5; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.30645;
   {swaption_maturity_in_year = 6; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.2877;
   {swaption_maturity_in_year = 7; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.27475;
   {swaption_maturity_in_year = 8; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.2664;
   {swaption_maturity_in_year = 9; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.26155;
   {swaption_maturity_in_year = 10; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.26035;
   {swaption_maturity_in_year = 15; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.292;
   {swaption_maturity_in_year = 20; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.2825;
   {swaption_maturity_in_year = 25; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.25685;
   {swaption_maturity_in_year = 30; swap_term_in_year = 7; swap_frequency = Freq_6M}, 0.2081;
   {swaption_maturity_in_year = 1; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.43395;
   {swaption_maturity_in_year = 2; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.39445;
   {swaption_maturity_in_year = 3; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.35885;
   {swaption_maturity_in_year = 4; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.3281;
   {swaption_maturity_in_year = 5; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.30395;
   {swaption_maturity_in_year = 6; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.28745;
   {swaption_maturity_in_year = 7; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.2767;
   {swaption_maturity_in_year = 8; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.27065;
   {swaption_maturity_in_year = 9; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.26625;
   {swaption_maturity_in_year = 10; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.26625;
   {swaption_maturity_in_year = 15; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.2921;
   {swaption_maturity_in_year = 20; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.2814;
   {swaption_maturity_in_year = 25; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.25265;
   {swaption_maturity_in_year = 30; swap_term_in_year = 8; swap_frequency = Freq_6M}, 0.2083;
   {swaption_maturity_in_year = 1; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.42285;
   {swaption_maturity_in_year = 2; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.3857;
   {swaption_maturity_in_year = 3; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.3521;
   {swaption_maturity_in_year = 4; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.3239;
   {swaption_maturity_in_year = 5; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.30285;
   {swaption_maturity_in_year = 6; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.2895;
   {swaption_maturity_in_year = 7; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.2799;
   {swaption_maturity_in_year = 8; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.27485;
   {swaption_maturity_in_year = 9; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.2712;
   {swaption_maturity_in_year = 10; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.27205;
   {swaption_maturity_in_year = 15; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.29205;
   {swaption_maturity_in_year = 20; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.27855;
   {swaption_maturity_in_year = 25; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.24945;
   {swaption_maturity_in_year = 30; swap_term_in_year = 9; swap_frequency = Freq_6M}, 0.219;
   {swaption_maturity_in_year = 1; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.41765;
   {swaption_maturity_in_year = 2; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.38095;
   {swaption_maturity_in_year = 3; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.34795;
   {swaption_maturity_in_year = 4; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.3217;
   {swaption_maturity_in_year = 5; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.30365;
   {swaption_maturity_in_year = 6; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.2916;
   {swaption_maturity_in_year = 7; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.2842;
   {swaption_maturity_in_year = 8; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.27985;
   {swaption_maturity_in_year = 9; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.2769;
   {swaption_maturity_in_year = 10; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.2775;
   {swaption_maturity_in_year = 15; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.306;
   {swaption_maturity_in_year = 20; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.2763;
   {swaption_maturity_in_year = 25; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.2458;
   {swaption_maturity_in_year = 30; swap_term_in_year = 10; swap_frequency = Freq_6M}, 0.22;
   {swaption_maturity_in_year = 1; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.37905;
   {swaption_maturity_in_year = 2; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.35465;
   {swaption_maturity_in_year = 3; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.33505;
   {swaption_maturity_in_year = 4; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.31725;
   {swaption_maturity_in_year = 5; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.3008;
   {swaption_maturity_in_year = 6; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.29075;
   {swaption_maturity_in_year = 7; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.28365;
   {swaption_maturity_in_year = 8; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.2787;
   {swaption_maturity_in_year = 9; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.27385;
   {swaption_maturity_in_year = 10; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.2709;
   {swaption_maturity_in_year = 15; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.2689;
   {swaption_maturity_in_year = 20; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.24225;
   {swaption_maturity_in_year = 25; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.2096;
   {swaption_maturity_in_year = 30; swap_term_in_year = 15; swap_frequency = Freq_6M}, 0.18285;
   {swaption_maturity_in_year = 1; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.37975;
   {swaption_maturity_in_year = 2; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.3605;
   {swaption_maturity_in_year = 3; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.3407;
   {swaption_maturity_in_year = 4; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.321;
   {swaption_maturity_in_year = 5; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.3063;
   {swaption_maturity_in_year = 6; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.29315;
   {swaption_maturity_in_year = 7; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.28395;
   {swaption_maturity_in_year = 8; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.2777;
   {swaption_maturity_in_year = 9; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.27205;
   {swaption_maturity_in_year = 10; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.26675;
   {swaption_maturity_in_year = 15; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.24875;
   {swaption_maturity_in_year = 20; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.21735;
   {swaption_maturity_in_year = 25; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.1939;
   {swaption_maturity_in_year = 30; swap_term_in_year = 20; swap_frequency = Freq_6M}, 0.17205;
   {swaption_maturity_in_year = 1; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.38115;
   {swaption_maturity_in_year = 2; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.3627;
   {swaption_maturity_in_year = 3; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.34425;
   {swaption_maturity_in_year = 4; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.3222;
   {swaption_maturity_in_year = 5; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.3084;
   {swaption_maturity_in_year = 6; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.2941;
   {swaption_maturity_in_year = 7; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.28285;
   {swaption_maturity_in_year = 8; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.2751;
   {swaption_maturity_in_year = 9; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.2663;
   {swaption_maturity_in_year = 10; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.26055;
   {swaption_maturity_in_year = 15; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.2338;
   {swaption_maturity_in_year = 20; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.20735;
   {swaption_maturity_in_year = 25; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.1823;
   {swaption_maturity_in_year = 30; swap_term_in_year = 25; swap_frequency = Freq_6M}, 0.1686;
   {swaption_maturity_in_year = 1; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.38285;
   {swaption_maturity_in_year = 2; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.3633;
   {swaption_maturity_in_year = 3; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.34125;
   {swaption_maturity_in_year = 4; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.3188;
   {swaption_maturity_in_year = 5; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.30305;
   {swaption_maturity_in_year = 6; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.2888;
   {swaption_maturity_in_year = 7; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.2748;
   {swaption_maturity_in_year = 8; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.26725;
   {swaption_maturity_in_year = 9; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.25985;
   {swaption_maturity_in_year = 10; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.25165;
   {swaption_maturity_in_year = 15; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.2267;
   {swaption_maturity_in_year = 20; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.1989;
   {swaption_maturity_in_year = 25; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.18115;
   {swaption_maturity_in_year = 30; swap_term_in_year = 30; swap_frequency = Freq_6M}, 0.16355;
  |]

let named_variables =
  let open Optimization in
  [|
    ("a", Optimize_value {lower_bound = 0.; upper_bound = 1.; initial_value = 0.02});
    ("b", Optimize_value {lower_bound = 0.; upper_bound = 1.; initial_value = 0.02});
    ("sigma", Optimize_value {lower_bound = 0.; upper_bound = 0.2; initial_value = 0.04});
    ("nu", Optimize_value {lower_bound = 0.; upper_bound = 0.2; initial_value = 0.01});
    ("rho", Optimize_value {lower_bound = -1.; upper_bound = 1.; initial_value = 0.0});
  |]

let variables = Array.map snd named_variables

let r = 0.03
let today = Date.of_string "2012-01-01"

let zc t = exp (-.0.03 *. (act_365 t today))
let sq x = x *. x

let () =
  let {Optimization.cr_parameters = params; cr_root_mean_squared_error; cr_quoted_prices = _; cr_calibrated_prices = _} =
    calibrate ~feedback: print_endline ~max_global: 2000 ~today ~swaption_quotes ~variables ~zc ()
  in
  let {g_a; g_b; g_sigma; g_nu; g_rho} = params in
  let l = Array.to_list swaption_quotes in
  let rms =
    List.fold_left
      (fun acc (swaption, quote) ->
        let g2pp_price = pricer_of_swaption ~today ~zc swaption params in
        let market_price = black_price ~today ~zc swaption quote in
        Printf.printf "%dY%dY Swaption Calibrated Price: %.2f Market Price: %.2f\n" swaption.swaption_maturity_in_year swaption.swap_term_in_year (10000. *. g2pp_price) (10000. *. market_price);
        acc +. sq ((g2pp_price -. market_price) /. market_price)
      ) 0. l;
  in
  let rms = 100. *. sqrt (rms /. (float (List.length l))) in
  Printf.printf "a = %.5f, b = %.5f, sigma = %.5f, nu = %.5f, rho = %.5f\n" g_a g_b g_sigma g_nu g_rho;
  Printf.printf "RMS = %.5f%%, re calculated RMS = %.5f%%\n" cr_root_mean_squared_error rms

let () =
  try
    let fn = Sys.getenv "OCAML_GC_STATS" in
    let oc = open_out fn in
    Gc.print_stat oc
  with _ -> ()
