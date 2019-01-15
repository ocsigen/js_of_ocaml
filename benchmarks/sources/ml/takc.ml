(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: takc.ml 7017 2005-08-12 09:22:04Z xleroy $ *)

let rec tak x y z =
  if x > y then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y) else z

let rec repeat n accu = if n <= 0 then accu else repeat (n - 1) (tak 18 12 6 + accu)

let _ = assert (repeat 2000 0 = 14000)

(*
 print_int (repeat 2000); print_newline(); exit 0
*)
