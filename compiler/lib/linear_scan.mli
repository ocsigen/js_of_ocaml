(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Stdlib

(** Variable coalescing by linear scan, shared by [Js_variable_coalescing]
    and the Wasm backend's [Var_coalescing].

    The client builds a control-flow graph of a function, whose nodes use
    or define variables; we compute the live range of each variable and
    assign each variable a representative, so that variables with the same
    representative have disjoint live ranges. *)

type node = int

type action =
  | Use of Code.Var.Set.t
  | Def of Code.Var.Set.t
  | DefUse of Code.Var.Set.t * Code.Var.Set.t  (** defs, uses *)
  | Nop

type graph =
  { entry : node
  ; size : int
  ; actions : action array
  ; succs : node list array
  ; hints : Code.Var.t Code.Var.Hashtbl.t
        (** [x -> y] when [x] is a copy of [y]: [x] should preferably get the
            representative of [y], making the copy a no-op. *)
  ; try_blocks : node Int.Hashtbl.t
        (** [catch_entry -> body_entry]. Every variable live at
            [catch_entry] is live throughout the try body, since an exception
            can be raised anywhere in the body. [body_entry] must come before
            [catch_entry] in reverse postorder. *)
  }

(** Graph construction. *)
module Builder : sig
  type t

  val create : unit -> t

  val reserve : t -> node
  (** A fresh node, to be defined later with [set]. *)

  val set : t -> node -> action -> node list -> unit

  val add : t -> action -> node list -> node

  val hint : t -> Code.Var.t -> Code.Var.t -> unit
  (** [hint b x y]: [x] is a copy of [y]. *)

  val try_block : t -> catch_entry:node -> body_entry:node -> unit

  val finish : t -> entry:node -> graph
end

val liveness : graph -> Code.Var.Set.t array
(** The variables live at the entry of each node. *)

module Live_range : sig
  type interval =
    { start_pos : int
    ; end_pos : int
    }

  type t =
    { id : Code.Var.t
    ; mutable ranges : interval list (* sorted by start_pos *)
    ; mutable free : bool
    }

  val print : Format.formatter -> t -> unit
end

val live_ranges :
     graph
  -> Code.Var.Set.t array
  -> candidates:Code.Var.Set.t
  -> param_vars:Code.Var.Set.t
  -> Live_range.t Code.Var.Hashtbl.t
(** Live ranges of the [candidates], given the result of [liveness]. The
    parameters [param_vars] are all live at the start of the function. *)

val allocate :
     ?compatible:(Code.Var.t -> Code.Var.t -> bool)
  -> graph
  -> Live_range.t Code.Var.Hashtbl.t
  -> assign:(Code.Var.t -> Code.Var.t -> unit)
  -> int * int
(** Calls [assign x r] with the representative [r] of each variable [x]
    (possibly [x] itself). Only [compatible] variables (by default, all of
    them) get the same representative. Returns the number of variables
    coalesced through a copy hint and opportunistically. *)
