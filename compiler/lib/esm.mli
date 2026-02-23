(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open Stdlib

(** ES Module types and graph construction.

    This module provides:
    - Type definitions for ES module representation
    - Module graph construction from entry points
    - Resolution of import/export dependencies

    For tree-shaking, see {!Esm_tree_shake}.
    For bundle generation, see {!Esm_bundle}.
*)

(** {1 Module Identifiers} *)

module ModuleId : sig
  type t

  val of_path : string -> t
  (** Create a module identifier from a file path. *)

  val to_path : t -> string
  (** Get the file path from a module identifier. *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** {1 Module Representation} *)

type export_kind =
  | Export_var
  | Export_fun
  | Export_class
  | Export_reexport of ModuleId.t * string  (** Re-export from another module *)

type export_entry =
  { exported_name : string  (** Name visible to importers *)
  ; local_ident : Javascript.ident  (** Local binding in this module *)
  ; kind : export_kind
  }

type import_binding =
  | ImportNamed of string * Javascript.ident  (** original name, local binding *)
  | ImportDefault of Javascript.ident
  | ImportNamespace of Javascript.ident
  | ImportSideEffect

type import_entry =
  { source : ModuleId.t
  ; bindings : import_binding list
  }

type esm_module =
  { id : ModuleId.t
  ; imports : import_entry list
  ; exports : export_entry StringMap.t  (** Map from exported name to export entry *)
  ; star_exports : ModuleId.t list  (** Sources of [export * from] statements *)
  ; body : Javascript.statement_list  (** Non-import/export statements *)
  ; has_default_export : bool
  }

type module_graph =
  { modules : esm_module ModuleId.Map.t
  ; deps : ModuleId.Set.t ModuleId.Map.t
        (** Dependencies: module -> set of modules it imports from *)
  }

(** {1 Module Analysis} *)

val analyze_module :
  resolve:(string -> ModuleId.t) -> ModuleId.t -> Javascript.program -> esm_module
(** [analyze_module ~resolve id program] analyzes a parsed JavaScript module
    and extracts its imports, exports, and body statements.

    @param resolve Function to resolve import specifiers to module IDs
    @param id The module's identifier
    @param program The parsed JavaScript program
*)

(** {1 Graph Construction} *)

val build_graph :
     parse:(string -> Javascript.program)
  -> resolve:(from:string -> string -> string option)
  -> entry_points:string list
  -> module_graph
(** [build_graph ~parse ~resolve ~entry_points] builds a complete module
    dependency graph starting from the given entry points.

    @param parse Function to parse a file path into a JavaScript program
    @param resolve Function to resolve an import specifier relative to a module
    @param entry_points List of entry point file paths
*)

(** {1 Utilities} *)

val topological_sort : module_graph -> ModuleId.t list
(** [topological_sort graph] returns modules in dependency order
    (dependencies before dependents). *)

val resolve_reexport : esm_module ModuleId.Map.t -> export_entry -> Javascript.ident
(** [resolve_reexport modules export] follows re-export chains to find the
    actual source identifier for an export. *)
