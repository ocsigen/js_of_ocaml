(* Wasm_of_ocaml compiler
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

type input =
  { module_name : string
  ; file : string
  ; code : string option
  ; opt_source_map : Source_map.Standard.t option
  }

type dependency =
  { name : string
  ; export : string option
  ; import : (string * string) option
  ; reaches : string list
  ; root : bool
  }
(** A node of the dependency graph used for dead code elimination, in the
    format of [wasm-metadce]: [export] and [import] associate the node to
    an export and an import of the linked module; [reaches] lists the
    nodes this node depends on (by name). *)

val parse_dependencies : string -> dependency list
(** Parse a dependency graph in the JSON format of [wasm-metadce] *)

type output =
  { source_map : Source_map.t
  ; imports : (string * string) list
        (** The imports of the linked module, as pairs (module name, name) *)
  }

val f :
     ?filter_export:(string -> bool)
  -> ?dependencies:dependency list
  -> ?names:bool
  -> input list
  -> output_file:string
  -> output
(** Link the input modules. When [dependencies] is provided, dead code is
    removed: only the exports reachable from the root nodes of the
    dependency graph are kept, and only the entries reachable from them or
    from the start functions are kept. The name section is omitted if [names] is false. *)
