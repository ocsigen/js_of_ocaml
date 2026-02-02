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

(** ES module bundle generation.

    This module generates bundled JavaScript output from an ES module graph.
    It merges all modules in topological order, resolves imports to their
    source identifiers, and generates final export statements.
*)

val bundle : Esm.module_graph -> entry_points:Esm.ModuleId.t list -> Javascript.program
(** [bundle graph ~entry_points] generates the final bundled program
    by merging all modules in topological order.

    All module-level declarations are renamed to unique identifiers during
    analysis, so no collision resolution is needed at bundle time.

    @param graph The module graph (possibly tree-shaken)
    @param entry_points The entry point module IDs (determines export visibility)
*)

val bundle_modules :
     parse:(string -> Javascript.program)
  -> resolve:(from:string -> string -> string option)
  -> entry_points:string list
  -> tree_shake:bool
  -> Javascript.program
(** [bundle_modules ~parse ~resolve ~entry_points ~tree_shake] is a convenience
    function that performs the complete bundling pipeline:
    1. Build the module graph (with all declarations renamed to unique identifiers)
    2. Optionally perform tree-shaking
    3. Generate the bundled output

    @param parse Function to parse a file path into a JavaScript program
    @param resolve Function to resolve an import specifier relative to a module
    @param entry_points List of entry point file paths
    @param tree_shake Whether to perform tree-shaking
*)
