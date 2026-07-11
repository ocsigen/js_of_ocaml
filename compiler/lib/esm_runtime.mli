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

(** Runtime files written as ES modules.

    A runtime file with an [.mjs] extension is parsed as an ES module and
    split into linkable pieces: its exports become provided primitives and
    its imports become dependency edges, so the linker only keeps the code
    needed for the primitives actually used (dead code elimination driven
    by import statements).

    Restrictions compared to full ES modules: only named imports and named
    exports are supported (no default or namespace imports/exports, no
    re-exports, no side-effect imports). Linking is name-based, like for
    annotated runtime scripts: the import specifier (file path) is not used
    to locate code; the imported names must be provided by some other
    runtime file. *)

type piece =
  { exported : bool  (** whether [name] is an export of the module *)
  ; name : string  (** provided name (export name or synthetic) *)
  ; requires : string list  (** names this piece depends on *)
  ; code : Javascript.statement_list
  ; parse_info : Parse_info.t
  }

val split :
     filename:string
  -> Javascript.program
  -> [ `Pieces of piece list | `No_exports of Javascript.statement_list ]
(** [split ~filename program] decomposes an ES module into linkable pieces.
    A module with no exports is returned as [`No_exports body]: it can only
    be meant for its side effects and is always included.

    Raises [Failure] on unsupported constructs (default or namespace
    imports, re-exports, [export * from], side-effect imports). *)
