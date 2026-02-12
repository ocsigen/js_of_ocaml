(* Wasm_of_ocaml runtime support
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

val loadfile : string -> unit
(** [loadfile filename] dynamically loads a pre-compiled [.wasmo] file.
    Reads the file, extracts the Wasm module, instantiates it, merges its
    exports into the running program, and runs its initialization code.

    Raises [Failure] if the file cannot be read or the module cannot be
    instantiated. *)

val load_module_bytes : bytes -> Obj.t
(** [load_module_bytes wasm_bytes] loads a raw Wasm binary module from the
    given bytes. The module is instantiated and its exports are merged into
    the running program. Returns the result of the module's init function. *)
