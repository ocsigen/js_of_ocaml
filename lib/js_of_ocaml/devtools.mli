(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(** Browser developer tools integration.

    Chrome DevTools (and other Chromium-based consoles) let a page install
    {{:https://firefox-source-docs.mozilla.org/devtools-user/custom_formatters/}
    custom formatters} in [globalThis.devtoolsFormatters] to control how objects
    are displayed in the console and in the debugger.

    When the compiler is built with Introcaml, OCaml values carry a descriptor of
    their type at runtime and can be printed in OCaml syntax through
    {!Introspect}. [register_formatters] installs a formatter that uses it, so
    that a value logged with {!Console} or inspected in the debugger shows as,
    e.g., [Circle ({x = 0; name = "o"}, 1)] instead of [Array(3)], and expands
    to its fields (with their names for records and constructors) rather than
    to the array of its raw representation.

    Custom formatters must be enabled in the DevTools settings
    (Settings > Preferences > Console > Enable custom formatters). *)

val register_formatters : unit -> unit
(** Install the formatter in [globalThis.devtoolsFormatters], creating the array
    if needed. Calling it more than once installs it once.

    This is a no-op when the compiler has no introspection support or when the
    program does not run under Js_of_ocaml (native code, Wasm_of_ocaml). When
    the program was compiled with [--disable introspection], the formatter is
    installed but no value carries a descriptor, so nothing is reformatted. *)
