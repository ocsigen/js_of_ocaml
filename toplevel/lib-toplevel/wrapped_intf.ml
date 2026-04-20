(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
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

module type Wrapped = sig
  type toplevel

  type 'a result

  type output

  val check : toplevel -> ?setenv:bool -> string -> unit result
  (** Parse and typecheck a given source code

      @param setenv should the resulting environment replace the current
                    environment ?

      @return [Success ()] in case of success and [Error err]
              where [err] contains the error message otherwise.

  *)

  val execute :
       toplevel
    -> ?ppf_code:output
    -> ?print_outcome:bool
    -> ppf_answer:output
    -> string
    -> bool result
  (** Execute a given source code. The evaluation stops after the first
      toplevel phrase (as terminated by ";;") that fails to compile or
      for which the evaluation raises an uncaught exception.

      @param ppf_code a formatter were the source code will be printed
             before its execution. The printing might be interleaved
             with call to "pp_answer" when a line finishes by ";;".

      @param ppf_answer a formatter were the compiler outputs will be
             printed.

      @param print_outcome should the toplevel print the computed
             values and their types ?

      @return Three outcomes:
              - [Success true]: every phrase compiled and evaluated
                without raising.
              - [Success false]: a phrase was compiled but raised an
                uncaught exception at evaluation time. The toplevel has
                already rendered the backtrace to [ppf_answer]; no
                structured error is returned here.
              - [Error err]: parsing or typechecking failed before any
                phrase could run; [err.msg] is the rendered error. *)

  val use_string :
       toplevel
    -> ?filename:string
    -> ?print_outcome:bool
    -> ppf_answer:output
    -> string
    -> bool result
  (** Execute a given source code. Unlike {!execute}, the whole buffer
      is parsed up front (using [Toploop.parse_use_file]); each
      resulting phrase is then typechecked and evaluated in sequence.
      Parse errors abort before any phrase runs.

      @param filename a faked filename which will be used in error messages

      @param ppf_answer see {!val:execute}.

      @param print_outcome see {!val:execute}.

      @return as {!val:execute}.

  *)

  val use_mod_string :
       toplevel
    -> ?print_outcome:bool
    -> ppf_answer:output
    -> modname:string
    -> ?sig_code:string
    -> string
    -> bool result
  (** Wrap a given source code into a module and bind it with a given name.

      @param print_outcome see {!val:execute}.

      @param ppf_answer see {!val:execute}.

      @param modname the module name, it must start with a capital
             character.

      @param sig_code source code for the module signature.

      @return as {!val:execute}.

  *)
end
