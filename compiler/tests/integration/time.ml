(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

open Integration_util

let%expect_test _ =
  compile_and_run {| print_float (Unix.time ()) |};
  [%expect {| [0-9]+\. (regexp) |}]

let%expect_test _ =
  compile_and_run {| print_float (Unix.gettimeofday ()) |};
  [%expect {| [0-9]+\.[0-9]* (regexp) |}]

let%expect_test _ =
  compile_and_run
    {|
    open Unix
    let {tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; tm_wday; tm_yday; tm_isdst}
      = gmtime (time ()) ;;

    print_int tm_sec;
    print_char '\n';

    print_int tm_min;
    print_char '\n';

    print_int tm_hour;
    print_char '\n';

    print_int tm_mday;
    print_char '\n';

    print_int tm_mon;
    print_char '\n';

    print_int tm_year;
    print_char '\n';

    print_int tm_wday;
    print_char '\n';

    print_int tm_yday;
    print_char '\n';

    print_endline (if tm_isdst then "true" else "false");
  |};
  [%expect
    {|
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  [0-9]+      (regexp)
  true\|false (regexp)
  |}]
