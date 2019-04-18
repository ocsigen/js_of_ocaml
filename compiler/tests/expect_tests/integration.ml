open Util

let%expect_test _ =
  {| console.log("hello world") |}
  |> Util.Format.js_source_of_string
  |> Util.Format.write_js
  |> Util.run_javascript
  |> print_endline;
  [%expect {| hello world |}]

let compile_and_run s =
  s
  |> Format.ocaml_source_of_string
  |> Format.write_ocaml
  |> compile_ocaml_to_bc
  |> compile_bc_to_javascript
  |> run_javascript
  |> print_endline

let%expect_test _ =
  compile_and_run {| print_endline "hello world" |};
  [%expect {| hello world |}]

let%expect_test _ =
  compile_and_run {| print_float (Unix.time ()) |};
  [%expect {| [0-9]+\. (regexp) |}]

let%expect_test _ =
  compile_and_run {| print_float (Unix.gettimeofday ()) |};
  [%expect {| [0-9]+\.[0-9]* (regexp) |}]

let%expect_test _ =
  compile_and_run {|
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
  [%expect {|
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
