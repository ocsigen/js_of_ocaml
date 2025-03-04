(* TEST
 include testing;
 flags = "-no-strict-formats";
*)

(*

A test file for the Printf module.

*)

open Testing;;
open Printf;;

let test_roundtrip fmt of_string s =
  test (sprintf fmt (of_string s) = s)
;;

try

  printf "d/i positive\n%!";
  test (sprintf "%d/%i" 42 43 = "42/43");
  test (sprintf "%-4d/%-5i" 42 43 = "42  /43   ");
  test (sprintf "%04d/%05i" 42 43 = "0042/00043");
  test (sprintf "%+d/%+i" 42 43 = "+42/+43");
  test (sprintf "% d/% i" 42 43 = " 42/ 43");
  test (sprintf "%#d/%#i" 42 43 = "42/43");
  test (sprintf "%#d/%#i" 123 123 = "123/123");
  test (sprintf "%#d/%#i" 1234 1234 = "1_234/1_234");
  test (sprintf "%#d/%#i" 12345 12345 = "12_345/12_345");
  test (sprintf "%#d/%#i" 123456 123456 = "123_456/123_456");
  test (sprintf "%#4d/%#5i" 1234 1234 = "1_234/1_234");
  test (sprintf "%#-6d/%#-7i" 1234 1234 = "1_234 /1_234  ");
  test (sprintf "%4d/%5i" 42 43 = "  42/   43");
  test (sprintf "%*d" (-4) 42 = "42  ");
  test (sprintf "%*d/%*i" 4 42 5 43 = "  42/   43");
  (*test (sprintf "%-0+#4d/%-0 #5i" 42 43 = "+42 / 43  ");*)
    (* >> '#' is incompatible with 'd' *)

  printf "\nd/i negative\n%!";
  test (sprintf "%d/%i" (-42) (-43) = "-42/-43");
  test (sprintf "%-4d/%-5i" (-42) (-43) = "-42 /-43  ");
  test (sprintf "%04d/%05i" (-42) (-43) = "-042/-0043");
  test (sprintf "%+d/%+i" (-42) (-43) = "-42/-43");
  test (sprintf "% d/% i" (-42) (-43) = "-42/-43");
  test (sprintf "%#d/%#i" (-42) (-43) = "-42/-43");
  test (sprintf "%#d/%#i" (-123) (-123) = "-123/-123");
  test (sprintf "%#d/%#i" (-1234) (-1234) = "-1_234/-1_234");
  test (sprintf "%#d/%#i" (-12345) (-12345) = "-12_345/-12_345");
  test (sprintf "%#d/%#i" (-123456) (-123456) = "-123_456/-123_456");
  test (sprintf "%#4d/%#5i" (-1234) (-1234) = "-1_234/-1_234");
  test (sprintf "%#-6d/%#-7i" (-1234) (-1234) = "-1_234/-1_234 ");
  test (sprintf "%4d/%5i" (-42) (-43) = " -42/  -43");
  test (sprintf "%*d" (-4) (-42) = "-42 ");
  test (sprintf "%*d/%*i" 4 (-42) 5 (-43) = " -42/  -43");
  (*test (sprintf "%-0+ #4d/%-0+ #5i" (-42) (-43) = "-42 /-43  ");*)
    (* >> '0' is incompatible with '-', '#' is incompatible with 'd' *)

  printf "\nu positive\n%!";
  test (sprintf "%u" 42 = "42");
  test (sprintf "%-4u" 42 = "42  ");
  test (sprintf "%04u" 42 = "0042");
  (*test (sprintf "%+u" 42 = "42");*)
    (* >> '+' is incompatible with 'u' *)
  (*test (sprintf "% u" 42 = "42");*)
    (* >> ' ' is incompatible with 'u' *)
  test (sprintf "%#u" 42 = "42");
  test (sprintf "%#u" 123 = "123");
  test (sprintf "%#u" 1234 = "1_234");
  test (sprintf "%#u" 12345 = "12_345");
  test (sprintf "%#u" 123456 = "123_456");
  test (sprintf "%#4u" 1234 = "1_234");
  test (sprintf "%#6u" 1234 = " 1_234");
  test (sprintf "%4u" 42 = "  42");
  test (sprintf "%*u" 4 42 = "  42");
  test (sprintf "%*u" (-4) 42 = "42  ");

  printf "\nu negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%u" (-1) = "2147483647");
     test (sprintf "%#u" (-1) = "2_147_483_647");
  | 32 ->
     test (sprintf "%u" (-1) = "4294967295");
     test (sprintf "%#u" (-1) = "4_294_967_295");
  | 63 ->
     test (sprintf "%u" (-1) = "9223372036854775807");
     test (sprintf "%#u" (-1) = "9_223_372_036_854_775_807");
  | _ -> test false
  end;

  printf "\nx positive\n%!";
  test (sprintf "%x" 42 = "2a");
  test (sprintf "%-4x" 42 = "2a  ");
  test (sprintf "%04x" 42 = "002a");
  (*test (sprintf "%+x" 42 = "2a");*)
    (* >> '+' is incompatible with 'x' *)
  (*test (sprintf "% x" 42 = "2a");*)
    (* >> ' ' is incompatible with 'x' *)
  test (sprintf "%#x" 42 = "0x2a");
  test (sprintf "%4x" 42 = "  2a");
  test (sprintf "%*x" 5 42 = "   2a");
  test (sprintf "%*x" (-5) 42 = "2a   ");
  test (sprintf "%#*x" 5 42 = " 0x2a");
  test (sprintf "%#*x" (-5) 42 = "0x2a ");
  test (sprintf "%#-*x" 5 42 = "0x2a ");
  test (sprintf "%-0+ #*x" 5 42 = "0x2a ");

  printf "\nx negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%x" (-42) = "7fffffd6");
  | 32 ->
     test (sprintf "%x" (-42) = "ffffffd6");
  | 63 ->
     test (sprintf "%x" (-42) = "7fffffffffffffd6");
  | _ -> test false
  end;

  printf "\nX positive\n%!";
  test (sprintf "%X" 42 = "2A");
  test (sprintf "%-4X" 42 = "2A  ");
  test (sprintf "%04X" 42 = "002A");
  (*test (sprintf "%+X" 42 = "2A");*)
    (* >> '+' is incompatible with 'X' *)
  (*test (sprintf "% X" 42 = "2A");*)
    (* >> ' ' is incompatible with 'X' *)
  test (sprintf "%#X" 42 = "0X2A");
  test (sprintf "%4X" 42 = "  2A");
  test (sprintf "%*X" 5 42 = "   2A");
  (*test (sprintf "%-0+ #*X" 5 42 = "0X2A ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nx negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%X" (-42) = "7FFFFFD6");
  | 32 ->
     test (sprintf "%X" (-42) = "FFFFFFD6");
  | 63 ->
     test (sprintf "%X" (-42) = "7FFFFFFFFFFFFFD6");
  | _ -> test false
  end;

  printf "\no positive\n%!";
  test (sprintf "%o" 42 = "52");
  test (sprintf "%-4o" 42 = "52  ");
  test (sprintf "%04o" 42 = "0052");
  (*test (sprintf "%+o" 42 = "52");*)
    (* >> '+' is incompatible with 'o' *)
  (*test (sprintf "% o" 42 = "52");*)
    (* >> '+' is incompatible with 'o' *)
  test (sprintf "%#o" 42 = "052");
  test (sprintf "%4o" 42 = "  52");
  test (sprintf "%*o" 5 42 = "   52");
  (*test (sprintf "%-0+ #*o" 5 42 = "052  ");*)
    (* >> '-' is incompatible with 'o' *)

  printf "\no negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%o" (-42) = "17777777726");
  | 32 ->
     test (sprintf "%o" (-42) = "37777777726");
  | 63 ->
     test (sprintf "%o" (-42) = "777777777777777777726");
  | _ -> test false
  end;

  printf "\ns\n%!";
  test (sprintf "%s" "foo" = "foo");
  test (sprintf "%-5s" "foo" = "foo  ");
  (*test (sprintf "%05s" "foo" = "  foo");*)
    (* >> '0' is incompatible with 's' *)
  (*test (sprintf "%+s" "foo" = "foo");*)
    (* >> '+' is incompatible with 's' *)
  (*test (sprintf "% s" "foo" = "foo");*)
    (* >> ' ' is incompatible with 's' *)
  (*test (sprintf "%#s" "foo" = "foo");*)
    (* >> '#' is incompatible with 's' *)
  test (sprintf "%5s" "foo" = "  foo");
  test (sprintf "%1s" "foo" = "foo");
  test (sprintf "%*s" 6 "foo" = "   foo");
  test (sprintf "%*s" (-6) "foo" = "foo   ");
  test (sprintf "%*s" 2 "foo" = "foo");
  (*test (sprintf "%-0+ #5s" "foo" = "foo  ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 's' *)
  test (sprintf "%s@" "foo" = "foo@");
  test (sprintf "%s@inria.fr" "foo" = "foo@inria.fr");
  test (sprintf "%s@%s" "foo" "inria.fr" = "foo@inria.fr");

  printf "\nS\n%!";
  test (sprintf "%S" "fo\"o" = "\"fo\\\"o\"");
(*  test (sprintf "%-5S" "foo" = "\"foo\"  ");   padding not done *)
(*  test (sprintf "%05S" "foo" = "  \"foo\"");   padding not done *)
  (*test (sprintf "%+S" "foo" = "\"foo\"");*)
    (* >> '#' is incompatible with 'S' *)
  (*test (sprintf "% S" "foo" = "\"foo\"");*)
    (* >> '#' is incompatible with 'S' *)
  (*test (sprintf "%#S" "foo" = "\"foo\"");*)
    (* >> '#' is incompatible with 'S' *)
(*  test (sprintf "%5S" "foo" = "  \"foo\"");    padding not done *)
  test (sprintf "%1S" "foo" = "\"foo\"");
  test (sprintf "%*S" 8 "foo" = "   \"foo\"");
  test (sprintf "%*S" (-8) "foo" = "\"foo\"   ");
  test (sprintf "%*S" 2 "foo" = "\"foo\"");
(*  test (sprintf "%-0+ #5S" "foo" = "\"foo\"  ");  padding not done *)
  test (sprintf "%S@" "foo" = "\"foo\"@");
  test (sprintf "%S@inria.fr" "foo" = "\"foo\"@inria.fr");
  test (sprintf "%S@%S" "foo" "inria.fr" = "\"foo\"@\"inria.fr\"");

  printf "\nc\n%!";
  test (sprintf "%c" 'c' = "c");
(*  test (sprintf "%-4c" 'c' = "c   ");    padding not done *)
(*  test (sprintf "%04c" 'c' = "   c");    padding not done *)
  (*test (sprintf "%+c" 'c' = "c");*)
    (* >> '#' is incompatible with 'c' *)
  (*test (sprintf "% c" 'c' = "c");*)
    (* >> '#' is incompatible with 'c' *)
  (*test (sprintf "%#c" 'c' = "c");*)
    (* >> '#' is incompatible with 'c' *)
(*  test (sprintf "%4c" 'c' = "   c");     padding not done *)
(*  test (sprintf "%*c" 2 'c' = " c");     padding not done *)
(*  test (sprintf "%-0+ #4c" 'c' = "c   ");  padding not done *)

  printf "\nC\n%!";
  test (sprintf "%C" 'c' = "'c'");
  test (sprintf "%C" '\'' = "'\\''");
(*  test (sprintf "%-4C" 'c' = "c   ");    padding not done *)
(*  test (sprintf "%04C" 'c' = "   c");    padding not done *)
  (*test (sprintf "%+C" 'c' = "'c'");*)
    (* >> '+' is incompatible with 'C' *)
  (*test (sprintf "% C" 'c' = "'c'");*)
    (* >> ' ' is incompatible with 'C' *)
  (*test (sprintf "%#C" 'c' = "'c'");*)
    (* >> '#' is incompatible with 'C' *)
(*  test (sprintf "%4C" 'c' = "   c");     padding not done *)
(*  test (sprintf "%*C" 2 'c' = " c");     padding not done *)
(*  test (sprintf "%-0+ #4C" 'c' = "c   ");  padding not done *)

  printf "\nf\n%!";
  test (sprintf "%f" (-42.42) = "-42.420000");
  test (sprintf "%-13f" (-42.42) = "-42.420000   ");
  test (sprintf "%013f" (-42.42) = "-00042.420000");
  test (sprintf "%+f" 42.42 = "+42.420000");
  test (sprintf "% f" 42.42 = " 42.420000");
  (*test (sprintf "%#f" 42.42 = "42.420000");*)
    (* >> '#' is incompatible with 'f' *)
  test (sprintf "%13f" 42.42 = "    42.420000");
  test (sprintf "%*f" 12 42.42 = "   42.420000");
  (*test (sprintf "%-0+ #12f" 42.42 = "+42.420000  ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'f' *)
  test (sprintf "%.3f" (-42.42) = "-42.420");
  test (sprintf "%.*f" (-3) 42.42 = "42.420");
    (* dynamically-provided negative precisions are currently silently
       turned into their absolute value; we could error on this
       in the future (the behavior is unspecified), but the previous
       buggy output "%.0-3f-" is not desirable. *)
  test (sprintf "%-13.3f" (-42.42) = "-42.420      ");
  test (sprintf "%013.3f" (-42.42) = "-00000042.420");
  test (sprintf "%+.3f" 42.42 = "+42.420");
  test (sprintf "% .3f" 42.42 = " 42.420");
  (*test (sprintf "%#.3f" 42.42 = "42.420");*)
    (* >> '#' is incompatible with 'f' *)
  test (sprintf "%13.3f" 42.42 = "       42.420");
  test (sprintf "%*.*f" 12 3 42.42 = "      42.420");
  (*test (sprintf "%-0+ #12.3f" 42.42 = "+42.420     ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'f' *)

  (* Under Windows (mingw and maybe also MSVC), the stdlib uses three
     digits for the exponent instead of the two used by Linux and BSD.
     Check that the two strings are equal, except that there may be an
     extra zero, and if there is one, there may be a missing space or
     zero. All in the first string relative to the second. *)
  let ( =* ) s1 s2 =
    let ss1 = s1 ^ "$" in
    let ss2 = s2 ^ "$" in
    let rec loop i1 i2 extra missing =
      if i1 = String.length ss1 && i2 = String.length ss2 then begin
        if extra then true else not missing
      end else if i1 = String.length ss1 || i2 = String.length ss2 then
        false
      else begin
        match ss1.[i1], ss2.[i2] with
        | x, y when x = y -> loop (i1+1) (i2+1) extra missing
        | '0', _ when not extra -> loop (i1+1) i2 true missing
        | _, (' '|'0') when not missing -> loop i1 (i2+1) extra true
        | _, _ -> false
      end
    in
    loop 0 0 false false
  in

  printf "\nF\n%!";
  test (sprintf "%F" 42.42 = "42.42");
  test (sprintf "%F" 42.42e42 =* "4.242e+43");
  test (sprintf "%F" 42.00 = "42.");
  test (sprintf "%F" 0.042 = "0.042");
  test (sprintf "%4F" 3. = "  3.");
  test (sprintf "%-4F" 3. = "3.  ");
  test (sprintf "%04F" 3. = "003.");
  test (sprintf "%+4F" 3. = " +3.");
  test (sprintf "%.3F" 42.42 = "42.4");
  test (sprintf "%12.3F" 42.42e42 =* "    4.24e+43");
  test (sprintf "%.3F" 42.00 = "42.");
  test (sprintf "%.3F" 0.0042 = "0.0042");
  test (sprintf "%F" nan = "nan");
  test (sprintf "%F" (-. nan) = "nan");
  test (sprintf "%F" infinity = "infinity");
  test (sprintf "%F" neg_infinity = "neg_infinity");

  printf "\n#F\n%!";
  test (sprintf "%+#F" (+0.) = "+0x0p+0");
  test (sprintf "%+#F" (-0.) = "-0x0p+0");
  test (sprintf "%+#F" (+1.) = "+0x1p+0");
  test (sprintf "%+#F" (-1.) = "-0x1p+0");
  test (sprintf "%+#F" (+1024.) = "+0x1p+10");
  test (sprintf "% #F" (+1024.) = " 0x1p+10");
  test (sprintf "%+#F" (-1024.) = "-0x1p+10");
  test (sprintf "%#F" 0x123.456 = "0x1.23456p+8");
  test (sprintf "%#F" 0x123456789ABCDE. = "0x1.23456789abcdep+52");
  test (sprintf "%#F" epsilon_float = "0x1p-52");
  test (sprintf "%#F" nan = "nan");
  test (sprintf "%#F" (-. nan) = "nan");
  test (sprintf "%#F" infinity = "infinity");
  test (sprintf "%#F" neg_infinity = "neg_infinity");

  printf "\nh\n%!";
  test (sprintf "%+h" (+0.) = "+0x0p+0");
  test (sprintf "%+h" (-0.) = "-0x0p+0");
  test (sprintf "%+h" (+1.) = "+0x1p+0");
  test (sprintf "%+h" (-1.) = "-0x1p+0");
  test (sprintf "%+h" (+1024.) = "+0x1p+10");
  test (sprintf "%+h" (-1024.) = "-0x1p+10");
  test (sprintf "%h" 0x123.456 = "0x1.23456p+8");
  test (sprintf "%h" 0x123456789ABCDE. = "0x1.23456789abcdep+52");
  test (sprintf "%h" epsilon_float = "0x1p-52");
  test (sprintf "%h" nan = "nan");
  test (sprintf "%h" infinity = "infinity");
  test (sprintf "%h" neg_infinity = "-infinity");
  test (sprintf "%h" (4. *. atan 1.) = "0x1.921fb54442d18p+1");

  printf "\nH\n%!";
  test (sprintf "%+H" (+0.) = "+0X0P+0");
  test (sprintf "%+H" (-0.) = "-0X0P+0");
  test (sprintf "%+H" (+1.) = "+0X1P+0");
  test (sprintf "%+H" (-1.) = "-0X1P+0");
  test (sprintf "%+H" (+1024.) = "+0X1P+10");
  test (sprintf "%+H" (-1024.) = "-0X1P+10");
  test (sprintf "%H" 0X123.456 = "0X1.23456P+8");
  test (sprintf "%H" 0X123456789ABCDE. = "0X1.23456789ABCDEP+52");
  test (sprintf "%H" epsilon_float = "0X1P-52");
  test (sprintf "%H" nan = "NAN");
  test (sprintf "%H" infinity = "INFINITY");
  test (sprintf "%H" neg_infinity = "-INFINITY");
  test (sprintf "%H" (4. *. atan 1.) = "0X1.921FB54442D18P+1");

  printf "\ne\n%!";
  test (sprintf "%e" (-42.42) =* "-4.242000e+01");
  test (sprintf "%-15e" (-42.42) =* "-4.242000e+01  ");
  test (sprintf "%015e" (-42.42) =* "-004.242000e+01");
  test (sprintf "%+e" 42.42 =* "+4.242000e+01");
  test (sprintf "% e" 42.42 =* " 4.242000e+01");
  (*test (sprintf "%#e" 42.42 =* "4.242000e+01");*)
    (* >> '#' is incompatible with 'e' *)
  test (sprintf "%15e" 42.42 =* "   4.242000e+01");
  test (sprintf "%*e" 14 42.42 =* "  4.242000e+01");
  (*test (sprintf "%-0+ #14e" 42.42 =* "+4.242000e+01 ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'e' *)
  test (sprintf "%.3e" (-42.42) =* "-4.242e+01");
  test (sprintf "%-15.3e" (-42.42) =* "-4.242e+01     ");
  test (sprintf "%015.3e" (-42.42) =* "-000004.242e+01");
  test (sprintf "%+.3e" 42.42 =* "+4.242e+01");
  test (sprintf "% .3e" 42.42 =* " 4.242e+01");
  (*test (sprintf "%#.3e" 42.42 =* "4.242e+01");*)
    (* >> '#' is incompatible with 'e' *)
  test (sprintf "%15.3e" 42.42 =* "      4.242e+01");
  test (sprintf "%*.*e" 11 3 42.42 =* "  4.242e+01");
  (*test (sprintf "%-0+ #14.3e" 42.42 =* "+4.242e+01    ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'e' *)

  printf "\nE\n%!";
  test (sprintf "%E" (-42.42) =* "-4.242000E+01");
  test (sprintf "%-15E" (-42.42) =* "-4.242000E+01  ");
  test (sprintf "%015E" (-42.42) =* "-004.242000E+01");
  test (sprintf "%+E" 42.42 =* "+4.242000E+01");
  test (sprintf "% E" 42.42 =* " 4.242000E+01");
  (*test (sprintf "%#E" 42.42 =* "4.242000E+01");*)
    (* >> '#' is incompatible with 'E' *)
  test (sprintf "%15E" 42.42 =* "   4.242000E+01");
  test (sprintf "%*E" 14 42.42 =* "  4.242000E+01");
  (*test (sprintf "%-0+ #14E" 42.42 =* "+4.242000E+01 ");*)
    (* >> '#' is incompatible with 'E' *)
  test (sprintf "%.3E" (-42.42) =* "-4.242E+01");
  test (sprintf "%-15.3E" (-42.42) =* "-4.242E+01     ");
  test (sprintf "%015.3E" (-42.42) =* "-000004.242E+01");
  test (sprintf "%+.3E" 42.42 =* "+4.242E+01");
  test (sprintf "% .3E" 42.42 =* " 4.242E+01");
  (*test (sprintf "%#.3E" 42.42 =* "4.242E+01");*)
    (* >> '#' is incompatible with 'E' *)
  test (sprintf "%15.3E" 42.42 =* "      4.242E+01");
  test (sprintf "%*.*E" 11 3 42.42 =* "  4.242E+01");
  (*test (sprintf "%-0+ #14.3E" 42.42 =* "+4.242E+01    ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'E' *)

  printf "\ng\n%!";
  test (sprintf "%g" (-42.42) = "-42.42");
  test (sprintf "%.3g" (-4242.) =* "-4.24e+03");
  test (sprintf "%-15g" (-42.42) = "-42.42         ");
  test (sprintf "%015g" (-42.42) = "-00000000042.42");
  test (sprintf "%+g" 42.42 = "+42.42");
  test (sprintf "% g" 42.42 = " 42.42");
  test (sprintf "%15g" 42.42 = "          42.42");
  test (sprintf "%*g" 14 42.42 = "         42.42");
  test (sprintf "%.3g" (-42.42) = "-42.4");

  printf "\nG\n%!";
  test (sprintf "%G" (-42.42) = "-42.42");
  test (sprintf "%.3G" (-4242.) =* "-4.24E+03");
  test (sprintf "%-15G" (-42.42) = "-42.42         ");
  test (sprintf "%015G" (-42.42) = "-00000000042.42");
  test (sprintf "%+G" 42.42 = "+42.42");
  test (sprintf "% G" 42.42 = " 42.42");
  test (sprintf "%15G" 42.42 = "          42.42");
  test (sprintf "%*G" 14 42.42 = "         42.42");
  test (sprintf "%.3G" (-42.42) = "-42.4");

  printf "\nB\n%!";
  test (sprintf "%B" true = "true");
  test (sprintf "%8B" true = "    true");
  test (sprintf "%B" false = "false");
  test (sprintf "%-8B" false = "false   ");

  printf "\nld/li positive\n%!";
  test (sprintf "%ld/%li" 42l 43l = "42/43");
  test (sprintf "%-4ld/%-5li" 42l 43l = "42  /43   ");
  test (sprintf "%04ld/%05li" 42l 43l = "0042/00043");
  test (sprintf "%+ld/%+li" 42l 43l = "+42/+43");
  test (sprintf "% ld/% li" 42l 43l = " 42/ 43");
  (*test (sprintf "%#ld/%#li" 42l 43l = "42/43");*)
    (* >> '#' is incompatible with 'ld' *)
  test (sprintf "%4ld/%5li" 42l 43l = "  42/   43");
  test (sprintf "%*ld/%*li" 4 42l 5 43l = "  42/   43");
  (*test (sprintf "%-0+#4ld/%-0 #5li" 42l 43l = "+42 / 43  ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'ld' *)

  printf "\nld/li negative\n%!";
  test (sprintf "%ld/%li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%-4ld/%-5li" (-42l) (-43l) = "-42 /-43  ");
  test (sprintf "%04ld/%05li" (-42l) (-43l) = "-042/-0043");
  test (sprintf "%+ld/%+li" (-42l) (-43l) = "-42/-43");
  test (sprintf "% ld/% li" (-42l) (-43l) = "-42/-43");
  (*test (sprintf "%#ld/%#li" (-42l) (-43l) = "-42/-43");*)
    (* >> '#' is incompatible with 'ld' *)
  test (sprintf "%4ld/%5li" (-42l) (-43l) = " -42/  -43");
  test (sprintf "%*ld/%*li" 4 (-42l) 5 (-43l) = " -42/  -43");
  (*test (sprintf "%-0+ #4ld/%-0+ #5li" (-42l) (-43l) = "-42 /-43  ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'ld' *)

  printf "\nlu positive\n%!";
  test (sprintf "%lu" 42l = "42");
  test (sprintf "%-4lu" 42l = "42  ");
  test (sprintf "%04lu" 42l = "0042");
  (*test (sprintf "%+lu" 42l = "42");*)
    (* >> '+' is incompatible with 'lu' *)
  (*test (sprintf "% lu" 42l = "42");*)
    (* >> ' ' is incompatible with 'lu' *)
  (*test (sprintf "%#lu" 42l = "42");*)
    (* >> '#' is incompatible with 'lu' *)
  test (sprintf "%4lu" 42l = "  42");
  test (sprintf "%*lu" 4 42l = "  42");
  (*test (sprintf "%-0+ #6ld" 42l = "+42   ");*)
    (* >> '-' is incompatible with '0', '#' is incompatible with 'ld' *)

  printf "\nlu negative\n%!";
  test (sprintf "%lu" (-1l) = "4294967295");

  printf "\nlx positive\n%!";
  test (sprintf "%lx" 42l = "2a");
  test (sprintf "%-4lx" 42l = "2a  ");
  test (sprintf "%04lx" 42l = "002a");
  (*test (sprintf "%+lx" 42l = "2a");*)
    (* >> '+' is incompatible with 'lx' *)
  (*test (sprintf "% lx" 42l = "2a");*)
    (* >> ' ' is incompatible with 'lx' *)
  test (sprintf "%#lx" 42l = "0x2a");
  test (sprintf "%4lx" 42l = "  2a");
  test (sprintf "%*lx" 5 42l = "   2a");
  (*test (sprintf "%-0+ #*lx" 5 42l = "0x2a ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nlx negative\n%!";
  test (sprintf "%lx" (-42l) = "ffffffd6");

  printf "\nlX positive\n%!";
  test (sprintf "%lX" 42l = "2A");
  test (sprintf "%-4lX" 42l = "2A  ");
  test (sprintf "%04lX" 42l = "002A");
  (*test (sprintf "%+lX" 42l = "2A");*)
    (* >> '+' is incompatible with 'lX' *)
  (*test (sprintf "% lX" 42l = "2A");*)
    (* >> ' ' is incompatible with 'lX' *)
  test (sprintf "%#lX" 42l = "0X2A");
  test (sprintf "%4lX" 42l = "  2A");
  test (sprintf "%*lX" 5 42l = "   2A");
  (*test (sprintf "%-0+ #*lX" 5 42l = "0X2A ");*)
    (* >> '-' is incompatible with '0' *)
  test_roundtrip "0x%lX" Int32.of_string "0x0";
  test_roundtrip "0x%lX" Int32.of_string "0x123";
  test_roundtrip "0x%lX" Int32.of_string "0xABCDEF";
  test_roundtrip "0x%lX" Int32.of_string "0x12345678";
  test_roundtrip "0x%lX" Int32.of_string "0x7FFFFFFF";

  printf "\nlX negative\n%!";
  test (sprintf "%lX" (-42l) = "FFFFFFD6");
  test_roundtrip "0x%lX" Int32.of_string "0x80000000";
  test_roundtrip "0x%lX" Int32.of_string "0xFFFFFFFF";

  printf "\nlo positive\n%!";
  test (sprintf "%lo" 42l = "52");
  test (sprintf "%-4lo" 42l = "52  ");
  test (sprintf "%04lo" 42l = "0052");
  (*test (sprintf "%+lo" 42l = "52");*)
    (* >> '+' is incompatible with 'lo' *)
  (*test (sprintf "% lo" 42l = "52");*)
    (* >> ' ' is incompatible with 'lo' *)
  test (sprintf "%#lo" 42l = "052");
  test (sprintf "%4lo" 42l = "  52");
  test (sprintf "%*lo" 5 42l = "   52");
  (*test (sprintf "%-0+ #*lo" 5 42l = "052  ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nlo negative\n%!";
  test (sprintf "%lo" (-42l) = "37777777726");

  (* Nativeint not tested: looks like too much work, and anyway it should
     work like Int32 or Int64. *)

  printf "\nLd/Li positive\n%!";
  test (sprintf "%Ld/%Li" 42L 43L = "42/43");
  test (sprintf "%-4Ld/%-5Li" 42L 43L = "42  /43   ");
  test (sprintf "%04Ld/%05Li" 42L 43L = "0042/00043");
  (*test (sprintf "%+Ld/%+Li" 42L 43L = "+42/+43");*)
    (* >> '+' is incompatible with 'Ld' *)
  (*test (sprintf "% Ld/% Li" 42L 43L = " 42/ 43");*)
    (* >> ' ' is incompatible with 'Ld' *)
  (*test (sprintf "%#Ld/%#Li" 42L 43L = "42/43");*)
    (* >> '#' is incompatible with 'Ld' *)
  test (sprintf "%4Ld/%5Li" 42L 43L = "  42/   43");
  test (sprintf "%*Ld/%*Li" 4 42L 5 43L = "  42/   43");
  (*test (sprintf "%-0+#4Ld/%-0 #5Li" 42L 43L = "+42 / 43  ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nLd/Li negative\n%!";
  test (sprintf "%Ld/%Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%-4Ld/%-5Li" (-42L) (-43L) = "-42 /-43  ");
  test (sprintf "%04Ld/%05Li" (-42L) (-43L) = "-042/-0043");
  (*test (sprintf "%+Ld/%+Li" (-42L) (-43L) = "-42/-43");*)
    (* >> '+' is incompatible with 'Ld' *)
  (*test (sprintf "% Ld/% Li" (-42L) (-43L) = "-42/-43");*)
    (* >> ' ' is incompatible with 'Ld' *)
  (*test (sprintf "%#Ld/%#Li" (-42L) (-43L) = "-42/-43");*)
    (* >> '#' is incompatible with 'Ld' *)
  test (sprintf "%4Ld/%5Li" (-42L) (-43L) = " -42/  -43");
  test (sprintf "%*Ld/%*Li" 4 (-42L) 5 (-43L) = " -42/  -43");
  (*test (sprintf "%-0+ #4Ld/%-0+ #5Li" (-42L) (-43L) = "-42 /-43  ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nLu positive\n%!";
  test (sprintf "%Lu" 42L = "42");
  test (sprintf "%-4Lu" 42L = "42  ");
  test (sprintf "%04Lu" 42L = "0042");
  (*test (sprintf "%+Lu" 42L = "42");*)
    (* >> '+' is incompatible with 'Lu' *)
  (*test (sprintf "% Lu" 42L = "42");*)
    (* >> ' ' is incompatible with 'Lu' *)
  (*test (sprintf "%#Lu" 42L = "42");*)
    (* >> '#' is incompatible with 'Lu' *)
  test (sprintf "%4Lu" 42L = "  42");
  test (sprintf "%*Lu" 4 42L = "  42");
  (*test (sprintf "%-0+ #6Ld" 42L = "+42   ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nLu negative\n%!";
  test (sprintf "%Lu" (-1L) = "18446744073709551615");

  printf "\nLx positive\n%!";
  test (sprintf "%Lx" 42L = "2a");
  test (sprintf "%-4Lx" 42L = "2a  ");
  test (sprintf "%04Lx" 42L = "002a");
  (*test (sprintf "%+Lx" 42L = "2a");*)
    (* >> '+' is incompatible with 'Lx' *)
  (*test (sprintf "% Lx" 42L = "2a");*)
    (* >> ' ' is incompatible with 'Lx' *)
  test (sprintf "%#Lx" 42L = "0x2a");
  test (sprintf "%4Lx" 42L = "  2a");
  test (sprintf "%*Lx" 5 42L = "   2a");
  (*test (sprintf "%-0+ #*Lx" 5 42L = "0x2a ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nLx negative\n%!";
  test (sprintf "%Lx" (-42L) = "ffffffffffffffd6");

  printf "\nLX positive\n%!";
  test (sprintf "%LX" 42L = "2A");
  test (sprintf "%-4LX" 42L = "2A  ");
  test (sprintf "%04LX" 42L = "002A");
  (*test (sprintf "%+LX" 42L = "2A");*)
    (* >> '+' is incompatible with 'LX' *)
  (*test (sprintf "% LX" 42L = "2A");*)
    (* >> ' ' is incompatible with 'LX' *)
  test (sprintf "%#LX" 42L = "0X2A");
  test (sprintf "%4LX" 42L = "  2A");
  test (sprintf "%*LX" 5 42L = "   2A");
  (*test (sprintf "%-0+ #*LX" 5 42L = "0X2A ");*)
    (* >> '-' is incompatible with '0' *)
  test_roundtrip "0x%LX" Int64.of_string "0x0";
  test_roundtrip "0x%LX" Int64.of_string "0x123";
  test_roundtrip "0x%LX" Int64.of_string "0xABCDEF";
  test_roundtrip "0x%LX" Int64.of_string "0x1234567812345678";
  test_roundtrip "0x%LX" Int64.of_string "0x7FFFFFFFFFFFFFFF";

  printf "\nLX negative\n%!";
  test (sprintf "%LX" (-42L) = "FFFFFFFFFFFFFFD6");
  test_roundtrip "0x%LX" Int64.of_string "0x8000000000000000";
  test_roundtrip "0x%LX" Int64.of_string "0xFFFFFFFFFFFFFFFF";

  printf "\nLo positive\n%!";
  test (sprintf "%Lo" 42L = "52");
  test (sprintf "%-4Lo" 42L = "52  ");
  test (sprintf "%04Lo" 42L = "0052");
  (*test (sprintf "%+Lo" 42L = "52");*)
    (* >> '+' is incompatible with 'Lo' *)
  (*test (sprintf "% Lo" 42L = "52");*)
    (* >> ' ' is incompatible with 'Lo' *)
  test (sprintf "%#Lo" 42L = "052");
  test (sprintf "%4Lo" 42L = "  52");
  test (sprintf "%*Lo" 5 42L = "   52");
  (*test (sprintf "%-0+ #*Lo" 5 42L = "052  ");*)
    (* >> '-' is incompatible with '0' *)

  printf "\nLo negative\n%!";
  test (sprintf "%Lo" (-42L) = "1777777777777777777726");

  printf "\na\n%!";
  let x = ref () in
  let f () y = if y == x then "ok" else "wrong" in
  test (sprintf "%a" f x = "ok");

  printf "\nt\n%!";
  let f () = "ok" in
  test (sprintf "%t" f = "ok");

  (* Work as expected. Prints the format string type digest.
     If you want to print the contents of the format string,
     do not use a meta format; simply convert the format string
     to a string and print it using %s. *)

  printf "\n{...%%}\n%!";
  let f = format_of_string "%4g/%s" in
  test (sprintf "%{%.4F%5S%}" f = "%f%s");

  printf "\n(...%%)\n%!";
  let f = format_of_string "%d/foo/%s" in
  test (sprintf "%(%d%s%)" f 42 "bar" = "42/foo/bar");

  printf "\n! %% @ , and constants\n%!";
  test (sprintf "%!" = "");
  test (sprintf "%%" = "%");
  test (sprintf "%@" = "@");
  test (sprintf "%," = "");
  test (sprintf "@" = "@");
  test (sprintf "@@" = "@@");
  test (sprintf "@%%" = "@%");

  printf "\nend of tests\n%!";
with e ->
  printf "unexpected exception: %s\n%!" (Printexc.to_string e);
  test false;
;;
