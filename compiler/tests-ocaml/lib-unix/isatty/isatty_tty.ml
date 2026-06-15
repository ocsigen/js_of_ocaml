(* TEST
 include unix;
 libwin32unix;
 {
   bytecode;
 }{
   native;
 }
*)

let console =
  try
    Unix.(openfile "/dev/tty" [O_RDWR] 0)
  with _ ->
    Unix.(openfile "CONIN$" [O_RDWR] 0)
in
Printf.printf "/dev/tty = %b\n" (Unix.isatty console);
(* In_channel.isatty / Out_channel.isatty exercise caml_sys_isatty,
   a different primitive from Unix.isatty (caml_unix_isatty). *)
Printf.printf "In_channel.isatty = %b\n"
  (In_channel.isatty (Unix.in_channel_of_descr console));
Printf.printf "Out_channel.isatty = %b\n"
  (Out_channel.isatty (Unix.out_channel_of_descr console))
