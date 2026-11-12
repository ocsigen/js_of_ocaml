(* Print the path of the findlib configuration file. A relocatable
   findlib locates it from the location of the standard library, which
   does not work when the compiler runs in JavaScript. *)

let () =
  Findlib.init ();
  print_string (Findlib.config_file ())
