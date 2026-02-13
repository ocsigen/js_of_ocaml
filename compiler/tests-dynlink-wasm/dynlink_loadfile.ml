let () =
  Dynlink.loadfile "plugin.cmo";
  Dynlink.loadfile "plugin2.cma";
  print_endline "done"
