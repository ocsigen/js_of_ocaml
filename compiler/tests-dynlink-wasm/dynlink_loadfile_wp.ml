let () =
  Dynlink.loadfile "plugin.cmo";
  Dynlink.loadfile "plugin2.cma";
  Dynlink.allow_unsafe_modules true;
  Dynlink.loadfile "plugin_js.cmo";
  print_endline "done"
