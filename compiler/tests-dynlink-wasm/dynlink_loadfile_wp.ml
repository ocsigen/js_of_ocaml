let () =
  Printf.printf "Plugin_dep.x = %d\n" Plugin_dep.x;
  Dynlink.loadfile "plugin.cmo";
  Dynlink.loadfile "plugin2.cma";
  (try Dynlink.loadfile "plugin_uses_dep.cmo"
   with Dynlink.Error (Inconsistent_import _) -> print_endline "CRC mismatch detected");
  Dynlink.allow_unsafe_modules true;
  Dynlink.loadfile "plugin_js.cmo";
  print_endline "done"
