(rule
 (targets main.4.14.output)
 (deps main.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 4.14)(< %{ocaml_version} 5.0)(not %{oxcaml_supported})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    main.bc))))

(rule
 (targets unix-Win32.4.14.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 4.14)(< %{ocaml_version} 5.0)(= %{os_type} Win32)(not %{oxcaml_supported})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets unix-Unix.4.14.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 4.14)(< %{ocaml_version} 5.0)(= %{os_type} Unix)(not %{oxcaml_supported})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets main.5.4.output)
 (deps main.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.4)(< %{ocaml_version} 5.5)(not %{oxcaml_supported})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    main.bc))))

(rule
 (targets unix-Win32.5.4.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.4)(< %{ocaml_version} 5.5)(= %{os_type} Win32)(not %{oxcaml_supported})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets unix-Unix.5.4.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.4)(< %{ocaml_version} 5.5)(= %{os_type} Unix)(not %{oxcaml_supported})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets main.5.2+ox.output)
 (deps main.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)%{oxcaml_supported}))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    main.bc))))

(rule
 (targets unix-Win32.5.2+ox.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)(= %{os_type} Win32)%{oxcaml_supported}))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets unix-Unix.5.2+ox.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)(= %{os_type} Unix)%{oxcaml_supported}))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

