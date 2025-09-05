(rule
 (targets main.4.14.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 4.14)(< %{ocaml_version} 5.0)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:main.bc}))))

(rule
 (targets unix-Win32.4.14.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 4.14)(< %{ocaml_version} 5.0)(= %{os_type} Win32)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:unix.bc}))))

(rule
 (targets unix-Unix.4.14.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 4.14)(< %{ocaml_version} 5.0)(= %{os_type} Unix)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:unix.bc}))))

(rule
 (targets main.5.2.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:main.bc}))))

(rule
 (targets unix-Win32.5.2.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)(= %{os_type} Win32)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:unix.bc}))))

(rule
 (targets unix-Unix.5.2.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)(= %{os_type} Unix)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:unix.bc}))))

(rule
 (targets main.5.3.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.3)(< %{ocaml_version} 5.4)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:main.bc}))))

(rule
 (targets unix-Win32.5.3.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.3)(< %{ocaml_version} 5.4)(= %{os_type} Win32)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:unix.bc}))))

(rule
 (targets unix-Unix.5.3.output)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.3)(< %{ocaml_version} 5.4)(= %{os_type} Unix)))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %{dep:unix.bc}))))

