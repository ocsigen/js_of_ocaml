
all: check_lwt compiler library runtime examples

include Makefile.conf

.PHONY: compiler library runtime examples check_lwt

compiler:
	make -C compiler
library:
	make -C lib
runtime:
	make -C runtime
examples: compiler library
	make -C examples

LWTERROR="Js_of_ocaml requires Lwt version 2.1 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | grep -q "version:.*2\\.0\\."; then \
	  echo $(LWTERROR); exit 1; \
	fi

install:
	ocamlfind install $(LIBRARY) lib/META lib/$(LIBNAME).cma lib/dll$(LIBNAME).so lib/*.cmi lib/*.mli lib/syntax/pa_js.cmo runtime/runtime.js
	install compiler/$(COMPILER) $(BINDIR)

uninstall:
	ocamlfind remove $(LIBRARY)
	rm -f $(BINDIR)/$(COMPILER)

depend:
	make -C compiler depend
	make -C lib depend

clean:
	make -C compiler clean
	make -C lib clean
	make -C runtime clean
	make -C examples clean
