
all: check_lwt compiler library runtime examples tests

include Makefile.conf

.PHONY: compiler library runtime examples check_lwt

compiler:
	$(MAKE) -C compiler
library:
	$(MAKE) -C lib
runtime:
	$(MAKE) -C runtime
examples: compiler library
	$(MAKE) -C examples
tests: compiler library
	$(MAKE) -C tests

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
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C runtime clean
	$(MAKE) -C examples clean
	$(MAKE) -C tests clean
