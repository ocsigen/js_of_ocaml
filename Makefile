
all: compiler library runtime examples

include Makefile.conf

.PHONY: compiler library runtime examples

compiler:
	make -C compiler
library:
	make -C lib
runtime:
	make -C runtime
obrowser: library
	make -C obrowser
examples: compiler library
	make -C examples

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
	make -C obrowser clean
	make -C examples clean
