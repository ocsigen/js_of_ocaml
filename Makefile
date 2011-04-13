
all: check_lwt compiler library doc runtime examples

include Makefile.conf

.PHONY: compiler library runtime examples check_lwt doc

compiler:
	$(MAKE) -C compiler
library:
	$(MAKE) -C lib
runtime:
	$(MAKE) -C runtime
toplevel: compiler library runtime
	$(MAKE) -C toplevel
examples: compiler library runtime
	$(MAKE) -C examples
tests: compiler library
	$(MAKE) -C tests
doc: library
	$(MAKE) -C doc

LWTERROR="Js_of_ocaml requires Lwt version 2.3.0 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.3.0; then \
	  echo $(LWTERROR); exit 1; \
	fi

include Makefile.filelist

install:
	ocamlfind install $(LIBRARY) lib/META $(INTF) $(IMPL) $(NATIMPL) $(OTHERS)
	install $(BIN) $(BINDIR)

uninstall:
	ocamlfind remove $(LIBRARY)
	rm -f $(BINDIR)/$(COMPILER)

reinstall: uninstall install

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C runtime clean
	$(MAKE) -C examples clean
ifeq ($(wildcard tests),tests)
	$(MAKE) -C tests clean
	$(MAKE) -C doc clean
endif

realclean: clean
	find . -name "*~" -print | xargs rm -f

dist:
	rm -r /tmp/js_of_ocaml &&\
        cd /tmp &&\
	darcs get http://ocsigen.org/darcs/js_of_ocaml/ &&\
	tar zcvf js_of_ocaml.tar.gz js_of_ocaml --exclude benchmarks --exclude _darcs --exclude tests
