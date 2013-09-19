
all: no_examples examples
no_examples: check_lwt compiler compiler_lib library ocamlbuild doc runtime

include Makefile.conf
-include Makefile.local

.PHONY: all no_examples compiler library ocamlbuild runtime examples check_lwt doc

compiler:
	$(MAKE) -C compiler
compiler_lib:
	$(MAKE) -C compiler lib
library:
	$(MAKE) -C lib
ocamlbuild:
	$(MAKE) -C ocamlbuild
runtime:
	$(MAKE) -C runtime
toplevel: compiler library runtime
	$(MAKE) -C toplevel
examples: compiler library runtime
	$(MAKE) -C examples
tests: compiler library runtime
	$(MAKE) -C tests
phantomtests: compiler library runtime
	$(MAKE) -C tests phantom
doc: library
	$(MAKE) -C doc

LWTERROR="Js_of_ocaml requires Lwt version 2.3.0 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.3.0; then \
	  echo $(LWTERROR); exit 1; \
	fi

include Makefile.filelist

VERSION := $(shell head -n 1 VERSION)
install:
	ocamlfind install -patch-version ${VERSION} $(LIBRARY) lib/META $(INTF) $(IMPL) $(OTHERS) $(DOC)
	ocamlfind install -patch-version ${VERSION} $(COMPILER_LIBRARY) compiler/META $(COMP_INTF) $(COMP_IMPL)
	install -d -m 755 $(BINDIR)
	install $(BIN) $(BINDIR)

uninstall:
	ocamlfind remove $(LIBRARY)
	ocamlfind remove $(COMPILER_LIBRARY)
	rm -f $(BINDIR)/$(COMPILER)

reinstall: uninstall install

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C ocamlbuild clean
	$(MAKE) -C runtime clean
	$(MAKE) -C examples clean
ifeq ($(wildcard tests),tests)
	$(MAKE) -C tests clean
	$(MAKE) -C doc clean
endif

realclean: clean
	find . -name "*~" -print | xargs rm -f

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
        cd /tmp &&\
	darcs get http://ocsigen.org/darcs/js_of_ocaml/ js_of_ocaml-${VERSION} &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude _darcs --exclude tests
