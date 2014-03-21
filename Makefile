
all: no_examples examples
no_examples: check_lwt compiler compiler_lib library ocamlbuild runtime doc

include Makefile.conf
-include Makefile.local

.PHONY: all no_examples compiler library ocamlbuild runtime examples check_lwt doc

compiler:
	$(MAKE) -C compiler
compiler_lib: compiler
	$(MAKE) -C compiler lib
library:
	$(MAKE) -C lib
ocamlbuild:
	$(MAKE) -C ocamlbuild
runtime:
	$(MAKE) -C runtime
toplevel: compiler compiler_lib library runtime
	$(MAKE) -C toplevel
examples: compiler library runtime
	$(MAKE) -C examples
tests: compiler library runtime
	$(MAKE) -C tests
phantomtests: compiler library runtime
	$(MAKE) -C tests phantom
doc: library ocamlbuild
	$(MAKE) -C doc

LWTERROR="Js_of_ocaml requires Lwt version 2.3.0 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.3.0; then \
	  echo $(LWTERROR); exit 1; \
	fi

include Makefile.filelist

VERSION := $(shell head -n 1 VERSION)

install: install-lib install-bin

install-lib:
	ocamlfind install -patch-version ${VERSION} $(LIBRARY) lib/META $(INTF) $(IMPL) $(OTHERS) $(DOC) $(COMP_INTF) $(COMP_IMPL)

install-bin:
	install -d -m 755 $(BINDIR)
	install $(BIN) $(BINDIR)

uninstall: uninstall-lib uninstall-bin

uninstall-lib:
	ocamlfind remove $(LIBRARY)

uninstall-bin:
	rm -f $(BINDIR)/$(COMPILER)
	rm -f $(BINDIR)/$(MINIFIER)

reinstall: uninstall install

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C ocamlbuild clean
	$(MAKE) -C runtime clean
	$(MAKE) -C toplevel clean
	$(MAKE) -C examples clean
ifeq ($(wildcard tests),tests)
	$(MAKE) -C tests clean
	$(MAKE) -C doc clean
endif

realclean: clean
	find . -name "*~" -print | xargs rm -f
	find . -name "*.tmpjs" -print | xargs rm -f
	find . -name "#*" -print | xargs rm -f


toplevel-version-%:
	@opam switch show
	opam switch $*
	eval `opam config env`
	opam install lwt menhir ocamlfind optcomp
	$(MAKE) realclean
	$(MAKE) toplevel
	cp toplevel/toplevel.js toplevel/v/toplevel-$*.js

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
        cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git js_of_ocaml-${VERSION} &&\
	(cd js_of_ocaml-${VERSION}; git checkout ${VERSION}) &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude .git --exclude tests
