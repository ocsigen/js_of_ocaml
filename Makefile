
all: no_examples examples
no_examples: build doc

build: check_lwt compiler library ocamlbuild runtime jsoo_tools toplevel_lib

include Makefile.conf
-include Makefile.local

.PHONY: all no_examples compiler library ocamlbuild runtime examples check_lwt doc build jsoo_tools toplevel_lib toplevel

compiler:
	$(MAKE) -C compiler all compilerlib
library:
	$(MAKE) -C lib
runtime:
	$(MAKE) -C runtime
jsoo_tools: compiler
	$(MAKE) -C jsoo_tools
toplevel_lib: compiler
	$(MAKE) -C lib toplevel_lib
ocamlbuild:
	$(MAKE) -C ocamlbuild
examples: compiler library runtime
	$(MAKE) -C examples
doc: library ocamlbuild
	$(MAKE) -C doc

toplevel:
	$(MAKE) -C toplevel

tests: compiler library runtime
	$(MAKE) -C tests
phantomtests: compiler library runtime
	$(MAKE) -C tests phantom

LWTERROR="Js_of_ocaml requires Lwt version 2.3.0 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.3.0; then \
	  echo $(LWTERROR); exit 1; \
	fi

include Makefile.filelist

VERSION := $(shell head -n 1 VERSION)

install: install-lib install-bin

install-lib:
	ocamlfind install -patch-version ${VERSION} $(LIBRARY) lib/META $(INTF) $(IMPL) $(OTHERS) $(DOC) $(COMP_INTF) $(COMP_IMPL) ${OCAMLFIND_BIN}

install-bin:
	install -d -m 755 $(BINDIR)
	install $(BIN) $(BINDIR)

uninstall: uninstall-lib uninstall-bin

uninstall-lib:
	ocamlfind remove $(LIBRARY)

uninstall-bin:
	rm -f $(BINDIR)/$(COMPILER)
	rm -f $(BINDIR)/$(MINIFIER)
	rm -f $(BINDIR)/$(LINKER)
	rm -f $(BINDIR)/$(MKTOP)
	rm -f $(BINDIR)/$(MKCMIS)
	rm -f $(BINDIR)/$(LISTUNITS)

reinstall: uninstall install

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C camlp4 clean
	$(MAKE) -C jsoo_tools clean
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

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
        cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git js_of_ocaml-${VERSION} &&\
	(cd js_of_ocaml-${VERSION}; git checkout ${VERSION}) &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude .git --exclude tests
