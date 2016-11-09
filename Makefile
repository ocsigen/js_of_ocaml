include Makefile.conf
include Makefile.filelist

VERSION := $(shell head -n 1 VERSION)

all: no_examples examples
no_examples: build doc

build: build-compiler build-library build-ocamlbuild build-runtime \
       build-toplevel build-camlp4

examples: build-compiler build-library build-runtime
	$(MAKE) -C examples

doc: build-library build-ocamlbuild build-toplevel build-camlp4
	$(MAKE) -C doc

toplevel-examples:
	$(MAKE) -C toplevel/examples

tests: build-compiler build-library build-runtime
	$(MAKE) -C tests

phantomtests: build-compiler build-library build-runtime
	$(MAKE) -C tests phantom

reinstall: uninstall install
install: install-libs install-bins
install-libs: install-library install-camlp4 install-compiler install-toplevel install-ocamlbuild
install-bins:
	install -d -m 755 $(BINDIR)
	install $(COMPILER_BIN) $(BINDIR)
	install $(TOPLEVEL_BIN) $(BINDIR)

uninstall: uninstall-libs uninstall-bin
uninstall-libs:
	ocamlfind remove $(LIBRARY)
	ocamlfind remove $(LIBRARY)-camlp4
	ocamlfind remove $(LIBRARY)-compiler
	ocamlfind remove $(LIBRARY)-toplevel
	ocamlfind remove $(LIBRARY)-ocamlbuild
uninstall-bin:
	rm -f $(BINDIR)/$(COMPILER)
	rm -f $(BINDIR)/$(MINIFIER)
	rm -f $(BINDIR)/$(MKTOP)
	rm -f $(BINDIR)/$(MKCMIS)
	rm -f $(BINDIR)/$(LISTUNITS)

build-compiler:
	$(MAKE) -C compiler
build-runtime:
	$(MAKE) -C runtime
build-ocamlbuild:
	$(MAKE) -C ocamlbuild
build-camlp4:
	$(MAKE) -C camlp4
build-toplevel:
	$(MAKE) -C toplevel/lib
	$(MAKE) -C toplevel/bin
build-library:
	$(MAKE) -C lib

install-compiler:
	ocamlfind install -patch-version ${VERSION} js_of_ocaml-compiler \
	compiler/META \
	$(addprefix compiler/lib/, $(COMPILER_INSTALL)) \
	$(addprefix compiler/findlib_support/, $(FINDLIB_INSTALL)) \
	$(addprefix runtime/,runtime.js $(JSOO_RUNTIME) $(JSOO_RUNTIME_EXTRA))
install-ocamlbuild:
	ocamlfind install -patch-version ${VERSION} js_of_ocaml-ocamlbuild \
	ocamlbuild/META \
	$(addprefix ocamlbuild/_build/, $(OCAMLBUILD_INSTALL))
install-camlp4:
	ocamlfind install -patch-version $(VERSION) js_of_ocaml-camlp4 \
	$(addprefix camlp4/, META $(CAMLP4_INSTALL))
install-toplevel:
	ocamlfind install -patch-version ${VERSION} js_of_ocaml-toplevel \
	$(addprefix toplevel/lib/, META $(TOPLEVEL_INSTALL))
install-library:
	ocamlfind install -patch-version ${VERSION} $(LIBRARY) \
	$(addprefix lib/, META $(LIBRARY_INSTALL)) \
	$(addprefix lib/, $(PPX_INSTALL)) \
	$(addprefix lib/, $(PPX_DRIVER_INSTALL)) \
	$(addprefix lib/, $(PPX_DERIVING_INSTALL))


install-doc:
	echo $(DOC)

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend
	$(MAKE) -C toplevel/lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C runtime clean
	$(MAKE) -C camlp4 clean
	$(MAKE) -C ocamlbuild clean
	$(MAKE) -C toplevel/lib clean
	$(MAKE) -C toplevel/bin clean
	$(MAKE) -C toplevel/examples clean
	$(MAKE) -C lib clean
	$(MAKE) -C examples clean
ifeq ($(wildcard tests),tests)
	$(MAKE) -C tests clean
	$(MAKE) -C doc clean
endif

realclean: clean
	find . -name "*~" -print | xargs rm -f
	find . -name "*.tmpjs" -print | xargs rm -f
	find . -name "#*" -print | xargs rm -f

.PHONY: all no_examples compiler library ocamlbuild runtime examples doc build toplevel_bin toplevel_lib toplevel

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
        cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git js_of_ocaml-${VERSION} &&\
	(cd js_of_ocaml-${VERSION}; git checkout ${VERSION}) &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude .git --exclude tests
