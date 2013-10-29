BINDIR := /usr/local/bin

OCB= BINDIR=$(BINDIR) ocamlbuild #-classic-display

all: no_examples examples install.sh


no_examples: check_lwt
	$(OCB) all.otarget

.PHONY: all no_examples compiler library ocamlbuild runtime examples check_lwt doc toplevel

compiler:
	$(OCB) compiler/compile.native

compiler_lib:
	$(OCB) compiler/compiler.cma compiler/compiler.cmxs compiler/compiler.cmxa

library:
	$(OCB) lib/js_of_ocaml.cma

ocamlbuild:
	$(OCB) ocamlbuild/ocamlbuild_js_of_ocaml.cma ocamlbuild/ocamlbuild_js_of_ocaml.cmxs ocamlbuild/ocamlbuild_js_of_ocaml.cmxa

runtime:
	$(OCB) runtime/runtime.js

examples:
	$(OCB) examples/all.otarget

doc:
	$(OCB) doc/api.docdir/index.html

toplevel:
	$(OCB) toplevel/toplevel_expunge.js

tests: check_phantomjs
	$(OCB) tests/tests.js

TESTS_LOG = $(patsubst %.ml,%.jslog,$(wildcard tests/test_*.ml))
phantomtests: check_phantomjs
	$(OCB) $(TESTS_LOG)

LWTERROR="Js_of_ocaml requires Lwt version 2.3.0 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.3.0; then \
	  echo $(LWTERROR); exit 1; \
	fi
PHANTOMJSERROR="You need phantomjs in your PATH to run this"
check_phantomjs:
	@if which phantomjs; then echo ""; else \
		echo $(PHANTOMJSERROR); exit 1; \
	fi

VERSION := $(shell head -n 1 VERSION)

install.sh:
	$(OCB) install.sh

install: install.sh
	cd _build && sh install.sh && cd -

uninstall:
	ocamlfind remove js_of_ocaml
	rm -f $(BINDIR)/js_of_ocaml
	rm -f $(BINDIR)/joo_minify

reinstall: uninstall install

clean:
	$(OCB) -clean

realclean: clean
	find . -name "*~" -print | xargs rm -f

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
	cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git js_of_ocaml-${VERSION} &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude _darcs --exclude tests
