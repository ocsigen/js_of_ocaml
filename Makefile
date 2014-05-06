PREFIX ?= /usr/local/

### Optional dependencies: deriving
WITH_DERIVING := $(shell (ocamlfind query deriving 1> /dev/null && echo true || echo false) 2> /dev/null)
WITH_GRAPHICS := $(shell (ocamlfind query graphics 1> /dev/null && echo true || echo false) 2> /dev/null)
WITH_NATDYNLINK := $(shell if [ -f `ocamlc -where`/dynlink.cmxa ]; then echo true; else echo false; fi)

OCB=ocamlbuild #-classic-display

.PHONY: all no_examples examples build
.PHONY: install uninstall clean realclean dist
.PHONY: doc toplevel tests phantomtests

all: no_examples examples
no_examples: build doc
build:
	ocaml pkg/build.ml native=true native-dynlink=$(WITH_NATDYNLINK) deriving=$(WITH_DERIVING) graphics=$(WITH_GRAPHICS)

examples:
	$(OCB) examples/all.otarget

doc:
	$(OCB) doc/api.docdir/index.html

toplevel:
	$(OCB) toplevel/toplevel_expunge.js

install: build
	opam-installer --prefix $(PREFIX) --install js_of_ocaml.install
uninstall:
	opam-installer --prefix $(PREFIX) --remove js_of_ocaml.install

tests:
	$(OCB) tests/tests.js

TESTS_LOG = $(patsubst %.ml,%.jslog,$(wildcard tests/test_*.ml))
phantomtests:
	$(OCB) $(TESTS_LOG)

clean:
	$(OCB) -clean

realclean: clean
	find . -name "*~" -print | xargs rm -f
	find . -name "*.tmpjs" -print | xargs rm -f
	find . -name "#*" -print | xargs rm -f

VERSION := $(shell head -n 1 VERSION)
dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
	cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git js_of_ocaml-${VERSION} &&\
	(cd js_of_ocaml-${VERSION}; git checkout ${VERSION}) &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude .git --exclude tests
