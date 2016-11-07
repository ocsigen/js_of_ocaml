
include ../Makefile.conf
-include ../Makefile.filelist

OCAMLBUILD := ocamlbuild -use-ocamlfind -package bytes

VERSION := $(shell head -n 1 ../VERSION)

all:
	$(OCAMLBUILD) $(OCAMLBUILD_IMPL)

install-meta:
	ocamlfind install -patch-version ${VERSION} js_of_ocaml-ocamlbuild META

clean:
	$(OCAMLBUILD) -clean
