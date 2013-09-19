OCAMLBUILD := ocamlbuild -use-ocamlfind

include ../Makefile.conf
-include ../Makefile.filelist

all:
	$(OCAMLBUILD) $(OCAMLBUILD_IMPL)

clean:
	$(OCAMLBUILD) -clean
