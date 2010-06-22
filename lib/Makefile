
MLOBJS= js.cmo dom.cmo dom_html.cmo xmlHttpRequest.cmo lwt_js.cmo firebug.cmo
MLINTFS= $(MLOBJS:.cmo=.mli)
COBJS= stubs.o
OBJS=$(MLOBJS) $(COBJS)

include ../Makefile.conf

all: $(LIBNAME).cma syntax/pa_js.cmo doc

$(LIBNAME).cma: $(OBJS)
	ocamlmklib -o $(LIBNAME) $(OBJS)

syntax/pa_js.cmo: syntax/pa_js.ml
	ocamlfind ocamlc -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

%.cmo: %.ml syntax/pa_js.cmo
	ocamlfind ocamlc -pp "camlp4o syntax/pa_js.cmo" -package lwt -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package lwt -c $<

%.o: %.c
	ocamlfind ocamlc -package lwt -c $<

stubs.c: $(MLOBJS:cmo=ml)
	(echo "#include <stdlib.h>"; \
	 echo "#include <stdio.h>"; \
	 echo "#define D(f) void f () { fprintf(stderr, \"Unimplemented Javascript primitive %s!\\\\n\", #f); exit(1); }"; \
	 (sed -n -e 's/.*external.*"\([^"%]*\)".*/D(\1)/p' $(MLOBJS:cmo=ml) | \
	  sort | uniq)) \
        > stubs.c

.PHONY: doc
doc: doc/index.html
doc/index.html: $(MLINTFS)
	mkdir -p doc
	ocamlfind ocamldoc -d doc -package lwt -html $^

clean:
	rm -f *.cm[ioa] *.[ao] *.so
	rm -f syntax/*.cm[io]
	rm -f stubs.c

depend: syntax/pa_js.cmo
	find . -maxdepth 1 \! -name pa_js.ml -regex ".*\\.mli?" | \
	xargs ocamldep -pp "camlp4o syntax/pa_js.cmo" > .depend

include .depend
