MLOBJS= js.cmo dom.cmo dom_html.cmo dom_events.cmo lwt_js.cmo firebug.cmo regexp.cmo url.cmo file.cmo form.cmo xmlHttpRequest.cmo event_arrows.cmo
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
	ocamlfind ocamlc -pp "camlp4o syntax/pa_js.cmo" -package lwt -c -g $<

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

ocsi_doc:
	mkdir -p docdiv
	ocamlopt -shared -I /usr/lib/ocaml/ocamldoc -o ../tools/odoc_ocsigen.cmxs ../tools/odoc_ocsigen.ml
	ocamlfind ocamldoc -package lwt -d docdiv -g ../tools/odoc_ocsigen.cmxs $(MLINTFS) -t "Js_of_ocaml - API Reference"


clean:
	rm -f *.cm[ioa] *.[ao] *.so
	rm -f syntax/*.cm[io]
	rm -f stubs.c

depend: syntax/pa_js.cmo
	ls *.ml *.mli | grep -v pa_js.ml | \
	xargs ocamldep -pp "camlp4o syntax/pa_js.cmo" > .depend

include .depend
