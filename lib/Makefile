
MLOBJS= js.cmo dom.cmo dom_html.cmo xmlHttpRequest.cmo lwt_js.cmo \
	goog.cmo goog_net.cmo
COBJS= stubs.o
OBJS=$(MLOBJS) $(COBJS)

LIB=ocaml2js

all: $(LIB).cma pa_js.cmo

$(LIB).cma: $(OBJS)
	ocamlmklib -o $(LIB) $(OBJS)

pa_js.cmo: pa_js.ml
	ocamlfind ocamlc -package camlp4.extend,camlp4.quotations -syntax camlp4o -c pa_js.ml

%.cmo: %.ml pa_js.cmo
	ocamlfind ocamlc -pp "camlp4o pa_js.cmo" -package lwt -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package lwt -c $<

%.o: %.c
	ocamlfind ocamlc -package lwt -c $<

stubs.c: $(MLOBJS:cmo=ml)
	(echo "#include <stdlib.h>"; \
	 echo "#define D(f) void f () { exit(1); }"; \
	 (sed -n -e 's/.*external.*"\([^"%]*\)".*/D(\1)/p' $(MLOBJS:cmo=ml) | \
	  sort | uniq)) \
        > stubs.c

clean:
	rm -f *.cm[ioa] *.[ao] *.so
	rm -f stubs.c

depend:
	find . -maxdepth 1 \! -name pa_js.ml -regex ".*\\.mli?" | \
	xargs ocamldep -pp "camlp4o pa_js.cmo" > .depend

include .depend
