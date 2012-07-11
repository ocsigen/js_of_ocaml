MLOBJS= js.cmo dom.cmo typed_array.cmo dom_html.cmo file.cmo dom_events.cmo lwt_js.cmo firebug.cmo regexp.cmo CSS.cmo url.cmo form.cmo xmlHttpRequest.cmo event_arrows.cmo lwt_js_events.cmo json.cmo webGL.cmo
MLINTFS= $(MLOBJS:.cmo=.mli)
COBJS= stubs$(OBJEXT)
OBJS=$(MLOBJS) $(COBJS)

JSON_OBJS=$(addprefix deriving_json/,deriving_Json_lexer.cmo deriving_Json.cmo)

include ../Makefile.conf
-include ../Makefile.local

ifneq "${DERIVING}" ""
PA_DERIVING=syntax/pa_deriving_Json.cmo
DERIVING_JSON=deriving_json.cma
ifeq "$(BEST)" "opt"
DERIVING_JSON += deriving_json.cmxs
endif
endif

all: $(LIBNAME).cma syntax/pa_js.cmo ${PA_DERIVING} ${DERIVING_JSON}

$(LIBNAME).cma: $(OBJS)
	ocamlmklib -o $(LIBNAME) $(OBJS)

deriving_json.cma: $(JSON_OBJS)
	ocamlfind ocamlc -a -o $@ $^

deriving_json.cmxa: $(JSON_OBJS:.cmo=.cmx)
	ocamlfind ocamlopt -a -o $@ $^
deriving_json.cmxs: deriving_json.cmxa
	ocamlfind ocamlopt -shared -linkall -o $@ $^

syntax/pa_js.cmo: syntax/pa_js.ml
	ocamlfind ocamlc -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmo: syntax/pa_deriving_Json.ml
	ocamlfind ocamlc -package deriving-ocsigen.syntax,camlp4.quotations.o -syntax camlp4o -c $<

%.cmo: %.ml syntax/pa_js.cmo
	ocamlfind ocamlc -pp "camlp4o syntax/pa_js.cmo" -package lwt -c -g $<

%.cmi: %.mli
	ocamlfind ocamlc -package lwt -I deriving_json -c $<

%.ml: %.mll
	ocamllex $<

${JSON_OBJS}: %.cmo: %.ml ${PA_DERIVING}
	ocamlfind ocamlc -package deriving-ocsigen.syntax -syntax camlp4o -ppopt ${PA_DERIVING} -I deriving_json -c $<

${JSON_OBJS:.cmo=.cmx}: %.cmx: %.ml ${PA_DERIVING}
	ocamlfind ocamlopt -package deriving-ocsigen.syntax -syntax camlp4o -ppopt ${PA_DERIVING} -I deriving_json -c $<


%$(OBJEXT): %.c
	ocamlfind ocamlc -package lwt -c $<

stubs.c: $(MLOBJS:cmo=ml)
	(echo "#include <stdlib.h>"; \
	 echo "#include <stdio.h>"; \
	 echo "#define D(f) void f () { fprintf(stderr, \"Unimplemented Javascript primitive %s!\\\\n\", #f); exit(1); }"; \
	 (sed -n -e 's/.*external.*"\([^"%]*\)".*/D(\1)/p' $(MLOBJS:cmo=ml) | \
	  sort | uniq)) \
        > stubs.c

clean:
	rm -f *.cm[ioa] *.[ao] *.so
	rm -f syntax/*.cm[io]
	rm -f deriving_json/*.cm[ioa] deriving_json/*.[ao] deriving_json/*.so
	rm -f deriving_json/deriving_Json_lexer.ml
	rm -f stubs.c

depend: syntax/pa_js.cmo ${PA_DERIVING}
	ocamldep -pp "camlp4o syntax/pa_js.cmo" *.ml *.mli > .depend
ifneq "${DERIVING}" ""
	${MAKE} --no-print-directory deriving_json/deriving_Json_lexer.ml
	ls deriving_json/*.mli deriving_json/*.ml | \
	  grep -v deriving_Json_stdlib.mli | \
	  xargs ocamlfind ocamldep -I deriving_json -package deriving-ocsigen.syntax -syntax camlp4o -ppopt ${PA_DERIVING} >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I deriving_json deriving_json/deriving_Json_stdlib.mli  >> .depend
endif

include .depend
