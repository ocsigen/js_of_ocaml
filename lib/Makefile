MLOBJS= js.cmo dom.cmo typed_array.cmo dom_html.cmo file.cmo dom_events.cmo firebug.cmo lwt_js.cmo sys_js.cmo regexp.cmo cSS.cmo url.cmo form.cmo xmlHttpRequest.cmo lwt_js_events.cmo json.cmo webGL.cmo webSockets.cmo keycode.cmo
MLINTFS= $(MLOBJS:.cmo=.mli)
COBJS= stubs$(OBJEXT)
OBJS=$(MLOBJS) $(COBJS)

JSON_OBJS=$(addprefix deriving_json/,deriving_Json_lexer.cmo deriving_Json.cmo)

include ../Makefile.conf
-include ../Makefile.local

ifneq "${WITH_DERIVING}" ""
PA_DERIVING=syntax/pa_deriving_Json.cmo
DERIVING_JSON=deriving_json.cma
ifeq "$(BEST)" "opt"
DERIVING_JSON += deriving_json.cmxs
ifeq "${NATDYNLINK}" "YES"
PA_DERIVING_NDL= syntax/pa_deriving_Json.cmx syntax/pa_deriving_Json.cmxs
endif
endif
endif

ifneq "${WITH_GRAPHICS}" ""
GRAPHICS_CMA=graphics/graphics.cma
endif

PA_JS= syntax/pa_js.cmo
ifeq "${BEST}" "opt"
ifeq "${NATDYNLINK}" "YES"
PA_JS_NDL= syntax/pa_js.cmx syntax/pa_js.cmxs
endif
endif

all: $(LIBNAME).cma \
	${PA_JS} ${PA_JS_NDL} \
	${PA_DERIVING} 	${PA_DERIVING_NDL} \
	${DERIVING_JSON} \
	${GRAPHICS_CMA}

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

syntax/pa_js.cmx: syntax/pa_js.ml
	ocamlfind ocamlopt -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

syntax/pa_js.cmxs: syntax/pa_js.cmx
	ocamlfind ocamlopt -shared -linkall -o $@ $<

syntax/pa_deriving_Json.cmo: syntax/pa_deriving_Json.ml syntax/pa_deriving_Json.cmi
	ocamlfind ocamlc -I syntax -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmi: syntax/pa_deriving_Json.mli
	ocamlfind ocamlc -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmx: syntax/pa_deriving_Json.ml
	ocamlfind ocamlopt -I syntax -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmxs: syntax/pa_deriving_Json.cmx
	ocamlfind ocamlopt -I syntax -shared -linkall -o $@ $<

%.cmo: %.ml syntax/pa_js.cmo
	ocamlfind ocamlc -pp "camlp4o syntax/pa_js.cmo" -package lwt -c -g $<

sys_js.ml: sys_js.mlp
	sed -e "s|%%VERSION%%|`sed -e 1q ../VERSION`|" sys_js.mlp >sys_js.ml

%.cmi: %.mli
	ocamlfind ocamlc -package lwt -I deriving_json -c $<

%.ml: %.mll
	ocamllex $<

${JSON_OBJS}: %.cmo: %.ml ${PA_DERIVING}
	ocamlfind ocamlc -package deriving.syntax.common -syntax camlp4o -ppopt ${PA_DERIVING} -I deriving_json -c $<

${JSON_OBJS:.cmo=.cmx}: %.cmx: %.ml ${PA_DERIVING}
	ocamlfind ocamlopt -package deriving.syntax.common -syntax camlp4o -ppopt ${PA_DERIVING} -I deriving_json -c $<

graphics/graphics_js.cmo: graphics/graphics_js.ml graphics/graphics_js.cmi syntax/pa_js.cmo
	ocamlfind ocamlc -syntax camlp4o -package lwt,lwt.syntax,graphics -ppopt syntax/pa_js.cmo  -I ./ -I graphics -c graphics/graphics_js.ml -o $@

graphics/graphics_js.cmi: graphics/graphics_js.ml
	ocamlfind ocamlc -package lwt,graphics -I ./ graphics/graphics_js.mli -o $@

graphics/graphics.cma: graphics/graphics_js.cmo
	ocamlfind ocamlc -a -o $@ $^

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
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]
	rm -f syntax/*.cm[xioa] syntax/*.[ao] syntax/*.so syntax/*.cmx[sa]
	rm -f deriving_json/*.cm[xioa] deriving_json/*.[ao] deriving_json/*.so
	rm -f deriving_json/deriving_Json_lexer.ml
	rm -f stubs.c
	rm -f sys_js.ml

depend: syntax/pa_js.cmo ${PA_DERIVING} sys_js.ml
	ocamldep -pp "camlp4o syntax/pa_js.cmo" *.ml *.mli > .depend
ifneq "${WITH_GRAPHICS}" ""
	ocamldep -pp "camlp4o syntax/pa_js.cmo" graphics/*.ml  >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I graphics graphics/graphics_js.mli  >> .depend
endif
ifneq "${WITH_DERIVING}" ""
	${MAKE} --no-print-directory deriving_json/deriving_Json_lexer.ml
	ls deriving_json/*.mli deriving_json/*.ml | \
	  grep -v deriving_Json_stdlib.mli | \
	  xargs ocamlfind ocamldep -I deriving_json -package deriving.syntax.common -syntax camlp4o -ppopt ${PA_DERIVING} >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I deriving_json deriving_json/deriving_Json_stdlib.mli  >> .depend
endif

include .depend
