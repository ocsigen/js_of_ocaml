MLOBJS= js.cmo dom.cmo typed_array.cmo dom_html.cmo dom_svg.cmo file.cmo dom_events.cmo firebug.cmo lwt_js.cmo sys_js.cmo regexp.cmo cSS.cmo url.cmo form.cmo xmlHttpRequest.cmo lwt_js_events.cmo json.cmo jsonp.cmo webGL.cmo webSockets.cmo keycode.cmo eventSource.cmo
MLINTFS= $(MLOBJS:.cmo=.mli)
COBJS= stubs$(OBJEXT)
OBJS=$(MLOBJS) $(COBJS)

JSON_OBJS=$(addprefix deriving_json/,deriving_Json_lexer.cmo deriving_Json.cmo)

include ../Makefile.conf
-include ../Makefile.local

ifeq "${WITH_DERIVING}" "YES"
PA_DERIVING=syntax/pa_deriving_Json.cmo
DERIVING_JSON=deriving_json.cma
ifeq "$(BEST)" "opt"
DERIVING_JSON += deriving_json.cmxs
ifeq "${NATDYNLINK}" "YES"
PA_DERIVING_NDL= syntax/pa_deriving_Json.cmx syntax/pa_deriving_Json.cmxs
endif
endif
endif

ifeq "${WITH_GRAPHICS}" "YES"
GRAPHICS_CMA=graphics/graphics.cma
endif

ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
TYXML_CMA=tyxml/tyxml.cma
endif

ifeq "${WITH_TOPLEVEL}" "YES"
TOPLEVELLIB= toplevel/jsooTop.cmo
endif

PA_JS= syntax/pa_js.cmo
ifeq "${BEST}" "opt"
ifeq "${NATDYNLINK}" "YES"
PA_JS_NDL= syntax/pa_js.cmx syntax/pa_js.cmxs
endif
endif

all: $(LIBNAME).cma log/logger.cma \
	${PA_JS} ${PA_JS_NDL} \
	${PA_DERIVING} 	${PA_DERIVING_NDL} \
	${DERIVING_JSON} \
	${GRAPHICS_CMA} \
	${TYXML_CMA}

$(LIBNAME).cma: $(OBJS)
	ocamlmklib -o $(LIBNAME) $(OBJS)

deriving_json.cma: $(JSON_OBJS)
	ocamlfind ocamlc -a -o $@ $^

deriving_json.cmxa: $(JSON_OBJS:.cmo=.cmx)
	ocamlfind ocamlopt -a -o $@ $^
deriving_json.cmxs: deriving_json.cmxa
	ocamlfind ocamlopt -shared -linkall -o $@ $^

syntax/pa_js.cmo: syntax/pa_js.ml
	ocamlfind ocamlc $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

syntax/pa_js.cmx: syntax/pa_js.ml
	ocamlfind ocamlopt $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

syntax/pa_js.cmxs: syntax/pa_js.cmx
	ocamlfind ocamlopt -shared -linkall -o $@ $<

syntax/pa_deriving_Json.cmo: syntax/pa_deriving_Json.ml syntax/pa_deriving_Json.cmi
	ocamlfind ocamlc -I syntax $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmi: syntax/pa_deriving_Json.mli
	ocamlfind ocamlc $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmx: syntax/pa_deriving_Json.ml
	ocamlfind ocamlopt -I syntax $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmxs: syntax/pa_deriving_Json.cmx
	ocamlfind ocamlopt -I syntax -shared -linkall -o $@ $<

graphics/%.cmo: graphics/%.ml graphics/%.cmi syntax/pa_js.cmo
	ocamlfind ocamlc -syntax camlp4o $(SAFESTRING) -package lwt,lwt.syntax,graphics -ppopt syntax/pa_js.cmo -I graphics -c $< -o $@

graphics/%.cmi: graphics/%.mli
	ocamlfind ocamlc $(SAFESTRING) -package lwt,graphics -I ./ $< -o $@

graphics/graphics.cma: graphics/graphics_js.cmo
	ocamlfind ocamlc -a -o $@ $^

tyxml/%.cmo: tyxml/%.ml tyxml/%.cmi tyxml/tyxml_cast_sigs.cmi syntax/pa_js.cmo
	ocamlfind ocamlc -syntax camlp4o $(SAFESTRING) -package lwt,lwt.syntax,tyxml.functor,reactiveData,react -ppopt syntax/pa_js.cmo -I tyxml -c $< -o $@

tyxml/tyxml_cast_sig.cmi: tyxml/tyxml_cast_sig.mli
	ocamlfind ocamlc $(SAFESTRING) -package lwt,tyxml.functor,reactiveData,react -I . -I tyxml $< -o $@

tyxml/%.cmi: tyxml/%.mli tyxml/tyxml_cast_sigs.cmi
	ocamlfind ocamlc $(SAFESTRING) -package lwt,tyxml.functor,reactiveData,react -I . -I tyxml $< -o $@

tyxml/tyxml.cma: tyxml/tyxml_cast.cmo tyxml/tyxml_js.cmo
	ocamlfind ocamlc -a -o $@ $^


log/%.cmo: log/%.ml log/%.cmi syntax/pa_js.cmo
	ocamlfind ocamlc -syntax camlp4o $(SAFESTRING) -package lwt,lwt.syntax,lwt.log -ppopt syntax/pa_js.cmo -I log -c $< -o $@

log/%.cmi: log/%.mli
	ocamlfind ocamlc $(SAFESTRING) -package lwt,lwt.log -I . -I log $< -o $@

log/logger.cma: log/lwt_log_js.cmo
	ocamlfind ocamlc -a -o $@ $^

toplevel_lib: $(TOPLEVELLIB)

toplevel/jsooTop.cmo: toplevel/jsooTop.ml toplevel/jsooTop.cmi syntax/pa_js.cmo ../compiler/compiler.cma
	ocamlfind ocamlc -syntax camlp4o \
		$(SAFESTRING) -package lwt,lwt.syntax \
		-ppopt syntax/pa_js.cmo \
		-I toplevel -I ../compiler -I +compiler-libs \
		-c $< -o $@

toplevel/jsooTop.cmi: toplevel/jsooTop.mli
	ocamlfind ocamlc $(SAFESTRING) -package lwt -I . -I log $< -o $@

%.ml: %.cppo.ml
	cppo $< -o $@ -V OCAML:`ocamlc -version`

%.cmo: %.ml syntax/pa_js.cmo
	ocamlfind ocamlc -pp "camlp4o syntax/pa_js.cmo" $(SAFESTRING) -package lwt -c -g $<

sys_js.ml: sys_js.mlp
	sed -e "s|%%VERSION%%|`sed -e 1q ../VERSION`|" sys_js.mlp >sys_js.ml

%.cmi: %.mli
	ocamlfind ocamlc -package lwt -I deriving_json $(SAFESTRING) -c $<

%.ml: %.mll
	ocamllex $<

${JSON_OBJS}: %.cmo: %.ml ${PA_DERIVING}
	ocamlfind ocamlc $(SAFESTRING) -package deriving.syntax.common -syntax camlp4o -ppopt ${PA_DERIVING} -I deriving_json -c $<

${JSON_OBJS:.cmo=.cmx}: %.cmx: %.ml ${PA_DERIVING}
	ocamlfind ocamlopt $(SAFESTRING) -package deriving.syntax.common -syntax camlp4o -ppopt ${PA_DERIVING} -I deriving_json -c $<

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
	rm -f log/*.cm[xioa] log/*.[ao] log/*.so log/*.cmx[sa]
	rm -f tyxml/*.cm[xioa] tyxml/*.[ao] tyxml/*.so tyxml/*.cmx[sa]
	rm -f graphics/*.cm[xioa] graphics/*.[ao] graphics/*.so graphics/*.cmx[sa]
	rm -f toplevel/*.cm[xioa] toplevel/*.[ao] toplevel/*.so toplevel/*.cmx[sa]
	rm -f deriving_json/*.cm[xioa] deriving_json/*.[ao] deriving_json/*.so
	rm -f deriving_json/deriving_Json_lexer.ml
	rm -f stubs.c
	rm -f sys_js.ml

depend: syntax/pa_js.cmo ${PA_DERIVING} sys_js.ml
	ocamldep -pp "camlp4o syntax/pa_js.cmo" *.ml *.mli > .depend
	ocamldep -pp "camlp4o syntax/pa_js.cmo" log/*.ml log/*.mli >> .depend
	ocamldep -pp "camlp4o syntax/pa_js.cmo" toplevel/*.ml toplevel/*.mli >> .depend
ifeq "${WITH_GRAPHICS}" "YES"
	ocamldep -pp "camlp4o syntax/pa_js.cmo" graphics/*.ml  >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I graphics graphics/graphics_js.mli  >> .depend
endif
ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
	ocamldep -pp "camlp4o syntax/pa_js.cmo" tyxml/*.ml tyxml/*.mli >> .depend
endif
ifeq "${WITH_DERIVING}" "YES"
	${MAKE} --no-print-directory deriving_json/deriving_Json_lexer.ml
	ls deriving_json/*.mli deriving_json/*.ml | \
	  grep -v deriving_Json_stdlib.mli | \
	  xargs ocamlfind ocamldep -I deriving_json -package deriving.syntax.common -syntax camlp4o -ppopt ${PA_DERIVING} >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I deriving_json deriving_json/deriving_Json_stdlib.mli  >> .depend
endif

include .depend
