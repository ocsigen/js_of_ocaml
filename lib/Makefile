MLOBJS= js.cmo dom.cmo typed_array.cmo dom_html.cmo dom_svg.cmo file.cmo dom_events.cmo firebug.cmo lwt_js.cmo sys_js.cmo regexp.cmo cSS.cmo url.cmo form.cmo xmlHttpRequest.cmo lwt_js_events.cmo json.cmo jsonp.cmo webGL.cmo webSockets.cmo keycode.cmo eventSource.cmo geolocation.cmo jstable.cmo mutationObserver.cmo worker.cmo
MLINTFS= $(MLOBJS:.cmo=.mli)
COBJS= stubs$(OBJEXT)
OBJS=lib_version.cmo $(MLOBJS) $(COBJS)

JSON_OBJS=$(addprefix deriving_json/,deriving_Json_lexer.cmo deriving_Json.cmo)

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45

include ../Makefile.conf
-include ../Makefile.local

DERIVING_JSON=deriving_json.cma
ifeq "${WITH_DERIVING}" "YES"
PA_DERIVING=syntax/pa_deriving_Json.cmo
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
TYXML_CMA=tyxml/tyxml_js.cma
endif

ifeq "${WITH_ASYNC}" "YES"
ASYNC_CMA=async/async_js.cma
endif

ifeq "${WITH_TOPLEVEL}" "YES"
TOPLEVELLIB= toplevel/jsooTop.cmo toplevel/jsooTopError.cmo
TOPLEVELLIB+= toplevel/jsooTopPpx.cmo
ifeq ($(shell ocamlc -version | grep -q -E "4.0[^01]"; echo $$?),0)
TOPLEVELLIB+= toplevel/jsooTopCamlp4.cmo
endif
endif

PA_JS= syntax/pa_js.cmo
ifeq "${BEST}" "opt"
ifeq "${NATDYNLINK}" "YES"
PA_JS_NDL= syntax/pa_js.cmx syntax/pa_js.cmxs
endif
endif

ifeq "${WITH_PPX}" "YES"
PPX_EX= ppx/$(PPX)
PPX_LIB= ppx/ppx_js.cma ppx/ppx_js.cmxa
ifeq "${WITH_PPX_DRIVER}" "YES"
PPX_LIB+= ppx_driver/ppx_js_driver.cma ppx_driver/ppx_js_driver.cmxa
endif
ifeq "${NATDYNLINK}" "YES"
PPX_LIB_NDL= ppx/ppx_js.cmxs
ifeq "${WITH_PPX_DRIVER}" "YES"
PPX_LIB_NDL+= ppx_driver/ppx_js_driver.cmxs
endif
endif
endif

ifeq "${WITH_PPX_DERIVING}" "YES"
PPX_DERIVING_LIB= ppx/ppx_deriving_json.cma \
	ppx/ppx_deriving_json.a \
	ppx/ppx_deriving_json.cmxa \
	ppx/ppx_deriving_json.cmxs
endif

all: $(LIBNAME).cma log/logger.cma \
	${PA_JS} ${PA_JS_NDL} \
	${PA_DERIVING} 	${PA_DERIVING_NDL} \
	${DERIVING_JSON} \
	${GRAPHICS_CMA} \
	${TYXML_CMA} \
	${ASYNC_CMA} \
	$(PPX_EX) $(PPX_LIB) $(PPX_DERIVING_LIB) $(PPX_LIB_NDL) \
	META

VERSION := $(shell head -n 1 ../VERSION)

lib_version.ml: lib_version.ml.tmp
	if cmp -s $^ $@; then rm $^; else mv $^ $@; fi

.PHONY: lib_version.ml.tmp
lib_version.ml.tmp:
	echo "let s = \"${VERSION}\"" > $@
	echo "let git_version = \"${VERSION_GIT}\"" >> $@

$(LIBNAME).cma: $(OBJS)
	ocamlmklib -o $(LIBNAME) $(OBJS)

deriving_json.cma: $(JSON_OBJS)
	$(OCAMLC) -a -o $@ $^

deriving_json.cmxa: $(JSON_OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $^
deriving_json.cmxs: deriving_json.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $^

syntax/pa_js.cmo: syntax/pa_js.ml
	$(OCAMLC) -w -42-58 $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

syntax/pa_js.cmx: syntax/pa_js.ml
	$(OCAMLOPT) -w -42-58 $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

syntax/pa_js.cmxs: syntax/pa_js.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<

syntax/pa_deriving_Json.cmo: syntax/pa_deriving_Json.ml syntax/pa_deriving_Json.cmi
	$(OCAMLC) -w -27 -I syntax $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmi: syntax/pa_deriving_Json.mli
	$(OCAMLC) $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmx: syntax/pa_deriving_Json.ml
	$(OCAMLOPT) -w -27 -I syntax $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

syntax/pa_deriving_Json.cmxs: syntax/pa_deriving_Json.cmx
	$(OCAMLOPT) -I syntax -shared -linkall -o $@ $<

graphics/%.cmo: graphics/%.ml graphics/%.cmi syntax/pa_js.cmo
	$(OCAMLC) -syntax camlp4o $(SAFESTRING) -package lwt,lwt.syntax,graphics -ppopt syntax/pa_js.cmo -I graphics -c $< -o $@

graphics/%.cmi: graphics/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,graphics -I ./ $< -o $@

graphics/graphics.cma: graphics/graphics_js.cmo
	$(OCAMLC) -a -o $@ $^

tyxml/%.cmo: tyxml/%.ml tyxml/%.cmi tyxml/tyxml_cast_sigs.cmi syntax/pa_js.cmo
	$(OCAMLC) -syntax camlp4o $(SAFESTRING) -package lwt,lwt.syntax,tyxml.functor,reactiveData,react -ppopt syntax/pa_js.cmo -I tyxml -c $< -o $@

tyxml/tyxml_cast_sig.cmi: tyxml/tyxml_cast_sig.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,tyxml.functor,reactiveData,react -I . -I tyxml $< -o $@

tyxml/tyxml_cast.cmi: tyxml/tyxml_cast_sigs.cmi
tyxml/tyxml_js.cmi: tyxml/tyxml_cast_sigs.cmi

tyxml/%.cmi: tyxml/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,tyxml.functor,reactiveData,react -I . -I tyxml $< -o $@

tyxml/tyxml_js.cma: tyxml/tyxml_cast.cmo tyxml/tyxml_js.cmo
	$(OCAMLC) -a -o $@ $^


async/async_js.cma: async/async_js.cmo
	ocamlfind ocamlc -a -o $@ $^

async/%.cmo: async/%.ml async/%.cmi syntax/pa_js.cmo
	$(OCAMLC) -syntax camlp4o $(SAFESTRING) -package lwt.syntax,async_kernel -ppopt syntax/pa_js.cmo -I async -c $< -o $@

async/%.cmi: async/%.mli
	$(OCAMLC) -syntax camlp4o $(SAFESTRING) -package lwt.syntax,async_kernel -ppopt syntax/pa_js.cmo -I . -I async $< -o $@

log/%.cmo: log/%.ml log/%.cmi syntax/pa_js.cmo
	$(OCAMLC) -syntax camlp4o $(SAFESTRING) -package lwt,lwt.syntax,lwt.log -ppopt syntax/pa_js.cmo -I log -c $< -o $@

log/%.cmi: log/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.log -I . -I log $< -o $@

log/logger.cma: log/lwt_log_js.cmo
	$(OCAMLC) -a -o $@ $^

toplevel_lib: $(TOPLEVELLIB)

toplevel/jsooTop.cmo: toplevel/jsooTop.ml syntax/pa_js.cmo ../compiler/lib/compiler.cma
	$(OCAMLC) -syntax camlp4o \
		$(SAFESTRING) -package lwt,lwt.syntax \
		-ppopt syntax/pa_js.cmo \
		-I toplevel -I ../compiler/lib -I +compiler-libs \
		-c $< -o $@

toplevel/%.cmo: toplevel/%.ml
	$(OCAMLC) $(SAFESTRING) \
		-I toplevel -I +compiler-libs -package camlp4 \
		-c $< -o $@

toplevel/%.cmi: toplevel/%.mli
	$(OCAMLC) $(SAFESTRING) \
	-I toplevel -I +compiler-libs $< -o $@



ppx/ppx_js.byte: ppx/ppx_js.cmo ppx/ppx_js_bin.cmo
	$(OCAMLC) $(SAFESTRING) \
		-linkpkg -package ppx_tools.metaquot $^ -o ppx/ppx_js.byte

ppx/ppx_js.opt: ppx/ppx_js.cmx ppx/ppx_js_bin.cmx
	$(OCAMLOPT) $(SAFESTRING) \
		-linkpkg -package ppx_tools.metaquot $^ -o ppx/ppx_js.opt

ppx/%.cmx: ppx/%.ml
	$(OCAMLOPT) $(SAFESTRING) -I ppx -package ppx_tools.metaquot -g -c $<

ppx/%.cmo: ppx/%.ml
	$(OCAMLC) $(SAFESTRING) -I ppx -package ppx_tools.metaquot -c $<

ppx/ppx_js.cmi: ppx/ppx_js.mli
	$(OCAMLC) $(SAFESTRING) -package ppx_tools.metaquot -c $< -o $@

ppx/ppx_js.cma: ppx/ppx_js.cmo
	$(OCAMLC) -a -o $@ $^
ppx/ppx_js.cmxa: ppx/ppx_js.cmx
	$(OCAMLOPT) -a -o $@ $^
ppx/ppx_js.cmxs: ppx/ppx_js.cmxa
	$(OCAMLOPT) -shared -g -o $@ $^

ppx_driver/%.cmx: ppx_driver/%.ml ppx/ppx_js.cmi
	$(OCAMLOPT) $(SAFESTRING) -I ppx -package ppx_tools.metaquot -package ppx_driver -g -c $<
ppx_driver/%.cmo: ppx_driver/%.ml ppx/ppx_js.cmi
	$(OCAMLC) $(SAFESTRING) -I ppx -package ppx_tools.metaquot -package ppx_driver -c $<
ppx_driver/ppx_js_driver.cma: ppx_driver/ppx_js_driver.cmo
	$(OCAMLC) -linkall -I ppx -a -o $@ $^
ppx_driver/ppx_js_driver.cmxa: ppx_driver/ppx_js_driver.cmx
	$(OCAMLOPT) -linkall -I ppx -a -o $@ $^
ppx_driver/ppx_js_driver.cmxs: ppx_driver/ppx_js_driver.cmxa
	$(OCAMLOPT) -linkall -I ppx -shared -g -o $@ $^

$(PPX_EX): ppx/ppx_js.$(BEST)
	cp $< $@

ppx/ppx_deriving_json.cmo: ppx/ppx_deriving_json.ml
	$(OCAMLC) -w -40-42-48-27 -package ppx_deriving,ppx_tools,ppx_tools.metaquot -c $<

ppx/ppx_deriving_json.cma: ppx/ppx_deriving_json.cmo
	$(OCAMLC) -a -o $@ $^

ppx/ppx_deriving_json.cmx: ppx/ppx_deriving_json.ml
	$(OCAMLOPT) -w -40-42-48-27 -package ppx_deriving,ppx_tools.metaquot -c $<

ppx/ppx_deriving_json.a: ppx/ppx_deriving_json.cmx

ppx/ppx_deriving_json.cmxa: ppx/ppx_deriving_json.cmx
	$(OCAMLOPT) -a -o $@ $^

ppx/ppx_deriving_json.cmxs: ppx/ppx_deriving_json.cmx
	$(OCAMLOPT) -shared -o $@ $^

%.cmo: %.ml syntax/pa_js.cmo
	$(OCAMLC) -pp "camlp4o syntax/pa_js.cmo" $(SAFESTRING) -package lwt,uchar -c -g $<

%.cmi: %.mli
	$(OCAMLC) -package lwt,uchar -I deriving_json $(SAFESTRING) -c $<

%.ml: %.mll
	ocamllex $<

${JSON_OBJS}: %.cmo: %.ml ${PA_DERIVING}
	$(OCAMLC) $(SAFESTRING) -I deriving_json -c $<

${JSON_OBJS:.cmo=.cmx}: %.cmx: %.ml ${PA_DERIVING}
	$(OCAMLOPT) $(SAFESTRING) -I deriving_json -c $<

%$(OBJEXT): %.c
	$(OCAMLC) -package lwt -c $<

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
	rm -f ppx/*.cm[xioa] ppx/*.[ao] ppx/*.so ppx/*.cmx[sa]
	rm -f ppx_driver/*.cm[xioa] ppx_driver/*.[ao] ppx_driver/*.so ppx_driver/*.cmx[sa]
	rm -f async/*.cm[xioa] async/*.[ao] async/*.so async/*.cmx[sa]
	rm -f log/*.cm[xioa] log/*.[ao] log/*.so log/*.cmx[sa]
	rm -f tyxml/*.cm[xioa] tyxml/*.[ao] tyxml/*.so tyxml/*.cmx[sa]
	rm -f graphics/*.cm[xioa] graphics/*.[ao] graphics/*.so graphics/*.cmx[sa]
	rm -f toplevel/*.cm[xioa] toplevel/*.[ao] toplevel/*.so toplevel/*.cmx[sa]
	rm -f deriving_json/*.cm[xioa] deriving_json/*.[ao] deriving_json/*.so
	rm -f deriving_json/deriving_Json_lexer.ml
	rm -f ppx/ppx_deriving_json.ml
	rm -f stubs.c
	rm -f lib_version.ml
	rm -f toplevel/jsooTopError.ml

depend: syntax/pa_js.cmo ${PA_DERIVING} sys_js.ml toplevel/jsooTopError.ml ppx/ppx_js.ml
	ocamldep -pp "camlp4o syntax/pa_js.cmo" *.ml *.mli > .depend
	ocamldep -pp "camlp4o syntax/pa_js.cmo" -I async async/*.ml async/*.mli >> .depend
	ocamldep -pp "camlp4o syntax/pa_js.cmo" -I log log/*.ml log/*.mli >> .depend
	ocamldep -pp "camlp4o syntax/pa_js.cmo" -I toplevel toplevel/jsooTop.ml toplevel/jsooTopCamlp4.ml toplevel/*.mli >> .depend
	ocamldep toplevel/jsooTopError.ml toplevel/jsooTopPpx.ml -I toplevel >> .depend
ifeq "${WITH_GRAPHICS}" "YES"
	ocamldep -pp "camlp4o syntax/pa_js.cmo" graphics/*.ml  >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I graphics graphics/graphics_js.mli  >> .depend
endif
ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
	ocamldep -pp "camlp4o syntax/pa_js.cmo" -I tyxml tyxml/*.ml tyxml/*.mli >> .depend
endif
ifeq "${WITH_PPX}" "YES"
	ocamldep -I ppx ppx/ppx_js.ml ppx/ppx_js_bin.ml ppx/ppx_js.mli >> .depend
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
