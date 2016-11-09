include ../Makefile.conf
include ../Makefile.filelist

MLOBJS= $(LIBRARY_INTF:cmi=cmo)
COBJS= stubs$(OBJEXT)
OBJS=lib_version.cmo $(MLOBJS) $(COBJS)

JSON_OBJS=$(addprefix deriving_json/,deriving_Json_lexer.cmo deriving_Json.cmo)

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45
VERSION := $(shell head -n 1 ../VERSION)



all: $(LIBRARY).cma \
     $(PPX_INSTALL) $(PPX_DRIVER_INSTALL) \
     $(PPX_DERIVING_INSTALL) \
     META

lib_version.ml: lib_version.ml.tmp
	if cmp -s $^ $@; then rm $^; else mv $^ $@; fi

.PHONY: lib_version.ml.tmp

lib_version.ml.tmp:
	echo "let s = \"${VERSION}\" let git_version = \"${VERSION_GIT}\"" > $@

$(LIBRARY).cma: $(OBJS)
	ocamlmklib -o $(LIBRARY) $(OBJS)

deriving_json.cma: $(JSON_OBJS)
	$(OCAMLC) -a -o $@ $^

deriving_json.cmxa: $(JSON_OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $^
deriving_json.cmxs: deriving_json.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $^


graphics/%.cmo: graphics/%.ml graphics/%.cmi ppx/ppx_js.byte
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.syntax,graphics -ppx ppx/ppx_js.byte -I graphics -c $< -o $@

graphics/%.cmi: graphics/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,graphics -I ./ $< -o $@

graphics/graphics.cma: graphics/graphics_js.cmo
	$(OCAMLC) -a -o $@ $^

tyxml/%.cmo: tyxml/%.ml tyxml/%.cmi tyxml/tyxml_cast_sigs.cmi ppx/ppx_js.byte
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.syntax,tyxml.functor,reactiveData,react -ppx ppx/ppx_js.byte -I tyxml -c $< -o $@

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

async/%.cmo: async/%.ml async/%.cmi ppx/ppx_js.byte
	$(OCAMLC) $(SAFESTRING) -package lwt.syntax,async_kernel -ppx ppx/ppx_js.byte -I async -c $< -o $@

async/%.cmi: async/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt.syntax,async_kernel -ppx ppx/ppx_js.byte -I . -I async $< -o $@

log/%.cmo: log/%.ml log/%.cmi ppx/ppx_js.byte
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.syntax,lwt.log -ppx ppx/ppx_js.byte -I log -c $< -o $@

log/%.cmi: log/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.log -I . -I log $< -o $@

log/logger.cma: log/lwt_log_js.cmo
	$(OCAMLC) -a -o $@ $^


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

ppx/$(PPX): ppx/ppx_js.$(BEST)
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

%.cmo: %.ml ppx/ppx_js.byte
	$(OCAMLC) -ppx ppx/ppx_js.byte $(SAFESTRING) -package lwt,uchar -c -g $<

%.cmi: %.mli
	$(OCAMLC) -package lwt,uchar -I deriving_json $(SAFESTRING) -c $<

%.ml: %.mll
	ocamllex $<

${JSON_OBJS}: %.cmo: %.ml
	$(OCAMLC) $(SAFESTRING) -I deriving_json -c $<

${JSON_OBJS:.cmo=.cmx}: %.cmx: %.ml
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
	rm -f ppx/*.cm[xioa] ppx/*.[ao] ppx/*.so ppx/*.cmx[sa]
	rm -f ppx_driver/*.cm[xioa] ppx_driver/*.[ao] ppx_driver/*.so ppx_driver/*.cmx[sa]
	rm -f async/*.cm[xioa] async/*.[ao] async/*.so async/*.cmx[sa]
	rm -f log/*.cm[xioa] log/*.[ao] log/*.so log/*.cmx[sa]
	rm -f tyxml/*.cm[xioa] tyxml/*.[ao] tyxml/*.so tyxml/*.cmx[sa]
	rm -f graphics/*.cm[xioa] graphics/*.[ao] graphics/*.so graphics/*.cmx[sa]
	rm -f deriving_json/*.cm[xioa] deriving_json/*.[ao] deriving_json/*.so
	rm -f deriving_json/deriving_Json_lexer.ml
	rm -f ppx/ppx_deriving_json.ml
	rm -f stubs.c
	rm -f lib_version.ml


depend: ppx/ppx_js.byte sys_js.ml ppx/ppx_js.ml
	ocamldep -ppx ppx/ppx_js.byte *.ml *.mli > .depend
	ocamldep -ppx ppx/ppx_js.byte -I async async/*.ml async/*.mli >> .depend
	ocamldep -ppx ppx/ppx_js.byte -I log log/*.ml log/*.mli >> .depend
ifeq "${WITH_GRAPHICS}" "YES"
	ocamldep -ppx ppx/ppx_js.byte graphics/*.ml  >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I graphics graphics/graphics_js.mli  >> .depend
endif
ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
	ocamldep -ppx ppx/ppx_js.byte -I tyxml tyxml/*.ml tyxml/*.mli >> .depend
endif
ifeq "${WITH_PPX}" "YES"
	ocamldep -I ppx ppx/ppx_js.ml ppx/ppx_js_bin.ml ppx/ppx_js.mli >> .depend
endif
	${MAKE} --no-print-directory deriving_json/deriving_Json_lexer.ml
	ocamldep -I deriving_json deriving_json/deriving_Json.ml deriving_json/deriving_Json_lexer.ml{,i} >> .depend
include .depend
