include ../Makefile.conf
include ../Makefile.filelist

MLOBJS= $(LIBRARY_BASE_INTF:cmi=cmo)
COBJS= stubs$(OBJEXT)
OBJS=lib_version.cmo $(MLOBJS) $(COBJS)

JSON_OBJS=$(addprefix deriving_json/,deriving_Json_lexer.cmo deriving_Json.cmo)

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45
VERSION := $(shell head -n 1 ../VERSION)

PPX_JS=../ppx/ppx_js/bin/ppx_js.byte

all: $(LIBRARY_INSTALL) META

lib_version.ml: lib_version.ml.tmp
	if cmp -s $^ $@; then rm $^; else mv $^ $@; fi

$(PPX_JS):
	$(MAKE) -C ../ppx/ppx_js/ bin/ppx_js.byte

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


graphics/%.cmo: graphics/%.ml graphics/%.cmi $(PPX_JS)
	$(OCAMLC) $(SAFESTRING) -package lwt,graphics -ppx $(PPX_JS) -I graphics -c $< -o $@

graphics/%.cmi: graphics/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,graphics -I ./ $< -o $@

graphics/graphics.cma: graphics/graphics_js.cmo
	$(OCAMLC) -a -o $@ $^

tyxml/%.cmo: tyxml/%.ml tyxml/%.cmi tyxml/tyxml_cast_sigs.cmi $(PPX_JS)
	$(OCAMLC) $(SAFESTRING) -package lwt,tyxml.functor,reactiveData,react -ppx $(PPX_JS) -I tyxml -c $< -o $@

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

async/%.cmo: async/%.ml async/%.cmi $(PPX_JS)
	$(OCAMLC) $(SAFESTRING) -package async_kernel -ppx $(PPX_JS) -I async -c $< -o $@

async/%.cmi: async/%.mli
	$(OCAMLC) $(SAFESTRING) -package async_kernel -ppx $(PPX_JS) -I . -I async $< -o $@

log/%.cmo: log/%.ml log/%.cmi $(PPX_JS)
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.log -ppx $(PPX_JS) -I log -c $< -o $@

log/%.cmi: log/%.mli
	$(OCAMLC) $(SAFESTRING) -package lwt,lwt.log -I . -I log $< -o $@

log/logger.cma: log/lwt_log_js.cmo
	$(OCAMLC) -a -o $@ $^

js_of_ocaml.cmo: js_of_ocaml.ml
	$(OCAMLC) -no-alias-deps $(SAFESTRING) -c -g $<

%.cmo: %.ml $(PPX_JS)
	$(OCAMLC) -ppx $(PPX_JS) $(SAFESTRING) -package lwt,uchar -c -g $<

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
	rm -f async/*.cm[xioa] async/*.[ao] async/*.so async/*.cmx[sa]
	rm -f log/*.cm[xioa] log/*.[ao] log/*.so log/*.cmx[sa]
	rm -f tyxml/*.cm[xioa] tyxml/*.[ao] tyxml/*.so tyxml/*.cmx[sa]
	rm -f graphics/*.cm[xioa] graphics/*.[ao] graphics/*.so graphics/*.cmx[sa]
	rm -f deriving_json/*.cm[xioa] deriving_json/*.[ao] deriving_json/*.so
	rm -f deriving_json/deriving_Json_lexer.ml
	rm -f stubs.c
	rm -f lib_version.ml


depend: $(PPX_JS) sys_js.ml
	ocamldep -ppx $(PPX_JS) *.ml *.mli > .depend
	ocamldep -ppx $(PPX_JS) -I async async/*.ml async/*.mli >> .depend
	ocamldep -ppx $(PPX_JS) -I log log/*.ml log/*.mli >> .depend
ifeq "${WITH_GRAPHICS}" "YES"
	ocamldep -ppx $(PPX_JS) graphics/*.ml  >> .depend
	@# camlp4 3.12.0 fails on (module type of ...)
	ocamldep -I graphics graphics/graphics_js.mli  >> .depend
endif
ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
	ocamldep -ppx $(PPX_JS) -I tyxml tyxml/*.ml tyxml/*.mli >> .depend
endif
	${MAKE} --no-print-directory deriving_json/deriving_Json_lexer.ml
	ocamldep -I deriving_json deriving_json/deriving_Json.ml deriving_json/deriving_Json_lexer.ml{,i} >> .depend
include .depend
