include ../../Makefile.conf
include ../../Makefile.filelist

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45

all: $(PPX_INSTALL) $(PPX_DRIVER_INSTALL)

bin/ppx_js.byte: lib/ppx_js.cmo bin/ppx_js_bin.cmo
	$(OCAMLC) $(SAFESTRING) \
		-linkpkg -package ppx_tools.metaquot $^ -o bin/ppx_js.byte

bin/ppx_js.opt: lib/ppx_js.cmx bin/ppx_js_bin.cmx
	$(OCAMLOPT) $(SAFESTRING) \
		-linkpkg -package ppx_tools.metaquot $^ -o bin/ppx_js.opt

bin/%.cmx: bin/%.ml
	$(OCAMLOPT) $(SAFESTRING) -I lib -package ppx_tools.metaquot -g -c $<

bin/%.cmo: bin/%.ml
	$(OCAMLC) $(SAFESTRING) -I lib -package ppx_tools.metaquot -c $<

lib/%.cmx: lib/%.ml
	$(OCAMLOPT) $(SAFESTRING) -I lib -package ppx_tools.metaquot -g -c $<

lib/%.cmo: lib/%.ml
	$(OCAMLC) $(SAFESTRING) -I lib -package ppx_tools.metaquot -c $<

lib/ppx_js.cmi: lib/ppx_js.mli
	$(OCAMLC) $(SAFESTRING) -package ppx_tools.metaquot -c $< -o $@

lib/ppx_js.cma: lib/ppx_js.cmo
	$(OCAMLC) -a -o $@ $^
lib/ppx_js.cmxa: lib/ppx_js.cmx
	$(OCAMLOPT) -a -o $@ $^
lib/ppx_js.cmxs: lib/ppx_js.cmxa
	$(OCAMLOPT) -shared -g -o $@ $^

driver/%.cmx: ppx_driver/%.ml lib/ppx_js.cmi
	$(OCAMLOPT) $(SAFESTRING) -I ppx -package ppx_tools.metaquot -package ppx_driver -g -c $<
driver/%.cmo: ppx_driver/%.ml lib/ppx_js.cmi
	$(OCAMLC) $(SAFESTRING) -I ppx -package ppx_tools.metaquot -package ppx_driver -c $<
driver/ppx_js_driver.cma: ppx_driver/ppx_js_driver.cmo
	$(OCAMLC) -linkall -I ppx -a -o $@ $^
driver/ppx_js_driver.cmxa: ppx_driver/ppx_js_driver.cmx
	$(OCAMLOPT) -linkall -I ppx -a -o $@ $^
driver/ppx_js_driver.cmxs: ppx_driver/ppx_js_driver.cmxa
	$(OCAMLOPT) -linkall -I ppx -shared -g -o $@ $^

bin/$(PPX): bin/ppx_js.$(BEST)
	cp $< $@

tests/expect: tests/main.ml
	ocamlfind ocamlc -verbose -linkpkg -linkall -predicates create_toploop,ppx_driver \
	-package toplevel_expect_test,ppx_tools lib/ppx_js.cma driver/ppx_js_driver.cma tests/main.ml -o $@

tests: tests/expect tests/ppx.mlt
	./tests/expect tests/ppx.mlt
