include ../Makefile.conf
-include ../Makefile.local

all: toplevel.js eval.js


OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)
HIGLO=$(shell ocamlfind query higlo.ocaml -format "-package %p -rectypes" 2> /dev/null)
DERIVING=$(shell ocamlfind query js_of_ocaml.deriving -format "%p" 2> /dev/null)
ASYNC=$(shell ocamlfind query js_of_ocaml.async -format "%p" 2> /dev/null)

CPPO_OPT=
ifneq ($(OCPINDENT),)
CPPO_OPT+= -D ocpindent
endif

ifneq ($(HIGLO),)
CPPO_OPT+= -D higlo
endif

ifneq ($(DERIVING),)
DERIVING_SYNTAX = js_of_ocaml.deriving.syntax deriving.syntax.std
endif

ifneq ($(CAMLP4_SYNTAXES),)
SYNTAXES= lwt.syntax js_of_ocaml.syntax $(DERIVING_SYNTAX)
endif

JSFILES= +weak.js +toplevel.js +dynlink.js +nat.js

PACKAGES= \
	lwt bigarray tyxml.functor \
	react reactiveData \
	js_of_ocaml $(DERIVING) $(ASYNC_PKG) \
	js_of_ocaml.tyxml \
	js_of_ocaml.toplevel \
	dynlink

ifeq ($(WITH_PPX),YES)
PACKAGES += js_of_ocaml.ppx.internal
CPPO_OPT+= -D ppx
endif

ifeq ($(WITH_GRAPHICS),YES)
JSFILES += +graphics.js
PACKAGES += graphics js_of_ocaml.graphics
CPPO_OPT+= -D graphics
endif

ifeq ($(ARCH_32BIT),YES)
ifneq ($(ASYNC),)
ASYNC_PKG=core_kernel async_kernel js_of_ocaml.async
JSFILES += +bin_prot.js +core_kernel.js
endif
endif

ifeq ($(METAOCAML),1)
BER_EXPORT=trx runcode print_code
BER=metalib.cma bertop.cmo \
	${addprefix -export-unit , ${BER_EXPORT}}
#METAOCAML DOES NOT WORK WITH CAMLP4
SYNTAXES=
CPPO_OPT+= -D metaocaml
endif

#-jsopt "--pretty"
MKTOP=jsoo_mktop -verbose $(SAFESTRING) -g \
	${addprefix -top-syntax , ${SYNTAXES}} \
	-dont-export-unit gc \
	-jsopt "--disable shortvar" \
	${addprefix -export-package , ${PACKAGES}} \
	${addprefix -export-unit , html_types html_sigs xml_wrap } \

TOPLEVEL_NAME=toplevel
TOPLEVEL_OBJS=colorize.cmo indent.cmo toplevel.cmo
$(TOPLEVEL_NAME).js: $(TOPLEVEL_OBJS) examples.ml test_dynlink.cmo test_dynlink.js
	$(MKTOP) \
	$(BER) \
	$(OCPINDENT) $(HIGLO) \
	$(TOPLEVEL_OBJS) \
	${addprefix -jsopt , ${JSFILES}} \
	${addprefix -jsopt , -I ./ --file examples.ml} \
	${addprefix -jsopt , -I ./ --file test_dynlink.cmo} \
	${addprefix -jsopt , -I ./ --file test_dynlink.js} \
	-package base64 \
	-o $(TOPLEVEL_NAME).byte

test_dynlink.js: test_dynlink.cmo
	js_of_ocaml test_dynlink.cmo --pretty

EVAL_NAME=eval
EVAL_OBJS=eval.cmo
$(EVAL_NAME).js: $(EVAL_OBJS)
	$(MKTOP) $(EVAL_OBJS) \
	${addprefix -jsopt , ${JSFILES}} \
	-o $(EVAL_NAME).byte

%.cmis.js:
	jsoo_mkcmis $*

server.%: server.ml
	ocamlbuild -use-ocamlfind -package findlib,cohttp.lwt server.$*

indent.cmo: indent.cmi
colorize.cmo: colorize.cmi

%.ml: %.cppo.ml
	cppo ${CPPO_OPT} $< -o $@

%.cmi:%.mli
	ocamlfind ocamlc -g -c -package js_of_ocaml,js_of_ocaml.tyxml $<
%.cmo:%.ml
	ocamlfind ocamlc -g -c -syntax camlp4o $(SAFESTRING) \
		-package js_of_ocaml.syntax,lwt,js_of_ocaml.tyxml,js_of_ocaml.toplevel \
		-package base64 \
		-I camlp4 \
		$(OCPINDENT) $(HIGLO) \
		$< -c $@
clean::
	rm -f *.cm[io] *.byte *.native *.js
