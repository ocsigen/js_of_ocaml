include ../Makefile.conf
-include ../Makefile.local

all: toplevel.js eval.js


OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)
HIGLO=$(shell ocamlfind query higlo.ocaml -format "-package %p -rectypes" 2> /dev/null)

CPPO_OPT=
ifneq ($(OCPINDENT),)
CPPO_OPT+= -D ocpindent
endif

ifneq ($(HIGLO),)
CPPO_OPT+= -D higlo
endif

ifneq ($(NO_SYNTAXES),1)
SYNTAXES= lwt.syntax \
	js_of_ocaml.syntax \
	js_of_ocaml.deriving.syntax deriving.syntax.std
endif

JSFILES= +weak.js +toplevel.js +nat.js

PACKAGES= \
	lwt bigarray tyxml.functor \
	react reactiveData \
	js_of_ocaml \
	js_of_ocaml.deriving \
	js_of_ocaml.tyxml \
	js_of_ocaml.toplevel

ifeq ($(WITH_GRAPHICS),YES)
JSFILES += +graphics.js
PACKAGES += graphics js_of_ocaml.graphics
CPPO_OPT+= -D graphics
endif

ifeq ($(METAOCAML),1)
BER_EXPORT=trx runcode print_code
BER=metalib.cma bertop.cmo \
	${addprefix -export-unit , ${BER_EXPORT}}
#METAOCAML DOES NOT WORK WITH CAMLP4
SYNTAXES=
CPPO_OPT+= -D metaocaml
endif

#-jsopt "--pretty --disable shortvar"
MKTOP=jsoo_mktop -verbose $(SAFESTRING) \
	${addprefix -top-syntax , ${SYNTAXES}} \
	-dont-export-unit gc \
	${addprefix -export-package , ${PACKAGES}} \
	${addprefix -export-unit , html5_types html5_sigs xml_wrap } \

TOPLEVEL_NAME=toplevel
TOPLEVEL_OBJS=toplevel.cmo
$(TOPLEVEL_NAME).js: $(TOPLEVEL_OBJS)
	$(MKTOP) \
	$(BER) \
	$(OCPINDENT) $(HIGLO) \
	$(TOPLEVEL_OBJS) \
	${addprefix -jsopt , ${JSFILES}} \
	${addprefix -jsopt , -I ./ --file examples.ml} \
	-package base64 \
	-o $(TOPLEVEL_NAME).byte

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

%.ml: %.cppo.ml
	cppo ${CPPO_OPT} $< -o $@

%.cmo:%.ml
	ocamlfind ocamlc -c -syntax camlp4o $(SAFESTRING) \
		-package js_of_ocaml.syntax,lwt,js_of_ocaml.tyxml,js_of_ocaml.toplevel \
		-package base64 \
		$(OCPINDENT) $(HIGLO) \
		$< -c $@
clean::
	rm -f *.cm[io] *.byte *.native *.js
