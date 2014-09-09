include ../Makefile.conf
-include ../Makefile.local

all: toplevel.js eval.js


OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)
HIGLO=$(shell ocamlfind query higlo.ocaml -format "-package %p -rectypes" 2> /dev/null)

OPTCOMP_OPT=
ifneq ($(OCPINDENT),)
OPTCOMP_OPT+= -let ocpindent=true
endif

ifneq ($(HIGLO),)
OPTCOMP_OPT+= -let higlo=true
endif

ifneq ($(NO_SYNTAXES),1)
SYNTAXES= lwt.syntax \
	js_of_ocaml.syntax \
	js_of_ocaml.deriving.syntax deriving.syntax.std
endif

JSFILES= +weak.js +toplevel.js +graphics.js +nat.js

PACKAGES= \
	graphics lwt bigarray tyxml.functor \
	react reactiveData \
	js_of_ocaml \
	js_of_ocaml.deriving \
	js_of_ocaml.tyxml \
	js_of_ocaml.graphics \
	js_of_ocaml.toplevel

ifeq ($(METAOCAML),1)
BER_EXPORT=trx runcode print_code
BER=metalib.cma bertop.cmo \
	${addprefix -export-unit , ${BER_EXPORT}}
#METAOCAML DOES NOT WORK WITH CAMLP4
SYNTAXES=
OPTCOMP_OPT+= -let metaocaml=true
endif
#-jsopt "-pretty -disable shortvar"
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
	${addprefix -jsopt , -I ./ -file examples.ml} \
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

%.cmo:%.ml
	ocamlfind ocamlc -c -syntax camlp4o $(SAFESTRING) \
		-package optcomp,js_of_ocaml.syntax,lwt,js_of_ocaml.tyxml,js_of_ocaml.toplevel \
		-package base64 \
		$(OCPINDENT) $(HIGLO) \
		${addprefix -ppopt , ${OPTCOMP_OPT}} \
		$< -c $@
clean::
	rm -f *.cm[io] *.byte *.native *.js
