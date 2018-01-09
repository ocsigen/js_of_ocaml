include ../../../Makefile.conf

all: toplevel.js

ifeq ($(shell ocamlc -safe-string 2> /dev/null ; echo $$?),0)
SAFESTRING=-safe-string
else
SAFESTRING=-package bytes
endif

METAOCAML ?= 0

DERIVING=$(shell ocamlfind query js_of_ocaml.deriving -format "%p" 2> /dev/null)

ifneq ($(DERIVING),)
DERIVING_SYNTAX = js_of_ocaml-camlp4.deriving deriving.syntax.std
endif

ifneq ($(CAMLP4_SYNTAXES),)
SYNTAXES= lwt.syntax js_of_ocaml-camlp4 $(DERIVING_SYNTAX)
endif

JSFILES= +toplevel.js +dynlink.js +nat.js

PACKAGES= \
	lwt bigarray tyxml.functor \
	react reactiveData \
	js_of_ocaml js_of_ocaml-lwt $(DERIVING) \
	js_of_ocaml-tyxml \
	js_of_ocaml-toplevel \
	dynlink

ifeq ($(WITH_ASYNC_JS),YES)
PACKAGES += async_js core_kernel async_kernel base
JSFILES += +base/runtime.js +core_kernel/runtime.js +bin_prot/runtime.js +ppx_expect/collector/runtime.js
endif

ifeq ($(WITH_PPX),YES)
PACKAGES += js_of_ocaml-ppx.as-lib
endif

ifeq ($(WITH_GRAPHICS),YES)
JSFILES += +graphics.js
PACKAGES += graphics js_of_ocaml-lwt.graphics
endif

ifeq ($(METAOCAML),1)
METAOCAML_DIR=$(shell ocamlfind query stdlib)
BER_EXPORT=trx.cmi runcode.cmi print_code.cmi compiler-libs/asttypes.cmi
BER=metalib.cma bertop.cmo \
	${addprefix -export-unit ${METAOCAML_DIR}/, ${BER_EXPORT}}
#METAOCAML DOES NOT WORK WITH CAMLP4
SYNTAXES=
#CPPO_OPT+= -D metaocaml
endif

TYXML_DIR=$(shell ocamlfind query tyxml)

#-jsopt "--pretty"
MKTOP=jsoo_mktop -verbose $(SAFESTRING) -g \
	${addprefix -top-syntax , ${SYNTAXES}} \
	-jsopt "--disable shortvar --pretty" \
	${addprefix -export-package , ${PACKAGES}} \
	${addprefix -export-unit $(TYXML_DIR)/, html_types.cmi html_sigs.cmi xml_wrap.cmi } \

TOPLEVEL_NAME=toplevel
$(TOPLEVEL_NAME).js: examples.ml test_dynlink.cmo test_dynlink.js
	$(MKTOP) \
	$(BER) \
	${addprefix -jsopt , ${JSFILES}} \
	${addprefix -jsopt , -I ./ --file examples.ml} \
	${addprefix -jsopt , -I ./ --file test_dynlink.cmo} \
	${addprefix -jsopt , -I ./ --file test_dynlink.js} \
	-package base64,js_of_ocaml-toplevel.lwt_toplevel_example \
	-o $(TOPLEVEL_NAME).byte

test_dynlink.js: test_dynlink.cmo
	js_of_ocaml test_dynlink.cmo --pretty

test_dynlink.cmo: test_dynlink.ml
	ocamlc -c test_dynlink.ml

clean::
	rm -f *.cm[io] *.byte *.native *.js
