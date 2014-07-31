include ../Makefile.conf
-include ../Makefile.local

all: toplevel.js eval.js

ifeq ($(METAOCAML),1)
BER=metalib.cma bertop.cmo
BER_EXPORT= -export-unit trx -export-unit runcode
endif

OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)
HIGLO=$(shell ocamlfind query higlo.ocaml -format "-package %p -rectypes" 2> /dev/null)

OPTCOMP_OPT=
ifneq ($(OCPINDENT),)
OPTCOMP_OPT+=-ppopt -let -ppopt ocpindent=true
endif

ifneq ($(HIGLO),)
OPTCOMP_OPT+=-ppopt -let -ppopt higlo=true
endif

JSFILES=-jsopt +weak.js -jsopt +toplevel.js -jsopt +graphics.js -jsopt +nat.js

MKTOP=jsoo_mktop -verbose \
	-top-syntax lwt.syntax -top-syntax js_of_ocaml.syntax \
	-top-syntax js_of_ocaml.deriving.syntax \
	-top-syntax deriving.syntax.std \
	-dont-export-unit gc \
	-export-package graphics \
	-export-package lwt \
	-export-package bigarray \
	-export-package tyxml.functor \
	-export-package react \
	-export-package reactiveData \
	-export-package js_of_ocaml \
	-export-package js_of_ocaml.deriving \
	-export-package js_of_ocaml.tyxml \
	-export-package js_of_ocaml.graphics \
	-export-package js_of_ocaml.toplevel \
	-jsopt -I -jsopt $(shell ocamlfind query tyxml) \
	-jsopt -file -jsopt html5_types.cmi:/cmis \
	-jsopt -file -jsopt html5_sigs.cmi:/cmis \
  -jsopt -file -jsopt xml_wrap.cmi:/cmis \

TOPLEVEL_NAME=toplevel
TOPLEVEL_OBJS=toplevel.cmo
$(TOPLEVEL_NAME).js: $(TOPLEVEL_OBJS)
	$(MKTOP) \
	$(BER) $(BER_EXPORT) \
	$(OCPINDENT) $(HIGLO) \
	$(TOPLEVEL_OBJS) \
	$(JSFILES) -jsopt -I -jsopt ./ -jsopt -file -jsopt examples.ml \
	-o $(TOPLEVEL_NAME).byte

EVAL_NAME=eval
EVAL_OBJS=eval.cmo
$(EVAL_NAME).js: $(EVAL_OBJS)
	$(MKTOP) $(EVAL_OBJS) $(JSFILES) -o $(EVAL_NAME).byte

%.cmis.js:
	jsoo_mkcmis $*

server.%: server.ml
	ocamlbuild -use-ocamlfind -package findlib,cohttp.lwt server.$*

%.cmo:%.ml
	ocamlfind ocamlc -c -syntax camlp4o \
		-package optcomp,js_of_ocaml.syntax,lwt,tyxml.functor,js_of_ocaml.toplevel \
		$(OCPINDENT) $(HIGLO) $(OPTCOMP_OPT) \
		$< -c $@
clean::
	rm -f *.cm[io] *.byte *.native *.js
