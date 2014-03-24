
NAME=toplevel
OBJS=toplevel.cmo

all: $(NAME).js

include ../Makefile.conf
-include ../Makefile.local

ifeq ($(shell ocamlc -v | grep -q "version 4"; echo $$?),0)
OCAMLLIB=ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma
else
OCAMLLIB=toplevellib.cma
endif

ifeq ($(METAOCAML),1)
BER=metalib.cma bertop.cmo
BER_MODULE=trx runcode
endif

OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)

ifneq ($(OCPINDENT),)
OPTCOMP_OPT=-ppopt -let -ppopt ocpindent=true
endif

COMP=../compiler/$(COMPILER)
JSFILES=../runtime/runtime.js ../runtime/weak.js toplevel_runtime.js
OCAMLC=ocamlfind ocamlc -package lwt,optcomp -syntax camlp4o $(OCPINDENT) $(OPTCOMP_OPT) -ppopt "../lib/syntax/pa_js.cmo" -I +compiler-libs -I ../lib -I ../compiler
STDLIB= ../lib/$(LIBNAME).cma ../compiler/compiler.cma $(OCAMLLIB) $(BER)
EXPUNGE=$(shell ocamlc -where)/expunge
# Removed gc and sys
STDLIB_MODULES=\
  arg \
  array \
  arrayLabels \
  buffer \
  callback \
  camlinternalLazy \
  camlinternalMod \
  camlinternalOO \
  char \
  complex \
  digest \
  filename \
  format \
  genlex \
  hashtbl \
  int32 \
  int64 \
  lazy \
  lexing \
  list \
  listLabels \
  map \
  marshal \
  moreLabels \
  nativeint \
  obj \
  oo \
  parsing \
  pervasives \
  printexc \
  printf \
  queue \
  random \
  scanf \
  set \
  sort \
  stack \
  stdLabels \
  stream \
  string \
  stringLabels \
  sys \
  weak

JOO=js dom typed_array dom_html file dom_events firebug lwt_js sys_js regexp CSS url \
  form xmlHttpRequest event_arrows lwt_js_events json webGL webSockets keycode
JOO=

PERVASIVES=$(STDLIB_MODULES) $(JOO) $(BER_MODULE) outcometree topdirs toploop

#toplevel.byte: $(OBJS:cmx=cmo) toplevel.cmo
#	ocamlfind ocamlc -linkall -g -package str -linkpkg toplevellib.cma -o $@.tmp $^

$(NAME).js: $(NAME).byte $(COMP) $(JSFILES) examples.ml
	$(COMP) -I $(shell ocamlc -where)/compiler-libs -toplevel -noruntime $(JSFILES) $(NAME).byte $(OPTIONS) -I ./ -I ../lib/ -file examples.ml

$(NAME).byte: $(OBJS) ../compiler/compiler.cma
	$(OCAMLC) -linkall -package findlib $(OCPINDENT) -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
	$(EXPUNGE) $@.tmp $@ $(PERVASIVES)
	rm -f $@.tmp

%.cmo: %.ml ../compiler/compiler.cma
	$(OCAMLC) -c $<

../compiler/compiler.cma:
	$(MAKE) -C ../compiler compiler.cma

server: server.ml
	ocamlfind ocamlc -linkpkg -package findlib,cohttp.lwt server.ml -o server

clean::
	rm -f *.cm[io] $(NAME).byte $(NAME).js

depend:
	ocamlfind ocamldep -syntax camlp4o -package optcomp -ppopt "../lib/syntax/pa_js.cmo" -I ../compiler *.ml *.mli > .depend

include .depend
