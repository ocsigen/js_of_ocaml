
NAME=toplevel
OBJS=toplevel.cmo

all: $(NAME).js

include ../Makefile.conf
-include ../Makefile.local

ifeq ($(OCAML4),"YES")
OCAMLLIB=ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma
else
OCAMLLIB=toplevellib.cma
endif

COMPILER_LIBS=$(shell ocamlc -where)/compiler-libs

ifeq ($(METAOCAML),1)
BER=metalib.cma bertop.cmo
BER_MODULE=trx runcode
endif

OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)

ifneq ($(OCPINDENT),)
OPTCOMP_OPT=-ppopt -let -ppopt ocpindent=true
endif

COMP=../compiler/$(COMPILER)
JSFILES=../runtime/runtime.js ../runtime/weak.js ../runtime/toplevel.js ../runtime/graphics.js ../runtime/nat.js
OCAMLC=ocamlfind ocamlc -package lwt,optcomp,tyxml.functor,reactiveData -syntax camlp4o $(OCPINDENT) $(OPTCOMP_OPT) -ppopt "../lib/syntax/pa_js.cmo" \
	-I $(COMPILER_LIBS) -I ../lib -I ../compiler -I ../lib/graphics -I ../lib/tyxml
STDLIB= ../lib/$(LIBNAME).cma ../compiler/compiler.cma $(OCAMLLIB) $(BER) ../lib/graphics/graphics.cma ../lib/tyxml/tyxml.cma
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
  weak \
	graphics \
	graphics_js \
	lwt \
	lwt_js \
	bigarray \
	tyxml_js \
	html5_types \

JOO=js dom typed_array dom_html dom_svg file dom_events firebug lwt_js sys_js regexp CSS url \
  form xmlHttpRequest lwt_js_events json webGL webSockets keycode
JOO=js

PERVASIVES=$(STDLIB_MODULES) $(JOO) $(BER_MODULE) outcometree topdirs toploop

#toplevel.byte: $(OBJS:cmx=cmo) toplevel.cmo
#	ocamlfind ocamlc -linkall -g -package str -linkpkg toplevellib.cma -o $@.tmp $^

$(NAME).js: $(NAME).byte $(COMP) $(JSFILES) examples.ml
	$(COMP) -toplevel -noruntime $(JSFILES) $(NAME).byte \
	-I $(COMPILER_LIBS) \
	-I $(shell ocamlfind query lwt) \
	-I $(shell ocamlfind query tyxml) \
	-I ./ -I ../lib/ -I ../lib/graphics -I ../lib/tyxml -file html5_types.cmi:/cmis \
	-ofs toplevel-fs.js $(OPTIONS) -file examples.ml

$(NAME).byte: $(OBJS) ../compiler/compiler.cma
	$(OCAMLC) -linkall -package findlib,graphics,lwt,bigarray,tyxml.functor $(OCPINDENT) -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
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
