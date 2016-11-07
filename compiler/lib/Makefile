include ../../Makefile.conf

PACKAGES=compiler-libs base64 yojson

OBJS=compiler_version.cmx \
     util.cmx pretty_print.cmx option.cmx reserved.cmx varPrinter.cmx \
     dgraph.cmx code.cmx javascript.cmx vlq64.cmx source_map.cmx \
     js_output.cmx js_simpl.cmx parse_info.cmx js_token.cmx js_parser.cmx \
     js_lexer.cmx parse_js.cmx jsoo_primitive.cmx annot_parser.cmx annot_lexer.cmx \
     instr.cmx jsoo_subst.cmx pure_fun.cmx jsoo_deadcode.cmx \
     flow.cmx specialize.cmx specialize_js.cmx eval.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     js_traverse.cmx js_assign.cmx \
     linker.cmx parse_bytecode.cmx jsoo_strongly_connected_components.cmx \
     generate_closure.cmx generate.cmx driver.cmx \
     pseudoFs.cmx link_js.cmx

OCAMLC=ocamlfind ocamlc -g  -w +A-4-7-9-37-38-41-44-45-58
OCAMLOPT=ocamlfind ocamlopt -g -w +A-4-7-9-37-38-41-44-45-58

all:

js_of_ocaml_compiler.cmo: $(OBJS:cmx=cmo)
	$(OCAMLC) -pack -o $@ $^

js_of_ocaml_compiler.cmi: js_of_ocaml_compiler.cma

js_of_ocaml_compiler.cma: js_of_ocaml_compiler.cmo
	$(OCAMLC) -a -o $@ $^

js_of_ocaml_compiler.cmx: $(OBJS)
	$(OCAMLOPT) -pack -g -o $@ $^

js_of_ocaml_compiler.cmxa: js_of_ocaml_compiler.cmx
	$(OCAMLOPT) -a -o $@ $^

js_of_ocaml_compiler.cmxs: js_of_ocaml_compiler.cmxa
	$(OCAMLOPT) -shared -g -o $@ $^

VERSION := $(shell head -n 1 ../../VERSION)

compiler_version.ml: compiler_version.ml.tmp
	if cmp -s $^ $@; then rm $^; else mv $^ $@; fi

.PHONY: compiler_version.ml.tmp
compiler_version.ml.tmp:
	echo "let s = \"${VERSION}\"" > $@
	echo "let git_version = \"${VERSION_GIT}\"" >> $@

%.cmx: %.ml
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -for-pack Js_of_ocaml_compiler -g -c $<

%.cmo: %.ml
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

%.cmi: %.mli
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

annot_parser.ml: annot_parser.mli
annot_parser.mli: annot_parser.mly jsoo_primitive.cmi
	menhir --infer --explain $<

js_parser.ml: js_parser.mli
js_parser.mli: js_parser.mly javascript.cmi js_token.cmi
	menhir --infer --external-tokens Js_token --explain $<

%.ml: %.mll
	ocamllex -q $<
clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *.conflicts
	rm -f lib/*.cm[aiox] lib/*.cmxa lib/*.cmxs lib/*.o lib/*.a
	rm -f *.opt *.byte
	rm -f $(MINIFIER) $(COMPILER) $(LINKER)
	rm -f compiler_version.ml
	rm -f annot_lexer.ml annot_parser.ml annot_parser.mli
	rm -f js_lexer.ml js_parser.ml js_parser.mli
	rm -f util.ml
	rm -Rf man

.PHONY: depend
depend: $(OBJS:cmx=mli) $(OBJS:cmx=ml)
	echo $^ | xargs -n1 | sort -u | xargs ocamldep > .depend

include .depend
