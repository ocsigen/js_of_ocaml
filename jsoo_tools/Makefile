

byte: jsoo_mkcmis.byte jsoo_mktop.byte
native: jsoo_mkcmis.native jsoo_mktop.native

%.byte: %.ml ../compiler/local/compiler.cma
	ocamlfind ocamlc -linkpkg \
	-package js_of_ocaml.compiler,compiler-libs.common,findlib \
	$*.ml -o $*.byte

%.native: %.ml ../compiler/local/compiler.cmxa
	ocamlfind ocamlc -linkpkg \
	-package js_of_ocaml.compiler,compiler-libs.common,findlib \
	$*.ml -o $*.native

../compiler/%:
	$(MAKE) -C ../compiler/ $*
