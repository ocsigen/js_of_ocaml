

byte: jsoo_mkcmis.byte jsoo_mktop.byte
native: jsoo_mkcmis.native jsoo_mktop.native

%.byte: %.ml ../compiler/local/compiler.cma
	ocamlfind ocamlc -linkpkg \
	-package compiler-libs.common,findlib \
	-I ../compiler/local/ compiler.cma \
	$*.ml -o $*.byte

%.native: %.ml ../compiler/local/compiler.cma
	ocamlfind ocamlopt -linkpkg \
	-package compiler-libs.common,findlib \
	-I ../compiler/local/ compiler.cmxa \
	$*.ml -o $*.native

../compiler/local/compiler.cma:
	$(MAKE) -C ../compiler local/compiler.cma

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa] *.{byte,native}
