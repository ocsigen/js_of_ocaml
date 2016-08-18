
include ../Makefile.conf
-include ../Makefile.local
include ../Makefile.filelist

MLIS := ${addprefix ../, ${DOC}}

ifeq "${WITH_GRAPHICS}" "YES"
OTHER=-package graphics -I ../lib/graphics
endif

ifeq "${WITH_DERIVING}" "YES"
OTHER += -package deriving.syntax.common
endif

ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
OTHER += -package tyxml.functor,react,reactiveData -I ../lib/tyxml
endif

ifeq "${WITH_ASYNC}" "YES"
OTHER += -package async_kernel -I ../lib/async
endif

ifeq "${WITH_TOPLEVEL}" "YES"
OTHER += -package compiler-libs.bytecomp,compiler-libs.common,compiler-libs.toplevel -I ../lib/toplevel
endif

ifeq "${WITH_PPX}" "YES"
OTHER += -package ppx_tools -I ../lib/ppx
endif


.PHONY: doc wikidoc
doc: api/html/index.html
api/html/index.html: ${MLIS} api/index
	mkdir -p api/html
	ocamlfind ocamldoc ${DOCOPT} -package lwt,ocamlbuild,uchar ${OTHER} -intro api/index -html \
	   -d api/html -I ../lib -I ../lib/deriving_json ${MLIS}

wikidoc: api/wiki/index.wiki cp-examples
api/wiki/index.wiki: ${MLIS} api/index
	mkdir -p api/wiki
	ocamlfind ocamldoc ${DOCOPT} -package lwt,ocamlbuild,uchar ${OTHER} -intro api/index \
	   -d api/wiki -I ../lib -I ../lib/deriving_json -I ../lib/syntax \
	   -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
	   ${MLIS}

EX_TOPLEVEL:=index.html toplevel.js test_dynlink.js *.cmis.js
EX_BOULDER:=index.html boulderdash.js sprites
EX_WEBGL:=index.html webgldemo.js
EX_GRAPH:=index.html jsviewer.js
EX_PLANET:=index.html texture.jpg planet.js
EX_WIKI:=index.html wiki.js
EX_WYSIWYG:=index.html wiki.js
EX_HYPER:=index.html hypertree.js icons thumbnails svg_icons
EX_MINE:=index.html minesweeper.js sprites
EX_CUBE:=index.html cubes.js

cp-examples:
	$(MAKE) -C .. toplevel examples
	mkdir -p ${addprefix manual/files/, toplevel/ boulderdash/ webgl/ graph_viewer/ planet/ wiki/ wysiwyg/ minesweeper/ cubes/ hyperbolic/}
	cp -R ${addprefix ../toplevel/, ${EX_TOPLEVEL}} manual/files/toplevel/
	cp -R ${addprefix ../examples/boulderdash/, ${EX_BOULDER}} manual/files/boulderdash/
	cp -R ${addprefix ../examples/webgl/, ${EX_WEBGL}} manual/files/webgl/
	cp -R ${addprefix ../examples/graph_viewer/, ${EX_GRAPH}} manual/files/graph_viewer/
	cp -R ${addprefix ../examples/planet/, ${EX_PLANET}} manual/files/planet/
	cp -R ${addprefix ../examples/wiki/, ${EX_WIKI}} manual/files/wiki/
	cp -R ${addprefix ../examples/wysiwyg/, ${EX_WYSIWYG}} manual/files/wysiwyg/
	cp -R ${addprefix ../examples/minesweeper/, ${EX_MINE}} manual/files/minesweeper/
	cp -R ${addprefix ../examples/cubes/, ${EX_CUBE}} manual/files/cubes/
	cp -R ${addprefix ../examples/hyperbolic/, ${EX_HYPER}} manual/files/hyperbolic/

clean:
	-rm -rf api/html/* api/wiki/*
