
include ../Makefile.conf
-include ../Makefile.local
include ../Makefile.filelist

MLIS := ${addprefix ../, ${DOC}}

ifeq "${WITH_GRAPHICS}" "YES"
OTHER=-package graphics -I ../lib/graphics
endif

.PHONY: doc docwiki
doc: api-html/index.html
api-html/index.html: ${MLIS} api-index
	mkdir -p api-html
	ocamlfind ocamldoc -package lwt -package ocamlbuild,deriving.syntax.common ${OTHER} -intro api-index -html \
	   -d api-html -I ../lib -I ../lib/deriving_json -I ../lib/syntax ${MLIS}


docwiki: api-wiki/index.wiki
api-wiki/index.wiki: ${MLIS} api-index
	mkdir -p api-wiki
	ocamlfind ocamldoc -package lwt -package ocamlbuild,deriving.syntax.common ${OTHER} -intro api-index \
	   -d api-wiki -I ../lib -I ../lib/deriving_json -I ../lib/syntax \
	   -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
	   ${MLIS}

EX_TOPLEVEL:=index.html toplevel.js toplevel-fs.js
EX_BOULDER:=index.html boulderdash.js sprites
EX_WEBGL:=index.html webgldemo.js
EX_GRAPH:=index.html jsviewer.js
EX_PLANET:=index.html texture.jpg planet.js
EX_WIKI:=index.html wiki.js
EX_WYSIWYG:=index.html wiki.js
EX_HYPER:=index.html hypertree.js icons
EX_MINE:=index.html minesweeper.js sprites
EX_CUBE:=index.html cubes.js

cp-examples:
	cp -R ${addprefix ../toplevel/, ${EX_TOPLEVEL}} manual-aux/toplevel/
	cp -R ${addprefix ../examples/boulderdash/, ${EX_BOULDER}} manual-aux/boulderdash/
	cp -R ${addprefix ../examples/webgl/, ${EX_WEBGL}} manual-aux/webgl/
	cp -R ${addprefix ../examples/graph_viewer/, ${EX_GRAPH}} manual-aux/graph_viewer/
	cp -R ${addprefix ../examples/planet/, ${EX_PLANET}} manual-aux/planet/
	cp -R ${addprefix ../examples/wiki/, ${EX_WIKI}} manual-aux/wiki/
	cp -R ${addprefix ../examples/wysiwyg/, ${EX_WYSIWYG}} manual-aux/wysiwyg/
	cp -R ${addprefix ../examples/minesweeper/, ${EX_MINE}} manual-aux/minesweeper/
	cp -R ${addprefix ../examples/cubes/, ${EX_CUBE}} manual-aux/cubes/
	cp -R ${addprefix ../examples/hyperbolic/, ${EX_HYPER}} manual-aux/hyperbolic/

clean:
	-rm -rf api-html/* api-wiki/*
