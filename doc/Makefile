include ../Makefile.conf

OTHER=
MLIS=
CMI_DIR=../_build/default/
SRC_DIR=../

MLIS+=$(wildcard ${SRC_DIR}lib/*.mli)
MLIS+=$(wildcard ${SRC_DIR}ocamlbuild/*.mli)
OTHER+=-I ${CMI_DIR}lib  -package ocamlbuild -I ${CMI_DIR}ocamlbuild -I ../_build/install/default/lib/js_of_ocaml -I ../_build/install/default/lib/js_of_ocaml/deriving -I ../_build/install/default/lib/js_of_ocaml-tyxml

ifeq "${WITH_LWT}" "YES"
OTHER += -package lwt -I ${CMI_DIR}lib/lwt -I ${CMI_DIR}lib/lwt/log
MLIS+=$(wildcard ${SRC_DIR}lib/lwt/*.mli) $(wildcard ${SRC_DIR}lib/lwt/log/*.mli)
ifeq "${WITH_GRAPHICS}" "YES"
OTHER+=-package graphics -I ${CMI_DIR}lib/lwt/graphics
MLIS+=$(wildcard ${SRC_DIR}lib/lwt/graphics/*.mli)
endif
endif

MLIS+=$(wildcard ${SRC_DIR}lib/deriving_json/*.mli)
OTHER += -I ${CMI_DIR}lib/deriving_json
ifeq "${WITH_DERIVING}" "YES"
MLIS+=$(wildcard ${SRC_DIR}camlp4/pa_deriving_json/lib/*.mli)
OTHER += -package deriving.syntax.common
endif
ifeq "${WITH_PPX_DERIVING}" "YES"
MLIS+=$(wildcard ${SRC_DIR}ppx/ppx_deriving_json/lib/*.mli)
OTHER += -package ppx_deriving
endif

ifeq "${WITH_TYXML}${WITH_REACT}" "YESYES"
OTHER += -package tyxml.functor,react,reactiveData -I ${CMI_DIR}lib/tyxml
MLIS+=$(wildcard ${SRC_DIR}lib/tyxml/*.mli)
endif

ifeq "${WITH_TOPLEVEL}" "YES"
MLIS+=$(wildcard ${SRC_DIR}toplevel/lib/*.mli)
OTHER += -package compiler-libs.bytecomp,compiler-libs.common,compiler-libs.toplevel -I ${SRC_DIR}toplevel/lib
endif

ifeq "${WITH_PPX}" "YES"
MLIS+=$(wildcard ${SRC_DIR}ppx/ppx_js/lib/*.mli)
OTHER += -package ppx_tools_versioned -I ${CMI_DIR}ppx/ppx_js/lib
endif

DOCOPT := -colorize-code -short-functors -charset utf-8

.PHONY: doc wikidoc
doc: api/html/index.html
api/html/index.html: ${MLIS} api/index
	mkdir -p api/html
	ocamlfind ocamldoc ${DOCOPT} -package ocamlbuild,uchar ${OTHER} -intro api/index -html \
	   -d api/html \
           ${MLIS}

wikidoc: api/wiki/index.wiki cp-examples
api/wiki/index.wiki: ${MLIS} api/index
	mkdir -p api/wiki
	ocamlfind ocamldoc  ${DOCOPT} -package ocamlbuild,uchar ${OTHER} -intro api/index \
	   -d api/wiki \
	   -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
	   ${MLIS}

EX_TOPLEVEL:=index.html toplevel.js test_dynlink.js *.cmis.js
EX_BOULDER:=index.html boulderdash.js sprites
EX_WEBGL:=index.html webgldemo.js
EX_GRAPH:=index.html viewer_js.js
EX_PLANET:=index.html texture.jpg planet.js
EX_WIKI:=index.html main.js
EX_WYSIWYG:=index.html main.js
EX_HYPER:=index.html hypertree.js icons thumbnails
EX_MINE:=index.html main.js sprites
EX_CUBE:=index.html cubes.js

cp-examples:
	$(MAKE) -C .. all toplevel-examples
	mkdir -p ${addprefix manual/files/, toplevel/ boulderdash/ webgl/ graph_viewer/ planet/ wiki/ wysiwyg/ minesweeper/ cubes/ hyperbolic/}
	cp -R ${addprefix ../toplevel/examples/lwt_toplevel_bin/, ${EX_TOPLEVEL}} manual/files/toplevel/
	cp -R ${addprefix ../_build/default/examples/boulderdash/, ${EX_BOULDER}} manual/files/boulderdash/
	cp -R ${addprefix ../_build/default/examples/webgl/, ${EX_WEBGL}} manual/files/webgl/
	cp -R ${addprefix ../_build/default/examples/graph_viewer/, ${EX_GRAPH}} manual/files/graph_viewer/
	cp -R ${addprefix ../_build/default/examples/planet/, ${EX_PLANET}} manual/files/planet/
	cp -R ${addprefix ../_build/default/examples/wiki/, ${EX_WIKI}} manual/files/wiki/
	cp -R ${addprefix ../_build/default/examples/wysiwyg/, ${EX_WYSIWYG}} manual/files/wysiwyg/
	cp -R ${addprefix ../_build/default/examples/minesweeper/, ${EX_MINE}} manual/files/minesweeper/
	cp -R ${addprefix ../_build/default/examples/cubes/, ${EX_CUBE}} manual/files/cubes/
	cp -R ${addprefix ../_build/default/examples/hyperbolic/, ${EX_HYPER}} manual/files/hyperbolic/

clean:
	-rm -rf api/html/* api/wiki/*
