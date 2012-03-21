
include ../Makefile.conf
-include ../Makefile.local
include ../Makefile.filelist

MLIS := ${addprefix ../, ${DOC}}

.PHONY: doc docwiki
doc: api-html/index.html
api-html/index.html: ${MLIS} api-index
	mkdir -p api-html
	ocamlfind ocamldoc -package lwt -intro api-index -html \
	   -d api-html -I ../lib -I ../lib/deriving_json ${MLIS}


docwiki: api-wiki/index.wiki
api-wiki/index.wiki: ${MLIS} api-index
	mkdir -p api-wiki
	ocamlfind ocamldoc -package lwt -intro api-index \
	   -d api-wiki -I ../lib -I ../lib/deriving_json \
	   -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
	   ${MLIS}

clean:
	-rm -rf api-html/* api-wiki/*
