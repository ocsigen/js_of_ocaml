
include ../Makefile.conf
include ../Makefile.filelist

MLIS := ${addprefix ../, ${DOC}}

.PHONY: doc
doc: api-html/index.html
api-html/index.html: ${MLIS} api-index
	mkdir -p api-html
	ocamlfind ocamldoc -package lwt -intro api-index -html \
	   -d api-html -I ../lib -I ../lib/deriving_json ${MLIS}

clean:
	rm -r api-html/*
