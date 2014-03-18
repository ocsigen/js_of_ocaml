OBJS=test.cmo wikicreole.cmo wiki_syntax.cmo main.cmo
NAME=wiki
OPTIONS=-pretty

include ../Makefile.common

wikicreole.ml: wikicreole.mll
	ocamllex $<

main.cmo: test.cmo wiki_syntax.cmo
wiki_syntax.cmo: wikicreole.cmo

clean::
	rm -f wikicreole.ml