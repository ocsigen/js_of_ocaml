
all: time.pdf size.pdf

.PHONY: time.svg size.svg size.gpl

time.svg:
	./report.ml -config report-times.config \
	 -omit binary_trees -omit fannkuch_redux \
	 -max 5.5 -svg 7 500 150 -edgecaption -ylabel Time \
	 > $@

size.svg: size.gpl
	gnuplot < $^ > $@

size.gpl:
	./report.ml -config report-sizes.config \
	 -omit binary_trees -omit fannkuch_redux \
	 -max 4.5 -ylabel Size -script \
	 -svg 7 500 150 \
	| perl -pe 'BEGIN{undef $$/;} s/e\nplot/e\nset key at 16,3.25\nplot/smg' \
	 > $@

%.pdf: %.svg
	inkscape -D --export-pdf="$@" -w 210 -h 297 $<
