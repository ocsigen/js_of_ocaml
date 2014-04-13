# graphs without precompiled programs:
GRAPHSNOPR = time.pdf time-optim.pdf time-ocamljs.pdf nativejs.pdf

# graphs with precompiled programs:
GRAPHSPR = size.pdf size-ocamljs.pdf size-optim.pdf compiletime.pdf

# For full benchs:
all: _perf graphsnopr graphspr _noperf

graphsnopr: _noprecomp $(GRAPHSNOPR)

graphspr: __precomp $(GRAPHSPR)

# For fast benchs:
test: _perf fastrun $(GRAPHS) _noperf

exes= $(wildcard programs/*)

exesnames = $(exes:programs/%=%)

missingsizesml = $(exesnames:%=results/sizes/ml/%)
missingsizesocamljs = $(exesnames:%=results/sizes/ocamljs/%)
missingsizes=$(missingsizesml) $(missingsizesocamljs)

missingcompiletimes = $(exesnames:%=results/compiletimes/`hostname`/byte/%) $(exesnames:%=results/compiletimes/`hostname`/opt/%)
possiblymissingbenchs = results/times/`hostname`/nitro/js_of_ocaml/kb results/times/`hostname`/tm/js_of_ocaml/hamming results/times/`hostname`/tm/js_of_ocaml/splay

OCAML=ocaml unix.cma
RUN=$(OCAML) ./run.ml
REPORT=$(OCAML) ./report.ml

.PHONY: $(GRAPHSNOPR:.pdf=.svg) $(GRAPHSPR:.pdf=.svg) size.gpl _noprecomp

# compile with precompiled programs for creating benchs
__precomp:
	mkdir -p build/byte
	ln -sf $(exes:%=../../%) build/byte
	if [ ! -d precomptmp ] ; then $(RUN) -compile ; else cd precomptmp ; find . -type f -exec mv {} ../{} \; ; fi
	rm -rf precomptmp
	touch __precomp

# for running the test we do not want precompiled programs
__run:
	make _noprecomp
	$(RUN) -all
	make __possiblymissingbenchs
	touch __run

fastrun:
	make _noprecomp
	echo "======================== WARNING: fast benchs!"
	$(RUN) -ffast -all
	make __possiblymissingbenchs
	touch __run

_noprecomp:
	if [ -f __precomp ] ; \
	then \
	  rm -f __precomp ;\
	  mkdir -p precomptmp/build/byte ;\
	  mkdir -p build ;\
	  mkdir -p results ;\
	  find build -type d -exec mkdir -p precomptmp/{} \; ;\
	  find results -type d -exec mkdir -p precomptmp/{} \; ;\
	  for i in $(exesnames) ; do if [ -f build/byte/$$i ] ; then mv -f build/byte/$$i precomptmp/build/byte/ ; fi ; done ;\
	  for i in $(exesnames) ; do for j in build/*/$$i.js ; do if [ -f $$j ] ; then mv $$j precomptmp/$$j ; fi ; done ; done ;\
	  for i in $(exesnames) ; do for j in results/sizes/js_of_ocaml/*/$$i ; do if [ -f $$j ] ; then mv $$j precomptmp/$$j ; fi ; done ; done ;\
	  for i in $(exesnames) ; do for j in results/sizes/*/$$i ; do if [ -f $$j ] ; then mv $$j precomptmp/$$j ; fi ;done ; done ;\
	fi

__missingcompiletimes:
	mkdir -p results/compiletimes/`hostname`/byte/
	for i in $(missingcompiletimes) ; do if [ ! -f $$i ] ; then echo 0 > $$i ; fi ; done
	touch __missingcompiletimes

__possiblymissingbenchs:
	for i in $(possiblymissingbenchs) ; do if [ ! -f $$i ] ; then echo 0 > $$i ; fi ; done
	touch __possiblymissingbenchs

__missingsizes: __precomp
	for i in $(missingsizes) ; do if [ ! -f $$i ] ; then echo 0 > $$i ; fi ; done
	if [ -f results/sizes/ml/unison ] ; then echo 828471 > results/sizes/ml/unison ; fi
	if [ -f results/sizes/ml/ocamlc ] ; then echo 800771 > results/sizes/ml/ocamlc ; fi
	if [ -f results/sizes/ml/ocsigen_server ] ; then echo 0 > results/sizes/ml/ocsigen_server ; fi
	if [ -f results/sizes/ml/js_of_ocaml ] ; then echo 171361 > results/sizes/ml/js_of_ocaml ; fi
	if [ -f results/sizes/ml/boulderdash ] ; then echo 12920 > results/sizes/ml/boulderdash ; fi
	if [ -f results/sizes/ml/canvas ] ; then echo 740 > results/sizes/ml/canvas ; fi
	if [ -f results/sizes/ml/cubes ] ; then echo 3773 > results/sizes/ml/cubes ; fi
	if [ -f results/sizes/ml/minesweeper ] ; then echo 7106 > results/sizes/ml/minesweeper ; fi
	if [ -f results/sizes/ml/planet ] ; then echo 16488 > results/sizes/ml/planet ; fi
	if [ -f results/sizes/ml/sudoku ] ; then echo 2837 > results/sizes/ml/sudoku ; fi
	touch __missingsizes
# file sizes (without comments and spaces)
# find ../examples/sudoku/ -name "*.ml" -exec cat {} \; | perl /home/balat/kroko/js_of_ocaml/benchmarks/lib/remove_comments.pl | sed 's/^ *//g' | wc -c

# To generate SVG, we first need to run the benchs,
# then to add data for precompiled programs
time.svg: __run
	$(REPORT) -config report-time.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop \
	 -max 5.5 -svg 7 500 150 -edgecaption -ylabel "Execution time" \
	 > $@

size.svg: size.gpl
	gnuplot < $^ > $@

size.gpl: __run __missingsizes
	$(REPORT) -config report-size.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc -omit kb_no_exc \
	 -append boulderdash \
	 -append canvas \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append sudoku \
	 -append js_of_ocaml \
	 -append ocsigen_server \
	 -append ocamlc \
	 -append unison \
	 -max 3 -ylabel Size -script \
	 -svg 7 650 150 \
	| perl -pe 'BEGIN{undef $$/;} s/e\nplot/e\nset key at 23.75,1.80\nplot/smg' \
	 > $@

time-optim.svg: __run
	$(REPORT) -config report-time-optim.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop \
	 -omit fannkuch_redux_2 \
	 -max 4 -svg 7 400 150 -edgecaption -ylabel "Execution time" \
	 > $@

size-optim.svg: __run __missingsizes
	$(REPORT) -config report-size-optim.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop  \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc -omit kb_no_exc \
	 -append boulderdash \
	 -append canvas \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append sudoku \
	 -append js_of_ocaml \
	 -append ocsigen_server \
	 -append ocamlc \
	 -append unison \
	 -max 2.5 -svg 7 650 150 -edgecaption -ylabel Size \
	 > $@

time-ocamljs.svg: __run
	$(REPORT) -config report-time-ocamljs.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop \
	 -omit fannkuch_redux_2 \
	 -max 5.5 -svg 7 500 150 -edgecaption -ylabel "Execution time" \
	 > $@

size-ocamljs.svg: __run __precomp
	$(REPORT) -config report-size-ocamljs.config -omit loop \
	 -omit binary_trees -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit spectral_norm \
	 -omit boyer_no_exc -omit kb_no_exc \
	 -omit boulderdash \
	 -omit canvas \
	 -omit cubes \
	 -omit minesweeper \
	 -omit planet \
	 -omit sudoku \
	 -omit js_of_ocaml \
	 -omit ocsigen_server \
	 -omit ocamlc \
	 -omit unison \
	 -max 5.5 -svg 7 500 150 -edgecaption -ylabel Size \
	 > $@

compiletime.svg: __run __precomp __missingcompiletimes
	$(REPORT) -config report-compiletime.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop \
	 -max 2 -svg 7 500 150 -edgecaption -ylabel "Compile time" \
	 > $@

nativejs.svg: __run
	$(REPORT) -config report-nativejs.config \
	 -omit binary_trees -omit fannkuch_redux -omit loop \
	 -max 2.8 -svg 7 200 150 -edgecaption -ylabel "Execution time" \
	 > $@


%.pdf: %.svg
	inkscape -D --export-pdf="$@" -w 210 -h 297 $<

_perf:
	for i in /sys/devices/system/cpu/cpu* ; do if [ -f $$i/cpufreq/scaling_governor ] ; then sudo bash -c "echo performance > $$i/cpufreq/scaling_governor" ; fi ;done

_noperf:
	for i in /sys/devices/system/cpu/cpu* ; do if [ -f $$i/cpufreq/scaling_governor ] ; then sudo bash -c "echo ondemand > $$i/cpufreq/scaling_governor" ; fi ; done


clean: _noperf
	rm -rf results build __precomp __run *.svg *.pdf *~ precomptmp __missingsizes __missingcompiletimes __possiblymissingbenchs
