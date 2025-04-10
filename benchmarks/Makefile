# graphs without precompiled programs:
GRAPHSNOPR = time.pdf time-optim.pdf nativejs.pdf

# graphs with precompiled programs:
GRAPHSPR = size.pdf size-optim.pdf compiletime.pdf

# graphs showing the impact of supporting effects
GRAPHSEFF = time-effects.pdf size-effects.pdf size-bzip2-effects.pdf

# For full benchs:
all: _perf graphsnopr graphspr graphseff _noperf

# This target is the one run by `current-bench`,
# see https://github.com/ocurrent/current-bench
.PHONY: bench microbenchmarks
bench:
	@date -u +"%FT%TZ - Installing dependencies"
	opam pin -yn --with-version=dev ..
	opam install -y wasm_of_ocaml-compiler js_of_ocaml-ppx gen_js_api brr
	$(MAKE) microbenchmarks
	$(MAKE) -C benchmark-fiat-crypto bench
	$(MAKE) -C benchmark-ocamlc bench
	$(MAKE) -C benchmark-partial-render-table bench
	$(MAKE) -C benchmark-camlboy bench

microbenchmarks:
	@date -u +"%FT%TZ - Microbenchmarks: starting"
	make _noprecomp
	@date -u +"%FT%TZ - Microbenchmarks: compiler"
	$(RUN) ./report-wasm-cb.config ./report-jsoo-cb.config
	$(REPORT) -format current-bench -config report-wasm-cb.config \
	  -ylabel "Wasm_of_ocaml" | sh utils/aggregate.sh wasm
	$(REPORT) -format current-bench -config report-jsoo-cb.config \
	  -ylabel "Js_of_ocaml" | sh utils/aggregate.sh js
	@date -u +"%FT%TZ - Microbenchmarks: done"

graphsnopr: _noprecomp $(GRAPHSNOPR)

graphspr: __precomp $(GRAPHSPR)

graphseff: __precomp $(GRAPHSEFF)

# For fast benchs:
test: _perf fastrun $(GRAPHS) _noperf

exes= $(wildcard programs/*)

exesnames = $(exes:programs/%=%)

missingsizesml = $(exesnames:%=results/sizes/ml/%)
missingsizes=$(missingsizesml)

missingcompiletimes = $(exesnames:%=results/compiletimes/`hostname`/byte/%) $(exesnames:%=results/compiletimes/`hostname`/opt/%)

RUN=dune exec ./run.exe --
REPORT=dune exec ./report.exe --

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
	touch __run

__run_effects:
	$(RUN) -fast report-time-effects.config
	touch __run_effects

fastrun:
	make _noprecomp
	echo "======================== WARNING: fast benchs!"
	$(RUN) -ffast -all
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

__missingsizes: __precomp
	for i in $(missingsizes) ; do if [ ! -f $$i ] ; then echo 0 > $$i ; fi ; done
	if [ ! -f results/sizes/ml/ocamlc ] ; then echo 800771 > results/sizes/ml/ocamlc ; fi
	if [ ! -f results/sizes/ml/js_of_ocaml ] ; then echo 171361 > results/sizes/ml/js_of_ocaml ; fi
	if [ ! -f results/sizes/ml/boulderdash ] ; then echo 12920 > results/sizes/ml/boulderdash ; fi
	if [ ! -f results/sizes/ml/cubes ] ; then echo 3773 > results/sizes/ml/cubes ; fi
	if [ ! -f results/sizes/ml/minesweeper ] ; then echo 7106 > results/sizes/ml/minesweeper ; fi
	if [ ! -f results/sizes/ml/planet ] ; then echo 16488 > results/sizes/ml/planet ; fi
	touch __missingsizes

# To generate SVG, we first need to run the benchs,
# then to add data for precompiled programs
time.svg: __run
	$(REPORT) -config report-time.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit loop \
	 -max 5.5 -svg 7 500 150 -edgecaption -ylabel "Execution time" \
	 > $@

size.svg: size.gpl
	gnuplot < $^ > $@

size.gpl: __run __missingsizes
	$(REPORT) -config report-size.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc \
	 -omit kb_no_exc \
	 -omit loop \
	 -append boulderdash \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append js_of_ocaml \
	 -append ocamlc \
	 -max 4.5 -ylabel Size -script \
	 -svg 7 650 150 \
	| perl -pe 'BEGIN{undef $$/;} s/e\nplot/e\nset key at 19.83,2.87\nplot/smg' \
	 > $@
# You'll need to adjust the perl invocation above to align the "Generated code" caption


time-optim.svg: __run
	$(REPORT) -config report-time-optim.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit loop \
	 -max 2.5 -svg 7 400 150 -edgecaption -ylabel "Execution time" \
	 > $@

size-optim.svg: __run __missingsizes
	$(REPORT) -config report-size-optim.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc \
	 -omit kb_no_exc \
	 -omit loop \
	 -append boulderdash \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append js_of_ocaml \
	 -append ocamlc \
	 -max 2.5 -svg 7 650 150 -edgecaption -ylabel Size \
	 > $@

time-effects.svg: __run_effects
	$(REPORT) -config report-time-effects.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit loop \
	 -omit boulderdash \
	 -omit cubes \
	 -omit minesweeper \
	 -omit planet \
	 -omit ocamlc \
	 -min 0.5 -max 1.5 -svg 7 400 150 -edgecaption -ylabel "Execution time" \
	 > $@

size-effects.svg: __run_effects
	$(REPORT) -config report-size-effects.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc \
	 -omit kb_no_exc \
	 -omit loop \
	 -append boulderdash \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append js_of_ocaml \
	 -append ocamlc \
	 -min 0.8 -max 1.25 -svg 7 650 150 -edgecaption -ylabel Size \
	 > $@

size-gzipped-effects.svg: __run_effects
	$(REPORT) -config report-size-gzipped-effects.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc \
	 -omit kb_no_exc \
	 -omit loop \
	 -append boulderdash \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append js_of_ocaml \
	 -append ocamlc \
	 -max 1.2 -min 0.8 -svg 7 650 150 -edgecaption -ylabel Size \
	 > $@

size-bzip2-effects.svg: __run_effects
	$(REPORT) -config report-size-bzip2-effects.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit fannkuch_redux_2 \
	 -omit boyer_no_exc \
	 -omit kb_no_exc \
	 -omit loop \
	 -append boulderdash \
	 -append cubes \
	 -append minesweeper \
	 -append planet \
	 -append js_of_ocaml \
	 -append ocamlc \
	 -max 1.1 -min 0.95 -svg 7 650 150 -edgecaption -ylabel Size \
	 > $@

compiletime.svg: __run __precomp __missingcompiletimes
	$(REPORT) -config report-compiletime.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit loop \
	 -max 2 -svg 7 500 150 -edgecaption -ylabel "Compile time" \
	 > $@

nativejs.svg: __run
	$(REPORT) -config report-nativejs.config \
	 -omit binary_trees \
	 -omit fannkuch_redux \
	 -omit loop \
	 -max 2.8 -svg 7 200 150 -edgecaption -ylabel "Execution time" \
	 > $@


%.pdf: %.svg
	inkscape -D --export-type="pdf" --export-filename="$@" -w 210 -h 297 $<


nativejs.png: nativejs.svg
	inkscape -D -o "$@" -w 500 $<

%.png: %.svg
	inkscape -D -o "$@" -w 1024 $<

_perf:
	for i in /sys/devices/system/cpu/cpu* ; \
	do \
	if [ -f $$i/cpufreq/scaling_governor ] ; then \
		sudo bash -c "echo performance > $$i/cpufreq/scaling_governor" ; \
	fi ; \
	done

_noperf:
	for i in /sys/devices/system/cpu/cpu* ; \
	do \
	if [ -f $$i/cpufreq/scaling_governor ] ; then \
		sudo bash -c "echo powersave > $$i/cpufreq/scaling_governor" ; \
	fi ; \
	done


clean: _noperf
	rm -rf results build __precomp __run __run_effects *.svg *.pdf *~ precomptmp __missingsizes __missingcompiletimes


copy-extra-bc:
	mkdir -p programs
	dune exec ./stripdebug.exe -- $(shell which ocamlc.byte) programs/ocamlc
	dune build --profile bench_no_debug ../compiler/bin-js_of_ocaml/js_of_ocaml.bc ../examples/
	cp -f ../_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.bc programs/js_of_ocaml
	cp -f ../_build/default/examples/boulderdash/boulderdash.bc programs/boulderdash
	cp -f ../_build/default/examples/cubes/cubes.bc programs/cubes
	cp -f ../_build/default/examples/minesweeper/main.bc programs/minesweeper
	cp -f ../_build/default/examples/planet/planet.bc programs/planet
