.PHONY: bench perform

export NAME=Partial Render Table

SHELL=/bin/bash -o pipefail

bench:
	cd ../.. && opam exec -- dune exec tools/ci_setup.exe ../janestreet .
	opam install ppxlib.0.35.0 # temporary workaround
	dune build --root ../../../janestreet --profile release lib/bonsai_web_components/partial_render_table/bench/bin/main.bc.wasm.js lib/bonsai_web_components/partial_render_table/bench/bin/main.bc.js
	cp -r ../../../janestreet/_build/default/lib/bonsai_web_components/partial_render_table/bench/bin/main.bc* .
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=main.bc.js KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=main.bc.wasm.js KIND=wasm

perform:
	/usr/bin/time -f "%E %R" $(COMPILER) --debug times --opt 2 --pretty main.bc-for-jsoo -o out.js 2>&1 | \
	tee /dev/stderr | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) "$(NAME)" | \
	sh ../utils/aggregate.sh $(KIND)
	node $(SCRIPT) | \
	tee /dev/stderr | \
	ocaml -I +str str.cma summarize_results.ml $(COMPILER) | \
	sh ../utils/aggregate.sh $(KIND)
