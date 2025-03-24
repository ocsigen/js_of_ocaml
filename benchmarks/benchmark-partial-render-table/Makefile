.PHONY: bench perform

export NAME=Partial Render Table

SHELL=/bin/bash -o pipefail

bench:
	$(MAKE) perform COMPILER=Js_of_ocaml SCRIPT=main.bc.js
	$(MAKE) perform COMPILER=Wasm_of_ocaml SCRIPT=main.bc.wasm.js

perform:
	/usr/bin/time -f "%E %R" $(COMPILER) --debug times --opt 2 --pretty `which ocamlc.byte` -o $(SCRIPT) 2>&1 | \
	tee /dev/stderr | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME)
	node $(SCRIPT) | \
	tee /dev/stderr | \
	ocaml -I +str str.cma summarize_results.ml $(COMPILER)
