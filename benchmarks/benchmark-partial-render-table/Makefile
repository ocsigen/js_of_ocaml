.PHONY: bench perform

export NAME=Partial Render Table

SHELL=/bin/bash -o pipefail

bench:
	$(MAKE) perform COMPILER=Js_of_ocaml SCRIPT=main.bc.js
	$(MAKE) perform COMPILER=Wasm_of_ocaml SCRIPT=main.bc.wasm.js

perform:
	node $(SCRIPT) | \
	tee /dev/stderr | \
	ocaml -I +str str.cma summarize_results.ml $(COMPILER)
