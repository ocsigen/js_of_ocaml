.PHONY: bench perform

export NAME=Partial Render Table

SHELL=/bin/bash -o pipefail

# Sources of bonsai_web_components, see bench.Dockerfile
ROOT=../../../bonsai_web_components
BIN_DIR=partial_render_table/bench/bin

bench:
	@date -u +"%FT%TZ - $(NAME): starting"
	dune build --root $(ROOT) --profile release $(BIN_DIR)/main.bc.wasm.js $(BIN_DIR)/main.bc.js
	cp -r $(ROOT)/_build/default/$(BIN_DIR)/main.bc* .
	@date -u +"%FT%TZ - $(NAME): compiled"
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=main.bc.js KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=main.bc.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME): done"

# The benchmark runs each test for 10s by default, 1s is enough
perform:
	/usr/bin/time -f "%E %M" $(COMPILER) --debug times --opt 2 --pretty main.bc-for-jsoo -o out.js 2>&1 | \
	tee /dev/stderr | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) "$(NAME)" out.js | \
	sh ../utils/aggregate.sh $(KIND)
	node $(SCRIPT) -quota 1s | \
	tee /dev/stderr | \
	ocaml -I +str str.cma summarize_results.ml $(COMPILER) | \
	sh ../utils/aggregate.sh $(KIND)
