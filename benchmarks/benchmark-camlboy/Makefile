.PHONY: bench perform

export NAME=Camlboy

SHELL=/bin/bash -o pipefail

DIR=CAMLBOY
SCRIPT=$(DIR)/_build/default/bin/web/bench_node.bc
ROM=$(DIR)/resource/games/tobu.gb

bench:
	@date -u +"%FT%TZ - $(NAME): starting"
	git clone --depth 1 https://github.com/ocaml-wasm/CAMLBOY -b node
	cd $(DIR) && opam exec -- dune build --root . --profile release ./bin/web
	@date -u +"%FT%TZ - $(NAME): compiled"
	$(MAKE) perform COMPILER=Js_of_ocaml SUFFIX=.js KIND=js
	$(MAKE) perform COMPILER=Wasm_of_ocaml SUFFIX=.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME): done"

perform:
	node $(SCRIPT)$(SUFFIX) $(ROM) | \
	tee /dev/stderr | \
	ocaml output_results.ml $(COMPILER) | \
	sh ../utils/aggregate.sh $(KIND)
