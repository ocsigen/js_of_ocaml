.PHONY: bench perform

export NAME=Fiat-Crypto

SHELL=/bin/bash -o pipefail

bench:
	@date -u +"%FT%TZ - $(NAME): starting"
	$(MAKE) perform COMPILER=js_of_ocaml EXTRA_ARGS="--enable effects" KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml EXTRA_ARGS="" KIND=wasm
	@date -u +"%FT%TZ - $(NAME): done"

perform: bedrock2_fiat_crypto.byte
	/usr/bin/time -f "%E %R" $(COMPILER) --debug times --source-map $(EXTRA_ARGS) $< -o out.js 2>&1 | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME) out.js | \
	sh ../utils/aggregate.sh $(KIND)

bedrock2_fiat_crypto.byte: bedrock2_fiat_crypto.ml
	ocamlfind ocamlc -w -20 -g -linkpkg -package js_of_ocaml -g $< -o $@

bedrock2_fiat_crypto.ml: bedrock2_fiat_crypto.ml.xz
	unxz --keep $<
