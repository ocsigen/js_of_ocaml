run-test::
	ocamlc nums.cma test.ml test_nats.ml test_big_ints.ml test_ratios.ml test_nums.ml test_io.ml -o test_num.byte
	../../compiler/js_of_ocaml -noruntime ../../runtime/runtime.js ../../runtime/nat.js test_num.byte
	node test_num.js
