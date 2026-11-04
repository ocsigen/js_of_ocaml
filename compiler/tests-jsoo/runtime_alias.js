// A primitive called through an alias defined in a runtime file. With
// separate compilation, the OCaml code is compiled without this file, so
// the runtime has to export the alias.

//Provides: jsoo_test_alias_target const
//Alias: jsoo_test_alias
function jsoo_test_alias_target(x) {
  return x + 1;
}
