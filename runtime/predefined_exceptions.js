//Requires: caml_global_data, caml_new_string
var predefined_exceptions =
    [ {index:0, name:"Out_of_memory"},
      {index:1, name:"Sys_error"},
      {index:2, name:"Failure"},
      {index:3, name:"Invalid_argument"},
      {index:4, name:"End_of_file"},
      {index:5, name:"Division_by_zero"},
      {index:6, name:"Not_found"},
      {index:7, name:"Match_failure"},
      {index:8, name:"Stack_overflow"},
      {index:9, name:"Sys_blocked_io"},
      {index:10,name:"Assert_failure"},
      {index:11,name:"Undefined_recursive_module"}]

for(var i = 0; i < predefined_exceptions.length; i++){
  var info = predefined_exceptions[i];
  var exn = [248, caml_new_string(info.name), - info.index];
  caml_register_global(info.index, exn, info.name);
}
