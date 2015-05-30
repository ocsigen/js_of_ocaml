//Requires: caml_global_data, caml_new_string
var predefined_exceptions =
    [ {i:0, n:"Out_of_memory"},
      {i:1, n:"Sys_error"},
      {i:2, n:"Failure"},
      {i:3, n:"Invalid_argument"},
      {i:4, n:"End_of_file"},
      {i:5, n:"Division_by_zero"},
      {i:6, n:"Not_found"},
      {i:7, n:"Match_failure"},
      {i:8, n:"Stack_overflow"},
      {i:9, n:"Sys_blocked_io"},
      {i:10,n:"Assert_failure"},
      {i:11,n:"Undefined_recursive_module"}]

for(var p = 0; p < predefined_exceptions.length; p++){
  var e = predefined_exceptions[p];
  caml_global_data[e.i + 1] = [248, caml_new_string(e.n), - e.i]
  caml_global_data[e.n] = caml_global_data[e.i + 1]
}
