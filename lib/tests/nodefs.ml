let ic = open_in "nodefs.ml"
let () =
  let l = ref [] in
  let () =
    try
      while true do
        l := input_line ic :: !l;
      done
    with End_of_file -> ()
  in
  ()  
