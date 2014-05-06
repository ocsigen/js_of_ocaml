
let read_lines ?(empty=false) file =
  let ic = open_in file in
  let l = ref [] in
  begin try
      while true do
        let s=input_line ic in
        if empty || s<>""
        then l:=s::!l
      done
    with End_of_file -> () end;
  List.rev !l

let read_mllist file =
  let b = Filename.dirname file in
  let l = read_lines file in
  List.map (fun f -> Filename.concat b (String.uncapitalize f)) l
