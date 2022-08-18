let () =
  let t = Tlbinf32.typeLibInfo () in
  t#setContainingFile {|c:\Windows\SysWOW64\tlbinf32.dll|};
  match t#coClasses with
  | None -> Printf.eprintf "coClasses is nothing?!\n"
  | Some col ->
    Printf.printf "|coClasses|=%d\n" col#count;
    for i = 1 to col#count do
      Printf.printf "- %s\n" (Option.get (col#item i))#name
    done
