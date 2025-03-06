let () =
  let ch = open_in "/static/file.txt" in
  let b = Bytes.create 1024 in
  while input ch b 0 1024 > 0 do
    ()
  done;
  close_in ch
