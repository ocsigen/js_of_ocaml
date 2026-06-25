let () =
  for i = 1 to Array.length Sys.argv - 1 do
    Format.printf
      "%s:%s@."
      (Filename.chop_suffix (Filename.basename Sys.argv.(i)) ".wat")
      Sys.argv.(i)
  done
