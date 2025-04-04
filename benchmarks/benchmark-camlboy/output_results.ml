let () =
  Format.printf
    {|{ "name": "%s",
  "results":
    [ { "name": "Camlboy",
        "metrics":
          [ { "name": "Frames per second",
              "units": "Hz",
               "value": %s } ] } ] }@.|}
    Sys.argv.(1)
    (read_line ())
