let _ = Approx_lexer.enable_extension "lwt"

let indent s in_lines =
  let output =
    { IndentPrinter.debug = false
    ; config = IndentConfig.default
    ; in_lines
    ; indent_empty = true
    ; adaptive = true
    ; kind = IndentPrinter.Print (fun s acc -> acc ^ s)
    }
  in
  let stream = Nstream.of_string s in
  IndentPrinter.proceed output stream IndentBlock.empty ""
