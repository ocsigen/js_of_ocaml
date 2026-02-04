(* This is demo input for wiki. It will be loaded automatically into window on startup *)

let test1 =
  "= Wiki Syntax Demo\n\n\
   This is a **live preview** of //Wikicreole// markup. Edit the text on the left and \
   the rendered output updates instantly.\n\n\
   == Text Formatting\n\n\
   You can write **bold text**, //italic text//, or combine them: **//bold and \
   italic//**. Use {{{monospace}}} for inline code.\n\n\
   Line breaks can be forced\\\\like this.\n\n\
   == Headings\n\n\
   Headings use equals signs, from {{{=}}} (h1) to {{{======}}} (h6).\n\n\
   === Third-Level Heading\n\
   ==== Fourth-Level Heading\n\n\
   == Lists\n\n\
   Unordered lists use asterisks:\n\
   * First item\n\
   * Second item with **bold**\n\
   * Third item with //emphasis//\n\n\
   Ordered lists use hash signs:\n\
   # Step one\n\
   # Step two\n\
   # Step three\n\n\
   == Links\n\n\
   Named link: [[http://ocsigen.org|Ocsigen Project]]\n\n\
   Plain URL: [[http://github.com/ocsigen/js_of_ocaml]]\n\n\
   Auto-linked: http://ocaml.org\n\n\
   == Tables\n\n\
   |=Language |=Paradigm |=Typing |\n\
   | OCaml | Functional | Static |\n\
   | JavaScript | Multi-paradigm | Dynamic |\n\
   | Rust | Systems | Static |\n\n\
   == Images\n\n\
   {{https://upload.wikimedia.org/wikipedia/commons/f/ff/OCaml_Logo.svg|OCaml Logo}}\n\n\
   == Videos\n\n\
   Embed a YouTube video:\n\n\
   <<youtube 9FGaKfA5xR8>>\n\n\
   == Horizontal Rule\n\n\
   ----\n\n\
   == Preformatted Block\n\n\
   {{{\n\
   let hello () =\n\
  \  print_endline \"Hello from OCaml!\"\n\
   }}}\n\n\
   This demo is powered by **js_of_ocaml** -- the wiki parser runs entirely in the \
   browser, compiled from OCaml to JavaScript.\n"
