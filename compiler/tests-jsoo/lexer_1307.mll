{
  open Parser_1307
}

rule token = parse
| _ { TOKEN }
| eof { EOF }