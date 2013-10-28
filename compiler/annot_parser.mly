


%token TProvides TRequires
%token<Primitive.kind > TAnnot
%token<string> TIdent
%token TComma TSemi EOF EOL
%token<string> TOTHER

%start annot
%type <Primitive.t> annot

%%

annot:
  | TProvides TSemi id=TIdent opt=option(TAnnot) endline
    { `Provides (None,id,match opt with None -> `Mutator | Some k -> k) }
  | TRequires TSemi l=separated_nonempty_list(TComma,TIdent) endline
    { `Requires (None,l) }

endline:
  | EOL { () }
  | EOF { () }
  | TOTHER { failwith $1  }
