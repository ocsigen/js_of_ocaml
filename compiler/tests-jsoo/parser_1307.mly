
%{

%}

%token EOF
%token TOKEN
%start root
%type <int> root
%%

root:
| expr EOF { 0 }
| expr error root { 1 }

expr:
 TOKEN TOKEN { 1 }
