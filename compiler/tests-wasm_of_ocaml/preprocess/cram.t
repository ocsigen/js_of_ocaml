Too many parentheses

  $ echo '())' | wasm_of_ocaml pp
  File "-", line 1, characters 2-3:
  Unexpected closing parenthesis.
  [1]

  $ echo '();)' | wasm_of_ocaml pp
  File "-", line 1, characters 2-4:
  Unmatched closing comment.
  [1]

Missing parenthesis

  $ echo '(()' | wasm_of_ocaml pp
  File "-", line 1, characters 0-1:
  Unclosed parenthesis.
  [1]

  $ echo '(; ()' | wasm_of_ocaml pp
  File "-", line 1, characters 0-2:
  Unclosed comment.
  [1]

  $ echo '(; (; ()' | wasm_of_ocaml pp
  File "-", line 1, characters 3-5:
  Unclosed comment.
  [1]

Unterminated string (we point at the newline)

  $ echo '"abcd' | wasm_of_ocaml pp
  File "-", line 1, characters 5-5:
  Malformed string.
  [1]

Bad conditional

  $ echo '(@if)' | wasm_of_ocaml pp
  File "-", line 1, characters 4-5:
  Expecting condition.
  [1]

  $ echo '(@if a)' | wasm_of_ocaml pp
  File "-", line 1, characters 6-7:
  Expecting @then clause.
  [1]

  $ echo '(@if a xxx)' | wasm_of_ocaml pp
  File "-", line 1, characters 7-10:
  Expecting @then clause.
  [1]

  $ echo '(@if a (@then) xx)' | wasm_of_ocaml pp
  File "-", line 1, characters 15-17:
  Expecting @else clause or closing parenthesis.
  [1]

  $ echo '(@if a (@then) (@else) xx)' | wasm_of_ocaml pp
  File "-", line 1, characters 23-25:
  Expecting closing parenthesis.
  [1]

Syntax error in condition

  $ echo '(@if () (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-7:
  Syntax error.
  [1]

  $ echo '(@if (not) (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-10:
  Syntax error.
  [1]

  $ echo '(@if (not (and) (or)) (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-21:
  Syntax error.
  [1]

  $ echo '(@if (= "a") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-12:
  Syntax error.
  [1]

  $ echo '(@if (= "a" "b" "c") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-20:
  Syntax error.
  [1]

Unicode escape sequences

  $ echo '(@if (= "\u{1F600}" "b") (@then))' | wasm_of_ocaml pp
                                   

  $ echo '(@if (= "\u{D800}" "b") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 8-18:
  Invalid Unicode escape sequences.
  [1]

  $ echo '(@if (= "\u{110000}" "b") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 8-20:
  Invalid Unicode escape sequences.
  [1]

Lonely @then or @else

  $ echo '(@then)' | wasm_of_ocaml pp
  File "-", line 1, characters 1-6:
  Unexpected @then clause. Maybe you forgot a parenthesis.
  [1]

  $ echo '(@else)' | wasm_of_ocaml pp
  File "-", line 1, characters 1-6:
  Unexpected @else clause. Maybe you forgot a parenthesis.
  [1]

  $ echo '(@if (and) (@then (@else)))' | wasm_of_ocaml pp
  File "-", line 1, characters 19-24:
  Unexpected @else clause. Maybe you forgot a parenthesis.
  [1]

Undefined variable

  $ echo '(@if a (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-6:
  Unknown variable 'a'.
  [1]

Wrong type
  $ echo '(@if "" (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 5-7:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (not "") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 10-12:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (and "") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 10-12:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (or "") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 9-11:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (= (and) "") (@then))' | wasm_of_ocaml pp
  File "-", line 1, characters 14-16:
  Expected a boolean but this is a string.
  [1]

Bad strings

  $ echo '(@string)' | wasm_of_ocaml pp
  File "-", line 1, characters 8-9:
  Expecting an id or a string.
  [1]

  $ echo '(@string a "b")' | wasm_of_ocaml pp
  File "-", line 1, characters 9-10:
  Expecting an id
  [1]

  $ echo '(@string $a b)' | wasm_of_ocaml pp
  File "-", line 1, characters 12-13:
  Expecting a string
  [1]

  $ echo '(@string $bad "\u{D800}")' | wasm_of_ocaml pp
  File "-", line 1, characters 14-24:
  Invalid Unicode escape sequences.
  [1]

  $ echo '(@string a)' | wasm_of_ocaml pp
  File "-", line 1, characters 9-10:
  Expecting a string
  [1]

  $ echo '(@string a b c)' | wasm_of_ocaml pp
  File "-", line 1, characters 13-14:
  Expecting a closing parenthesis.
  [1]

Bad characters

  $ echo '(@char "")' | wasm_of_ocaml pp
  File "-", line 1, characters 7-9:
  Expecting an ASCII character
  [1]


  $ echo '(@char "aa")' | wasm_of_ocaml pp
  File "-", line 1, characters 7-11:
  Expecting an ASCII character
  [1]


  $ echo '(@char "é")' | wasm_of_ocaml pp
  File "-", line 1, characters 7-10:
  Expecting an ASCII character
  [1]


  $ echo '(@char "\80")' | wasm_of_ocaml pp
  File "-", line 1, characters 7-12:
  Expecting an ASCII character
  [1]
