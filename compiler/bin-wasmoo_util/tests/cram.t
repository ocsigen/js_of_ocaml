Too many parentheses

  $ echo '())' | wasmoo_util pp
  File "-", line 1, characters 2-3:
  Unexpected closing parenthesis.
  [1]

  $ echo '();)' | wasmoo_util pp
  File "-", line 1, characters 2-4:
  Unmatched closing comment.
  [1]

Missing parenthesis

  $ echo '(()' | wasmoo_util pp
  File "-", line 1, characters 0-1:
  Unclosed parenthesis.
  [1]

  $ echo '(; ()' | wasmoo_util pp
  File "-", line 1, characters 0-2:
  Unclosed comment.
  [1]

  $ echo '(; (; ()' | wasmoo_util pp
  File "-", line 1, characters 3-5:
  Unclosed comment.
  [1]

Unterminated string (we point at the newline)

  $ echo '"abcd' | wasmoo_util pp
  File "-", line 1, characters 5-5:
  Malformed string.
  [1]

Bad conditional

  $ echo '(@if)' | wasmoo_util pp
  File "-", line 1, characters 4-5:
  Expecting condition.
  [1]

  $ echo '(@if a)' | wasmoo_util pp
  File "-", line 1, characters 6-7:
  Expecting @then clause.
  [1]

  $ echo '(@if a xxx)' | wasmoo_util pp
  File "-", line 1, characters 7-10:
  Expecting @then clause.
  [1]

  $ echo '(@if a (@then) xx)' | wasmoo_util pp
  File "-", line 1, characters 15-17:
  Expecting @else clause or closing parenthesis.
  [1]

  $ echo '(@if a (@then) (@else) xx)' | wasmoo_util pp
  File "-", line 1, characters 23-25:
  Expecting closing parenthesis.
  [1]

Syntax error in condition

  $ echo '(@if () (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-7:
  Syntax error.
  [1]

  $ echo '(@if (not) (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-10:
  Syntax error.
  [1]

  $ echo '(@if (not (and) (or)) (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-21:
  Syntax error.
  [1]

  $ echo '(@if (= "a") (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-12:
  Syntax error.
  [1]

  $ echo '(@if (= "a" "b" "c") (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-20:
  Syntax error.
  [1]

Unicode escape sequences are not supported

  $ echo '(@if (= "\u{1F600}" "b") (@then))' | wasmoo_util pp
  File "-", line 1, characters 8-19:
  Unicode escape sequences are not supported.
  [1]

Lonely @then or @else

  $ echo '(@then)' | wasmoo_util pp
  File "-", line 1, characters 1-6:
  Unexpected @then clause. Maybe you forgot a parenthesis.
  [1]

  $ echo '(@else)' | wasmoo_util pp
  File "-", line 1, characters 1-6:
  Unexpected @else clause. Maybe you forgot a parenthesis.
  [1]

  $ echo '(@if (and) (@then (@else)))' | wasmoo_util pp
  File "-", line 1, characters 19-24:
  Unexpected @else clause. Maybe you forgot a parenthesis.
  [1]

Undefined variable

  $ echo '(@if a (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-6:
  Unknown variable 'a'.
  [1]

Wrong type
  $ echo '(@if "" (@then))' | wasmoo_util pp
  File "-", line 1, characters 5-7:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (not "") (@then))' | wasmoo_util pp
  File "-", line 1, characters 10-12:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (and "") (@then))' | wasmoo_util pp
  File "-", line 1, characters 10-12:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (or "") (@then))' | wasmoo_util pp
  File "-", line 1, characters 9-11:
  Expected a boolean but this is a string.
  [1]

  $ echo '(@if (= (and) "") (@then))' | wasmoo_util pp
  File "-", line 1, characters 14-16:
  Expected a boolean but this is a string.
  [1]
