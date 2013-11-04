

type token =
  | T_WITH of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 8 "js_parser.ml"
)
  | T_WHILE of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 13 "js_parser.ml"
)
  | T_VOID of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 18 "js_parser.ml"
)
  | T_VIRTUAL_SEMICOLON of (
# 295 "js_parser.mly"
       (Parse_info.t)
# 23 "js_parser.ml"
)
  | T_VAR of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 28 "js_parser.ml"
)
  | T_TYPEOF of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 33 "js_parser.ml"
)
  | T_TRY of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 38 "js_parser.ml"
)
  | T_TRUE of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 43 "js_parser.ml"
)
  | T_THROW of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 48 "js_parser.ml"
)
  | T_THIS of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 53 "js_parser.ml"
)
  | T_SWITCH of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 58 "js_parser.ml"
)
  | T_STRING of (
# 248 "js_parser.mly"
      (string * Parse_info.t)
# 63 "js_parser.ml"
)
  | T_STRICT_NOT_EQUAL of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 68 "js_parser.ml"
)
  | T_STRICT_EQUAL of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 73 "js_parser.ml"
)
  | T_SEMICOLON of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 78 "js_parser.ml"
)
  | T_RSHIFT_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 83 "js_parser.ml"
)
  | T_RSHIFT3_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 88 "js_parser.ml"
)
  | T_RSHIFT3 of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 93 "js_parser.ml"
)
  | T_RSHIFT of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 98 "js_parser.ml"
)
  | T_RPAREN of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 103 "js_parser.ml"
)
  | T_RETURN of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 108 "js_parser.ml"
)
  | T_REGEX of (
# 249 "js_parser.mly"
      (string * Parse_info.t)
# 113 "js_parser.ml"
)
  | T_RCURLY of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 118 "js_parser.ml"
)
  | T_RBRACKET of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 123 "js_parser.ml"
)
  | T_PLUS_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 128 "js_parser.ml"
)
  | T_PLUS of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 133 "js_parser.ml"
)
  | T_PLING of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 138 "js_parser.ml"
)
  | T_PERIOD of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 143 "js_parser.ml"
)
  | T_OR of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 148 "js_parser.ml"
)
  | T_NUMBER of (
# 246 "js_parser.mly"
      (string * [`Float of float | `Int of int] * Parse_info.t)
# 153 "js_parser.ml"
)
  | T_NULL of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 158 "js_parser.ml"
)
  | T_NOT_EQUAL of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 163 "js_parser.ml"
)
  | T_NOT of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 168 "js_parser.ml"
)
  | T_NEW of (
# 259 "js_parser.mly"
       (Parse_info.t)
# 173 "js_parser.ml"
)
  | T_MULT_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 178 "js_parser.ml"
)
  | T_MULT of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 183 "js_parser.ml"
)
  | T_MOD_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 188 "js_parser.ml"
)
  | T_MOD of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 193 "js_parser.ml"
)
  | T_MINUS_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 198 "js_parser.ml"
)
  | T_MINUS of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 203 "js_parser.ml"
)
  | T_LSHIFT_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 208 "js_parser.ml"
)
  | T_LSHIFT of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 213 "js_parser.ml"
)
  | T_LPAREN of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 218 "js_parser.ml"
)
  | T_LESS_THAN_EQUAL of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 223 "js_parser.ml"
)
  | T_LESS_THAN of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 228 "js_parser.ml"
)
  | T_LCURLY of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 233 "js_parser.ml"
)
  | T_LBRACKET of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 238 "js_parser.ml"
)
  | T_INSTANCEOF of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 243 "js_parser.ml"
)
  | T_INCR_NB of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 248 "js_parser.ml"
)
  | T_INCR of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 253 "js_parser.ml"
)
  | T_IN of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 258 "js_parser.ml"
)
  | T_IF of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 263 "js_parser.ml"
)
  | T_IDENTIFIER of (
# 247 "js_parser.mly"
      (string * Parse_info.t)
# 268 "js_parser.ml"
)
  | T_GREATER_THAN_EQUAL of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 273 "js_parser.ml"
)
  | T_GREATER_THAN of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 278 "js_parser.ml"
)
  | T_FUNCTION of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 283 "js_parser.ml"
)
  | T_FOR of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 288 "js_parser.ml"
)
  | T_FINALLY of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 293 "js_parser.ml"
)
  | T_FALSE of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 298 "js_parser.ml"
)
  | T_EQUAL of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 303 "js_parser.ml"
)
  | T_ELSE of (
# 257 "js_parser.mly"
       (Parse_info.t)
# 308 "js_parser.ml"
)
  | T_DO of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 313 "js_parser.ml"
)
  | T_DIV_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 318 "js_parser.ml"
)
  | T_DIV of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 323 "js_parser.ml"
)
  | T_DELETE of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 328 "js_parser.ml"
)
  | T_DEFAULT of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 333 "js_parser.ml"
)
  | T_DECR_NB of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 338 "js_parser.ml"
)
  | T_DECR of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 343 "js_parser.ml"
)
  | T_CONTINUE of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 348 "js_parser.ml"
)
  | T_COMMA of (
# 262 "js_parser.mly"
       (Parse_info.t)
# 353 "js_parser.ml"
)
  | T_COLON of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 358 "js_parser.ml"
)
  | T_CATCH of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 363 "js_parser.ml"
)
  | T_CASE of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 368 "js_parser.ml"
)
  | T_BREAK of (
# 252 "js_parser.mly"
       (Parse_info.t)
# 373 "js_parser.ml"
)
  | T_BIT_XOR_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 378 "js_parser.ml"
)
  | T_BIT_XOR of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 383 "js_parser.ml"
)
  | T_BIT_OR_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 388 "js_parser.ml"
)
  | T_BIT_OR of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 393 "js_parser.ml"
)
  | T_BIT_NOT of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 398 "js_parser.ml"
)
  | T_BIT_AND_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 403 "js_parser.ml"
)
  | T_BIT_AND of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 408 "js_parser.ml"
)
  | T_ASSIGN of (
# 271 "js_parser.mly"
       (Parse_info.t)
# 413 "js_parser.ml"
)
  | T_AND of (
# 276 "js_parser.mly"
       (Parse_info.t)
# 418 "js_parser.ml"
)
  | TUnknown of (
# 298 "js_parser.mly"
       (Parse_info.t)
# 423 "js_parser.ml"
)
  | TCommentSpace of (
# 239 "js_parser.mly"
       (Parse_info.t * string)
# 428 "js_parser.ml"
)
  | TCommentNewline of (
# 239 "js_parser.mly"
       (Parse_info.t * string)
# 433 "js_parser.ml"
)
  | TCommentML of (
# 239 "js_parser.mly"
       (Parse_info.t * string)
# 438 "js_parser.ml"
)
  | TComment of (
# 239 "js_parser.mly"
       (Parse_info.t * string)
# 443 "js_parser.ml"
)
  | EOF of (
# 299 "js_parser.mly"
       (Parse_info.t)
# 448 "js_parser.ml"
)



let info_of_tok = function
  | TUnknown ii -> ii

  | TCommentSpace (ii,_) -> ii
  | TCommentNewline (ii,_) -> ii
  | TComment (ii,_) -> ii
  | TCommentML (ii,_) -> ii
  | EOF ii -> ii

  | T_NUMBER (s, _,ii) -> ii
  | T_IDENTIFIER (s, ii) -> ii
  | T_STRING (s, ii) -> ii
  | T_REGEX (s, ii) -> ii

  | T_FUNCTION ii -> ii
  | T_IF ii -> ii
  | T_IN ii -> ii
  | T_INSTANCEOF ii -> ii
  | T_RETURN ii -> ii
  | T_SWITCH ii -> ii
  | T_THIS ii -> ii
  | T_THROW ii -> ii
  | T_TRY ii -> ii
  | T_VAR ii -> ii
  | T_WHILE ii -> ii
  | T_WITH ii -> ii
  | T_NULL ii -> ii
  | T_FALSE ii -> ii
  | T_TRUE ii -> ii
  | T_BREAK ii -> ii
  | T_CASE ii -> ii
  | T_CATCH ii -> ii
  | T_CONTINUE ii -> ii
  | T_DEFAULT ii -> ii
  | T_DO ii -> ii
  | T_FINALLY ii -> ii
  | T_FOR ii -> ii
  | T_ELSE ii -> ii
  | T_NEW ii -> ii
  | T_LCURLY ii -> ii
  | T_RCURLY ii -> ii
  | T_LPAREN ii -> ii
  | T_RPAREN ii -> ii
  | T_LBRACKET ii -> ii
  | T_RBRACKET ii -> ii
  | T_SEMICOLON ii -> ii
  | T_COMMA ii -> ii
  | T_PERIOD ii -> ii
  | T_RSHIFT3_ASSIGN ii -> ii
  | T_RSHIFT_ASSIGN ii -> ii
  | T_LSHIFT_ASSIGN ii -> ii
  | T_BIT_XOR_ASSIGN ii -> ii
  | T_BIT_OR_ASSIGN ii -> ii
  | T_BIT_AND_ASSIGN ii -> ii
  | T_MOD_ASSIGN ii -> ii
  | T_DIV_ASSIGN ii -> ii
  | T_MULT_ASSIGN ii -> ii
  | T_MINUS_ASSIGN ii -> ii
  | T_PLUS_ASSIGN ii -> ii
  | T_ASSIGN ii -> ii
  | T_PLING ii -> ii
  | T_COLON ii -> ii
  | T_OR ii -> ii
  | T_AND ii -> ii
  | T_BIT_OR ii -> ii
  | T_BIT_XOR ii -> ii
  | T_BIT_AND ii -> ii
  | T_EQUAL ii -> ii
  | T_NOT_EQUAL ii -> ii
  | T_STRICT_EQUAL ii -> ii
  | T_STRICT_NOT_EQUAL ii -> ii
  | T_LESS_THAN_EQUAL ii -> ii
  | T_GREATER_THAN_EQUAL ii -> ii
  | T_LESS_THAN ii -> ii
  | T_GREATER_THAN ii -> ii
  | T_LSHIFT ii -> ii
  | T_RSHIFT ii -> ii
  | T_RSHIFT3 ii -> ii
  | T_PLUS ii -> ii
  | T_MINUS ii -> ii
  | T_DIV ii -> ii
  | T_MULT ii -> ii
  | T_MOD ii -> ii
  | T_NOT ii -> ii
  | T_BIT_NOT ii -> ii
  | T_INCR ii -> ii
  | T_DECR ii -> ii
  | T_INCR_NB ii -> ii
  | T_DECR_NB ii -> ii
  | T_DELETE ii -> ii
  | T_TYPEOF ii -> ii
  | T_VOID ii -> ii
  | T_VIRTUAL_SEMICOLON ii -> ii


let string_of_tok = function
  | TUnknown ii -> "COMMENT"

  | TCommentSpace (ii,_) -> "COMMENT"
  | TCommentNewline (ii,_) -> "COMMENT"
  | TComment (ii,_) -> "COMMENT"
  | TCommentML (ii,_) -> "COMMENT"
  | EOF ii -> "EOF"

  | T_NUMBER (s, _,ii) -> "T_NUMBER"
  | T_IDENTIFIER (s, ii) -> "T_IDENTIFIER"
  | T_STRING (s, ii) -> "T_STRING"
  | T_REGEX (s, ii) -> "T_REGEX"

  | T_FUNCTION ii -> " T_FUNCTION"
  | T_IF ii -> "T_IF"
  | T_IN ii -> "T_IN"
  | T_INSTANCEOF ii -> "T_INSTANCEOF"
  | T_RETURN ii -> "T_RETURN"
  | T_SWITCH ii -> "T_SWITCH"
  | T_THIS ii -> "T_THIS"
  | T_THROW ii -> "T_THROW"
  | T_TRY ii -> "T_TRY"
  | T_VAR ii -> "T_VAR"
  | T_WHILE ii -> "T_WHILE"
  | T_WITH ii -> "T_WITH"
  | T_NULL ii -> "T_NULL"
  | T_FALSE ii -> "T_FALSE"
  | T_TRUE ii -> "T_TRUE"
  | T_BREAK ii -> "T_BREAK"
  | T_CASE ii -> "T_CASE"
  | T_CATCH ii -> "T_CATCH"
  | T_CONTINUE ii -> "T_CONTINUE"
  | T_DEFAULT ii -> "T_DEFAULT"
  | T_DO ii -> "T_DO"
  | T_FINALLY ii -> "T_FINALLY"
  | T_FOR ii -> "T_FOR"
  | T_ELSE ii -> "T_ELSE"
  | T_NEW ii -> "T_NEW"
  | T_LCURLY ii -> "T_LCURLY"
  | T_RCURLY ii -> "T_RCURLY"
  | T_LPAREN ii -> "T_LPAREN"
  | T_RPAREN ii -> "T_RPAREN"
  | T_LBRACKET ii -> "T_LBRACKET"
  | T_RBRACKET ii -> "T_RBRACKET"
  | T_SEMICOLON ii -> "T_SEMICOLON"
  | T_COMMA ii -> "T_COMMA"
  | T_PERIOD ii -> "T_PERIOD"
  | T_RSHIFT3_ASSIGN ii -> "T_RSHIFT3"
  | T_RSHIFT_ASSIGN ii -> "T_RSHIFT"
  | T_LSHIFT_ASSIGN ii -> "T_LSHIFT"
  | T_BIT_XOR_ASSIGN ii -> "T_BIT"
  | T_BIT_OR_ASSIGN ii -> "T_BIT"
  | T_BIT_AND_ASSIGN ii -> "T_BIT"
  | T_MOD_ASSIGN ii -> "T_MOD"
  | T_DIV_ASSIGN ii -> "T_DIV"
  | T_MULT_ASSIGN ii -> "T_MULT"
  | T_MINUS_ASSIGN ii -> "T_MINUS"
  | T_PLUS_ASSIGN ii -> "T_PLUS"
  | T_ASSIGN ii -> "T_ASSIGN"
  | T_PLING ii -> "T_PLING"
  | T_COLON ii -> "T_COLON"
  | T_OR ii -> "T_OR"
  | T_AND ii -> "T_AND"
  | T_BIT_OR ii -> "T_BIT"
  | T_BIT_XOR ii -> "T_BIT"
  | T_BIT_AND ii -> "T_BIT"
  | T_EQUAL ii -> "T_EQUAL"
  | T_NOT_EQUAL ii -> "T_NOT"
  | T_STRICT_EQUAL ii -> "T_STRICT"
  | T_STRICT_NOT_EQUAL ii -> "T_STRICT"
  | T_LESS_THAN_EQUAL ii -> "T_LESS"
  | T_GREATER_THAN_EQUAL ii -> "T_GREATER"
  | T_LESS_THAN ii -> "T_LESS"
  | T_GREATER_THAN ii -> "T_GREATER"
  | T_LSHIFT ii -> "T_LSHIFT"
  | T_RSHIFT ii -> "T_RSHIFT"
  | T_RSHIFT3 ii -> "T_RSHIFT3"
  | T_PLUS ii -> "T_PLUS"
  | T_MINUS ii -> "T_MINUS"
  | T_DIV ii -> "T_DIV"
  | T_MULT ii -> "T_MULT"
  | T_MOD ii -> "T_MOD"
  | T_NOT ii -> "T_NOT"
  | T_BIT_NOT ii -> "T_BIT"
  | T_INCR ii -> "T_INCR"
  | T_DECR ii -> "T_DECR"
  | T_INCR_NB ii -> "T_INCR"
  | T_DECR_NB ii -> "T_DECR"
  | T_DELETE ii -> "T_DELETE"
  | T_TYPEOF ii -> "T_TYPEOF"
  | T_VOID ii -> "T_VOID"
  | T_VIRTUAL_SEMICOLON ii -> "T_VIRTUAL"

let is_comment = function
  | TCommentSpace _
  | TCommentNewline _
  | TComment _
  | TCommentML _ -> true
  | _ -> false
