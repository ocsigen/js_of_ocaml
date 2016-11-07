

type token =
  | T_WITH of Parse_info.t
  | T_WHILE of Parse_info.t
  | T_VOID of Parse_info.t
  | T_VIRTUAL_SEMICOLON of Parse_info.t
  | T_VAR of Parse_info.t
  | T_TYPEOF of Parse_info.t
  | T_TRY of Parse_info.t
  | T_TRUE of Parse_info.t
  | T_THROW of Parse_info.t
  | T_THIS of Parse_info.t
  | T_SWITCH of Parse_info.t
  | T_STRING of (string * Parse_info.t)
  | T_STRICT_NOT_EQUAL of Parse_info.t
  | T_STRICT_EQUAL of Parse_info.t
  | T_SEMICOLON of Parse_info.t
  | T_RSHIFT_ASSIGN of Parse_info.t
  | T_RSHIFT3_ASSIGN of Parse_info.t
  | T_RSHIFT3 of Parse_info.t
  | T_RSHIFT of Parse_info.t
  | T_RPAREN of Parse_info.t
  | T_RETURN of Parse_info.t
  | T_REGEX of (string * Parse_info.t)
  | T_RCURLY of Parse_info.t
  | T_RBRACKET of Parse_info.t
  | T_PLUS_ASSIGN of Parse_info.t
  | T_PLUS of Parse_info.t
  | T_PLING of Parse_info.t
  | T_PERIOD of Parse_info.t
  | T_OR of Parse_info.t
  | T_NUMBER of (string * float * Parse_info.t)
  | T_NULL of Parse_info.t
  | T_NOT_EQUAL of Parse_info.t
  | T_NOT of Parse_info.t
  | T_NEW of Parse_info.t
  | T_MULT_ASSIGN of Parse_info.t
  | T_MULT of Parse_info.t
  | T_MOD_ASSIGN of Parse_info.t
  | T_MOD of Parse_info.t
  | T_MINUS_ASSIGN of Parse_info.t
  | T_MINUS of Parse_info.t
  | T_LSHIFT_ASSIGN of Parse_info.t
  | T_LSHIFT of Parse_info.t
  | T_LPAREN of Parse_info.t
  | T_LESS_THAN_EQUAL of Parse_info.t
  | T_LESS_THAN of Parse_info.t
  | T_LCURLY of Parse_info.t
  | T_LBRACKET of Parse_info.t
  | T_INSTANCEOF of Parse_info.t
  | T_INCR_NB of Parse_info.t
  | T_INCR of Parse_info.t
  | T_IN of Parse_info.t
  | T_IF of Parse_info.t
  | T_IDENTIFIER of (string * Parse_info.t)
  | T_GREATER_THAN_EQUAL of Parse_info.t
  | T_GREATER_THAN of Parse_info.t
  | T_FUNCTION of Parse_info.t
  | T_FOR of Parse_info.t
  | T_FINALLY of Parse_info.t
  | T_FALSE of Parse_info.t
  | T_EQUAL of Parse_info.t
  | T_ELSE of Parse_info.t
  | T_DO of Parse_info.t
  | T_DIV_ASSIGN of Parse_info.t
  | T_DIV of Parse_info.t
  | T_DELETE of Parse_info.t
  | T_DEFAULT of Parse_info.t
  | T_DECR_NB of Parse_info.t
  | T_DECR of Parse_info.t
  | T_CONTINUE of Parse_info.t
  | T_COMMA of Parse_info.t
  | T_COLON of Parse_info.t
  | T_CATCH of Parse_info.t
  | T_CASE of Parse_info.t
  | T_BREAK of Parse_info.t
  | T_BIT_XOR_ASSIGN of Parse_info.t
  | T_BIT_XOR of Parse_info.t
  | T_BIT_OR_ASSIGN of Parse_info.t
  | T_BIT_OR of Parse_info.t
  | T_BIT_NOT of Parse_info.t
  | T_BIT_AND_ASSIGN of Parse_info.t
  | T_BIT_AND of Parse_info.t
  | T_ASSIGN of Parse_info.t
  | T_AND of Parse_info.t
  | T_DEBUGGER of Parse_info.t
  | TUnknown of (Parse_info.t * string)
  | TCommentSpace of (Parse_info.t * string)
  | TCommentNewline of (Parse_info.t * string)
  | TCommentML of (Parse_info.t * string)
  | TComment of (Parse_info.t * string)
  | EOF of Parse_info.t



let info_of_tok = function
  | TUnknown (ii,_) -> ii

  | TCommentSpace (ii,_) -> ii
  | TCommentNewline (ii,_) -> ii
  | TComment (ii,_) -> ii
  | TCommentML (ii,_) -> ii
  | EOF ii -> ii
  | T_DEBUGGER ii -> ii

  | T_NUMBER (_, _,ii) -> ii
  | T_IDENTIFIER (_, ii) -> ii
  | T_STRING (_, ii) -> ii
  | T_REGEX (_, ii) -> ii

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
  | TUnknown (_,_) -> "COMMENT"

  | TCommentSpace (_,_) -> "COMMENT"
  | TCommentNewline (_,_) -> "COMMENT"
  | TComment (_,_) -> "COMMENT"
  | TCommentML (_,_) -> "COMMENT"
  | EOF _ -> "EOF"

  | T_DEBUGGER _ -> "DEBUGGER"

  | T_NUMBER (_, _,_) -> "T_NUMBER"
  | T_IDENTIFIER (_, _) -> "T_IDENTIFIER"
  | T_STRING (_, _) -> "T_STRING"
  | T_REGEX (_, _) -> "T_REGEX"

  | T_FUNCTION _ -> " T_FUNCTION"
  | T_IF _ -> "T_IF"
  | T_IN _ -> "T_IN"
  | T_INSTANCEOF _ -> "T_INSTANCEOF"
  | T_RETURN _ -> "T_RETURN"
  | T_SWITCH _ -> "T_SWITCH"
  | T_THIS _ -> "T_THIS"
  | T_THROW _ -> "T_THROW"
  | T_TRY _ -> "T_TRY"
  | T_VAR _ -> "T_VAR"
  | T_WHILE _ -> "T_WHILE"
  | T_WITH _ -> "T_WITH"
  | T_NULL _ -> "T_NULL"
  | T_FALSE _ -> "T_FALSE"
  | T_TRUE _ -> "T_TRUE"
  | T_BREAK _ -> "T_BREAK"
  | T_CASE _ -> "T_CASE"
  | T_CATCH _ -> "T_CATCH"
  | T_CONTINUE _ -> "T_CONTINUE"
  | T_DEFAULT _ -> "T_DEFAULT"
  | T_DO _ -> "T_DO"
  | T_FINALLY _ -> "T_FINALLY"
  | T_FOR _ -> "T_FOR"
  | T_ELSE _ -> "T_ELSE"
  | T_NEW _ -> "T_NEW"
  | T_LCURLY _ -> "T_LCURLY"
  | T_RCURLY _ -> "T_RCURLY"
  | T_LPAREN _ -> "T_LPAREN"
  | T_RPAREN _ -> "T_RPAREN"
  | T_LBRACKET _ -> "T_LBRACKET"
  | T_RBRACKET _ -> "T_RBRACKET"
  | T_SEMICOLON _ -> "T_SEMICOLON"
  | T_COMMA _ -> "T_COMMA"
  | T_PERIOD _ -> "T_PERIOD"
  | T_RSHIFT3_ASSIGN _ -> "T_RSHIFT3"
  | T_RSHIFT_ASSIGN _ -> "T_RSHIFT"
  | T_LSHIFT_ASSIGN _ -> "T_LSHIFT"
  | T_BIT_XOR_ASSIGN _ -> "T_BIT"
  | T_BIT_OR_ASSIGN _ -> "T_BIT"
  | T_BIT_AND_ASSIGN _ -> "T_BIT"
  | T_MOD_ASSIGN _ -> "T_MOD"
  | T_DIV_ASSIGN _ -> "T_DIV"
  | T_MULT_ASSIGN _ -> "T_MULT"
  | T_MINUS_ASSIGN _ -> "T_MINUS"
  | T_PLUS_ASSIGN _ -> "T_PLUS"
  | T_ASSIGN _ -> "T_ASSIGN"
  | T_PLING _ -> "T_PLING"
  | T_COLON _ -> "T_COLON"
  | T_OR _ -> "T_OR"
  | T_AND _ -> "T_AND"
  | T_BIT_OR _ -> "T_BIT"
  | T_BIT_XOR _ -> "T_BIT"
  | T_BIT_AND _ -> "T_BIT"
  | T_EQUAL _ -> "T_EQUAL"
  | T_NOT_EQUAL _ -> "T_NOT"
  | T_STRICT_EQUAL _ -> "T_STRICT"
  | T_STRICT_NOT_EQUAL _ -> "T_STRICT"
  | T_LESS_THAN_EQUAL _ -> "T_LESS"
  | T_GREATER_THAN_EQUAL _ -> "T_GREATER"
  | T_LESS_THAN _ -> "T_LESS"
  | T_GREATER_THAN _ -> "T_GREATER"
  | T_LSHIFT _ -> "T_LSHIFT"
  | T_RSHIFT _ -> "T_RSHIFT"
  | T_RSHIFT3 _ -> "T_RSHIFT3"
  | T_PLUS _ -> "T_PLUS"
  | T_MINUS _ -> "T_MINUS"
  | T_DIV _ -> "T_DIV"
  | T_MULT _ -> "T_MULT"
  | T_MOD _ -> "T_MOD"
  | T_NOT _ -> "T_NOT"
  | T_BIT_NOT _ -> "T_BIT"
  | T_INCR _ -> "T_INCR"
  | T_DECR _ -> "T_DECR"
  | T_INCR_NB _ -> "T_INCR"
  | T_DECR_NB _ -> "T_DECR"
  | T_DELETE _ -> "T_DELETE"
  | T_TYPEOF _ -> "T_TYPEOF"
  | T_VOID _ -> "T_VOID"
  | T_VIRTUAL_SEMICOLON _ -> "T_VIRTUAL"

let is_comment = function
  | TCommentSpace _
  | TCommentNewline _
  | TComment _
  | TCommentML _ -> true
  | _ -> false
