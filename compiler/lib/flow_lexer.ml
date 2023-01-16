(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Js_token

module Loc = struct
  (* line numbers are 1-indexed; column numbers are 0-indexed *)
  type position =
    { line : int
    ; column : int
    }

  (* start is inclusive; end is exclusive *)
  (* If you are modifying this record, go look at ALoc.ml and make sure you understand the
   * representation there. *)
  type t =
    { source : string option
    ; start : position
    ; _end : position
    }
  [@@ocaml.warning "-69"]
end

module Parse_error = struct
  type t =
    | AccessorDataProperty
    | AccessorGetSet
    | AdjacentJSXElements
    | AmbiguousDeclareModuleKind
    | AmbiguousLetBracket
    | AsyncFunctionAsStatement
    | AwaitAsIdentifierReference
    | ComputedShorthandProperty
    | DeclareAsync
    | DeclareClassElement
    | DeclareClassFieldInitializer
    | DeclareExportConst
    | DeclareExportInterface
    | DeclareExportLet
    | DeclareExportType
    | DeclareOpaqueTypeInitializer
    | DuplicateConstructor
    | DuplicateDeclareModuleExports
    | DuplicateExport of string
    | DuplicatePrivateFields of string
    | ElementAfterRestElement
    | EnumBigIntMemberNotInitialized of
        { enum_name : string
        ; member_name : string
        }
    | EnumBooleanMemberNotInitialized of
        { enum_name : string
        ; member_name : string
        }
    | EnumDuplicateMemberName of
        { enum_name : string
        ; member_name : string
        }
    | EnumInconsistentMemberValues of { enum_name : string }
    | EnumInvalidEllipsis of { trailing_comma : bool }
    | EnumInvalidExplicitType of
        { enum_name : string
        ; supplied_type : string option
        }
    | EnumInvalidExport
    | EnumInvalidInitializerSeparator of { member_name : string }
    | EnumInvalidMemberName of
        { enum_name : string
        ; member_name : string
        }
    | EnumInvalidMemberSeparator
    | EnumNumberMemberNotInitialized of
        { enum_name : string
        ; member_name : string
        }
    | EnumStringMemberInconsistentlyInitailized of { enum_name : string }
    | ExpectedJSXClosingTag of string
    | ExpectedPatternFoundExpression
    | ExportSpecifierMissingComma
    | FunctionAsStatement of { in_strict_mode : bool }
    | GeneratorFunctionAsStatement
    | GetterArity
    | GetterMayNotHaveThisParam
    | IllegalBreak
    | IllegalContinue
    | IllegalReturn
    | IllegalUnicodeEscape
    | ImportSpecifierMissingComma
    | ImportTypeShorthandOnlyInPureImport
    | InexactInsideExact
    | InexactInsideNonObject
    | InvalidClassMemberName of
        { name : string
        ; static : bool
        ; method_ : bool
        ; private_ : bool
        }
    | InvalidFloatBigInt
    | InvalidIndexedAccess of { has_bracket : bool }
    | InvalidJSXAttributeValue
    | InvalidLHSInAssignment
    | InvalidLHSInExponentiation
    | InvalidLHSInForIn
    | InvalidLHSInForOf
    | InvalidNonTypeImportInDeclareModule
    | InvalidOptionalIndexedAccess
    | InvalidRegExp
    | InvalidRegExpFlags of string
    | InvalidSciBigInt
    | InvalidTupleOptionalSpread
    | InvalidTupleVariance
    | InvalidTypeof
    | JSXAttributeValueEmptyExpression
    | LiteralShorthandProperty
    | MalformedUnicode
    | MethodInDestructuring
    | MissingTypeParam
    | MissingTypeParamDefault
    | MultipleDefaultsInSwitch
    | NewlineAfterThrow
    | NewlineBeforeArrow
    | NoCatchOrFinally
    | NoUninitializedConst
    | NoUninitializedDestructuring
    | NullishCoalescingUnexpectedLogical of string
    | OptionalChainNew
    | OptionalChainTemplate
    | ParameterAfterRestParameter
    | PrivateDelete
    | PrivateNotInClass
    | PropertyAfterRestElement
    | Redeclaration of string * string
    | SetterArity
    | SetterMayNotHaveThisParam
    | StrictCatchVariable
    | StrictDelete
    | StrictDuplicateProperty
    | StrictFunctionName
    | StrictLHSAssignment
    | StrictLHSPostfix
    | StrictLHSPrefix
    | StrictModeWith
    | StrictNonOctalLiteral
    | StrictOctalLiteral
    | StrictParamDupe
    | StrictParamName
    | StrictParamNotSimple
    | StrictReservedWord
    | StrictVarName
    | SuperPrivate
    | ThisParamAnnotationRequired
    | ThisParamBannedInArrowFunctions
    | ThisParamBannedInConstructor
    | ThisParamMayNotBeOptional
    | ThisParamMustBeFirst
    | TrailingCommaAfterRestElement
    | UnboundPrivate of string
    | Unexpected of string
    | UnexpectedEOS
    | UnexpectedExplicitInexactInObject
    | UnexpectedOpaqueTypeAlias
    | UnexpectedProto
    | UnexpectedReserved
    | UnexpectedReservedType
    | UnexpectedSpreadType
    | UnexpectedStatic
    | UnexpectedSuper
    | UnexpectedSuperCall
    | UnexpectedTokenWithSuggestion of string * string
    | UnexpectedTypeAlias
    | UnexpectedTypeAnnotation
    | UnexpectedTypeDeclaration
    | UnexpectedTypeExport
    | UnexpectedTypeImport
    | UnexpectedTypeInterface
    | UnexpectedVariance
    | UnexpectedWithExpected of string * string
    | UnknownLabel of string
    | UnsupportedDecorator
    | UnterminatedRegExp
    | WhitespaceInPrivateName
    | YieldAsIdentifierReference
    | YieldInFormalParameters
end

module Lex_env = struct
  (* bol = Beginning Of Line *)
  type bol =
    { line : int
    ; offset : int
    }

  type lex_state = { lex_errors_acc : (Loc.t * Parse_error.t) list } [@@ocaml.unboxed]

  type t =
    { lex_source : string option
    ; lex_lb : Sedlexing.lexbuf
    ; lex_bol : bol
    ; lex_state : lex_state
    ; lex_last_loc : Loc.t
    }
  [@@ocaml.warning "-69"]

  let line env = env.lex_bol.line

  let source env = env.lex_source

  let bol_offset env = env.lex_bol.offset

  let empty_lex_state = { lex_errors_acc = [] }

  (* The lex_last_loc should initially be set to the beginning of the first line, so that
     comments on the first line are reported as not being on a new line. *)
  let initial_last_loc =
    { Loc.source = None
    ; start = { Loc.line = 1; column = 0 }
    ; _end = { Loc.line = 1; column = 0 }
    }

  let create lex_lb =
    let s, _ = Sedlexing.lexing_positions lex_lb in
    let lex_source =
      match s.pos_fname with
      | "" -> None
      | s -> Some s
    in
    { lex_source
    ; lex_lb
    ; lex_bol = { line = 1; offset = 0 }
    ; lex_state = empty_lex_state
    ; lex_last_loc = initial_last_loc
    }
end

module Lex_result = struct
  type t =
    { lex_token : Js_token.t
    ; lex_loc : Lexing.position * Lexing.position
    ; lex_errors : (Loc.t * Parse_error.t) list
    ; lex_comments : string list
    }
  [@@ocaml.warning "-69"]

  let token result = result.lex_token

  let loc result = result.lex_loc

  let _comments result = result.lex_comments

  let _errors result = result.lex_errors
end

let lexeme = Sedlexing.Utf8.lexeme

let lexeme_to_buffer lexbuf b = Buffer.add_string b (Sedlexing.Utf8.lexeme lexbuf)

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '$']

let id_letter = [%sedlex.regexp? letter | '_']

let digit = [%sedlex.regexp? '0' .. '9']

let digit_non_zero = [%sedlex.regexp? '1' .. '9']

let decintlit = [%sedlex.regexp? '0' | '1' .. '9', Star digit]

(* DecimalIntegerLiteral *)

let alphanumeric = [%sedlex.regexp? digit | letter]

let word = [%sedlex.regexp? letter, Star alphanumeric]

let hex_digit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let non_hex_letter = [%sedlex.regexp? 'g' .. 'z' | 'G' .. 'Z' | '$']

let bin_digit = [%sedlex.regexp? '0' | '1']

let oct_digit = [%sedlex.regexp? '0' .. '7']

(* This regex could be simplified to (digit Star (digit OR '_' digit))
 * That makes the underscore and failure cases faster, and the base case take x2-3 the steps
 * As the codebase contains more base cases than underscored or errors, prefer this version *)
let underscored_bin =
  [%sedlex.regexp? Plus bin_digit | bin_digit, Star (bin_digit | '_', bin_digit)]

let underscored_oct =
  [%sedlex.regexp? Plus oct_digit | oct_digit, Star (oct_digit | '_', oct_digit)]

let underscored_hex =
  [%sedlex.regexp? Plus hex_digit | hex_digit, Star (hex_digit | '_', hex_digit)]

let underscored_digit =
  [%sedlex.regexp? Plus digit | digit_non_zero, Star (digit | '_', digit)]

let underscored_decimal = [%sedlex.regexp? Plus digit | digit, Star (digit | '_', digit)]

(* Different ways you can write a number *)
let binnumber = [%sedlex.regexp? '0', ('B' | 'b'), underscored_bin]

let octnumber = [%sedlex.regexp? '0', ('O' | 'o'), underscored_oct]

let legacyoctnumber = [%sedlex.regexp? '0', Plus oct_digit]

(* no underscores allowed *)

let legacynonoctnumber = [%sedlex.regexp? '0', Star oct_digit, '8' .. '9', Star digit]

let hexnumber = [%sedlex.regexp? '0', ('X' | 'x'), underscored_hex]

let scinumber =
  [%sedlex.regexp?
    ( (decintlit, Opt ('.', Opt underscored_decimal) | '.', underscored_decimal)
    , ('e' | 'E')
    , Opt ('-' | '+')
    , underscored_digit )]

let wholenumber = [%sedlex.regexp? underscored_digit, Opt '.']

let floatnumber = [%sedlex.regexp? Opt underscored_digit, '.', underscored_decimal]

let binbigint = [%sedlex.regexp? binnumber, 'n']

let octbigint = [%sedlex.regexp? octnumber, 'n']

let hexbigint = [%sedlex.regexp? hexnumber, 'n']

let scibigint = [%sedlex.regexp? scinumber, 'n']

let wholebigint = [%sedlex.regexp? underscored_digit, 'n']

let floatbigint = [%sedlex.regexp? (floatnumber | underscored_digit, '.'), 'n']

(* 2-8 alphanumeric characters. I could match them directly, but this leads to
 * ~5k more lines of generated lexer
   let htmlentity = "quot" | "amp" | "apos" | "lt" | "gt" | "nbsp" | "iexcl"
     | "cent" | "pound" | "curren" | "yen" | "brvbar" | "sect" | "uml" | "copy"
     | "ordf" | "laquo" | "not" | "shy" | "reg" | "macr" | "deg" | "plusmn"
     | "sup2" | "sup3" | "acute" | "micro" | "para" | "middot" | "cedil" | "sup1"
     | "ordm" | "raquo" | "frac14" | "frac12" | "frac34" | "iquest" | "Agrave"
     | "Aacute" | "Acirc" | "Atilde" | "Auml" | "Aring" | "AElig" | "Ccedil"
     | "Egrave" | "Eacute" | "Ecirc" | "Euml" | "Igrave" | "Iacute" | "Icirc"
     | "Iuml" | "ETH" | "Ntilde" | "Ograve" | "Oacute" | "Ocirc" | "Otilde"
     | "Ouml" | "times" | "Oslash" | "Ugrave" | "Uacute" | "Ucirc" | "Uuml"
     | "Yacute" | "THORN" | "szlig" | "agrave" | "aacute" | "acirc" | "atilde"
     | "auml" | "aring" | "aelig" | "ccedil" | "egrave" | "eacute" | "ecirc"
     | "euml" | "igrave" | "iacute" | "icirc" | "iuml" | "eth" | "ntilde"
     | "ograve" | "oacute" | "ocirc" | "otilde" | "ouml" | "divide" | "oslash"
     | "ugrave" | "uacute" | "ucirc" | "uuml" | "yacute" | "thorn" | "yuml"
     | "OElig" | "oelig" | "Scaron" | "scaron" | "Yuml" | "fnof" | "circ" | "tilde"
     | "Alpha" | "Beta" | "Gamma" | "Delta" | "Epsilon" | "Zeta" | "Eta" | "Theta"
     | "Iota" | "Kappa" | "Lambda" | "Mu" | "Nu" | "Xi" | "Omicron" | "Pi" | "Rho"
     | "Sigma" | "Tau" | "Upsilon" | "Phi" | "Chi" | "Psi" | "Omega" | "alpha"
     | "beta" | "gamma" | "delta" | "epsilon" | "zeta" | "eta" | "theta" | "iota"
     | "kappa" | "lambda" | "mu" | "nu" | "xi" | "omicron" | "pi" | "rho"
     | "sigmaf" | "sigma" | "tau" | "upsilon" | "phi" | "chi" | "psi" | "omega"
     | "thetasym" | "upsih" | "piv" | "ensp" | "emsp" | "thinsp" | "zwnj" | "zwj"
     | "lrm" | "rlm" | "ndash" | "mdash" | "lsquo" | "rsquo" | "sbquo" | "ldquo"
     | "rdquo" | "bdquo" | "dagger" | "Dagger" | "bull" | "hellip" | "permil"
     | "prime" | "Prime" | "lsaquo" | "rsaquo" | "oline" | "frasl" | "euro"
     | "image" | "weierp" | "real" | "trade" | "alefsym" | "larr" | "uarr" | "rarr"
     | "darr" | "harr" | "crarr" | "lArr" | "uArr" | "rArr" | "dArr" | "hArr"
     | "forall" | "part" | "exist" | "empty" | "nabla" | "isin" | "notin" | "ni"
     | "prod" | "sum" | "minus" | "lowast" | "radic" | "prop" | "infin" | "ang"
     | "and" | "or" | "cap" | "cup" | "'int'" | "there4" | "sim" | "cong" | "asymp"
     | "ne" | "equiv" | "le" | "ge" | "sub" | "sup" | "nsub" | "sube" | "supe"
     | "oplus" | "otimes" | "perp" | "sdot" | "lceil" | "rceil" | "lfloor"
     | "rfloor" | "lang" | "rang" | "loz" | "spades" | "clubs" | "hearts" | "diams"
 *)
let htmlentity =
  [%sedlex.regexp?
    ( alphanumeric
    , alphanumeric
    , Opt alphanumeric
    , Opt alphanumeric
    , Opt alphanumeric
    , Opt alphanumeric
    , Opt alphanumeric
    , Opt alphanumeric )]

(* https://tc39.github.io/ecma262/#sec-white-space *)
let whitespace =
  [%sedlex.regexp?
    ( 0x0009 | 0x000B | 0x000C | 0x0020 | 0x00A0 | 0xfeff | 0x1680
    | 0x2000 .. 0x200a
    | 0x202f | 0x205f | 0x3000 )]

(* minus sign in front of negative numbers
   (only for types! regular numbers use T_MINUS!) *)
let neg = [%sedlex.regexp? '-', Star whitespace]

let line_terminator_sequence = [%sedlex.regexp? '\n' | '\r' | "\r\n" | 0x2028 | 0x2029]

let line_terminator_sequence_start = [%sedlex.regexp? '\n' | '\r' | 0x2028 | 0x2029]

let hex_quad = [%sedlex.regexp? hex_digit, hex_digit, hex_digit, hex_digit]

let unicode_escape = [%sedlex.regexp? "\\u", hex_quad]

let codepoint_escape = [%sedlex.regexp? "\\u{", Plus hex_digit, '}']

let js_id_start =
  [%sedlex.regexp? '$' | '_' | id_start | unicode_escape | codepoint_escape]

let js_id_continue =
  [%sedlex.regexp? '$' | '_' | id_continue | unicode_escape | codepoint_escape]

let pos_at_offset env offset =
  { Loc.line = Lex_env.line env; column = offset - Lex_env.bol_offset env }

let loc_of_offsets env start_offset end_offset =
  { Loc.source = Lex_env.source env
  ; start = pos_at_offset env start_offset
  ; _end = pos_at_offset env end_offset
  }

let start_pos_of_lexbuf env (lexbuf : Sedlexing.lexbuf) =
  let start_offset = Sedlexing.lexeme_start lexbuf in
  pos_at_offset env start_offset

let end_pos_of_lexbuf env (lexbuf : Sedlexing.lexbuf) =
  let end_offset = Sedlexing.lexeme_end lexbuf in
  pos_at_offset env end_offset

let loc_of_lexbuf env (lexbuf : Sedlexing.lexbuf) =
  let start_offset = Sedlexing.lexeme_start lexbuf in
  let end_offset = Sedlexing.lexeme_end lexbuf in
  loc_of_offsets env start_offset end_offset

let loc_of_token env lex_token =
  match lex_token with
  (* | T_IDENTIFIER { loc; _ } | T_STRING (loc, _, _, _) -> loc
     | T_TEMPLATE_PART (loc, _, _) -> loc
       | T_REGEXP (loc, _, _) -> loc *)
  | _ -> loc_of_lexbuf env env.lex_lb

let lex_error (env : Lex_env.t) loc err : Lex_env.t =
  let lex_errors_acc = (loc, err) :: env.lex_state.lex_errors_acc in
  { env with lex_state = { lex_errors_acc } }

let illegal (env : Lex_env.t) (loc : Loc.t) =
  lex_error env loc (Parse_error.Unexpected "token ILLEGAL")

let new_line env lexbuf =
  let offset = Sedlexing.lexeme_end lexbuf in
  let lex_bol = { Lex_env.line = Lex_env.line env + 1; offset } in
  { env with Lex_env.lex_bol }

let recover env lexbuf ~f =
  let env = illegal env (loc_of_lexbuf env lexbuf) in
  Sedlexing.rollback lexbuf;
  f env lexbuf

type result =
  | Token of Lex_env.t * Js_token.t
  | Comment of Lex_env.t * string
  | Continue of Lex_env.t

let rec comment env buf lexbuf =
  match%sedlex lexbuf with
  | line_terminator_sequence ->
      let env = new_line env lexbuf in
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf
  | "*/" ->
      lexeme_to_buffer lexbuf buf;
      env, end_pos_of_lexbuf env lexbuf
  | "*-/" ->
      Buffer.add_string buf "*-/";
      comment env buf lexbuf
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (line_terminator_sequence_start | '*')) | any ->
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf
  | _ ->
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      env, end_pos_of_lexbuf env lexbuf

let rec line_comment env buf lexbuf =
  match%sedlex lexbuf with
  | eof -> env, end_pos_of_lexbuf env lexbuf
  | line_terminator_sequence ->
      let { Loc.line; column } = end_pos_of_lexbuf env lexbuf in
      let env = new_line env lexbuf in
      let len = Sedlexing.lexeme_length lexbuf in
      let end_pos = { Loc.line; column = column - len } in
      env, end_pos
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | line_terminator_sequence_start)) | any ->
      lexeme_to_buffer lexbuf buf;
      line_comment env buf lexbuf
  | _ -> failwith "unreachable line_comment"

let string_escape env lexbuf =
  match%sedlex lexbuf with
  | eof | '\\' ->
      let str = lexeme lexbuf in
      env, str
  | 'x', hex_digit, hex_digit ->
      let str = lexeme lexbuf in
      (* 0xAB *)
      env, str
  | '0' .. '7', '0' .. '7', '0' .. '7' ->
      let str = lexeme lexbuf in
      env, str
  | '0' .. '7', '0' .. '7' ->
      let str = lexeme lexbuf in
      (* 0o01 *)
      env, str
  | '0' -> env, "0"
  | 'b' -> env, "b"
  | 'f' -> env, "f"
  | 'n' -> env, "n"
  | 'r' -> env, "r"
  | 't' -> env, "t"
  | 'v' -> env, "v"
  | '0' .. '7' ->
      let str = lexeme lexbuf in
      (* 0o1 *)
      env, str
  | 'u', hex_quad ->
      let str = lexeme lexbuf in
      env, str
  | "u{", Plus hex_digit, '}' ->
      let str = lexeme lexbuf in
      let hex = String.sub str 2 (String.length str - 3) in
      let code = int_of_string ("0x" ^ hex) in
      (* 11.8.4.1 *)
      let env = if code > 0x10FFFF then illegal env (loc_of_lexbuf env lexbuf) else env in
      env, str
  | 'u' | 'x' | '0' .. '7' ->
      let str = lexeme lexbuf in
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      env, str
  | line_terminator_sequence ->
      let str = lexeme lexbuf in
      let env = new_line env lexbuf in
      env, str
  | any ->
      let str = lexeme lexbuf in
      env, str
  | _ -> failwith "unreachable string_escape"

(* Really simple version of string lexing. Just try to find beginning and end of
 * string. We can inspect the string later to find invalid escapes, etc *)
let rec string_quote env q buf lexbuf =
  match%sedlex lexbuf with
  | "'" | '"' ->
      let q' = lexeme lexbuf in
      if q = q'
      then env, end_pos_of_lexbuf env lexbuf
      else (
        Buffer.add_string buf q';
        string_quote env q buf lexbuf)
  | '\\', line_terminator_sequence ->
      let env = new_line env lexbuf in
      string_quote env q buf lexbuf
  | '\\' ->
      Buffer.add_string buf "\\";
      let env, str = string_escape env lexbuf in
      Buffer.add_string buf str;
      string_quote env q buf lexbuf
  | '\n' ->
      Format.eprintf "LEXER: WEIRD newline in quoted string@.";
      let x = lexeme lexbuf in
      Buffer.add_string buf x;
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      let env = new_line env lexbuf in
      string_quote env q buf lexbuf
  (* env, end_pos_of_lexbuf env lexbuf *)
  | eof ->
      let x = lexeme lexbuf in
      Buffer.add_string buf x;
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      env, end_pos_of_lexbuf env lexbuf
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl ("'" | '"' | '\\' | '\n' | eof)) | any ->
      lexeme_to_buffer lexbuf buf;
      string_quote env q buf lexbuf
  | _ -> failwith "unreachable string_quote"

let rec template_part env cooked raw literal lexbuf =
  match%sedlex lexbuf with
  | eof ->
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      env, true
  | '`' ->
      Buffer.add_char literal '`';
      env, true
  | "${" ->
      Buffer.add_string literal "${";
      env, false
  | '\\' ->
      Buffer.add_char raw '\\';
      Buffer.add_char literal '\\';
      let env, str = string_escape env lexbuf in
      Buffer.add_string raw str;
      Buffer.add_string literal str;
      template_part env cooked raw literal lexbuf
  (* ECMAScript 6th Syntax, 11.8.6.1 Static Semantics: TV's and TRV's
   * Long story short, <LF> is 0xA, <CR> is 0xA, and <CR><LF> is 0xA
   * *)
  | "\r\n" ->
      Buffer.add_string raw "\r\n";
      Buffer.add_string literal "\r\n";
      Buffer.add_string cooked "\n";
      let env = new_line env lexbuf in
      template_part env cooked raw literal lexbuf
  | "\n" | "\r" ->
      let lf = lexeme lexbuf in
      Buffer.add_string raw lf;
      Buffer.add_string literal lf;
      Buffer.add_char cooked '\n';
      let env = new_line env lexbuf in
      template_part env cooked raw literal lexbuf
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '`' | '$' | '\\' | '\r' | '\n')) | any ->
      let c = lexeme lexbuf in
      Buffer.add_string raw c;
      Buffer.add_string literal c;
      Buffer.add_string cooked c;
      template_part env cooked raw literal lexbuf
  | _ -> failwith "unreachable template_part"

let token (env : Lex_env.t) lexbuf : result =
  match%sedlex lexbuf with
  | line_terminator_sequence ->
      let env = new_line env lexbuf in
      Continue env
  | Plus whitespace -> Continue env
  | "/*" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env, _end_pos = comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  | "//" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env, _end_pos = line_comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  (* Support for the shebang at the beginning of a file. It is treated like a
   * comment at the beginning or an error elsewhere *)
  | "#!" ->
      if Sedlexing.lexeme_start lexbuf = 0
      then
        let env, _ = line_comment env (Buffer.create 127) lexbuf in
        Continue env
      else Token (env, T_ERROR "#!")
  (* Values *)
  | "'" | '"' ->
      let quote = lexeme lexbuf in
      let p1 = Sedlexing.lexeme_start lexbuf in
      let start = start_pos_of_lexbuf env lexbuf in
      let buf = Buffer.create 127 in
      let env, _end = string_quote env quote buf lexbuf in
      let p2 = Sedlexing.lexeme_end lexbuf in
      let _loc = { Loc.source = Lex_env.source env; start; _end } in
      Token
        ( env
        , T_STRING (Stdlib.Utf8_string.of_string_exn (Buffer.contents buf), p2 - p1 - 1)
        )
  | '`' ->
      let cooked = Buffer.create 127 in
      let raw = Buffer.create 127 in
      let literal = Buffer.create 127 in
      lexeme_to_buffer lexbuf literal;

      let start = start_pos_of_lexbuf env lexbuf in
      let env, is_tail = template_part env cooked raw literal lexbuf in
      let _end = end_pos_of_lexbuf env lexbuf in
      let _loc = { Loc.source = Lex_env.source env; start; _end } in
      Token
        ( env
        , T_TEMPLATE_PART (Stdlib.Utf8_string.of_string_exn (Buffer.contents raw), is_tail)
        )
  | binbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | binbigint -> Token (env, T_BIGINT (BIG_BINARY, lexeme lexbuf))
          | _ -> failwith "unreachable token bigint")
  | binbigint -> Token (env, T_BIGINT (BIG_BINARY, lexeme lexbuf))
  | binnumber, (letter | '2' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | binnumber -> Token (env, T_NUMBER (BINARY, lexeme lexbuf))
          | _ -> failwith "unreachable token bignumber")
  | binnumber -> Token (env, T_NUMBER (BINARY, lexeme lexbuf))
  | octbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | octbigint -> Token (env, T_BIGINT (BIG_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token octbigint")
  | octbigint -> Token (env, T_BIGINT (BIG_OCTAL, lexeme lexbuf))
  | octnumber, (letter | '8' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | octnumber -> Token (env, T_NUMBER (OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token octnumber")
  | octnumber -> Token (env, T_NUMBER (OCTAL, lexeme lexbuf))
  | legacynonoctnumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | legacynonoctnumber -> Token (env, T_NUMBER (LEGACY_NON_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token legacynonoctnumber")
  | legacynonoctnumber -> Token (env, T_NUMBER (LEGACY_NON_OCTAL, lexeme lexbuf))
  | legacyoctnumber, (letter | '8' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | legacyoctnumber -> Token (env, T_NUMBER (LEGACY_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token legacyoctnumber")
  | legacyoctnumber -> Token (env, T_NUMBER (LEGACY_OCTAL, lexeme lexbuf))
  | hexbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | hexbigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token hexbigint")
  | hexbigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | hexnumber, non_hex_letter, Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | hexnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token hexnumber")
  | hexnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
  | scibigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | scibigint ->
              let loc = loc_of_lexbuf env lexbuf in
              let env = lex_error env loc Parse_error.InvalidSciBigInt in
              Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token scibigint")
  | scibigint ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.InvalidSciBigInt in
      Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | scinumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | scinumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token scinumber")
  | scinumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
  | floatbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | floatbigint ->
              let loc = loc_of_lexbuf env lexbuf in
              let env = lex_error env loc Parse_error.InvalidFloatBigInt in
              Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token floatbigint")
  | wholebigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | wholebigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholebigint")
  | floatbigint ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.InvalidFloatBigInt in
      Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | wholebigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | (wholenumber | floatnumber), word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | wholenumber | floatnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholenumber")
  | wholenumber | floatnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
  (* Syntax *)
  | "{" -> Token (env, T_LCURLY)
  | "}" -> Token (env, T_RCURLY)
  | "(" -> Token (env, T_LPAREN)
  | ")" -> Token (env, T_RPAREN)
  | "[" -> Token (env, T_LBRACKET)
  | "]" -> Token (env, T_RBRACKET)
  | "..." -> Token (env, T_ELLIPSIS)
  | "." -> Token (env, T_PERIOD)
  | ";" -> Token (env, T_SEMICOLON)
  | "," -> Token (env, T_COMMA)
  | ":" -> Token (env, T_COLON)
  | "?.", digit -> (
      Sedlexing.rollback lexbuf;
      match%sedlex lexbuf with
      | "?" -> Token (env, T_PLING)
      | _ -> failwith "expected ?")
  | "?." -> Token (env, T_PLING_PERIOD)
  | "??" -> Token (env, T_PLING_PLING)
  | "?" -> Token (env, T_PLING)
  | "&&" -> Token (env, T_AND)
  | "||" -> Token (env, T_OR)
  | "===" -> Token (env, T_STRICT_EQUAL)
  | "!==" -> Token (env, T_STRICT_NOT_EQUAL)
  | "<=" -> Token (env, T_LESS_THAN_EQUAL)
  | ">=" -> Token (env, T_GREATER_THAN_EQUAL)
  | "==" -> Token (env, T_EQUAL)
  | "!=" -> Token (env, T_NOT_EQUAL)
  | "++" -> Token (env, T_INCR)
  | "--" -> Token (env, T_DECR)
  | "<<=" -> Token (env, T_LSHIFT_ASSIGN)
  | "<<" -> Token (env, T_LSHIFT)
  | ">>=" -> Token (env, T_RSHIFT_ASSIGN)
  | ">>>=" -> Token (env, T_RSHIFT3_ASSIGN)
  | ">>>" -> Token (env, T_RSHIFT3)
  | ">>" -> Token (env, T_RSHIFT)
  | "+=" -> Token (env, T_PLUS_ASSIGN)
  | "-=" -> Token (env, T_MINUS_ASSIGN)
  | "*=" -> Token (env, T_MULT_ASSIGN)
  | "**=" -> Token (env, T_EXP_ASSIGN)
  | "%=" -> Token (env, T_MOD_ASSIGN)
  | "&=" -> Token (env, T_BIT_AND_ASSIGN)
  | "|=" -> Token (env, T_BIT_OR_ASSIGN)
  | "^=" -> Token (env, T_BIT_XOR_ASSIGN)
  | "??=" -> Token (env, T_NULLISH_ASSIGN)
  | "&&=" -> Token (env, T_AND_ASSIGN)
  | "||=" -> Token (env, T_OR_ASSIGN)
  | "<" -> Token (env, T_LESS_THAN)
  | ">" -> Token (env, T_GREATER_THAN)
  | "+" -> Token (env, T_PLUS)
  | "-" -> Token (env, T_MINUS)
  | "*" -> Token (env, T_MULT)
  | "**" -> Token (env, T_EXP)
  | "%" -> Token (env, T_MOD)
  | "|" -> Token (env, T_BIT_OR)
  | "&" -> Token (env, T_BIT_AND)
  | "^" -> Token (env, T_BIT_XOR)
  | "!" -> Token (env, T_NOT)
  | "~" -> Token (env, T_BIT_NOT)
  | "=" -> Token (env, T_ASSIGN)
  | "=>" -> Token (env, T_ARROW)
  | "/=" -> Token (env, T_DIV_ASSIGN)
  | "/" -> Token (env, T_DIV)
  | "@" -> Token (env, T_AT)
  | "#" -> Token (env, T_POUND)
  (* To reason about its correctness:
     1. all tokens are still matched
     2. tokens like opaque, opaquex are matched correctly
       the most fragile case is `opaquex` (matched with `opaque,x` instead)
     3. \a is disallowed
     4. a世界 recognized
  *)
  | js_id_start, Star js_id_continue -> (
      let raw = Sedlexing.Utf8.lexeme lexbuf in
      (*let nenv, value = decode_identifier env raw in *)
      match raw with
      | "async" -> Token (env, T_ASYNC)
      | "await" -> Token (env, T_AWAIT)
      | "break" -> Token (env, T_BREAK)
      | "case" -> Token (env, T_CASE)
      | "catch" -> Token (env, T_CATCH)
      | "class" -> Token (env, T_CLASS)
      | "const" -> Token (env, T_CONST)
      | "continue" -> Token (env, T_CONTINUE)
      | "debugger" -> Token (env, T_DEBUGGER)
      | "declare" -> Token (env, T_DECLARE)
      | "default" -> Token (env, T_DEFAULT)
      | "delete" -> Token (env, T_DELETE)
      | "do" -> Token (env, T_DO)
      | "else" -> Token (env, T_ELSE)
      | "enum" -> Token (env, T_ENUM)
      | "export" -> Token (env, T_EXPORT)
      | "extends" -> Token (env, T_EXTENDS)
      | "false" -> Token (env, T_FALSE)
      | "finally" -> Token (env, T_FINALLY)
      | "for" -> Token (env, T_FOR)
      | "function" -> Token (env, T_FUNCTION)
      | "if" -> Token (env, T_IF)
      | "implements" -> Token (env, T_IMPLEMENTS)
      | "import" -> Token (env, T_IMPORT)
      | "in" -> Token (env, T_IN)
      | "instanceof" -> Token (env, T_INSTANCEOF)
      | "interface" -> Token (env, T_INTERFACE)
      | "let" -> Token (env, T_LET)
      | "new" -> Token (env, T_NEW)
      | "null" -> Token (env, T_NULL)
      | "of" -> Token (env, T_OF)
      | "opaque" -> Token (env, T_OPAQUE)
      | "package" -> Token (env, T_PACKAGE)
      | "private" -> Token (env, T_PRIVATE)
      | "protected" -> Token (env, T_PROTECTED)
      | "public" -> Token (env, T_PUBLIC)
      | "return" -> Token (env, T_RETURN)
      | "static" -> Token (env, T_STATIC)
      | "super" -> Token (env, T_SUPER)
      | "switch" -> Token (env, T_SWITCH)
      | "this" -> Token (env, T_THIS)
      | "throw" -> Token (env, T_THROW)
      | "true" -> Token (env, T_TRUE)
      | "try" -> Token (env, T_TRY)
      | "type" -> Token (env, T_TYPE)
      | "typeof" -> Token (env, T_TYPEOF)
      | "var" -> Token (env, T_VAR)
      | "void" -> Token (env, T_VOID)
      | "while" -> Token (env, T_WHILE)
      | "with" -> Token (env, T_WITH)
      | "yield" -> Token (env, T_YIELD)
      | _ -> Token (env, T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn raw)))
  | eof -> Token (env, T_EOF)
  | any ->
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      Token (env, T_ERROR (lexeme lexbuf))
  | _ -> failwith "unreachable token"

let rec regexp_class env buf lexbuf =
  match%sedlex lexbuf with
  | eof -> env
  | "\\\\" ->
      Buffer.add_string buf "\\\\";
      regexp_class env buf lexbuf
  | '\\', ']' ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf ']';
      regexp_class env buf lexbuf
  | ']' ->
      Buffer.add_char buf ']';
      env
  | line_terminator_sequence ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      let env = new_line env lexbuf in
      env
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '\\' | ']' | line_terminator_sequence_start)) | any ->
      let str = lexeme lexbuf in
      Buffer.add_string buf str;
      regexp_class env buf lexbuf
  | _ -> failwith "unreachable regexp_class"

let rec regexp_body env buf lexbuf =
  match%sedlex lexbuf with
  | eof ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      env, ""
  | '\\', line_terminator_sequence ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      let env = new_line env lexbuf in
      env, ""
  | '\\', any ->
      let s = lexeme lexbuf in
      Buffer.add_string buf s;
      regexp_body env buf lexbuf
  | '/', Plus id_letter ->
      let flags =
        let str = lexeme lexbuf in
        String.sub str 1 (String.length str - 1)
      in
      env, flags
  | '/' -> env, ""
  | '[' ->
      Buffer.add_char buf '[';
      let env = regexp_class env buf lexbuf in
      regexp_body env buf lexbuf
  | line_terminator_sequence ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      let env = new_line env lexbuf in
      env, ""
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '\\' | '/' | '[' | line_terminator_sequence_start)) | any ->
      let str = lexeme lexbuf in
      Buffer.add_string buf str;
      regexp_body env buf lexbuf
  | _ -> failwith "unreachable regexp_body"

let regexp env lexbuf =
  match%sedlex lexbuf with
  | eof -> Token (env, T_EOF)
  | line_terminator_sequence ->
      let env = new_line env lexbuf in
      Continue env
  | Plus whitespace -> Continue env
  | "//" ->
      let _start_pos = start_pos_of_lexbuf env lexbuf in
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env, _end_pos = line_comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  | "/*" ->
      let _start_pos = start_pos_of_lexbuf env lexbuf in
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env, _end_pos = comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  | '/' ->
      let start = start_pos_of_lexbuf env lexbuf in
      let buf = Buffer.create 127 in
      let env, flags = regexp_body env buf lexbuf in
      let _end = end_pos_of_lexbuf env lexbuf in
      let _loc = { Loc.source = Lex_env.source env; start; _end } in
      Token (env, T_REGEXP (Stdlib.Utf8_string.of_string_exn (Buffer.contents buf), flags))
  | any ->
      let env = illegal env (loc_of_lexbuf env lexbuf) in
      Token (env, T_ERROR (lexeme lexbuf))
  | _ -> failwith "unreachable regexp"

let wrap f =
  let rec helper comments env =
    Sedlexing.start env.Lex_env.lex_lb;
    let start, _ = Sedlexing.lexing_positions env.Lex_env.lex_lb in
    match f env env.Lex_env.lex_lb with
    | Token (env, t) ->
        let _, stop = Sedlexing.lexing_positions env.Lex_env.lex_lb in
        let loc = loc_of_token env t in
        let lex_comments = if comments = [] then [] else List.rev comments in
        let lex_token = t in
        let lex_errors_acc = env.lex_state.lex_errors_acc in
        if lex_errors_acc = []
        then
          ( { env with lex_last_loc = loc }
          , { Lex_result.lex_token; lex_loc = start, stop; lex_comments; lex_errors = [] }
          )
        else
          ( { env with lex_last_loc = loc; lex_state = Lex_env.empty_lex_state }
          , { Lex_result.lex_token
            ; lex_loc = start, stop
            ; lex_comments
            ; lex_errors = List.rev lex_errors_acc
            } )
    | Comment (env, comment) ->
        let _, stop = Sedlexing.lexing_positions env.Lex_env.lex_lb in
        ( { env with lex_last_loc = env.lex_last_loc }
        , { Lex_result.lex_token = TComment comment
          ; lex_loc = start, stop
          ; lex_comments = []
          ; lex_errors = []
          } )
    | Continue env -> helper comments env
  in
  fun env -> helper [] env

let regexp = wrap regexp

let token = wrap token

let is_valid_identifier_name s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  match%sedlex lexbuf with
  | js_id_start, Star js_id_continue, eof -> true
  | _ -> false
