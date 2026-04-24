datatype token =
  NUMBER of int
| IDENTIFIER of string
| ASSIGN
| COLON
| SEMICOLON
| RANGE
| LPAREN
| RPAREN
| LBRACKET
| RBRACKET
| EQUALS
| NEQUALS
| GEQUALS
| LEQUALS
| GREATER
| LESS
| PLUS
| MINUS
| TIMES
| DIVIDE
| KW_BEGIN
| KW_CALL
| KW_CONST
| KW_DO
| KW_ELSE
| KW_END
| KW_IF
| KW_PROCEDURE
| KW_READ
| KW_THEN
| KW_TYPE
| KW_VAR
| KW_WHILE
| KW_WRITE;

signature STRING_MAP =
sig
  type 'a map
  val empty: 'a map
  val insert: 'a map * string * 'a -> 'a map
  val find: 'a map * string -> 'a option
  val exists: 'a map * string -> bool
end

structure StringMap: STRING_MAP =
struct
  type 'a map = (string * 'a) list

  val empty: 'a map = []

  fun insert (m, k, v) =
    (k, v) :: List.filter (fn (k', _) => k <> k') m

  fun find (m, k) =
    case List.find (fn (k', _) => k = k') m of
      NONE => NONE
    | SOME (_, v) => SOME v

  fun exists (m, k) =
    Option.isSome (find (m, k))

end

val kw_map = StringMap.empty
val kw_map = StringMap.insert (kw_map, "KW_BEGIN", KW_BEGIN)


val kw_map =
  foldl (fn ((k, v), m) => StringMap.insert (m, k, v)) StringMap.empty
    [ ("begin", KW_BEGIN)
    , ("call", KW_CALL)
    , ("const", KW_CONST)
    , ("do", KW_DO)
    , ("else", KW_ELSE)
    , ("end", KW_END)
    , ("if", KW_IF)
    , ("procedure", KW_PROCEDURE)
    , ("read", KW_READ)
    , ("then", KW_THEN)
    , ("type", KW_TYPE)
    , ("var", KW_VAR)
    , ("while", KW_WHILE)
    , ("write", KW_WRITE)
    ]

signature LEXER =
sig
  val lex: char list -> token list
  val lex_token: char list -> (token * char list)
  val lex_ident: char list -> (token * char list)
  val lex_num: char list -> (token * char list)
  val lex_other: char list -> (token * char list)
  val span: ('a -> bool) -> 'a list -> 'a list * 'a list

end

structure Lexer: LEXER =
struct


  fun span pred [] = ([], [])
    | span pred (x :: xs) =
        if pred x then let val (l, r) = span pred xs in ((x :: l), r) end
        else ([], x :: xs)

  fun lex_ident xs =
    let
      val (word, r) = span Char.isAlphaNum xs
      val s_word = String.implode word
    in
      case StringMap.find (kw_map, s_word) of
        SOME token => (token, r)
      | NONE => (IDENTIFIER s_word, r)
    end

  fun lex_num xs =
    let
      val (num, r) = span Char.isDigit xs
      val s = String.implode num
      val i_num = valOf (Int.fromString s)
    in
      (NUMBER i_num, r)
    end

  fun lex_other (#";" :: cs) = (SEMICOLON, cs)
    | lex_other (#"(" :: cs) = (LPAREN, cs)
    | lex_other (#")" :: cs) = (RPAREN, cs)
    | lex_other (#"[" :: cs) = (LBRACKET, cs)
    | lex_other (#"]" :: cs) = (RBRACKET, cs)
    | lex_other (#"+" :: cs) = (PLUS, cs)
    | lex_other (#"-" :: cs) = (MINUS, cs)
    | lex_other (#"*" :: cs) = (TIMES, cs)
    | lex_other (#"/" :: cs) = (DIVIDE, cs)
    | lex_other (#"=" :: cs) = (EQUALS, cs)
    | lex_other (#":" :: #"=" :: cs) = (ASSIGN, cs)
    | lex_other (#":" :: cs) = (COLON, cs)
    | lex_other (#"." :: #"." :: cs) = (RANGE, cs)
    | lex_other (#"!" :: #"=" :: cs) = (NEQUALS, cs)
    | lex_other (#">" :: #"=" :: cs) = (GEQUALS, cs)
    | lex_other (#">" :: cs) = (GREATER, cs)
    | lex_other (#"<" :: #"=" :: cs) = (LEQUALS, cs)
    | lex_other (#"<" :: cs) = (LESS, cs)
    | lex_other (c :: _) =
        raise Fail ("unexpected character: " ^ str c ^ " [lex_other]")
    | lex_other [] = raise Fail "unexpected EOF [lex_other]"


  fun lex_token (x :: xs) =
        if Char.isAlpha x then lex_ident (x :: xs)
        else if Char.isDigit x then lex_num (x :: xs)
        else lex_other (x :: xs)
    | lex_token [] = raise Fail "unexpected EOF [lex_token]"


  fun lex [] = []
    | lex (x :: xs) =
        if Char.isSpace x then
          lex xs
        else
          let val (token, rest) = lex_token (x :: xs)
          in token :: lex rest
          end

end
