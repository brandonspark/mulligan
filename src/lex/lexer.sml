
structure Table = SymbolHashTable

signature LEXER =
  sig
    val lex : char Stream.stream -> DToken.t Stream.stream
    val lex_string : string -> DToken.t list
    val lex_file : string -> DToken.t list
  end

structure Lexer :> LEXER =
  struct
    open DToken
    open Error

    fun revappend l1 l2 =
      (case l1 of
        x :: rest =>
          revappend rest (x :: l2)
      | [] => l2)

    open Stream

    structure Arg =
      struct
        structure Streamable = StreamStreamable

        type symbol = char
        val ord = Char.ord

        datatype tlex = LEX of char stream -> t
        withtype t = tlex -> int -> DToken.t front

        type self = { main : symbol Streamable.t -> t }
        type info = { match : symbol list,
                      len : int,
                      start : symbol Streamable.t,
                      follow : symbol Streamable.t,
                      self : self }

        val keywords_list =
          [ ("step", STEP)
          , ("reveal", REVEAL)
          , ("stop", STOP)
          , ("set", SET)
          , ("prev", PREV)
          , ("break", BREAK)
          , ("run", RUN)
          , ("clear", CLEAR)
          , ("print", PRINT)
          , ("assign", ASSIGN)
          , ("report", REPORT)
          , ("last", LAST)
          , ("help", HELP)
          ]

        val keywords : DToken.t Table.table = Table.table 60

        val () =
          List.app
          (fn (str, token) => Table.insert keywords (Symbol.fromValue str) token)
          keywords_list

        fun identify table str follow =
          let
            val sym = Symbol.fromValue str
          in
            (case Table.find table sym of
              NONE => SYMBOL sym
            | SOME tok => tok
            )
          end

        fun action f ({ match, len, follow, self, ...}: info) (k as LEX cont) pos =
          Cons (f (match, len, follow, pos), lazy (fn () => cont follow k (pos + len)))

        fun simple tok ({ match, len, follow, self, ...}: info) (k as LEX cont)
          pos =
          Cons (tok, lazy (fn () => cont follow k (pos + len)))

        val lex_keyword =
          action
            (fn (match, len, follow, _) => identify keywords (implode match) follow)

        val lex_number =
          action
            (fn (match, len, follow, pos) =>
              (case Int.fromString (implode match) of
                SOME n => NUM n
              | NONE =>
                  err
                    (LexError
                      { reason = "failed to lex number"
                      , pos = pos
                      , rest = Stream.toList follow
                      }
                    )
              )
            )

        val equal = simple EQUAL

        fun skip ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          #main self follow k (pos + len)

        fun eof _ _ _ =
          Cons (EOF, eager Nil)

        fun error ({follow, ...}: info) _ pos =
          err
            (LexError
              { reason = "illegal lexeme"
              , pos = pos
              , rest = Stream.toList follow
              }
            )
      end

    structure Input =
      struct

       structure Streamable = StreamStreamable
       structure Arg = Arg
      end

    structure LexMain =
      LexMainFun (Input)

    fun doLex f s = lazy (fn () => f s (Arg.LEX f) 0)

    fun lex s = doLex LexMain.main s

    fun lex_string s = Stream.toList (lex (Stream.fromList (String.explode s)))

    fun lex_file s = lex_string (TextIO.inputAll (TextIO.openIn s))
  end
