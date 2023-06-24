(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open DToken
open Error
open Stream
structure Table = SymbolHashTable

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A lexer for debugger commands, or "directives".
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

fun revappend l1 l2 =
  (case l1 of
    x :: rest =>
      revappend rest (x :: l2)
  | [] => l2)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature LEXER =
  sig
    val lex : char Stream.stream -> DToken.t Stream.stream
    val lex_string : string -> DToken.t list
    val lex_file : string -> DToken.t list
  end

(*****************************************************************************)
(* Lexer actions *)
(*****************************************************************************)

structure Arg =
  struct
    structure Streamable = StreamStreamable

    type symbol = char
    val ord = Char.ord

    datatype tlex = LEX of char stream -> t
    withtype t = tlex -> int -> DToken.t front

    type self = { main : symbol Streamable.t -> t
                , primary : symbol Streamable.t -> t
                }
    type info = { match : symbol list,
                  len : int,
                  start : symbol Streamable.t,
                  follow : symbol Streamable.t,
                  self : self }

    val keywords_list =
      [ ("step", STEP)
      , ("s", STEP)
      , ("eval", EVALUATE)
      , ("evaluate", EVALUATE)
      , ("reveal", REVEAL)
      , ("stop", STOP)
      , ("set", SET)
      , ("prev", PREV)
      , ("breakbind", BREAKBIND)
      , ("breakfn", BREAKFN)
      , ("run", RUN)
      , ("r", RUN)
      , ("clear", CLEAR)
      , ("print", PRINT)
      , ("report", REPORT)
      , ("last", LAST)
      , ("help", HELP)
      , ("typeof", TYPEOF)
      ]

    val keywords : DToken.t Table.table = 
      let
        val table : DToken.t Table.table = Table.table 60
      
        val () =
          List.app
          (fn (str, token) => Table.insert table (Symbol.fromValue str) token)
          keywords_list
      in
        table
      end

    fun identify table str =
      let
        val sym = Symbol.fromValue str
      in
        (case Table.find table sym of
          NONE => IDENT [sym]
        | SOME tok => tok
        )
      end

    fun action f ({ match, len, follow, ...}: info) (k as LEX cont) pos =
      Cons (f (match, len, follow, pos), lazy (fn () => cont follow k (pos + len)))

    fun simple tok ({ follow, len, ...}: info) (k as LEX cont)
      pos =
      Cons (tok, lazy (fn () => cont follow k (pos + len)))

    fun enter_main ({ match, len, follow, self, ...}: info) (k as LEX _) pos =
      Cons (identify keywords (implode match), lazy (fn () => #main self follow k (pos + len)))

    val lex_bindable =
      action
        (fn (match, _, _, _) => IDENT [Symbol.fromValue (implode match)])

    fun longidentify curr store match =
      let
        fun process chars =
          Symbol.fromValue (String.implode (List.rev chars))
      in
        case match of
          [] =>
            (case curr of
              [] => IDENT (List.rev store)
            | _ => IDENT (List.rev (process curr :: store))
            )
        | #"." :: rest =>
            longidentify [] (process curr :: store) rest
        | ch :: rest =>
            longidentify (ch :: curr) store rest
      end

    val lex_longident =
      action
        (fn (match, _, _, _) => longidentify [] [] match)

    val lex_number =
      action
        (fn (match, _, follow, pos) =>
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

    fun skip ({ len, follow, self, ...} : info) (k as LEX _) pos =
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

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Lexer :> LEXER =
  struct
    structure LexMain =
      LexMainFun (structure Streamable = StreamStreamable structure Arg = Arg)

    fun doLex f s = lazy (fn () => f s (Arg.LEX f) 0)

    fun lex s = doLex LexMain.primary s

    fun lex_string s = Stream.toList (lex (Stream.fromList (String.explode s)))

    fun lex_file s = lex_string (TextIO.inputAll (TextIO.openIn s))
  end
