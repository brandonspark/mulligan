(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

type symbol = Symbol.symbol
type result = ((symbol list * symbol list * symbol list) * CM_Token.elem list) * CM_Token.token list
signature PARSER =
  sig
    val parse : char StreamStreamable.t -> (string list, result) Either.t

    val parse_string : string -> (string list, result) Either.t

    val parse_file : string -> (string list, result) Either.t

    val parse_file_to_string : string -> string
  end

structure CM_Parser :> PARSER =
  struct
    structure S = Stream

    type symbol = Symbol.symbol

    fun identity x = x
    fun null () = []
    fun sing x = [x]
    fun pair (x, y) = [x, y]

    val option_to_bool = fn
      SOME _ => true
    | NONE => false

    fun nyi () = ()
    fun assert_fvalbinds_valid _ = nyi ()
    fun assert_valbinds_valid _ _ = nyi ()
    fun assert_valid_precedence _ = nyi ()

    structure Arg =
      struct
        datatype terminal = datatype CM_Token.token

        datatype elem = datatype CM_Token.elem

        type files = elem list
        type symbol = symbol

        datatype export =
          STRUCT of symbol | SIG of symbol | FUN of symbol

        type exports = symbol list * symbol list * symbol list

        val functor_export = FUN
        val signature_export = SIG
        val structure_export = STRUCT

        fun nil_exports () = ([], [], [])
        fun cons_exports (export, (l1, l2, l3)) =
          case export of
            STRUCT s => (s :: l1, l2, l3)
          | SIG s => (l1, s :: l2, l3)
          | FUN s => (l1, l2, s :: l3)

        val nil_files = null
        val cons_files = op::

        type main = (symbol list * symbol list * symbol list) * files
        val main_prog = identity

        exception Error of CM_Token.token StreamStreamable.t
        fun error x = Error x
      end

    (* Sidestepping the NJ extension so it can parse itself. *)
    structure Input =
      struct
        structure Streamable = StreamStreamable
        structure Arg = Arg
      end

    structure ParseMain =
      ParserFun (Input)

    fun parse cs =
      let
        val (elems, stream) = ParseMain.parse (CM_Lexer.lex cs)
      in
        Either.INR (elems, Stream.toList stream)
      end
        handle Arg.Error x => Either.INL (List.map CM_Token.token_to_string
        (Stream.toList x))

    fun parse_string s = parse (Stream.fromList (String.explode s))

    fun parse_file s =
      let
        val instream = TextIO.openIn s
        val input = TextIO.inputAll instream
      in
        parse_string input
      end

    fun parse_file_to_string s =
      case parse_file s of
        Either.INL _ => raise Fail "Failed to parse!"
      | Either.INR (((l1, l2, l3), elems), _) =>
            String.concatWith ", "
            (List.map (fn s => "structure " ^ s) (List.map Symbol.toValue l1)) ^ "\n"
          ^ String.concatWith ", "
            (List.map (fn s => "signature " ^ s) (List.map Symbol.toValue l2)) ^ "\n"
          ^ String.concatWith ", "
            (List.map (fn s => "functor " ^ s) (List.map Symbol.toValue l3)) ^ "\n"
          ^ String.concatWith " " (List.map CM_Token.elem_to_string elems)
  end
