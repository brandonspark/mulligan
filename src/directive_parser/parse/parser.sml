(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

type result = Directive.t * DToken.t list
signature PARSER =
  sig
    val parse : char StreamStreamable.t -> (string list, result) Directive.either

    val parse_string : string -> (string list, result) Directive.either

    val parse_file : string -> (string list, result) Directive.either

    val parse_exn : string -> Directive.t
    val parse_opt : string -> Directive.t option
  end

structure DirectiveParser :> PARSER =
  struct
    open Directive

    structure S = Stream

    type symbol = Symbol.symbol

    fun identity x = x
    fun null x = fn () => x
    fun sing x = [x]
    fun pair (x, y) = [x, y]

    exception ParseFail of string

    structure Arg =
      struct
        open Directive

        datatype terminal = datatype DToken.t

        type int = int
        type symbol = symbol
        type longid = symbol list
        type directive = Directive.t

        val main = identity

        fun num_reveal i = Reveal (SOME i)
        fun num_prev i = Prev (SOME i)
        val bare_reveal = null (Reveal NONE)
        val stop = null Stop
        val step = null Step
        val evaluate = null Evaluate
        val run = null Run
        val prev = null (Prev NONE)
        fun break_fn s = BreakFn s
        fun bare_clear () = Clear NONE
        fun sym_clear s = Clear (SOME s)
        fun sym_print s = Print s
        fun break_bind s =
          case s of
            [s] => BreakBind s
          | _ => raise ParseFail "Cannot break bind on longid."
        fun value_ident longid =
          case longid of
            [s] => Directive.VALUE s
          | _ => raise ParseFail "Value cannot be empty or long identifier."
        fun value_num num = Directive.NUM num
        fun change_setting (s, v) =
          case s of
            [s] => Set (s, v)
          | _ => raise ParseFail "Setting cannot be empty or long identifier."
        fun report s =
          case s of
            [s] => Report s
          | _ => raise ParseFail "Cannot report on longid."
        fun bare_last () = Last NONE
        fun num_last i = Last (SOME i)
        val do_help = null Help
        val typeof_id = TypeOf

        exception Error of DToken.t StreamStreamable.t
        fun error x = Error x
      end

    structure ParseMain =
      ParserFun (structure Streamable = StreamStreamable structure Arg = Arg)

    fun parse cs =
      let
        val (elems, stream) = ParseMain.parse (Lexer.lex cs)
      in
        INR (elems, Stream.toList stream)
      end
        handle Arg.Error x => INL (List.map DToken.to_string
        (Stream.toList x))

    fun parse_string s = parse (Stream.fromList (String.explode s))

    fun parse_file s =
      let
        val instream = TextIO.openIn s
        val input = TextIO.inputAll instream
      in
        parse_string input
      end

    fun parse_opt cs =
      ( case parse (Stream.fromList (String.explode cs)) of
        INL _ => NONE
      | INR (elems, []) => SOME elems
      | INR _ => NONE
      ) handle ParseFail s =>
        ( print ("Parse failure: " ^ s ^ "\n")
        ; NONE
        )

    fun parse_exn cs =
      case parse_opt cs of
        NONE => raise Fail "failed to parse"
      | SOME ans => ans
  end
