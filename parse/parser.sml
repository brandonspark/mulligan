
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

    structure Arg =
      struct
        open Directive

        datatype terminal = datatype DToken.t

        type int = int
        type symbol = symbol
        type directive = Directive.t

        val main = identity

        fun num_reveal i = Reveal (SOME i)
        fun num_prev i = Prev (SOME i)
        val bare_reveal = null (Reveal NONE)
        val stop = null Stop
        val step = null Step
        val run = null Run
        val prev = null (Prev NONE)
        fun break_fn s = Break s
        fun bare_clear () = Clear NONE
        fun sym_clear s = Clear (SOME s)
        fun sym_print s = Print s
        fun break_assign s = BreakAssign s
        fun change_setting (s1, s2) = Set (s1, s2)

        exception Error of DToken.t StreamStreamable.t
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
      case parse (Stream.fromList (String.explode cs)) of
        INL _ => NONE
      | INR (elems, []) => SOME elems
      | INR _ => NONE

    fun parse_exn cs =
      case parse_opt cs of
        NONE => raise Fail "failed to parse"
      | SOME ans => ans
  end
