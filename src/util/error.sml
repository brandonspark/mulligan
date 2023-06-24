(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure TC = TerminalColors

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Errors that may arise during execution.
 *
 * These include errors which occur during the static analyzing of the given
 * program, structural errors like invalid file targeting, and errors which
 * may arise due to invalid states of the dynamics.
 *
 * These are collected here, and should be handled gracefully by the top-level
 * runner.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

datatype warning =
    ParseWarning of (string * string list)
  | InvalidExt of string
  | InvalidFile of string
  | LexWarning of
                {filename : string, reason : string, pos : int, rest : char list}
  | GeneralWarning of
                    { filename : string
                    , reason : string
                    , span : (int * int) option
                    }

datatype error =
    ParseError of (string * string list)
  | LexError of {reason : string, pos : int, rest : char list}
  | TypeError of {reason : string}
  | EvalError of string
  | UserError of string
  | InvalidProgramError of string

datatype signal = SigError of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val border  = "==========================================================\n"
val border2 = "----------------------------------------------------------\n\n"

fun relative_pos_of_charpos filename pos =
  let
    val instream = TextIO.openIn filename

    fun iterate line_num pos =
      case (TextIO.inputLine instream, pos) of
        (NONE, 0) => (line_num, 0)
      | (NONE, _) => raise Fail "error"
      | (SOME line, _) =>
          let val length = String.size line in
            if pos - length < 0 then
              (line_num, pos)
            else
              iterate (line_num + 1) (pos - length)
          end
    val (line_num, pos) = iterate 1 pos
  in
    (TextIO.closeIn instream; (line_num, pos))
  end

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature ERROR =
  sig

    datatype warning = datatype warning
    datatype error = datatype error
    datatype signal = datatype signal

    exception Signal of signal

    val warn : 'a -> warning -> 'a

    val err : (error -> 'a)

    val error_eq : (error * error) -> bool
    val show_error : error -> string

    val eval_err : string -> 'a
    val user_err : string -> 'a
    val prog_err : string -> 'a
    val type_err : string -> 'a

    val mk_reason : string -> string

    val surround : TerminalColors.color -> string -> string
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Error :> ERROR =
  struct
    datatype warning = datatype warning
    datatype error = datatype error
    datatype signal = datatype signal

    exception Signal of signal

    fun source (filename, pos) =
      let
        val (line_num, offset) = relative_pos_of_charpos filename pos
      in
        spf (`""fs":"fs":"fs"")
          (lightblue filename)
          (lightblue (Int.toString line_num))
          (lightblue (Int.toString offset))
      end

    fun surround color s = TC.text color border ^ s ^ TC.text color border2

    fun warn x warning =
      let
        val warning_msg =
          case warning of
            ParseWarning (filename, rest) =>
              spf (`"Parse error\n"
                       fs": Failure to produce derived file, skipping...\nRemaining filestream: "fs"\n")
                (lightblue filename)
                (lightblue
                  (String.substring
                    (String.concatWith " " rest, 0, 25))
                )
          | (InvalidExt filename) =>
              "Invalid extension\n"
              ^ lightblue filename
              ^ ": Expected .sml, .sig, .fun, or .cm extension instead.\n"

          | (InvalidFile filename) =>
              spf (`"Invalid file\n"
                        fs": Expected an extension to this file.\n")
                (lightblue filename)

          | (LexWarning {filename, reason, pos, rest}) =>
              spf (`"Lex error\n"
                       fs": "fs
                       "\nRemaining token stream: "fs"\n")
                (source (filename, pos))
                reason
                (String.concatWith " "
                    (List.map Char.toString
                      (List.take (rest, 25)))
                )
          | GeneralWarning {filename, reason, span} =>
              spf (`"\n"fs": "fs"\n")
                (case span of
                  NONE => ""
                | SOME span => source (filename, # 1 span))
                reason
      in
        ( print (surround TC.yellow (orange "Warning: " ^ warning_msg))
        ; x
        )
      end

    fun error_eq (error1, error2) =
      case (error1, error2) of
        (ParseError (s, strs), ParseError (s', strs')) =>
          s = s' andalso ListPair.allEq (op=) (strs, strs')
      | ( LexError {reason, pos, rest}
        , LexError {reason = reason', pos = pos', rest = rest'}
        ) =>
          reason = reason' andalso pos = pos' andalso ListPair.allEq (op=)
          (rest, rest')
      | (TypeError {reason}, TypeError {reason = reason'}) =>
          reason = reason'
      | (EvalError s, EvalError s')
      | (UserError s, UserError s')
      | (InvalidProgramError s, InvalidProgramError s') =>
          s = s'
      | _ => false

    fun show_error error =
      case error of
        LexError {reason, pos, ...} => lightblue (Int.toString pos) ^ ": " ^ reason
      | TypeError {reason, ...} => reason
      | ParseError (s, _) => s
      | EvalError s
      | UserError s
      | InvalidProgramError s => s


    fun err error = raise Signal (SigError error)

    fun eval_err s = raise Signal (SigError (EvalError s))
    fun user_err s = raise Signal (SigError (UserError s))
    fun prog_err s = raise Signal (SigError (InvalidProgramError s))
    fun type_err s = raise Signal (SigError (TypeError {reason = s}))

    fun mk_reason s = lightblue "Reason" ^ ": " ^ s ^ "\n"
  end
