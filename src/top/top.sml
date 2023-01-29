(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open Error
structure TC = TerminalColors

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The top-level entry point for the `mulligan` program.
 * 
 * This is where everything starts.
 * 
 * Here, we first handle all the command-line arguments before dispatching to
 * the actual interactive loop.
 *)

(*****************************************************************************)
(* File-handling logic *)
(*****************************************************************************)

(* This function deals with recursing into all the dependencies of a CM
 * file, or actually evaluating the source text of an SML program.
 *)
fun handle_file cur_path filename ctx =
  let
    val filename =
      if OS.Path.isAbsolute filename then
        filename
      else 
        OS.Path.mkCanonical (OS.Path.concat (cur_path, filename))

    val dir = OS.Path.dir filename
  in
    ( case (String.sub (filename, 0), OS.Path.ext filename) of
        (_, NONE) =>
          warn ctx (InvalidExt filename)
      | (#"$", _) =>
          warn ctx (GeneralWarning { filename = filename 
                                   , reason = "Anchors not yet supported."
                                   , span = NONE
                                   }
                   )
      (* Given an `sml`, `sig`, or `fun` file, then we run the debugger!
       *)
      | (_, SOME ("sml" | "sig" | "fun")) =>
          RunDebugger.eval_source
            (Source.loadFromFile (FilePath.fromUnixPath filename))
            ctx
      (* On a `cm` file, we transitively parse the dependencies, and handle 
       * the files described within. 
       *)
      | (_, SOME "cm") =>
          (case CM_Parser.parse_file filename of
            INL rest => err (ParseError (filename, rest))
          | INR (((structs, sigs, functors), sources), []) =>
              let
                val ctx' =
                  List.foldl
                    (fn (source, ctx) =>
                      handle_file dir 
                        ( case source of
                          CM_Token.PATH sym => Symbol.toValue sym
                        | CM_Token.STRING s => s
                        )
                        ctx
                    )
                    ctx
                    sources
              in
                Context.cm_export filename ctx ctx'
                  {structs=structs, sigs=sigs, functors=functors}
              end
          | Either.INR (_, rest) =>
              err (ParseError (filename, List.map CM_Token.token_to_string rest))
          )
      | _ => warn ctx (InvalidExt filename)
    )
    handle exn => file_error_handler filename exn
  end

and file_error_handler path exn =
  case exn of
    Signal (SigError error) =>
      let 
        val error_msg =
          surround TC.softred <|
            (case error of
              ParseError (filename, rest) =>
                "Parse failure\n"
                ^ lightblue filename ^ ": Cannot parse evaluated file\n"
                ^ "Remaining tokens: " ^ lightblue (String.concatWith " " (List.take (rest, 20))) ^ "\n"
            (* TODO: why do I not use the reason here? *)
            | LexError {reason, pos, rest} =>
                "Lex failure\n"
                ^ lightblue path ^ ": Cannot lex evaluated file\n"
                ^ "Remaining tokens: "
                ^ lightblue (String.concatWith " " (List.map Char.toString (List.take (rest, 20)))) ^ "\n"
            | EvalError reason =>
                "Evaluation error\n"
                ^ mk_reason reason
            | UserError reason =>
                "User-induced error\n"
                ^ mk_reason reason
            | InvalidProgramError reason =>
                "Invalid program\n"
                ^ lightblue path ^ ": Error handling file\n"
                ^ mk_reason reason
            | TypeError {reason} =>
                "Type error\n"
              ^ mk_reason reason
            )
      in
        ( print <| red "Error" ^ ": " ^ error_msg
        ; OS.Process.exit OS.Process.failure
        )
      end
  (* I copy pasted this code.
   * Hopefully it doesn't happen. If it does, then I may have to update the parser.
   *)
  | ParseSMLError.Error e =>
      ( TCS.print
          (ParseSMLError.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e)
      ; if List.null (MLton.Exn.history exn) then () else
          print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") (MLton.Exn.history exn)))
        ; OS.Process.exit OS.Process.failure
        )
    | _ => raise exn

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Top =
  struct
    fun run () =
      let
        val args = CommandLineArgs.positional ()
        val doHelp = CommandLineArgs.parseFlag "help"
      in
        if doHelp orelse List.null args then
          ( print RunDebugger.help_message
          ; OS.Process.exit OS.Process.success
          )
        else
          ( List.foldl
              (fn (filename, ctx) =>
                handle_file "" filename ctx
              )
              Basis.initial
              args
          ; OS.Process.exit OS.Process.success
          )
      end

  end

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* let's go! *)
val _ = Top.run ()
