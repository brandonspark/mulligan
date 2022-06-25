
structure Top =
  struct
    open Error

    structure TC = TerminalColors

    infix |>
    fun x |> f = f x

    fun handle_file cur_path filename ctx =
      let
        val canonical_new =
          OS.Path.mkCanonical (OS.Path.concat (cur_path, filename))

        val (new_dir, new_path) =
          if OS.Path.isAbsolute filename then
            (OS.Path.dir filename, filename)
          else
            (OS.Path.dir canonical_new, canonical_new)
      in
        ( case (String.sub (filename, 0), OS.Path.ext filename) of
            (_, NONE) =>
              warn ctx (InvalidExt new_path)
          | (#"$", _) =>
              warn ctx (GeneralWarning { filename = new_path
                                       , reason = "Anchors not yet supported."
                                       , span = NONE
                                       }
                       )
          | (_, SOME ("sml" | "sig" | "fun")) =>
              RunDebugger.eval_source
                (Source.loadFromFile (FilePath.fromUnixPath filename))
                ctx
          | (_, SOME "cm") =>
              (case CM_Parser.parse_file new_path of
                Either.INL rest => err (ParseError (new_path, rest))
              | Either.INR (((structs, sigs, functors), sources), []) =>
                  let
                    val ctx' =
                      List.foldl
                        (fn (source, ctx) =>
                          handle_file new_dir
                            ( case source of
                              CM_Token.PATH sym => Symbol.toValue sym
                            | CM_Token.STRING s => s
                            )
                            ctx
                        )
                        ctx
                        sources
                  in
                    Context.cm_export new_path ctx ctx'
                      {structs=structs, sigs=sigs, functors=functors}
                  end
              | Either.INR (_, rest) =>
                  err (ParseError (new_path, List.map CM_Token.token_to_string rest))
              )
          | _ => warn ctx (InvalidExt new_path)
        )
        handle exn => file_error_handler new_path exn
      end

    and file_error_handler path exn =
      case exn of
        Signal (SigError error) =>
          ( red "Error" ^ ": "
            ^ (case error of
                ParseError (filename, rest) =>
                  "Parse failure\n"
                  ^ lightblue filename ^ ": Cannot parse evaluated file\n"
                  ^ "Remaining tokens: " ^ lightblue (String.concatWith " " (List.take (rest, 20))) ^ "\n"
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
            |> surround TC.softred
            |> print
          ; OS.Process.exit OS.Process.failure
          )
      | ParseSMLError.Error e =>
          ( TCS.print
              (ParseSMLError.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e)
          ; if List.null (MLton.Exn.history exn) then () else
              print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") (MLton.Exn.history exn)))
          ; OS.Process.exit OS.Process.failure
          )
      | _ => raise exn

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

val _ = Top.run ()
