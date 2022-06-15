
structure Top =
  struct
    open Error

    fun file_to_ast filename =
      let
        val filepath = FilePath.fromUnixPath filename
        val source = Source.loadFromFile filepath
        val ast = Parser.parse source (* may err *)
        val (derived_ast, _) = Elaborate.elab_ast (ast, Basis.initial)
      in
        derived_ast
      end

    val help_message =
    "usage: mulligan [ARGS] FILE ... FILE\n" ^
    "Command-line arguments:\n" ^
    "  --help             displays this message\n\n" ^

    "Debugger commands:\n" ^
    "  step               steps evaluation forward by one\n\
    \  step <i>           steps evaluation forward by <i>\n\
    \  prev               steps evaluation backward by one\n\
    \  prev <i>           steps evaluation backward by <i>\n\
    \  stop               exits program\n\
    \  reveal             reveals evaluation context by <print_depth> layers\n\
    \  reveal <i>         reveals evaluation context by <i> layers\n\
    \  break <id>         sets a breakpoint for when the function value bound to <id>\
                         \is invoked\n\
    \  break bind <id>    sets a breakpoint when the identifier <id> is bound to\n\
    \  clear              clears all breakpoints\n\
    \  clear <id>         clears breakpoint on function value bound to <id>\n\
    \  run                runs program until breakpoint or end of evaluation\n\
    \  last               rewinds evaluation until the last breakpoint\n\
    \  print <id>         prints value bound to identifier <id>\n\
    \  set <name> = <v>   sets the setting <name> to value <v>\n\
    \  help               displays this message\n\n" ^

    "Settings:\n" ^
    "  substitute = <b>   substitute values for identifiers in pretty printer\n\
    \  print_dec = <b>    print context until nearest val binding when stepping\n\
    \  print_depth = <i>  print evaluation context <i> layers deep when stepping\n"



    infix |>
    fun x |> f = f x

    fun println s = print (s ^ "\n")

    datatype 'a frame =
        Frame of 'a
      | Starred of 'a

    datatype run_status =
        Running
      | Stepping

    datatype prog_status =
        Step of Context.t * Location.location list * Debugger.focus
      | Finished of Context.t

    structure TC = TerminalColors

    fun text color s = TC.foreground color ^ s ^ TC.reset
    val orange = text TC.orange
    val red = text TC.red
    val lightblue = text TC.lightblue

    fun print_focus ctx location focus numopt =
      case focus of
        (Debugger.VAL (exp, _, _) | Debugger.EXP (exp, _)) =>
          PrettyPrintAst.report
            ctx
            exp
            (Option.getOpt (numopt, Context.get_print_depth ctx))
            location
          ^ "\n"
      | Debugger.PROG ast => PrettyPrintAst.pretty ctx ast true ^ "\n"

    (* The eval_file function takes in an SML file and gets us into the main loop, via
     * Perform being raised on the first expression redex.
     *)
    fun eval_file filename ctx =
      let
        val _ =
          surround TC.lightblue ("Evaluating file " ^ orange filename ^ "...\n")
          |> print

        val ast = file_to_ast filename

        val _ =
          print
            ( "Loaded program:\n"
            ^ PrettyPrintAst.pretty ctx ast true
            ^ "\n\n"
            )

        (* The step function is responsible for doing the stepping. Upon handling
         * a Perform upon the first redex, it re-enters the interactive loop
         * with the new evaluation information, pausing the evaluation.
         *)

        val store : ( Context.t
            * Location.location list
            * Debugger.focus
            ) frame list ref = ref []

        val run : run_status ref = ref Stepping

        val breaks : SMLSyntax.symbol option ref list ref = ref []

        fun handle_break (b, cont) =
          ( run := Stepping
          ; case !store of
              [] => ()
            | (Frame x)::rest => store := (Starred x) :: rest
            | (Starred _) :: _ => ()
          ; Cont.throw cont ()
          )

        fun display (ctx, location, focus) =
          print ("==> \n"
            ^ print_focus ctx location focus NONE
            ^ "\n"
            )

        (* This is my step handler.
         *
         * I never knew my real handler.
         *)
        fun step_handler (ctx, location, focus) exn =
          case exn of
            Debugger.Perform
              ( Debugger.Step { context = ctx, location, focus, stop } ) =>
                ( if stop then run := Stepping
                  else ()
                ; Step (ctx, location, focus)
                )
          | Debugger.Perform (Debugger.Break cont) => handle_break cont
          | Signal (SigError (EvalError reason)) =>
              ( red "Error" ^ ": Evaluation failure\n"
                ^ mk_reason reason
                ^ "Context:\n"
                ^ print_focus ctx location focus NONE
                |> surround TC.softred
                |> print
              ; OS.Process.exit OS.Process.failure
              )
          | Signal (SigError (UserError reason)) =>
              ( red "Error" ^ ": User-induced error\n"
                ^ mk_reason reason
                ^ "Context:\n"
                ^ print_focus ctx location focus NONE
                |> surround TC.softred
                |> print
              ; Step (ctx, location, focus)
              )
          | Signal (SigError (InvalidProgramError reason)) =>
              ( red "Error" ^ ": Invalid program\n"
                ^ mk_reason reason
                ^ "Context:\n"
                ^ print_focus ctx location focus NONE
                |> surround TC.softred
                |> print
              ; OS.Process.exit OS.Process.failure
              )
          | _ => raise exn

        fun step (ctx, location, focus) =
          let
            val _ = store := Frame (ctx, location, focus) :: !store

            val new_info =
              ( case focus of
                Debugger.VAL (_, value, cont) =>
                  Cont.throw cont value
              | Debugger.EXP (exp, cont) =>
                ( case Value.exp_to_value ctx exp of
                    SOME value =>
                      Cont.throw cont value
                  | _ => Debugger.eval location exp ctx cont
                )
              | Debugger.PROG ast =>
                  Finished (Debugger.eval_program ast ctx)
              )
              handle exn => step_handler (ctx, location, focus) exn

          in
            case new_info of
              Finished ctx =>
                ( print (orange "Program evaluation finished.\n\n")
                ; ctx
                )
            | Step info =>
                ( display info
                ; start_loop info
                )
          end

        and execute_prev (info as (ctx, location, focus)) num_opt =
          let
            fun print_report (info as (ctx, location, focus)) =
              ( display (ctx, location, focus)
              ; info
              )

            fun rewind n l =
              case (n, l) of
                ( (_, []) | (0, _) ) =>
                  (store := l; print_report info)
              | (1, (Frame info | Starred info) :: rest) =>
                  ( store := rest
                  ; print_report info
                  )
              | (_, _ :: rest) =>
                  rewind (n - 1) rest
          in
            start_loop (rewind (Option.getOpt (num_opt, 1)) (!store))
          end

        and start_loop info =
          (case !run of
            Running => step info
          | Stepping => main_loop info
          )

        (* The main loop is responsible for accepting user input and controlling
         * what is done by the program.
         *)
        and main_loop (info as (ctx, location, focus)) =
          let
            fun recur x = start_loop info
          in
          ( TextIO.output (TextIO.stdOut, "- ")
          ; TextIO.flushOut TextIO.stdOut
          ; case DirectiveParser.parse_opt (TextIO.input TextIO.stdIn) of
              SOME Directive.Step => step info
            | SOME Directive.Stop =>
                ( print (lightblue "Bye bye!\n")
                ; OS.Process.exit OS.Process.success
                )
            | SOME (Directive.Reveal i') =>
                let
                  val print_dec = #print_dec (Context.get_settings ctx)
                  val old_setting = !print_dec
                in
                  ( print_dec := false
                  ; print ("Revealing:\n"
                          ^ print_focus ctx location focus i'
                          ^ "\n")
                  ; print_dec := old_setting
                  )
                  |> recur
                end
            | SOME (Directive.Prev num_opt) => execute_prev info num_opt
            | SOME (Directive.Last num_opt) =>
                let
                  fun last l i =
                    case (l, i) of
                      ([], _) => (info, [])
                    | ([(Frame x | Starred x)], _) => (x, [])
                    | (Starred x :: rest, 1) => (x, rest)
                    | (Frame x :: rest, _) => last rest i
                    | (Starred x :: rest, _) => last rest (i - 1)

                  val (info, rest) = last (!store) (Option.getOpt (num_opt, 1))
                in
                  ( store := rest
                  ; display info
                  )
                  |> recur
                end
            | SOME (Directive.Break s) =>
                ( print ( "Breakpoint set on function value bound to "
                        ^ orange (Symbol.toValue s)
                        ^ "\n"
                        )
                ; let
                    val (ctx, broken) = Context.break_fn ctx [s] true
                    val _ = breaks := broken :: !breaks
                  in
                    start_loop info
                  end
                )
            | SOME (Directive.BreakAssign s) =>
                let
                  val break_assigns = Context.get_break_assigns ctx
                in
                  ( print ("Breaking assignment to identifier " ^ orange (Symbol.toValue s) ^ "\n")
                  ; break_assigns := SymSet.insert (!break_assigns) s
                  )
                  |> recur
                end
            | SOME Directive.Run =>
                recur (run := Running)
            | SOME (Directive.Clear NONE) =>
                ( List.map (fn broken => broken := NONE) (!breaks)
                ; breaks := []
                ; Context.get_break_assigns ctx := SymSet.empty
                ; print "All breakpoints cleared.\n"
                )
                |> recur
            | SOME (Directive.Clear (SOME sym)) =>
                ( print ("Breaking function " ^ orange (Symbol.toValue sym) ^ "\n")
                ; Context.break_fn ctx [sym] false
                )
                |> recur
            | SOME (Directive.Print sym) =>
                ( print ("Printing value of identifier " ^ orange (Symbol.toValue sym) ^ "\n")
                ; Context.get_val ctx [sym]
                  |> PrettyPrintAst.show_value ctx
                  |> PrettySimpleDoc.toString true
                  |> println
                )
                |> recur
            | SOME (Directive.Set (setting, value)) =>
                let
                  fun sym_to_bool s =
                    case Symbol.toValue s of
                      "true" => SOME true
                    | "false" => SOME false
                    | _ => NONE
                in
                  recur
                    ( case (Symbol.toValue setting, value) of
                      (_, Directive.VALUE s) =>
                        (case (Symbol.toValue setting, sym_to_bool s) of
                          (_, NONE) => print "Expected a boolean value.\n"
                        | ("substitute", SOME b) =>
                            Context.set_substitute ctx b
                        | ("print_dec", SOME b) =>
                            #print_dec (Context.get_settings ctx) := b
                        | _ => print "Unrecognized setting"
                        )
                    | ("print_depth", Directive.NUM i) =>
                        #print_depth (Context.get_settings ctx) := i
                    | _ => print "Unrecognized setting.\n"
                    )
                end
            | SOME (Directive.Report s) =>
                recur
                  ( case Symbol.toValue s of
                    "ctx" => print (PrettyPrintAst.ctx_toString ctx ^ "\n")
                  | "location" => print (PrettyPrintAst.location_toString ctx location ^ "\n")
                  | _ => print "Unrecognized report.\n"
                  )
            | SOME Directive.Help =>
                recur (print help_message)
            | NONE =>
                recur (print "Unrecognized command.\n")
          )
          end
      in
        (* This call will result in entering the interactive loop.
         *
         * This handler gets entered at any point that we checkpoint _after_ a
         * throw.
         *)
        start_loop (ctx, [Location.PROG ast], Debugger.PROG ast)
      end

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
              eval_file new_path ctx
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
              | GeneralError {filename, reason} =>
                  "\n"
                  ^ lightblue filename ^ reason ^ "\n"
              | EvalError reason =>
                  "Evaluation error\n"
                  ^ mk_reason reason
              | UserError reason =>
                  "User-induced error\n"
                  ^ mk_reason reason
              | InvalidProgramError reason =>
                  "Invalid program\n"
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
          ( print help_message
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
